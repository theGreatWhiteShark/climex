### All functions and modules in charge of fitting a GEV or GP function to
### the provided time series in the Climex app.

##' @title Selection input to determine the fitting routine/method 
##'
##' @family climex-fitting
##'
##' @import shiny
##' 
##' @return selectInput
##' @author Philipp Mueller 
generalFittingRoutineInput <- function(){
  selectInput( "selectOptimization", "Fitting routine",
              choices = c( "Nelder-Mead", "CG", "BFGS", "SANN",
                          "dfoptim::nmk" ),
              selected = c( "Nelder-Mead" ) )
}

##' @title Function to perform the GEV/GP fit within the Climex app.
##' @details This function does not wait for the initialization of its
##' slider, checkbox etc. inputs. This way the fit can be performed with its
##' default settings in the leaflet tab without switching to the General tab
##' first.
##'
##' @param x.kept Time series of class 'xts'. Removing clicked or brushed
##' values has to be done beforehand.
##' @param x.initial Initial parameters to start the fitting routine at. See
##' \code{\link{likelihood.initials}}. Default = NULL.
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param selectOptimization Character (select) input to determine which
##' optimization routine/method is going to be used when fitting the maximum
##' likelihood function of the GEV/GP distribution. The choices are given in
##' \code{\link{generalFittingRoutineInput}} and the default value is set to
##' "Nelder-Mead".
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param checkBoxRerun Logical (checkbox) input from the Likelihood tab.
##' It determines whether or not to start the optimization at the results
##' of the first run again to escape local minima.
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point.
##'
##' @family climex-fitting
##'
##' @return Object of class 'climex.fit.gev' or 'climex.fit.gpd', depending
##' on the choice in input$radioEvdStatistics.
##' @author Philipp Mueller 
fit.interactive <- function( x.kept, x.initial = NULL,
                            radioEvdStatistics, selectOptimization,
                            buttonMinMax, checkBoxRerun, sliderThreshold ){
  ## Don't wait for initialization here or the summary statistic table in
  ## the leaflet tab will be only available after switching to the General
  ## tab and back.
  if ( is.null( radioEvdStatistics() ) || radioEvdStatistics() == "GEV" ){
    model <- "gev"
  } else {
    model <- "gpd"
  }
  ## When considering the minima instead of the maxima x*(-1)
  ## will be fitted and the location parameter will be multiplied
  ## by -1 afterwards
  if ( ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" ) &&
       ( model == "gev" ) ){
    x.kept <- x.kept*( -1 )
    if ( !is.null( x.initial ) )
      x.initial[ 1 ] <- -1* x.initial[ 1 ]
  }
  ## Check whether the supplied initial parameter combination can
  ## still be evaluated. This might fail, e.g. when changing between
  ## time series or minimal und maximal extremes.
  if ( is.nan( climex::likelihood( x.initial, x.kept, model = model ) ) ){
    shinytoastr::toastr_warning(
                     "Initial parameters can not be evaluated. They have been reseted during the fitting procedure!" )
    x.initial <- NULL
  } 
  if ( is.null( x.initial ) ){
    x.initial <- climex::likelihood.initials( x.kept, model = model )
  } else {
    ## While changing the EVD statistics from "GEV" to "GP" the initial
    ## parameter combination has to be reset. This is nevertheless a
    ## little bit problematic since both reactive.fitting() and
    ## initial.parameters() are labeled dirty during this procedure. So
    ## one can not really control which is evaluted first. But since the
    ## reseting of initial.parameters() would result in the default
    ## setting, we are save to use it in here too.
    if ( model == "gev" && length( x.initial ) != 3 )
      x.initial <- NULL
    if ( model == "gpd" && length( x.initial ) != 2 )
      x.initial <- NULL
  }
  if ( model == "gev" ){
    ## Fits of GEV parameters to blocked data set
    x.fit.evd <- suppressWarnings( switch(
        selectOptimization(),
        "Nelder-Mead" = climex::fit.gev( x.kept, initial = x.initial,
                                        rerun = checkBoxRerun(),
                                        method = "Nelder-Mead",
                                        error.estimation = "none" ),
        "CG" = climex::fit.gev( x.kept, initial = x.initial,
                               rerun = checkBoxRerun(),
                               method = "CG", error.estimation = "none" ),
        "BFGS" = climex::fit.gev( x.kept, initial = x.initial,
                                 rerun = checkBoxRerun(),
                                 method = "BFGS",
                                 error.estimation = "none" ),
        "SANN" = climex::fit.gev( x.kept, initial = x.initial,
                                 rerun = checkBoxRerun(),
                                 method = "SANN",
                                 error.estimation = "none" ),
        "dfoptim::nmk" = climex::fit.gev( x.kept, initial = x.initial,
                                         rerun = checkBoxRerun(),
                                         method = "nmk",
                                         error.estimation = "none" ),
        NULL = climex::fit.gev( x.kept, initial = x.initial,
                               method = "Nelder-Mead",
                               rerun = checkBoxRerun(),
                               error.estimation = "none" ) ) ) 
  } else {
    ## Fits of GPD parameters to blocked data set
    if ( is.null( sliderThreshold() ) ){
      threshold <- max( x.kept )* .8
    } else {
      threshold <- sliderThreshold()
    }
    ## There will be a warning since the total length of the original time
    ## series was not provided and the return level con not be given in
    ## years but in number of observations. But since I do not use the
    ## return levels of this object anyway, I just suppress those warnings
    x.fit.evd <- suppressWarnings( switch(
        selectOptimization(),
        "Nelder-Mead" = climex::fit.gpd( x.kept, initial = x.initial,
                                        threshold = threshold,
                                        rerun = checkBoxRerun(),
                                        method = "Nelder-Mead",
                                        error.estimation = "none" ),
        "CG" = climex::fit.gpd( x.kept, initial = x.initial, 
                               threshold = threshold,
                               rerun = checkBoxRerun(),
                               method = "CG",
                               error.estimation = "none" ),
        "BFGS" = climex::fit.gpd( x.kept, initial = x.initial,
                                 threshold = threshold,
                                 rerun = checkBoxRerun(),
                                 method = "BFGS",
                                 error.estimation = "none" ),
        "SANN" = climex::fit.gpd( x.kept, initial = x.initial,
                                 threshold = threshold,
                                 rerun = checkBoxRerun(),
                                 method = "SANN",
                                 error.estimation = "none" ),
        "dfoptim::nmk" = climex::fit.gpd( x.kept, initial = x.initial,
                                         threshold = threshold,
                                         rerun = checkBoxRerun(),
                                         method = "nmk",
                                         error.estimation = "none" ),
        NULL = climex::fit.gpd( x.kept, initial = x.initial,
                               threshold = threshold,
                               rerun = checkBoxRerun(),
                               method = "Nelder-Mead",
                               error.estimation = "none" ) ) )
  }
  if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" &&
         radioEvdStatistics() == "GEV" ){
    x.fit.evd$x <- x.fit.evd$x* ( -1 )
    x.fit.evd$par[ 1 ] <- x.fit.evd$par[ 1 ]* ( -1 )
  }
  return( x.fit.evd )
}

## Fitting of the time series selected via a click on the map or
  ## the select form in the sidebar.
  ## For this time series it is possible to exclude individual points
## via clicking on them in the time series::remaining plot
##' @title Reactive value performing the GEV/GP fit.
##' @details The fit is performed using the fit.interactive function. Using
##' the reactive.rows input specific points of the time series can be
##' removed.
##'
##' @param reactive.extreme Reactive value returning a list containing
##' three elements: 1. the blocked time series, 2. the deseasonalized time
##' series, and 3. the pure time series.
##' @param initial.parameters Reactive value holding the initial parameters
##' to start the time series fit at. \code{\link{data.initial}}. Those can
##' be specified in the top right box of the Likelihood tab.
##' @param reactive.rows Reactive value holding a logical vector indicating
##' which values of the time series provided by \code{\link{data.extremes}}
##' to use after clicking and brushing.
##' @param fit.interactive Function used to perform the actual GEV/GP
##' fit. \code{\link{fit.interactive}}
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param selectOptimization Character (select) input to determine which
##' optimization routine/method is going to be used when fitting the
##' maximum likelihood function of the GEV/GP distribution. The choices
##' are given in \code{\link{generalFittingRoutineInput}} and the default
##' value is set to "Nelder-Mead".
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param checkboxRerun Logical (checkbox) input from the Likelihood tab.
##' It determines whether or not to start the optimization at the results
##' of the first run again to escape local minima.
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point.
##' 
##' @import shiny
##'
##' @family climex-fitting
##'
##' @return Reactive value holding a fitted object of class
##' 'climex.fit.gev' or 'climex.fit.gpd', depending on the choice of
##' input$radioEvdStatistics
##' @author Philipp Mueller 
data.fitting <- function( reactive.extreme, initial.parameters,
                         reactive.rows, fit.interactive,
                         radioEvdStatistics, selectOptimization,
                         buttonMinMax, checkboxRerun, sliderThreshold ){
  reactive( {
    if ( is.null( reactive.extreme() ) ||
         is.null( initial.parameters() ) ||
         is.null( reactive.rows ) ){
      return( NULL )
    }
    x.extreme <- reactive.extreme()[[ 1 ]]
    x.initial <- initial.parameters()
    ## Removing all points marked by clicking or brushing in the ggplot2
    ## plot of the extreme events in the bottom right box in the General
    ## tab
    x.kept <- x.extreme[ reactive.rows$keep.rows ]
    return( fit.interactive( x.kept, x.initial, radioEvdStatistics,
                            selectOptimization, buttonMinMax,
                            checkboxRerun,
                            sliderThreshold ) )
  } )
}
