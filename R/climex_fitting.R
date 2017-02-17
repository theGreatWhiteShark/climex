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
##' @param checkboxRerun Logical (checkbox) input from the Likelihood tab.
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
                            buttonMinMax, checkboxRerun, sliderThreshold ){
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
    ## reactive.initials() are labeled dirty during this procedure. So
    ## one can not really control which is evaluted first. But since the
    ## reseting of reactive.initials() would result in the default
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
                                        rerun = checkboxRerun(),
                                        method = "Nelder-Mead",
                                        error.estimation = "none" ),
        "CG" = climex::fit.gev( x.kept, initial = x.initial,
                               rerun = checkboxRerun(),
                               method = "CG", error.estimation = "none" ),
        "BFGS" = climex::fit.gev( x.kept, initial = x.initial,
                                 rerun = checkboxRerun(),
                                 method = "BFGS",
                                 error.estimation = "none" ),
        "SANN" = climex::fit.gev( x.kept, initial = x.initial,
                                 rerun = checkboxRerun(),
                                 method = "SANN",
                                 error.estimation = "none" ),
        "dfoptim::nmk" = climex::fit.gev( x.kept, initial = x.initial,
                                         rerun = checkboxRerun(),
                                         method = "nmk",
                                         error.estimation = "none" ),
        NULL = climex::fit.gev( x.kept, initial = x.initial,
                               method = "Nelder-Mead",
                               rerun = checkboxRerun(),
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
                                        rerun = checkboxRerun(),
                                        method = "Nelder-Mead",
                                        error.estimation = "none" ),
        "CG" = climex::fit.gpd( x.kept, initial = x.initial, 
                               threshold = threshold,
                               rerun = checkboxRerun(),
                               method = "CG",
                               error.estimation = "none" ),
        "BFGS" = climex::fit.gpd( x.kept, initial = x.initial,
                                 threshold = threshold,
                                 rerun = checkboxRerun(),
                                 method = "BFGS",
                                 error.estimation = "none" ),
        "SANN" = climex::fit.gpd( x.kept, initial = x.initial,
                                 threshold = threshold,
                                 rerun = checkboxRerun(),
                                 method = "SANN",
                                 error.estimation = "none" ),
        "dfoptim::nmk" = climex::fit.gpd( x.kept, initial = x.initial,
                                         threshold = threshold,
                                         rerun = checkboxRerun(),
                                         method = "nmk",
                                         error.estimation = "none" ),
        NULL = climex::fit.gpd( x.kept, initial = x.initial,
                               threshold = threshold,
                               rerun = checkboxRerun(),
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
##' @param reactive.initial Reactive value holding the initial parameters
##' to start the time series fit at. \code{\link{data.initials}}. Those can
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
data.fitting <- function( reactive.extreme, reactive.initials,
                         reactive.rows, fit.interactive,
                         radioEvdStatistics, selectOptimization,
                         buttonMinMax, checkboxRerun, sliderThreshold ){
  reactive( {
    if ( is.null( reactive.extreme() ) ||
         is.null( reactive.initials() ) ||
         is.null( reactive.rows$keep.rows ) ){
      return( NULL )
    }
    x.extreme <- reactive.extreme()[[ 1 ]]
    x.initial <- reactive.initials()
    ## Removing all points marked by clicking or brushing in the ggplot2
    ## plot of the extreme events in the bottom right box in the General
    ## tab
    if ( length( reactive.rows$keep.rows ) != length( x.extreme ) ){
      ## Sometime, when switching between time series, the updating of
      ## reactive.rows needs longer/is evaluated at a later step.
      ## Therefore its length does not correspond to the selected time
      ## series anymore. In such a case, just return NULL and wait for
      ## the next round (updating reactive.rows)
      return( NULL )
    }
    x.kept <- x.extreme[ reactive.rows$keep.rows ]
    return( fit.interactive( x.kept, x.initial, radioEvdStatistics,
                            selectOptimization, buttonMinMax,
                            checkboxRerun,
                            sliderThreshold ) )
  } )
}

##' @title Reactive value containing the initial parameters to start the
##' GEV/GP fit at.
##' @details The initial parameter combination can be set by the numerical
##' input in the Likelihood tab. Until that happened, just the default
##' values obtained by \code{\link{likelihood.initials}} will be used.
##'
##' @param initialLocation Numerical (numerical) input determining the
##' location parameter at which to start the fitting procedure.
##' @param initialScale Numerical (numerical) input determining the
##' scale parameter at which to start the fitting procedure.
##' @param initialShape Numerical (numerical) input determining the
##' shape parameter at which to start the fitting procedure.
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param reactive.extreme Reactive value returning a list containing
##' three elements: 1. the blocked time series, 2. the deseasonalized time
##' series, and 3. the pure time series.
##'
##' @import shiny
##'
##' @family climex-fitting
##'
##' @return Numerical vector containing three elements: location, scale
##' and shape parameter to start the GEV fit at.
##' @author Philipp Mueller 
data.initials <- function( initialLocation, initialScale, initialShape,
                          radioEvdStatistics, reactive.extreme ){
  reactive( {
    if ( is.null( initialLocation() ) || is.null( initialScale() ) ||
         is.null( initialShape() ) ){
      ## If the sliders arn't set yet, I will just use the default
      ## values
      x.extreme <- reactive.extreme()[[ 1 ]]
      if ( is.null( x.extreme ) ){
        ## if the initialization has not finished yet just wait a
        ## little longer
        return( NULL )
      }
      if ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ){
        model <- "gev"
      } else {
        model <- "gpd"
      }
      return( climex::likelihood.initials( x.extreme, model = model ) )
    } else {
      if ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ){
        return( c( initialLocation(), initialScale(),
                  initialShape() ) )
      } else {
        return( c( initialScale(), initialShape() ) )
      }
    }
  } )
}

##' @title Table to display the results of the GEV/GP fitting procedure.
##' @details Provides the UI part of \code{\link{generalFitStatistics}}
##' but is not a proper Shiny module.
##'
##' @importFrom shinydashboard box
##' @import shiny
##'
##' @family climex-fitting
##'
##' @return box
##' @author Philipp Mueller 
generalFitStatisticsTable <- function(){
  box( title = h2( "Results" ), width = 3,
      background = "orange", id = "boxGevResults",
      uiOutput( "generalFitStatistics", colHeaders = "provided" ) )
}

##' @title Table to display the results of the GEV/GP fitting procedure.
##' @details Displaying of AIC, nllh, BIC and fitted parameters as well as
##' the difference to the three last fits! (and highlight positive values
##' with with green and negative with red). This function will define
##' some global variables to hold the results of the former fits. This is
##' necessary in order to mark the progress in green or red.
##'
##' @param reactive.fitting Reactive value containing the results of the
##' fit (\code{\link{fit.gev}} or \code{\link{fit.gpd}} depending on
##' radioEvdStatistic) to the blocked time series in
##' reactive.extreme()[[ 1 ]].
##' @param reactive.extreme Reactive value containing a list of the
##' extracted extreme events of a time series, the deseasonalized and the
##' pure version of this very time series. All three elements are provided
##' as class 'xts'.
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point.
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param color.table Function adding color to the table constructed
##' within generalFitStatistics. It will replace some placeholders by
##' color tags. \code{\link{color.table}}
##'
##' @import shiny
##'
##' @family climex-fitting
##' 
##' @return renderUI containing a table
##' @author Philipp Mueller 
generalFitStatistics <- function( reactive.fitting, reactive.extreme,
                                 sliderThreshold, buttonMinMax,
                                 radioEvdStatistics, color.table ){
  ## Initialization.
  last.1.aux <- last.1.int <- current.white <- rep( 0,  )
  renderUI({
    if ( is.null( reactive.fitting() ) || is.null( reactive.extreme() ) ){
      return( NULL )
    }
    ## Define the colour for increasing or decreasing values
    ## >0, <0, normal
    css.colours <- c( "#C53100",
                     "#0D8F20" )
    x.fit.evd <- reactive.fitting()
    x.data <- reactive.extreme()
    x.extreme <- x.data[[ 1 ]]
    if ( radioEvdStatistics() == "GEV" ){
      current <- c( x.fit.evd$par[ 1 ], x.fit.evd$par[ 2 ],
                   x.fit.evd$par[ 3 ],
                   x.fit.evd$value, climex:::aic( x.fit.evd ),
                   climex:::bic( x.fit.evd ),
                   climex::return.level( x.fit.evd$par,
                                        error.estimation = "none",
                                        model = "gev" ) )
    } else {
      current <- c( 0, x.fit.evd$par[ 1 ], x.fit.evd$par[ 2 ],
                   x.fit.evd$value, climex:::aic( x.fit.evd ),
                   climex:::bic( x.fit.evd ),
                   climex::return.level(
                               x.fit.evd, error.estimation = "none",
                               threshold = sliderThreshold(),
                               model = "gpd",
                               total.length = x.data[[ 1 ]] ) )
    }
    ## Negating the return level to get the correct results for
    ## the minimum
    if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" &&
         radioEvdStatistics() == "GEV" ){
        current[ 7 ] <- ( -1 )* current[ 7 ]
    }
    ## History of the statistics
    last.3 <<- last.2
    last.2 <<- last.1
    last.1.aux <- current - last.values
    ## For the fitted parameters any deviation of more than 1
    ## percent is marked red
    for ( ll in 1 : length( x.fit.evd$par ) ){
      if ( all ( last.1.aux == 0 ) ){
        ## This happens right in the beginning on initialization
        ## The following prevents the output of coloured zeros
        last.1.int <- last.1.aux
        break
      } else if( abs( last.1.aux[ ll ] - current[ ll ] ) <
                 0.01* current[ ll ] ){
        if ( last.1.aux[ ll ] > 0 ){
          last.1.int[ ll ] <- paste0( 
              "+", as.character( format(  last.1.aux[ ll ],
                                        digits = 4 ) ),
              " ", css.colours[ 1 ] )
        } else {
          last.1.int[ ll ] <- paste( 
              as.character( format(  last.1.aux[ ll ], digits = 4 ) ),
              css.colours[ 1 ] )
        }
      } else {
        last.1.int[ ll ] <- paste(
            as.character( format(  last.1.aux[ ll ], digits = 4 ) ),
            " ", css.colours[ 2 ] )
      }
    }
    ## For the test statistic all changes to lower values are
    ## marked green
    for ( ll in ( length( x.fit.evd$par ) + 1 ) :
             length( last.1.aux ) ){
      if( last.1.aux[ ll ] > 0 ){
        last.1.int[ ll ] <- paste0(
            "+", as.character( format(  last.1.aux[ ll ],
                                      digits = 4 ) ),
            " ", css.colours[ 1 ] )
      } else if ( last.1.aux[ ll ] < 0 ){
        last.1.int[ ll ] <- paste(
            as.character( format( last.1.aux[ ll ], digits = 4 ) ),
            " ", css.colours[ 2 ] )
      } else {
        last.1.int[ ll ] <- as.character( format(  last.1.aux[ ll ],
                                                 digits = 4 ) )
      }
    }
    last.1 <<- last.1.int
    if ( all( last.values == 0 ) ){
      ## I don't want to see the statistics during the initialization
      last.1 <<- rep( 0, length( last.1 ) ) }
    last.values <<- current
    x.table <- data.frame( current = current, h_1 = last.1,
                          h_2 = last.2, h_3 = last.3,
                          row.names = c( "location", "scale",
                                        "shape", "nllh", "AIC",
                                        "BIC", "rlevel" ) )
    ## Generate a html table with the 'pander' and the 'markdown'
    ## package
    x.html.table <- markdown::markdownToHTML(
                                  text = pander::pandoc.table.return(
                                                     x.table,
                                                     style = "rmarkdown",
                                                     split.tables = Inf ),
                                  fragment.only = TRUE )
    x.color.table <- color.table( x.html.table, css.colours )
  })
}

##' @title Adds font color to a HTML table.
##'
##' @details In each element of the table where the font should be
##' colored, the corresponding color has to be added via a
##' paste( as.character( table[ x, y ] ), "#0000FF" ).
##'
##' @param x.html.table HTML table.
##' @param css.colours Character vector containing the colors in hex.
##' @param style Additional style tags for the output table.
##'
##' @family climex-fitting
##' 
##' @seealso \code{\link{pander::pandoc.table.return}} and
##' \code{\link{markdown::markdownToHTML}}
##'
##' @return Same format as input.
##' @author Philipp Mueller 
color.table <- function( x.html.table, css.colours,
                        style = "table-condensed table-bordered" ){
  x.html.table.split <- stringr::str_split( x.html.table, "\n" )[[ 1 ]]
  ids <- paste0( "\"center\"><font color='", css.colours, "'>" )
  for ( ii in 1 : length( css.colours ) ){
    locations <- grep( css.colours[[ ii ]], x.html.table.split )
    x.html.table.split[ locations ] <- gsub( css.colours[ ii ],
                                            "</font>",
                                            x.html.table.split[
                                                locations ],
                                            fixed = TRUE ) 
    x.html.table.split[ locations ] <- gsub( "\"center\">", ids[ ii ],
                                            x.html.table.split[
                                                locations ] ) }
  x.html.table <- paste( x.html.table.split, collapse = "\n" )
  Encoding( x.html.table ) <- "UTF-8"
  return( list(
      htmltools::tags$script( sprintf(
                          '$( "table" ).addClass( "table %s" );',
                          style ) ),
      htmltools::HTML( x.html.table ) ) )
}
