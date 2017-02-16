### Mostly functions associated with preprocessing the individual time
### series. Most of those steps can be controlled via the General tab

##' @title Extract the extreme events from a given time series.
##' @details Provides the shinydashboard::menuItemOutput for \code{\link{
##' generalExtremeExtraction}}. See the later one for details.
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family preprocessing
##'
##' @return menuItemOutput
##' @author Philipp Mueller 
generalExtremeExtractionInput <- function(){
  menuItemOutput( "generalExtremeExtraction" )
}

##' @title Extract the extreme events from a given time series.
##' @details Provides a slider input to determine either the block length
##' (in case of the GEV distribution) or the height of the threshold (GP)
##' 
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param deseasonalize.interactive Function used to remove seasonality
##' from a given time series. \code{link{deseasonalize.interactive}}
##' @param selectDeseasonalize Character (select) input determining which
##' deseasonalization method should be used to remove the short-range
##' correlations from the provided time series.
##' \code{\link{deseasonalizeInput}}
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param reactive.selection Reactive value contains the xts type time
##' series of the individual station/input chosen via the sidebar or the
##' leaflet map. \code{\link{data.selection}}
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "input", "DWD", "artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##' 
##' @import shiny
##'
##' @family preprocessing
##'
##' @return renderMenu
##' @author Philipp Mueller
generalExtremeExtraction <- function( radioEvdStatistics,
                                     deseasonalize.interactive,
                                     selectDeseasonalize,
                                     buttonMinMax, reactive.selection,
                                     selectDataBase ){
  renderMenu( {
    x.xts <- reactive.selection()
    if ( !is.null( radioEvdStatistics() ) &&
         radioEvdStatistics() == "GEV" ){
      isolate( {
        ## I do not want the blocklength to be reset when changing
        ## the deseasonalization method.
        x.deseasonalized <- deseasonalize.interactive(
            x.xts, selectDeseasonalize, selectDataBase )
      } )
    } else {
      x.deseasonalized <- deseasonalize.interactive(
          x.xts, selectDeseasonalize, selectDataBase )
    }
    if ( is.null( x.deseasonalized ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( radioEvdStatistics() == "GEV" ){
      sliderInput( "sliderBlockLength", "Box length in days", 1,
                  365*3, 365 )
    } else {
      if ( is.null( buttonMinMax() ) ||
           buttonMinMax() == "Max" ){
        sliderInput( "sliderThreshold", "Threshold:",
                    round( min( x.deseasonalized, na.rm = TRUE ) ),
                    round( max( x.deseasonalized, na.rm = TRUE ) ),
                    round( 0.8* max( x.deseasonalized, na.rm = TRUE ) ) )
      } else {
        sliderInput( "sliderThreshold", "Threshold:",
                    round( min( x.deseasonalized, na.rm = TRUE ) ),
                    round( max( x.deseasonalized, na.rm = TRUE ) ),
                    round( 0.8* min( x.deseasonalized, na.rm = TRUE ) ) )
      }
    }
  } )
}

##' @title Function to get rid of artifacts within the Climex app
##' @details The app does two things: First it replaces all -999 and NaN
##' in the time series by NaN (the former is the default indicator in
##' the data of the German weather service for missing values).
##' Second it removes all incomplete years (GEV) or cluster (GP) when
##' the corresponding checkbox is checked.
##'
##' @param x.xts Time series of class 'xts' which has to be cleaned.
##' @param checkBoxIncompleteYears Logical (checkbox) input determining
##' whether to remove all incomplete years of a time series. This box
##' will be only available if input$radioEvdStatistics == "GEV" and else
##' will be NULL.
##' @param checkBoxDecluster Logical (checkbox) input determining
##' whether to remove all clusters in a time series and replace them by
##' their maximal value. This box will be only available if
##' input$radioEvdStatistics == "GP" and else will be NULL.
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point. This one
##' is only used in declustering the time series.
##'
##' @family preprocessing
##'
##' @return Time series of class 'xts'.
##' @author Philipp Mueller 
cleaning.interactive <- function( x.xts, checkBoxIncompleteYears,
                                 checkBoxDecluster, sliderThreshold ){
    x.xts[ which( is.na( x.xts ) ) ] <- NaN
    x.xts[ which( x.xts == -999 ) ] <- NaN
    if ( !is.null( checkBoxIncompleteYears() ) &&
         checkBoxIncompleteYears() ){
        ## Remove all incomplete years from time series
      x.xts <- climex::remove.incomplete.years( x.xts )
    }
    if ( !is.null( checkBoxDecluster() ) &&
         checkBoxDecluster() ){
      x.xts <- climex::decluster( x.xts, sliderThreshold() )
    }
    if ( any( is.nan( x.xts ) ) )
      shinytoastr::toastr_warning(
                       "The current time series contains missing values. Please be sure to check 'Remove incomplete years' in the sidebar to avoid wrong results!" )
    return( x.xts )
}

##' @title Removing the seasonality.
##' @details Not a real shiny module, since I have to use this select
##' input outside its namespace.
##'
##' @import shiny
##'
##' @family preprocessing
##'
##' @return selectInput
##' @author Philipp Mueller 
deseasonalizeInput <- function(){
  selectInput( "selectDeseasonalize", "Deseasonalization method",
              choices = c( "Anomalies", "stl", "decompose",
                          "deseasonalize::ds", "none" ),
              selected = "Anomalies" )
}

##' @title Function for removing the seasonality of a given time series
##' within the Climex app.
##'
##' @param x.xts Time series of class 'xts' which has to be cleaned.
##' @param selectDeseasonalize Character (select) input determining which
##' deseasonalization method should be used to remove the short-range
##' correlations from the provided time series.
##' \code{\link{deseasonalizeInput}}
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "input", "DWD", "artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##'
##' @family preprocessing
##'
##' @return Time series of class 'xts'.
##' @author Philipp Mueller 
deseasonalize.interactive <- function( x.xts, selectDeseasonalize,
                                      selectDataBase ){
    if ( is.null( x.xts ) || is.null( selectDeseasonalize() ) ||
         is.null( selectDataBase() ) ){
      ## if the initialization has not finished yet just wait a little
      ## longer
      return( NULL )
    }
    if ( selectDataBase() == "artificial data" ){
      ## For the artificial data there is no need for deseasonalization
      shinytoastr::toastr_info( "Since there is no seasonality in the artificial time series, the choice of deseasonalization method won't affect them at all." )
      return( x.xts )
    }
    ## Removing all NaN or most algorithms won't work. But anyway. Just
    ## removing the values won't make then run correctly. But the user
    ## is warned to remove the incomplete years.
    if ( any( is.na( x.xts ) ) ){
      x.no.nan <- na.omit( x.xts )
    } else {
      x.no.nan <- x.xts
    }
    x.deseasonalized <- switch(
        selectDeseasonalize(),
        "Anomalies" = climex::anomalies( x.xts ),
        "decompose" = {
          x.decomposed <-
            stats::decompose(
                       stats::ts( as.numeric( x.no.nan ),
                                 frequency = 365.25 ) )
          if ( any( is.nan( x.xts ) ) ){
            ## Adjusting the length of the results by adding the NaN
            ## again
            x.aux <- rep( NaN, length( x.xts ) )
            x.aux[ which( x.xts %in% x.no.nan ) ] <-
              as.numeric( x.decomposed$seasonal )
          } else {
            x.aux <- as.numeric( x.decomposed$seasonal )
          }
          x.xts - x.aux
        },
        "stl" = {
          x.decomposed <- stats::stl(
                                     stats::ts( as.numeric( x.no.nan ),
                                               frequency = 365.25 ),
                                     30 )
          if ( any( is.nan( x.xts ) ) ){
            ## Adjusting the length of the results by adding
            ## the NaN again
            x.aux <- rep( NaN, length( x.xts ) )
            x.aux[ which( x.xts %in% x.no.nan ) ] <- as.numeric(
                x.decomposed$time.series[ , 1 ] )
          } else
            x.aux <- as.numeric( x.decomposed$time.series[ , 1 ] )
            x.xts - x.aux }, 
        "deseasonalize::ds" = {
          x.ds <- deseasonalize::ds( x.no.nan )$z
          if ( any( is.nan( x.xts ) ) ){
            ## Adjusting the length of the results by adding
            ## the NaN again
            x.aux <- rep( NaN, length( x.xts ) )
            x.aux[ which( x.xts %in% x.no.nan ) ] <-
              as.numeric( x.ds )
          } else {
            x.aux <- as.numeric( x.ds )
          }
          xts( x.aux, order.by = index( x.xts ) ) 
        },
        "none" = x.xts )
    if ( is.na( max( x.deseasonalized ) ) ){
      ## I don't wanna any NaN in my time series. In some cases the
      ## deseasonalization methods themselves produce them. It's a
      ## dirty solution, but just omitting them and informing the user
      ## will work for now.
      x.deseasonalized <- na.omit( x.deseasonalized )
      shinytoastr::toastr_error(
                       "NaNs produced during the deseasonalization." )
    }
    return( x.deseasonalized )
}

##' @title Function to extract the extreme event from a time series.
##' @details If the input$radioEvdStatistics is set to "GEV" the time
##' series will be block. If it's on the other hand set to "GP", all
##' values above a certain threshold will be extracted.
##' 
##' @param x.xts Time series of class 'xts' which has to be cleaned.
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param sliderBlockLength Numerical (slider) input determining the
##' block length used in the GEV flavor of extreme value theory. On
##' default it is set to one year.
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point.
##' @param checkBoxDecluster Logical (checkbox) input determining
##' whether to remove all clusters in a time series and replace them by
##' their maximal value. This box will be only available if
##' input$radioEvdStatistics == "GP" and else will be NULL.
##'
##' @family preprocessing
##'
##' @return Time series of class 'xts'.
##' @author Philipp Mueller 
extremes.interactive <- function( x.xts, buttonMinMax,
                                 radioEvdStatistics, sliderBlockLength,
                                 sliderThreshold, checkBoxDecluster ){
  ## Toggle if maxima of minima are going to be used
  if ( is.null( buttonMinMax() ) || buttonMinMax() == "Max" ){
    block.mode <- "max"
  } else
    block.mode <- "min"
  if ( is.null( radioEvdStatistics() ) ){
    ## While initialization input$radioEvdStatistics and
    ## input$sliderBoxLength are NULL. Therefore this is the
    ## fallback default x.block
    x.block <- climex::block( x.xts, separation.mode = "years",
                             block.mode = block.mode )
  } else if ( radioEvdStatistics() == "GEV" ){
    x.block <- climex::block( x.xts, block.length = sliderBlockLength(),
                             block.mode = block.mode )
  } else if ( radioEvdStatistics() == "GP" ){
    ## Due to "historical" reasons the vector containing the
    ## resulting values will still be called x.block. The
    ## "block.mode" and the corresponding "input$buttonMinMax"
    ## are still use full and decide if the values above or below
    ## the threshold are going to be extracted
    x.block <- climex::threshold( x.xts,
                                 threshold = sliderThreshold(),
                                 decluster = checkBoxDecluster(),
                                 na.rm = TRUE )
    return( x.block )
  }
}
