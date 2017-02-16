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
##' from a given time series.
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param reactive.selection Reactive value contains the xts type time
##' series of the individual station/input chosen via the sidebar or the
##' leaflet map.
##' 
##' @import shiny
##'
##' @family preprocessing
##'
##' @return renderMenu
##' @author Philipp Mueller
generalExtremeExtraction <- function( radioEvdStatistics,
                                     deseasonalize.interactive,
                                     buttonMinMax, reactive.selection ){
  renderMenu( {
    x.xts <- reactive.selection()
    if ( !is.null( radioEvdStatistics() ) &&
         radioEvdStatistics() == "GEV" ){
      isolate( {
        ## I do not want the blocklength to be reset when changing
        ## the deseasonalization method.
        x.deseasonalized <- deseasonalize.interactive( x.xts )
      } )
    } else {
      x.deseasonalized <- deseasonalize.interactive( x.xts )
    }
    if ( is.null( x.deseasonalized ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( radioEvdStatistics() == "GEV" ){
      sliderInput( "sliderBoxLength", "Box length in days", 1,
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
      
