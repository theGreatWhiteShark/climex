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
