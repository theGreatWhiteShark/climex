### All functions and modules related to the plots in the General tab

##' @title Determines the propoper label of the y label of the plots in
##' the General tab of the Climex app.
##' @details It's more or less just the input$selectDataType being
##' considered. For artificial data the label will be "temperature".
##'
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "input", "DWD", "artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##' @param selectDataType Character (select) input to determine which set
##' measurements should be used for the selected station. In case of the
##' default import of the DWD data, there are three options:
##' c( "Daily max. temp", "Daily min. temp", "Daily precipitation" ).
##' Determined by menuSelectDataType.
##'
##' @family climex-plot
##' 
##' @return Character
##' @author Philipp Mueller 
function.get.y.label <- function( selectDataBase, selectDataType ){
    if ( is.null( selectDataBase() ) ){
      y.label <- "temperature in °C"            
    } else if ( selectDataBase() == "artificial data" ){
      y.label <- "EVD sample"
    } else if ( selectDataBase() == "DWD" ){
      if ( is.null( selectDataType() ) ){
        y.label <- "temperature in °C"
      } else if ( selectDataType() == "Daily precipitation" ){
        y.label <- "precipitation in mm"
      } else {
        y.label <- "temperature in °C" }
    } else if ( selectDataBase() == "input" ){
      y.label <- "input"
    }
    return( y.label )
}
