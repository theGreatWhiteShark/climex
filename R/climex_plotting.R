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

##' @title TabBox containing a plot of the pure and deseasonalized time
##' series as well as one of all its extreme events.
##' @details This function provides the ouput to
##' \code{\link{generalTimeSeriesPlot}}. Both the pure time series and the
##' deseasonalized one are rendered using the dygraphs package. The extreme
##' events are rendered using the ggplot2 package.
##'
##' @param id Namespace prefix
##'
##' @family climex-plot
##'
##' @import shiny
##' @importFrom shinydashboard tabBox
##' @importFrom dygraphs dygraphOutput
##'
##' @return tabBox
##' @author Philipp Mueller 
generalTimeSeriesPlotOutput <- function( id ){
  # Create a namespace function using the provided id
  ns <- NS( id )
  tabBox( title = h2( "Time series" ),
              selected = "Remaining", width = 9, id = "boxTimeSeries",
              tabPanel( "Pure",
                dygraphOutput( ns( "plotTimeSeries" ), height = 250 ) ),
              tabPanel( "Deseasonalized",
                dygraphOutput( ns( "plotDeseasonalized" ), height = 250 ) ),
              tabPanel( "Remaining",
                plotOutput( ns( "plotBlocked" ), height = 250,
                  click = ns( "plotBlockedClick" ),
                  brush = brushOpts( id = ns( "plotBlockedBrush" ) ) ),
                actionButton( ns( "excludeBlockedReset" ), "Reset" ),
                actionButton( ns( "excludeBlockedToggle" ), "Brush" ) ) )
}

##' @title TabBox containing a plot of the pure and deseasonalized time
##' series as well as one of all its extreme events.
##' @details This function provides the ouput to
##' \code{\link{generalTimeSeriesPlot}}. Both the pure time series and the
##' deseasonalized one are rendered using the dygraphs package. The extreme
##' events are rendered using the ggplot2 package. For the later one
##' clicking or brushing points enables the user to exclude them. This is
##' handled by a reactive value returned by this function.
##'
##' @param input Namespace input. For more details check out
##' \link{ \url{ http://shiny.rstudio.com/articles/modules.html } }
##' @param output Namespace output.
##' @param session Namespace session.
##' @param reactive.extreme Reactive value containing a list of the
##' extracted extreme events of a time series, the deseasonalized and the
##' pure version of this very time series. All three elements are provided
##' as class 'xts'.
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
##' @param function.get.y.label Function determining the label of the y axis
##' in all three plots. \code{\link{function.get.y.label}}. You might wonder
##' why I am not just referring to the package internal function right away
##' instead of requiring it as an input. Well, this way the whole app
##' becomes way more easy to customize.
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point.
##'
##' @family climex-plot
##'
##' @import shiny
##' @import dygraphs
##'
##' @return Reactive value holding a logical vector indicating which values
##' of the time series provided by \code{\link{data.extremes}} to use after
##' clicking and brushing.
##' @author Philipp Mueller 
generalTimeSeriesPlot <- function( input, output, session,
                                  reactive.extreme, selectDataBase,
                                  selectDataType, function.get.y.label,
                                  radioEvdStatistics, sliderThreshold ){
  observe({
    if ( is.null( reactive.extreme() ) ){
      return( NULL )
    }
  }, priority = 1 )
  ## General color definitions throughout the entire app.
  colour.ts <- grDevices::rgb( 0.098, 0.098, 0.44 )
  colour.extremes <- grDevices::rgb( 1, 0.55, 0 )
  colour.ts.light <- "#7171EC"
  colour.extremes.light <- grDevices::rgb( 1, 0.9, 0.8 )
  ## Reactive logical value determining whether or not to use a specific
  ## element in the reactive.extreme xts vector. It will be updated
  ## whenever the users uses clicking of brushing in the extreme's ggplot2
  ## plot within the General tab
  reactive.rows <- reactiveValues( keep.rows = NULL )
  observe( {
    x.data <- reactive.extreme()
    ## use the x.extreme variable to update the reactive value
    ## keep.row (containing a listing of all the points of the
    ## actual time series which are used during the fitting procedure)
    reactive.rows$keep.rows <- rep( TRUE, length( x.data[[ 1 ]] ) )
  }, priority = 1 ) # or else the plot functions are reached beforehand
  ## Toggle points that are clicked
  observeEvent( input$plotBlockedClick, {
    x.extreme <- reactive.extreme()[[ 1 ]]
    df.block <- data.frame( date = index( x.extreme ),
                           value = as.numeric( x.extreme ) )
    result <- nearPoints( df.block, input$plotBlockedClick,
                         allRows = TRUE )
    reactive.rows$keep.rows <- xor( reactive.rows$keep.rows,
                                     result$selected_ ) } )
  ## Toggle points that are brushed
  observeEvent( input$excludeBlockedToggle, {
    x.extreme <- reactive.extreme()[[ 1 ]]
    df.block <- data.frame( date = index( x.extreme ),
                           value = as.numeric( x.extreme ) )
    result <- brushedPoints( df.block, input$plotBlockedBrush,
                            allRows = TRUE )
    reactive.rows$keep.rows <- xor( reactive.rows$keep.rows,
                                     result$selected_ ) } )
  ## Reset plot
  observeEvent( input$excludeBlockedReset, {
    x.extreme <- reactive.extreme()[[ 1 ]]
    reactive.rows$keep.rows <- rep( TRUE, length( x.extreme ) ) } )
  
  ## Pure time series 
  output$plotTimeSeries <- renderDygraph( {
    x.data <- reactive.extreme( )
    x.extreme <- x.data[[ 3 ]][ which( index( x.data[[ 3 ]] ) %in%
                                       index( x.data[[ 1 ]] ) ) ]
    plot.extremes <- x.data[[ 3 ]]
    plot.extremes[ !index( x.data[[ 3 ]] ) %in% index( x.extreme ) ] <- NA
    y.label <- function.get.y.label( selectDataBase, selectDataType )
    bind.dy <- cbind( x.data[[ 3 ]], plot.extremes )
    names( bind.dy ) <- c( "pure ts", "annual maxima" )        
    dygraph( bind.dy, ylab = y.label ) %>%
      dySeries( "pure ts", color = colour.ts ) %>%
      dySeries( "annual maxima", color = colour.extremes,
               drawPoints = TRUE,
               strokeWidth = 0, pointSize = 2 ) } )
  ## deseasonalized time series
  output$plotDeseasonalized <- renderDygraph( {
    x.data <- reactive.extreme()
    x.extreme <- x.data[[ 2 ]][ which( index( x.data[[ 2 ]] ) %in%
                                       index( x.data[[ 1 ]] ) ) ]
    plot.extremes <- x.data[[ 2 ]]
    plot.extremes[ !index( x.data[[ 2 ]] ) %in%
                  index( x.extreme ) ] <- NA
    y.label <- function.get.y.label( selectDataBase, selectDataType )
    bind.dy <- cbind( x.data[[ 2 ]], plot.extremes )
    names( bind.dy ) <- c( "deseasonalized ts", "annual maxima" )        
    dygraph( bind.dy, ylab = y.label ) %>%
      dySeries( "deseasonalized ts", color = colour.ts ) %>%
      dySeries( "annual maxima", color = colour.extremes,
               drawPoints = TRUE,
               strokeWidth = 0, pointSize = 2 ) } )
  ## Using ggplot2 for an interactive excluding of points in x.block.
  output$plotBlocked <- renderPlot( {
    x.extreme <- reactive.extreme()[[ 1 ]]
    ## In general just checking for the input$sliderThreshold to be not NULL
    ## would be sufficient and there is no real need for
    ## input$radioEvdStatistics. But why outsmart ourselves?
    if ( radioEvdStatistics() == "GP" && !is.null( sliderThreshold() ) ){
      x.extreme <- x.extreme + sliderThreshold()
    }
    x.kept <- x.extreme[ reactive.rows$keep.rows ]
    x.excluded <- x.extreme[ !reactive.rows$keep.rows ]
    plot.kept <- data.frame( date = index( x.kept ),
                            value = as.numeric( x.kept ) )
    plot.excluded <- data.frame( date = index( x.excluded ),
                                value = as.numeric( x.excluded ) )
    y.label <- function.get.y.label( selectDataBase, selectDataType )
    ggplot() + geom_line( data = plot.kept, aes( x = date, y = value ),
                         colour = colour.ts ) +
      geom_point( data = plot.kept, aes( x = date, y = value ),
                 colour = colour.extremes,
                 fill = colour.extremes, size = 2, shape = 21 ) +
      geom_point( data = plot.excluded, aes( x = date, y = value ),
                 colour = colour.extremes,
                 fill = "white", size = 2, shape = 21 ) +
      ylab( y.label ) + theme_bw() +
      theme( axis.title = element_text( size = 17, colour = colour.ts ),
            axis.text = element_text( size = 13, colour = colour.ts ) )
  } )
  return( reactive.rows )
}
