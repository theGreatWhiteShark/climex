### All functions and modules related to the plots in the General tab

##' @title Determines the propoper label of the y label of the plots in
##' the General tab of the Climex app.
##' @details It's more or less just the input$selectDataType being
##' considered. For artificial data the label will be "temperature".
##'
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "Input", "DWD", "Artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{\link{download.data.dwd}}). The third one allows
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
      y.label <- expression( paste( "temperature in ", "", degree, "C" ) )            
    } else if ( selectDataBase() == "Artificial data" ){
      y.label <- "EVD sample"
    } else if ( selectDataBase() == "DWD" ){
      if ( is.null( selectDataType() ) ){
        y.label <- expression( paste( "temperature in ", "", degree, "C" ) ) 
      } else if ( selectDataType() == "Daily precipitation" ){
        y.label <- "precipitation in mm"
      } else {
        y.label <- expression( paste( "temperature in ", "", degree, "C" ) )
      }
    } else if ( selectDataBase() == "Input" ){
      y.label <- "Input"
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
                actionButton( ns( "excludeBlockedToggle" ), "Brush" ) ),
         height = 370 )
}

##' @title TabBox containing a plot of the pure and deseasonalized time
##' series as well as one of all its extreme events.
##' @details Both the pure time series and the
##' deseasonalized one are rendered using the dygraphs package. The extreme
##' events are rendered using the ggplot2 package. For the later one
##' clicking or brushing points enables the user to exclude them. This is
##' handled by a reactive value returned by this function.
##'
##' @param input Namespace input. For more details check out
##' \url{http://shiny.rstudio.com/articles/modules.html}
##' @param output Namespace output.
##' @param session Namespace session.
##' @param reactive.extreme Reactive value containing a list of the
##' extracted extreme events of a time series, the deseasonalized and the
##' pure version of this very time series. All three elements are provided
##' as class 'xts'.
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "Input", "DWD", "Artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{\link{download.data.dwd}}). The third one allows
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
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##'
##' @family climex-plot
##'
##' @import shiny
##' @import dygraphs
##' @import ggplot2
##'
##' @return Reactive value holding a logical vector indicating which values
##' of the time series provided by \code{\link{data.extremes}} to use after
##' clicking and brushing.
##' @author Philipp Mueller 
generalTimeSeriesPlot <- function( input, output, session,
                                  reactive.extreme, selectDataBase,
                                  selectDataType, function.get.y.label,
                                  radioEvdStatistics, sliderThreshold,
                                  buttonMinMax ){
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
    if ( is.null( x.data ) ){
      return( NULL )
    }
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
    if ( radioEvdStatistics() != "GEV" ){
      df.block$value <- df.block$value + sliderThreshold()
    }
    result <- nearPoints( df.block, input$plotBlockedClick,
                         allRows = TRUE )
    reactive.rows$keep.rows <- xor( reactive.rows$keep.rows,
                                     result$selected_ ) } )
  ## Toggle points that are brushed
  observeEvent( input$excludeBlockedToggle, {
    x.extreme <- reactive.extreme()[[ 1 ]]
    df.block <- data.frame( date = index( x.extreme ),
                           value = as.numeric( x.extreme ) )
    if ( radioEvdStatistics() != "GEV" ){
      df.block$value <- df.block$value + sliderThreshold()
    }
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
    if ( is.null( reactive.extreme() ) || is.null( reactive.rows ) ||
        is.null( buttonMinMax() ) ){
      return( NULL )
    }
    x.data <- reactive.extreme( )
    x.extreme <- x.data[[ 3 ]][ which( index( x.data[[ 3 ]] ) %in%
                                       index( x.data[[ 1 ]] ) ) ]
    plot.extremes <- x.data[[ 3 ]]
    plot.extremes[ !index( x.data[[ 3 ]] ) %in% index( x.extreme ) ] <- NA
    y.label <- function.get.y.label( selectDataBase, selectDataType )
    bind.dy <- cbind( x.data[[ 3 ]], plot.extremes )
    names( bind.dy ) <- c( "pure ts", "annual maxima" )
    if ( class( y.label ) == "expression" ){
      ## dygraphs is not able to handle neither expressions nor MathJax
      y.label <- "temperature in &deg;C"
    }
    dygraph( bind.dy, ylab = y.label ) %>%
      dySeries( "pure ts", color = colour.ts ) %>%
      dySeries( "annual maxima", color = colour.extremes,
               drawPoints = TRUE,
               strokeWidth = 0, pointSize = 2 ) } )
  ## deseasonalized time series
  output$plotDeseasonalized <- renderDygraph( {
    if ( is.null( reactive.extreme() ) || is.null( reactive.rows ) ||
        is.null( buttonMinMax() ) ){
      return( NULL )
    }
    x.data <- reactive.extreme()
    x.extreme <- x.data[[ 2 ]][ which( index( x.data[[ 2 ]] ) %in%
                                       index( x.data[[ 1 ]] ) ) ]
    plot.extremes <- x.data[[ 2 ]]
    plot.extremes[ !index( x.data[[ 2 ]] ) %in%
                  index( x.extreme ) ] <- NA
    y.label <- function.get.y.label( selectDataBase, selectDataType )
    bind.dy <- cbind( x.data[[ 2 ]], plot.extremes )
    names( bind.dy ) <- c( "deseasonalized ts", "annual maxima" )
    print( y.label )
    if ( class( y.label ) == "expression" ){
      ## dygraphs is not able to handle neither expressions nor MathJax
      y.label <- "temperature in &deg;C"
    }
    dygraph( bind.dy, ylab = y.label ) %>%
      dySeries( "deseasonalized ts", color = colour.ts ) %>%
      dySeries( "annual maxima", color = colour.extremes,
               drawPoints = TRUE,
               strokeWidth = 0, pointSize = 2 ) } )
  ## Using ggplot2 for an interactive excluding of points in x.block.
  output$plotBlocked <- renderPlot( {
    if ( is.null( reactive.extreme() ) || is.null( reactive.rows ) ||
        is.null( buttonMinMax() ) ){
      return( NULL )
    }
    x.extreme <- reactive.extreme()[[ 1 ]]
    ## In general just checking for the input$sliderThreshold to be not NULL
    ## would be sufficient and there is no real need for
    ## input$radioEvdStatistics. But why outsmart ourselves?
    if ( radioEvdStatistics() == "GP" && !is.null( sliderThreshold() ) &&
        selectDataBase() != "Artificial data" ){
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



##' @title Box containing the plot of the performed GEV/GP fitting as well
##' as three goodness-of-fit plots.
##' @details This function provides the ouput to
##' \code{\link{generalFitPlot}}. All four plots are rendered using the
##' ggplot2 package.
##'
##' @param id Namespace prefix
##'
##' @family climex-plot
##'
##' @import shiny
##' @importFrom shinydashboard box
##'
##' @return tabBox
##' @author Philipp Mueller 
generalFitPlotOutput <- function( id ){
  # Create a namespace function using the provided id
  ns <- NS( id )
  box( title = h2( "GEV fit" ), status = "primary", height = 505,
      solidheader = TRUE, width = 8, id = "boxPlotFit",
      column( 9, id = "plotEvd",plotOutput( ns( "plotFitEvd" ) ) ),
      column( 3, id = "plotGOF",
             plotOutput( ns( "plotFitPP" ), height = 140 ),
             plotOutput( ns( "plotFitQQ" ), height = 140 ),
             plotOutput( ns( "plotFitReturnLevel" ), height = 140 ) ) )
}

##' @title Box containing the plot of the performed GEV/GP fitting as well
##' as three goodness-of-fit plots.
##' @details All four plots are rendered using the ggplot2 package.
##'
##' @param input Namespace input. For more details check out
##' \url{http://shiny.rstudio.com/articles/modules.html}
##' @param output Namespace output.
##' @param session Namespace session.
##' @param reactive.extreme Reactive value containing a list of the
##' extracted extreme events of a time series, the deseasonalized and the
##' pure version of this very time series. All three elements are provided
##' as class 'xts'.
##' @param reactive.rows Reactive value holding a logical vector indicating
##' which values of the time series provided by \code{\link{data.extremes}}
##' to use after clicking and brushing.
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param function.get.y.label Function determining the label of the y axis
##' in all three plots. \code{\link{function.get.y.label}}. You might wonder
##' why I am not just referring to the package internal function right away
##' instead of requiring it as an input. Well, this way the whole app
##' becomes way more easy to customize.
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "Input", "DWD", "Artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{\link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##' @param selectDataType Character (select) input to determine which set
##' measurements should be used for the selected station. In case of the
##' default import of the DWD data, there are three options:
##' c( "Daily max. temp", "Daily min. temp", "Daily precipitation" ).
##' Determined by menuSelectDataType.
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point.
##'
##' @family climex-plot
##'
##' @import shiny
##' @import ggplot2
##'
##' @return Nothing in particular.
##' @author Philipp Mueller 
generalFitPlot <- function( input, output, session, reactive.extreme,
                           reactive.rows, reactive.fitting,
                           buttonMinMax, radioEvdStatistics,
                           function.get.y.label, selectDataBase,
                           selectDataType, sliderThreshold ){
  ## Wait for proper initialization
  observe( {
    if ( is.null( reactive.extreme() ) || is.null( reactive.fitting() ) ||
         is.null( reactive.rows ) ){
      return( NULL )
    }
  }, priority = 1 )
  ## General color definitions throughout the entire app.
  colour.ts <- grDevices::rgb( 0.098, 0.098, 0.44 )
  colour.extremes <- grDevices::rgb( 1, 0.55, 0 )
  colour.ts.light <- "#7171EC"
  colour.extremes.light <- grDevices::rgb( 1, 0.9, 0.8 )

  ## Plots the result of the fitted GEV
  output$plotFitEvd <- renderPlot( {
    if ( is.null( reactive.extreme() ) || is.null( reactive.fitting() ) ||
        is.null( buttonMinMax() ) ){
      return( NULL )
    }
    x.extreme <- reactive.extreme()[[ 1 ]]
    x.kept <- x.extreme[ reactive.rows$keep.rows ]
    x.fit.evd <- reactive.fitting()
    if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" ){
      ## To just perform an ordinary plot is not sufficient when working
      ## with minimal extremes. Since the GEV is fitted to the negated
      ## time series the whole density has to be multiplied by -1 instead
      ## of just moving it with -1*location
      x.fit.evd$x <- x.fit.evd$x* -1
      x.fit.evd$par[ 1 ] <- x.fit.evd$par[ 1 ]* -1
    }
    ## The amount of bins is changing whenever a single event
    ## is toggled. This is distracting only if a certain amount
    ## of points are toggled (5%) a different number of bins shall
    ## be used. The number of boxes should be half the number of
    ## points in 5% steps. gg1.bins gives a factor which multiplied
    ## with the length of x.kept yields the number of blocks.
    gg1.bins <- ( ( ( length( x.kept ) - 1 )*100/
                    length( x.extreme ) )  %/% 5 )* 0.025
    x.label <- function.get.y.label( selectDataBase, selectDataType )
    gg.evd <- plot( x.fit.evd, bin.factor = gg1.bins ) + ylab( "density" ) +
      xlab( x.label ) + theme( legend.position = "none" ) +
      theme( axis.title = element_text( size = 17, colour = colour.ts ),
            axis.text = element_text( size = 13, colour = colour.ts ) )
    if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" ){
      gg.evd <- gg.evd + scale_x_reverse(
                             labels = function( lab ) return( -lab ) )
    }
    return( gg.evd )
  } )

  ## PP plot for fit statistics
  ## The PP plot is the CDF of the theoretical distribution vs.
  ## the CDF of the empirical data.
  output$plotFitPP <- renderPlot( {
    if ( is.null( reactive.extreme() ) || is.null( reactive.fitting() ) ||
        is.null( buttonMinMax() ) ){
      return( NULL )
    }
    x.extreme <- reactive.extreme()[[ 1 ]]
    x.kept <- x.extreme[ reactive.rows$keep.rows ]
    x.fit.evd <- reactive.fitting()
    if ( radioEvdStatistics() == "GEV" ){
      if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min"  ){
        x.kept <- -1* x.kept
        x.fit.evd$par[ 1 ] <- -1* x.fit.evd$par[ 1 ]
      }
      if ( x.fit.evd$par[ 3 ] != 0 ){
        ## GEV
        theoretical <-
          Reduce( c, lapply( sort( as.numeric( x.kept ) ), function( xx )
            exp( -1* ( 1 + x.fit.evd$par[ 3 ]*( xx - x.fit.evd$par[ 1 ] )/
                       x.fit.evd$par[ 2 ] )^( -1/x.fit.evd$par[ 3 ] ) ) ) )
      } else {
        ## Gumbel
        theoretical <-
          Reduce( c, lapply( sort( as.numeric( x.kept ) ), function( xx )
            exp( -1* exp( -1* ( xx - x.fit.evd$par[ 1 ] )/ x.fit.evd$par[ 2 ] ) )
            ) )
      }
      if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min"  ){
        empirical <- -1* stats::ppoints( length( x.kept ), 0 )
        theoretical <- -1* theoretical
      } else {
        empirical <- stats::ppoints( length( x.kept ), 0 )
      }
    } else {
      ## radioEvdStatistics() == "GP"
      if ( selectDataBase() == "Artificial data" ){
        ## Since I right now don't see the point of introducing constant
        ## threshold in the artificial data, it will be set to zero.
        threshold <- 0
      } else {
        threshold <- sliderThreshold()
      }
      if ( x.fit.evd$par[ 2 ] != 0 ){
        ## GP
        theoretical <-
          Reduce( c, lapply( sort( as.numeric( x.kept ) ), function( xx )
            as.numeric( 1 - ( 1 + ( x.fit.evd$par[ 2 ]*( xx )/
                                    x.fit.evd$par[ 1 ] ) )^(
                                      -1/ x.fit.evd$par[ 2 ] ) ) ) )
      } else {
        ## Exponential distribution
        theoretical <-
          Reduce( c, lapply( sort( as.numeric( x.kept ) ), function( xx )
            as.numeric( 1 - exp( -( xx )/ x.fit.evd$par[ 1 ] ) ) ) )
      }
      empirical <- stats::ppoints( length( x.kept ), 0 )
    }
    plot.data <- data.frame( theoretical = theoretical, empirical = empirical )
    plot.fit <- stats::lm( empirical ~ theoretical, plot.data )[[ 1 ]]
    gg.pp <- ggplot() +
      geom_point( data = plot.data, aes( x = theoretical, y = empirical ),
                 colour = colour.ts, shape = 1, size = 2, alpha = 0.8 ) +
      geom_abline( intercept = plot.fit[ 1 ], slope = plot.fit[ 2 ],
                  colour = colour.ts, linetype = 2 ) +
      geom_abline( intercept = 0, slope = 1, colour = colour.extremes ) +
      xlab( "theoretical CDF" ) + ylab( "empirical CDF" ) +
      annotate( "text", size = 5, color = colour.ts, 
               x = min( plot.data$theoretical ) +
                 ( max( plot.data$theoretical ) -
                        min( plot.data$theoretical ) )* .2,
               y = min( plot.data$empirical ) +
                 ( max( plot.data$empirical ) -
                        min( plot.data$empirical ) )* .91,
               label = "P-P plot" ) +
      theme_bw() +
      theme( axis.title = element_text( size = 15, colour = colour.ts ),
            axis.text = element_text( size = 12, colour = colour.ts ) )
    return( gg.pp ) } )

  ## Quantile-quantile plot for fit statistics with samples
  ## drawn from the fitted distribution
  output$plotFitQQ <- renderPlot( {
    if ( is.null( reactive.extreme() ) || is.null( reactive.fitting() ) ||
        is.null( buttonMinMax() ) ){
      return( NULL )
    }
    x.extreme <- reactive.extreme()[[ 1 ]]
    x.kept <- x.extreme[ reactive.rows$keep.rows ]
    x.fit.evd <- reactive.fitting()
    if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" ){
      x.kept <- -1* x.kept
      x.fit.evd$par[ 1 ] <- -1* x.fit.evd$par[ 1 ]
    }
    if ( radioEvdStatistics() == "GEV" ){
      theoretical <- Reduce(
          c, lapply( stats::ppoints( length( x.kept ), 0 ),
                    function( ss )
                      climex:::qevd( ss, location = x.fit.evd$par[ 1 ],
                                    scale = x.fit.evd$par[ 2 ],
                                    shape = x.fit.evd$par[ 3 ],
                                    model = "gev" ) ) )
      if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" ){
        theoretical <- -1* theoretical
        empirical <- -1* sort( as.numeric( x.kept ) )
      } else {
        empirical <- sort( as.numeric( x.kept ) )
      }
    } else {
      if ( selectDataBase() == "Artificial data" ){
        ## Since I right now don't see the point of introducing constant
        ## threshold in the artificial data, it will be set to zero.
        threshold <- 0
      } else {
        threshold <- sliderThreshold()
      }
      theoretical <- Reduce(
              c, lapply( stats::ppoints( length( x.kept ), 0 ),
                        function( ss )
                          climex:::qevd( ss, threshold = threshold,
                                        scale = x.fit.evd$par[ 1 ],
                                        shape = x.fit.evd$par[ 2 ],
                                        model = "gpd",
                                        lower.tail = FALSE ) ) )
      empirical <- sort( as.numeric( x.kept ) + threshold )
    }
    ## Writing the data in a format used by ggplot2
    plot.data = data.frame( theoretical = theoretical,
                           empirical = empirical )
    plot.fit <- stats::lm( empirical ~ theoretical, plot.data )[[ 1 ]]
    gg.qq <- ggplot() +
      geom_point( data = plot.data, aes( x = theoretical, y = empirical ),
                 colour = colour.ts, shape = 1, size = 2, alpha = 0.8,
                 na.rm = TRUE ) +
      geom_abline( intercept = plot.fit[ 1 ], slope = plot.fit[ 2 ],
                  colour = colour.ts, linetype = 2 ) + theme_bw() +
      geom_abline( intercept = 0, slope = 1, colour = colour.extremes ) +
      xlab( "theoretical quantile" ) + ylab( "empirical" ) +
      annotate( "text", size = 5, color = colour.ts,
               x = min( plot.data$theoretical ) +
                 ( max( plot.data$theoretical ) -
                        min( plot.data$theoretical ) )* .2,
               y = min( plot.data$empirical ) +
                 ( max( plot.data$empirical ) -
                        min( plot.data$empirical ) )* .91,
               label = "Q-Q plot" ) +
      theme( axis.title = element_text( size = 15, colour = colour.ts ),
            axis.text = element_text( size = 12, colour = colour.ts ) )
    return( gg.qq )        
  } )

  output$plotFitReturnLevel <- renderPlot( {
    if ( is.null( reactive.extreme() ) || is.null( reactive.fitting() ) ||
        is.null( buttonMinMax() ) ){
      return( NULL )
    }
    x.data <- reactive.extreme()
    x.extreme <- x.data[[ 1 ]]
    x.kept <- x.extreme[ reactive.rows$keep.rows ]
    x.fit.evd <- reactive.fitting()
    x.period <- c( 2, 5, 10, 20, 50, 80, 100, 120, 200,
                  250, 300, 500, 800 )
    fit.aux <- x.fit.evd
    if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" &&
         radioEvdStatistics() == "GEV" ){
      ## For calculating the return level of the minimum extremes,
      ## one has to multiply the time series with (-1), fit the
      ## data using standard GEV, calculate the return level using
      ## standard GEV and then multiply the location parameter as
      ## well as the return levels by (-1).
      ## For determining the error of the return level in the
      ## bottom-most goodness of fit plot the MLE-approach was
      ## used. It underestimates the error but is way faster than
      ## the Monte Carlo alternative.
      fit.aux$par[ 1 ] <- fit.aux$par[ 1 ]* ( -1 )
      fit.aux$x <- fit.aux$x* ( -1 )
      if ( is.nan( climex::likelihood( fit.aux$par, fit.aux$x ) ) ){
        ## Just multiplying by -1 does not always work since
        ## the processes in the real world are rarely symmetric
        ## and the minimum and maximum extremes behave
        ## differently.
        ## I don't consider all the input argument since it's
        ## just one in three goodness of fit plots
        fit.aux <- climex::fit.gev( -1* x.kept,
                                   error.estimation = "none" )
      }
    }
    if ( radioEvdStatistics() == "GEV" ){
      model <- "gev"
    } else {
      model <- "gpd"
    }
    ## Calculating the return levels and their error estimation.
    x.return.levels <- 
      climex::return.level( fit.aux, return.period = x.period,
                           error.estimation = "MLE",
                           threshold = fit.aux$threshold,
                           total.length = length( x.data[[ 2 ]] ),
                           model = model )
    ## In case some of the return periods have been omitted in the
    ## GP case because their were to small, one has to discard the
    ## corresponding return.periods too
    if ( length( x.return.levels$return.level ) <
         length( x.period ) ){
      x.period <- x.period[ length( x.period ) - 1 -
                            length(
                                x.return.levels$return.level ) :
                            length( x.period ) ]
    }
    if ( !is.null( buttonMinMax() ) && buttonMinMax() == "Min" &&
         radioEvdStatistics() == "GEV" ){
      x.return.levels$return.level <-
        x.return.levels$return.level* ( -1 )
      x.return.levels$ci.low <- x.return.levels$return.level +
        as.numeric( x.return.levels$error )
      x.return.levels$ci.high <- x.return.levels$return.level -
        as.numeric( x.return.levels$error )
    } else {
      x.return.levels$ci.low <- x.return.levels$return.level -
        as.numeric( x.return.levels$error )
      x.return.levels$ci.high <- x.return.levels$return.level +
        as.numeric( x.return.levels$error )
    }
    ## Generate a data.frame for the return level plot.
    if ( radioEvdStatistics() == "GEV" ){
      plot.data <- data.frame(
          x = Reduce( c, lapply( x.period, function( xx )
            -log( 1/ xx ) ) ),
          y = x.return.levels$return.level,
          y.low = x.return.levels$ci.low,
          y.high = x.return.levels$ci.high )
    } else {
      ## GP
      plot.data <- data.frame(
          x = Reduce( c, lapply( x.period, function( xx )
            -log( 1/ xx ) ) ),
          y = x.return.levels$return.level,
          y.low = x.return.levels$ci.low,
          y.high = x.return.levels$ci.high )
    }
    ## Plot with confidence intervals
    gg.rl <- ggplot( data = plot.data, aes( x = x ) ) +
      geom_line( aes( y = y ),
                colour = colour.ts, na.rm = TRUE ) +
      geom_point( aes( y = y ), colour = colour.ts,
                 shape = 1, size = 2, alpha = 0.8, na.rm = TRUE ) +
      geom_line( aes( y = y.low ), linetype = 2,
                colour = colour.extremes, na.rm = TRUE ) +
      geom_line( aes( y = y.high ), linetype = 2,
                colour = colour.extremes, na.rm = TRUE ) +
      xlab( "return period" ) + ylab( "return level" ) +
      theme_bw() + scale_x_log10(
                       breaks = plot.data$x[ c( 1, 2, 4, 11 ) ],
                       labels = function( ll ) round( exp( ll ) ) ) +
      theme( axis.title = element_text( size = 15, colour = colour.ts ),
            axis.text = element_text( size = 12, colour = colour.ts ) )
    if ( !is.null( buttonMinMax() ) ){
      if ( buttonMinMax() == "Min" && radioEvdStatistics() == "GEV" ){
        gg.rl <- gg.rl + scale_y_reverse()
      }
    }
    return( gg.rl ) } )
}
