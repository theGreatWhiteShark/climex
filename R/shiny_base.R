##' @title Shiny app combining most of the tools used in extreme value
##' analysis using GEV fitting via maximum likelihood.
##'
##' @details It possible to provide a time series of type \pkg{xts} to
##' a list of elements of this type. This app need the its own css file.
##' This function will be exclusively called when the climex app is run
##' on localhost. In order to assure its correct behavior the necessary
##' file/folder structure in the CLIMEX.PATH (global variable which has
##' to be set beforehand; see vignette for details) will be check and
##' if necesary generated. The input time series is accepted in three
##' different formats
##' \enumerate{
##'  \item As a single time series in the formatted using the \pkg{xts}
##' package 
##'  \item As a named list of various xts time series 
##'  \item As a list containing two elements;  a list of xts time
##' series and a data.frame with the columns \emph{longitude},
##' \emph{latitude}, \emph{altitude}, \emph{name} corresponding to the
##' stations geo-coordinates, height and name 
##' }
##'
##' @param x.input Time series of one of the formats mentioned in the
##' details section. 
##'
##' @family shiny
##'
##' @return starts a shiny app.
##' @export
##' @import shiny
##' @import leaflet
##' @import xts
##' @author Philipp Mueller 
climex <- function( x.input = NULL ){
  source.data( pick.default = TRUE, import = "global" )
  if ( missing( x.input ) ){
    ## since we define this amigo global we also have to set it to
    ## NULL by hand when not providing it to this function
    x.input <<- NULL
  }
  if ( !is.null( x.input ) ){
    ## checking for the right format of the input series
    if ( any( class( x.input ) == "xts" ) ){
      ## global because the other functions have to have access to it
      x.input <<- x.input
    } else if ( class( x.input ) == "list" ){
      if ( all( Reduce( c, lapply( x.input, function ( y )
        any( class( x ) == "xts" ) ) ) ) || (
          length( x.input ) == 2 &&
          "list" %in% Reduce( c, lapply( x.input, class ) ) &&
          "data.frame" %in% Reduce( c, lapply( x.input, class ) ) ) ) {
        x.input <<- x.input
      } else {
        x.input <<- NULL
        stop( "the input time series has the wrong format!" )
      }
    } else {
      x.input <<- NULL
      stop( "the input time series has the wrong format!" )
    }
  }
  if ( !"CLIMEX.PATH" %in% ls( envir = .GlobalEnv ) )
    stop( "Please define a global variable named CLIMEX.PATH (storing of the DWD data and the app interna) (see vignette for details)!" )
  ## will contain the folder in which the images of the animation
  ## can be found
  image.folder <<- paste0( CLIMEX.PATH, "app/www/images" )
  ## this is unfortunately necessary since some JavaScript scripts
  ## are written out and some images are plotted which have to be
  ## accessible within the shiny app.
  ## If there are not the appropriate files present to run the shiny
  ## app, the apps as well as the folder structure has to be generated
  if ( !dir.exists( paste0( CLIMEX.PATH, "app" ) ) )
    dir.create( paste0( CLIMEX.PATH, "app" ) )
  if ( !dir.exists( paste0( CLIMEX.PATH, "app/www" ) ) )
    dir.create( paste0( CLIMEX.PATH, "app/www" ) )
  ## In order to display the animation the app needs the jquery
  ## scianimator. Since it is not able to access a system file on
  ## its own the wrapper climex() will copy it to the apps folder
  if ( !dir.exists( paste0( CLIMEX.PATH,
                           "app/www/jquery.scianimator.min.js" ) ) ){
    file.copy( paste0( system.file( "climex_app", package = "climex" ),
                      "/js/jquery.scianimator.min.js" ),
              to = paste0( CLIMEX.PATH,
                          "app/www/jquery.scianimator.min.js" ),
              overwrite = TRUE )
  }
  ## source of the gif: http://i.imgur.com/seuaOqf.gif
  ## since this is a non-commercial product there shouldn't be
  ## any problems http://imgur.com/tos
  file.copy( paste0( system.file( "climex_app", package = "climex" ),
                    "/js/loadingGif.js" ),
            to = paste0( CLIMEX.PATH, "app/www/loadingGif.js" ),
            overwrite = TRUE ) 
  file.copy( paste0( system.file( "climex_app", package = "climex" ),
                    "/res/loading.gif" ),
            to = paste0( CLIMEX.PATH, "app/www/loading.gif" ) )      
  writeLines( "shinyUI( climex.ui() )", con = paste0( CLIMEX.PATH,
                                                     "app/ui.R" ) )
  writeLines( "shinyServer( climex.server )",
             con = paste0( CLIMEX.PATH, "app/server.R" ) )
  ## If one or more time series without any map information are
  ## provided as input arguments, start the app in the "General" tab
  ## right away
  if ( !is.null( x.input ) && length( x.input ) != 2 ){ 
    writeLines( "shinyUI( climex.ui( selected = 'General' ) )",
               con = paste0( CLIMEX.PATH, "app/ui.R" ) )
  }
  runApp( paste0( CLIMEX.PATH, "app" ) )
}


##' @title Server-side part of the \code{\link{climex}} function.
##'
##' @details Since it grew organically most of the features are defined
##' inside this function. Okay, why did I decided to define the
##' climex.server and the climex.ui function? Since in this way I can
##' provide a local and a server side variant of the code which is
##' maintained in the same place. I do not really like the idea of
##' installing the newest version of the code from Github and than
##' copy/pasting the code in some files somewhere in /srv/shiny-server.
##'
##' @return Function acting as the shiny server.
##' @import shiny
##' @import xts
##' @import dygraphs
##' @import ggplot2
##' @export
##' @author Philipp Mueller 
climex.server <- function( input, output, session ){
########################################################################
#################### Customizing the sidebar menu ######################
########################################################################
  ## Everything in my code is always of the style this.is.a.name. So
  ## why do I use the camel case thisIsAName for the shiny objects?
  ## Well, since CSS file do not support the point separator.
  
  ## Type of database (input, DWD, artificial data)
  output$sidebarDataBase <- climex:::sidebarDataBase()
  ## Individual station or location (GEV)/scale(GP) for artificial data
  output$sidebarDataSource <-
    climex:::sidebarDataSource( reactive( input$selectDataBase ),
                               reactive( input$radioEvdStatistics ),
                               reactive.chosen, selected.station )
  ## Measurement type or scale (GEV)/shape(GP) for artificial data
  output$sidebarDataType <-
    climex:::sidebarDataType( reactive( input$selectDataBase ),
                             reactive( input$radioEvdStatistics ) )
  ## File input prompt for "input" or shape for GEV artificial data
  output$sidebarLoading <-
    climex:::sidebarLoading( session, reactive( input$selectDataBase ),
                            reactive( input$radioEvdStatistics ) )
  ## Removing incomplete years (GEV) or clusters (GP)
  output$sidebarCleaning <-
    climex:::sidebarCleaning( reactive( input$radioEvdStatistics ) )
  ## Reactive value which holds the selected stations chosen either via
  ## the leaflet map or the sidebar
  reactive.chosen <- climex:::data.chosen(
                                  reactive( input$selectDataBase ),
                                  reactive( input$sliderYears ),
                                  reactive( input$selectDataType ),
                                  file.loading, x.input )
  ## Reactive value selecting a specific time series according to the
  ## choices in the sidebar/leaflet map
  reactive.selection <-
    climex:::data.selection(
                 reactive.chosen, reactive( input$selectDataSource ),
                 reactive( input$selectDataBase ),
                 reactive( input$sliderYears ),
                 reactive( input$sliderThreshold ),
                 reactive( input$radioEvdStatistics ),
                 reactive( input$sliderArtificialDataLocation ),
                 reactive( input$sliderArtificialDataScale ),
                 reactive( input$sliderArtificialDataShape ),
                 cleaning.interactive )
                                     
########################################################################
  
########################################################################
#################### Data generation and selection #####################
########################################################################
  ## Slider input determining the block length (GEV) or threshold (GP)
  output$generalExtremeExtraction <-
    climex:::generalExtremeExtraction(
                 reactive( input$radioEvdStatistics ),
                 deseasonalize.interactive,
                 reactive( input$buttonMinMax ), reactive.selection )

  file.loading <- reactive( {
    ## If no file is chosen, don't do anything
    if ( is.null( input$fileInputSelection$datapath ) )
      return( NULL )
      ## load selected file
      load( input$fileInputSelection$datapath,
           file.load.env <- new.env() )
      if ( length( ls( file.load.env ) ) == 1 ){
        ## Just one object is contained in the .RData file
        file.input <- get( ls( file.load.env ), envir = file.load.env )
        if ( any( class( file.input ) ) == "xts" ){
          x.input <<- file.input
          return( x.input )
        } else if ( class( file.input ) == "list" ){
          if ( all( class( x.input ) == "xts" ) || (
            length( x.input ) == 2 &&
            "list" %in% Reduce( c, lapply( x.input, class ) ) &&
            "data.frame" %in% Reduce( c, lapply( x.input, class ) ) ) ){
            x.input <<- file.input
            return( x.input )
          }
        }
      }
      shinytoastr::toastr_error( "Sorry but this feature is implemented for the format in which the argument x.input is accepted in climex::climex only! Please do the conversion and formatting in R beforehand and just save a .RData containing a single object" )
      return( NULL )
  } ) 
  cleaning.interactive <- function( x.xts ){
    ## getting rid of artifacts since they would spoil our results
    ## (the German weather service DWD uses -999 to mark missing
    ## measurements)
    x.xts[ which( is.na( x.xts ) ) ] <- NaN
    x.xts[ which( x.xts == -999 ) ] <- NaN
    if ( !is.null( input$checkBoxIncompleteYears ) ){
      if ( input$checkBoxIncompleteYears ){
        ## Remove all incomplete years from time series
        x.xts <- remove.incomplete.years( x.xts ) }
    }
    if ( any( is.nan( x.xts ) ) )
      shinytoastr::toastr_warning(
                       "The current time series contains missing values. Please be sure to check 'Remove incomplete years' in the sidebar to avoid wrong results!" )
    return( x.xts )
  }
  data.blocking <- reactive( {
    ## Decides if either the GEV distribution with block maxima or
    ## the Pareto distribution if threshold exceedance should be
    ## considered. Import data set, cut it to the desired interval,
    ## deseasonalize and block it
    x.xts <- reactive.selection()
    if ( is.null( x.xts ) ||
         is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      if ( is.null( input$sliderBoxLength ) )
        return( NULL )
    } else {
      if ( is.null( input$sliderThreshold ) ||
          is.null( input$checkBoxDecluster ) )
        return( NULL )
    }
    x.deseasonalized <- deseasonalize.interactive( x.xts )
    if ( input$radioEvdStatistics == "GP" &&
         max( x.deseasonalized ) < input$sliderThreshold ){
      return( NULL )
    }
    x.block <- blocking.interactive( x.deseasonalized )
    return( list( blocked.data = x.block,
                 deseasonalized.data = x.deseasonalized,
                 pure.data = x.xts ) ) } )
  ## wrapper blocking time series according to the set slider etc.
  ## values
  blocking.interactive <- function( x.xts ){
    ## Toggle if maxima of minima are going to be used
    if ( is.null( input$buttonMinMax ) ||
         input$buttonMinMax == "Max" ){
      block.mode <- "max"
    } else
      block.mode <- "min"
    if ( is.null( input$radioEvdStatistics ) ){
      ## While initialization input$radioEvdStatistics and
      ## input$sliderBoxLength are NULL. Therefore this is the
      ## fallback default x.block
      x.block <- block( x.xts, separation.mode = "years",
                       block.mode = block.mode )
    } else if ( input$radioEvdStatistics == "GEV" ){
        x.block <- block( x.xts, block.length = input$sliderBoxLength,
                         block.mode = block.mode )
    } else if ( input$radioEvdStatistics == "GP" ){
      ## Due to "historical" reasons the vector containing the
      ## resulting values will still be called x.block. The
      ## "block.mode" and the corresponding "input$buttonMinMax"
      ## are still use full and decide if the values above or below
      ## the threshold are going to be extracted
      x.block <- climex::threshold( x.xts,
                                   threshold = input$sliderThreshold,
                                   decluster = input$checkBoxDecluster,
                                   na.rm = TRUE )
      return( x.block )
    }
  }
########################################################################
  
########################################################################
##################### Interactive stuff and auxillary functions ########
########################################################################
  ## By doing this the value is temporary removed from the ts (after
  ## removal of incomplete years) and a new extremum is calculated
  ## for the correspondig year
  reactive.values <- reactiveValues( keep.rows = NULL )
  observe( {
    x.data <- data.blocking()
    ## use the x.block variable to update the reactive value
    ## keep.row (containing a listing of all the points of the
    ## actual time series which are used during the fitting procedure)
    reactive.values$keep.rows <- rep( TRUE, length( x.data[[ 1 ]] ) )
  }, priority = 1 ) # or else the plot functions are reached beforehand
  ## Toggle points that are clicked
  observeEvent( input$plotBlockedClick, {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      ## Just a precaution but no way this point is reached before
      ## initialization 
      return( NULL )
    }
    df.block <- data.frame( date = index( x.block ),
                           value = as.numeric( x.block ) )
    result <- nearPoints( df.block, input$plotBlockedClick,
                         allRows = TRUE )
    reactive.values$keep.rows <- xor( reactive.values$keep.rows,
                                     result$selected_ ) } )
  ## Toggle points that are brushed
  observeEvent( input$excludeBlockedToggle, {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      ## Just a precaution but no way this point is reached before
      ## initialization 
      return( NULL )
    }
    df.block <- data.frame( date = index( x.block ),
                           value = as.numeric( x.block ) )
    result <- brushedPoints( df.block, input$plotBlockedBrush,
                            allRows = TRUE )
    reactive.values$keep.rows <- xor( reactive.values$keep.rows,
                                     result$selected_ ) } )
  ## Reset plot
  observeEvent( input$excludeBlockedReset, {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      ## Just a precaution but no way this point is reached before
      ## initialization 
      return( NULL )
    }
    reactive.values$keep.rows <- rep( TRUE, length( x.block ) ) } )
########################################################################
  
########################################################################
############### Plotting of the time series and fit ####################
########################################################################
  ## Displaying of the original, intermediate and final time series.
  colour.ts <- grDevices::rgb( 0.098, 0.098, 0.44 )
  colour.extremes <- grDevices::rgb( 1, 0.55, 0 )
  colour.ts.light <- "#7171EC"
  colour.extremes.light <- grDevices::rgb( 1, 0.9, 0.8 )
  function.get.y.label <- function( input ){
    if ( is.null( input$selectDataBase ) ){
      y.label <- "temperature in °C"            
    } else if ( input$selectDataBase == "artificial data" ){
      y.label <- "EVD sample"
    } else if ( input$selectDataBase == "DWD" ){
      if ( is.null( input$selectDataType ) ){
        y.label <- "temperature in °C"
      } else if ( input$selectDataType == "Daily precipitation" ){
        y.label <- "precipitation in mm"
      } else {
        y.label <- "temperature in °C" }
    } else if ( input$selectDataBase == "input" ){
      y.label <- "input"
    }
    return( y.label ) }
  ## Pure time series 
  output$plotTimeSeries <- renderDygraph( {
    x.data <- data.blocking( )
    x.blocked <- x.data[[ 3 ]][ which( index( x.data[[ 3 ]] ) %in%
                                       index( x.data[[ 1 ]] ) ) ]
    plot.blocked <- x.data[[ 3 ]]
    plot.blocked[ !index( x.data[[ 3 ]] ) %in%
                  index( x.blocked ) ] <- NA
    y.label <- function.get.y.label( input )
    bind.dy <- cbind( x.data[[ 3 ]], plot.blocked )
    names( bind.dy ) <- c( "pure ts", "annual maxima" )        
    dygraph( bind.dy, ylab = y.label ) %>%
      dySeries( "pure ts", color = colour.ts ) %>%
      dySeries( "annual maxima", color = colour.extremes,
               drawPoints = TRUE,
               strokeWidth = 0, pointSize = 2 ) } )
  ## deseasonalized time series
  output$plotDeseasonalized <- renderDygraph( {
    x.data <- data.blocking( )
    if ( is.null( x.data ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.blocked <- x.data[[ 2 ]][ which( index( x.data[[ 2 ]] ) %in%
                                       index( x.data[[ 1 ]] ) ) ]
    plot.blocked <- x.data[[ 2 ]]
    plot.blocked[ !index( x.data[[ 2 ]] ) %in%
                  index( x.blocked ) ] <- NA
    y.label <- function.get.y.label( input )
    bind.dy <- cbind( x.data[[ 2 ]], plot.blocked )
    names( bind.dy ) <- c( "deseasonalized ts", "annual maxima" )        
    dygraph( bind.dy, ylab = y.label ) %>%
      dySeries( "deseasonalized ts", color = colour.ts ) %>%
      dySeries( "annual maxima", color = colour.extremes,
               drawPoints = TRUE,
               strokeWidth = 0, pointSize = 2 ) } )
  ## Using ggplot2 for an interactive excluding of points in x.block.
  output$plotBlocked <- renderPlot( {
    x.block <- data.blocking( )[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GP" &&
         !is.null( input$sliderThreshold ) ){
      x.block <- x.block + input$sliderThreshold
    }
    x.kept <- x.block[ reactive.values$keep.rows ]
    x.excluded <- x.block[ !reactive.values$keep.rows ]
    plot.kept <- data.frame( date = index( x.kept ),
                            value = as.numeric( x.kept ) )
    plot.excluded <- data.frame( date = index( x.excluded ),
                                value = as.numeric( x.excluded ) )
    y.label <- function.get.y.label( input )
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
  ## GEV|GPD fit and analysis plots
  output$plotFitEvd <- renderPlot( {
    ## Plots the result of the fitted GEV
    x.block <- data.blocking( )[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.kept <- x.block[ reactive.values$keep.rows ]
    x.fit.evd <- evd.fitting( )
    if ( is.null( x.fit.evd ) ){
      return( NULL )
    }
    if ( input$buttonMinMax == "Min" &&
         input$radioEvdStatistics == "GEV" ){
      x.kept <- x.kept* ( -1 )
      x.fit.evd$par[ 1 ] <- x.fit.evd$par[ 1 ]* -1
      shinytoastr::toastr_info( "Since the minimal extremes are chosen the GEV distribution \n will be fitted to the negated time series" )
    }
    x.lim <- c( max( x.kept ), min( x.kept ) )
    ## the amount of bins is changing whenever a single event
    ## is toggled. This is distracting only if a certain amount
    ## of points are toggled (5%) a different number of bins shall
    ## be used. The number of boxes should be half the number of
    ## points in 5% steps. gg1.bins gives a factor which multiplied
    ## with the length of x.kept yields the number of blocks.
    gg1.bins <- ( ( ( length( x.kept ) - 1 )*100/
                    length( x.block ) )  %/% 5 )* 0.025
    x.label <- function.get.y.label( input )
    plot( x.fit.evd, bin.factor = gg1.bins ) +
      xlab( x.label ) +
      theme( legend.position = "none" ) +
      theme( axis.title = element_text( size = 17,
                                       colour = colour.ts ),
            axis.text = element_text( size = 13,
                                     colour = colour.ts ) )
  } )
  output$plotFitQQ <- renderPlot( {
    ## Quantile-quantile plot for fit statistics
    x.block <- data.blocking( )[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.kept <- x.block[ reactive.values$keep.rows ]
    x.fit.evd <- evd.fitting()
    if ( is.null( x.fit.evd ) ){
      return( NULL )
      }
    if ( input$radioEvdStatistics == "GEV" ){
      if ( !is.null( input$buttonMinMax ) &&
           input$buttonMinMax == "Min"  ){
        x.kept <- -1* x.kept
        x.fit.evd$par[ 1 ] <- -1* x.fit.evd$par[ 1 ]
      }
      model <- climex:::qevd( p = stats::ppoints(
                                             length( x.kept ), 0 ),
                             location = x.fit.evd$par[ 1 ],
                             scale = x.fit.evd$par[ 2 ],
                             shape = x.fit.evd$par[ 3 ],
                             model = "gev", silent = TRUE )
      if ( !is.null( input$buttonMinMax ) &&
           input$buttonMinMax == "Min"  ){
        empirical <- -1* sort( as.numeric( x.kept ) )
        model <- -1* model
      } else {
        empirical <- sort( as.numeric( x.kept ) )
      }
      plot.data <- data.frame( model = model, empirical = empirical )
    } else {
      ## input$radioEvdStatistics == "GP"
      threshold <- input$sliderThreshold
      plot.data <- data.frame(
          model = sort(
              climex:::qevd( p = stats::ppoints(
                                            length( x.kept ), 0 ),
                            scale = x.fit.evd$par[ 1 ], 
                            shape = x.fit.evd$par[ 2 ],
                            model = "gpd", silent = TRUE,
                            threshold = threshold ) ),
          empirical = sort( as.numeric( x.kept ) ) +
            threshold )
    }
    plot.fit <- stats::lm( empirical ~ model, plot.data )[[ 1 ]]
    gg.qq1 <- ggplot() +
      geom_point( data = plot.data, aes( x = model, y = empirical ),
                 colour = colour.ts,
                 shape = 1, size = 2, alpha = 0.8 ) +
      geom_abline( intercept = plot.fit[ 1 ],
                  slope = plot.fit[ 2 ], colour = colour.ts,
                  linetype = 2 ) +
      geom_abline( intercept = 0, slope = 1,
                  colour = colour.extremes ) +
      theme_bw() +
      theme( axis.title = element_text( size = 15,
                                       colour = colour.ts ),
            axis.text = element_text( size = 12,
                                     colour = colour.ts ) )
    return( gg.qq1 ) } )
  output$plotFitQQ2 <- renderPlot( {
    ## Quantile-quantile plot for fit statistics with samples
    ## drawn from fitted GEV
    x.block <- data.blocking( )[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.kept <- x.block[ reactive.values$keep.rows ]
    x.fit.evd <- evd.fitting()
    if ( is.null( x.fit.evd ) ){
      return( NULL )
    }
    if ( !is.null( input$buttonMinMax ) &&
         input$buttonMinMax == "Min" ){
      x.kept <- -1* x.kept
      x.fit.evd$par[ 1 ] <- -1* x.fit.evd$par[ 1 ]
    }
    if ( input$radioEvdStatistics == "GEV" ){
      sampled <- sort(
          climex:::revd( n = length( x.kept ),
                        location = x.fit.evd$par[ 1 ],
                        scale = x.fit.evd$par[ 2 ], silent = TRUE,
                        shape = x.fit.evd$par[ 3 ], model = "gev" ) )
      if ( !is.null( input$buttonMinMax ) &&
           input$buttonMinMax == "Min" ){
        sampled <- -1* sampled
        empirical <- -1* sort( as.numeric( x.kept ) )
      } else {
        empirical <- sort( as.numeric( x.kept ) )
      }
    } else {
      sampled <- sort(
          climex:::revd( n = length( x.kept ),
                        scale = x.fit.evd$par[ 1 ], silent = TRUE,
                        shape = x.fit.evd$par[ 2 ], model = "gpd",
                        threshold = input$sliderThreshold ) )
      empirical <- sort( as.numeric( x.kept ) +
                         input$sliderThreshold )
    }
    length.e <- length( empirical )
    length.s <- length( sampled )
    ## inspired by extRemes::qqplot( plot.data$empirical,
    ## plot.data$sampled )
    ## function giving a linear interpolation 
    function.sampled.interpolate <-
      stats::approxfun( seq( 0, 1, length = length( sampled ) ),
                       sort( sampled ), yleft = NA, yright = NA )
    if ( is.null( input$buttonMinMax ) ){
      period <- ( 1 : length( empirical ) - 1 )/
        ( length( empirical ) - 1 )
    } else if ( input$buttonMinMax == "Min" &&
                input$radioEvdStatistics == "GEV" ){
      period <- rev( ( 1 : length( empirical ) - 1 )/
                     ( length( empirical ) - 1 ) )
    } else {
      period <- ( 1 : length( empirical ) - 1 )/
        ( length( empirical ) - 1 )
    }
    sampled.interpolate <- function.sampled.interpolate( period )
    sampled.ci.low <- function.sampled.interpolate(
        period - 1.36/ sqrt( length.e* length.s/
                             ( length.e + length.s ) ) )
    sampled.ci.high <- function.sampled.interpolate(
        period + 1.36/ sqrt( length.e* length.s/
                             ( length.e + length.s ) ) )
    plot.data <- data.frame( empirical = empirical,
                            sampled = sampled.interpolate,
                            ci.low = sampled.ci.low,
                            ci.high = sampled.ci.high )
    plot.fit <- stats::lm( empirical ~ sampled, plot.data )[[ 1 ]]
    gg.qq2 <- ggplot() +
      geom_point( data = plot.data,
                 aes( x = sampled, y = empirical ),
                 colour = colour.ts, shape = 1, size = 2,
                 alpha = 0.8, na.rm = TRUE ) +
      geom_line( data = plot.data, na.rm = TRUE,
                aes( x = sampled.ci.low, y = empirical ),
                linetype = 2, colour = colour.extremes ) +
      geom_line( data = plot.data, linetype = 2,
                aes( x = sampled.ci.high, y = empirical ),
                colour = colour.extremes, na.rm = TRUE ) +
      geom_abline( intercept = plot.fit[ 1 ],
                  slope = plot.fit[ 2 ], colour = colour.ts,
                  linetype = 2 ) + theme_bw() +
      geom_abline( intercept = 0, slope = 1,
                  colour = colour.extremes ) +
      theme( axis.title = element_text( size = 15,
                                       colour = colour.ts ),
            axis.text = element_text( size = 12,
                                     colour = colour.ts ) )
    return( gg.qq2 )        
  } )
  output$plotFitReturnLevel <- renderPlot( {
    x.data <- data.blocking()
    x.block <- x.data[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.kept <- x.block[ reactive.values$keep.rows ]
    x.fit.evd <- evd.fitting()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    x.period <- c( 2, 5, 10, 20, 50, 80, 100, 120, 200,
                  250, 300, 500, 800 )
    ## Forget about the extRemes::ci.fevd.mle.
    ## I have an implementation of the error estimate of the
    ## return level myself! The confidence level will be set to
    ## one standard deviation.
    fit.aux <- x.fit.evd
    if ( input$buttonMinMax == "Min" &&
         input$radioEvdStatistics == "GEV" ){
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
      if ( is.nan( climex::likelihood( fit.aux$par,
                                      fit.aux$x ) ) ){
        ## Just multiplying by -1 does not always work since
        ## the processes in the real world are rarely symmetric
        ## and the minimum and maximum extremes behave
        ## differently.
        ## I don't consider all the input argument since it's
        ## just one in three goodness of fit plots
        fit.aux <- fit.gev( -1* x.kept,
                           error.estimation = "none" )
      }
    }
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
    } else {
      model <- "gpd"
    }
    if ( input$selectOptimization == "SANN" ||
         input$selectOptimization == "dfoptim::nmk" ){
      ## When performing simulated annealing the hessian was not
      ## calculated so the error estimate of the return levels
      ## will just be calculated via the Monte Carlo approach
      x.confidence.intervals.aux <-
        climex::return.level( fit.aux, return.period = x.period,
                             error.estimation = "MC",
                             threshold = fit.aux$threshold,
                             total.length = length( x.data[[ 3 ]] ),
                             model = model )
    } else {           
      x.confidence.intervals.aux <-
        climex::return.level( fit.aux, return.period = x.period,
                             error.estimation = "MLE",
                             threshold = fit.aux$threshold,
                             total.length = length( x.data[[ 3 ]] ),
                             model = model )
    }
    if ( input$buttonMinMax == "Min" &&
         input$radioEvdStatistics == "GEV" ){
      x.confidence.intervals.aux$return.levels <-
        x.confidence.intervals.aux$return.levels* ( -1 )
    }
    x.confidence.intervals <- data.frame(
        ci.low = x.confidence.intervals.aux$return.levels -
          as.numeric( x.confidence.intervals.aux$errors ),
        level = x.confidence.intervals.aux$return.levels,
        ci.high = x.confidence.intervals.aux$return.levels +
          as.numeric( x.confidence.intervals.aux$errors ) )
    if ( input$radioEvdStatistics == "GEV" ){
      if ( input$buttonMinMax == "Min" ){
        plot.data <- data.frame(
            x = -1/ log( stats::ppoints( length( x.kept ), 0 ) ),
            y = -1* sort( as.numeric( x.kept* -1 ) ) )
      } else {
        plot.data <- data.frame(
            x = -1/ log( stats::ppoints( length( x.kept ), 0 ) ),
            y = sort( as.numeric( x.kept ) ) )
      } 
    } else {
      ## GP
      plot.data <-  data.frame(
          x = -1/ log( stats::ppoints( length( x.kept ), 0 ) ),
          y = sort( as.numeric( x.kept +
                                input$sliderThreshold ) ) )
    }
    plot.y.limits <- c( plot.data$y[ which.min(
                                      abs( plot.data$x - 1 ) ) ],
                       max( x.confidence.intervals[ , 3 ] ) )
    plot.statistics <- data.frame(
        period = -1/ ( log( 1 - 1/ x.period ) ),
        level = as.numeric( x.confidence.intervals[ , 2 ] ),
        ci.low = as.numeric( x.confidence.intervals[ , 1 ] ), 
        ci.high = as.numeric( x.confidence.intervals[ , 3 ] ) )
    gg.rl <- ggplot() +
      geom_point( data = plot.data, aes( x = x, y = y ),
                 colour = colour.ts, shape = 1, size = 2,
                 alpha = 0.8, na.rm = TRUE ) +
      geom_line( data = plot.statistics,
                aes( x = period, y = level ),
                colour = colour.extremes, na.rm = TRUE ) +
      geom_line( data = plot.statistics,
                aes( x = period, y = ci.low ), linetype = 2,
                colour = colour.extremes, na.rm = TRUE ) +
      geom_line( data = plot.statistics,
                aes( x = period, y = ci.high ), linetype = 2,
                colour = colour.extremes, na.rm = TRUE ) +
      xlab( "return period" ) + ylab( "return level" ) +
      theme_bw() + scale_x_log10( limits = c( 1, 1000 ) ) +
      theme( axis.title = element_text( size = 15,
                                       colour = colour.ts ),
            axis.text = element_text( size = 12,
                                     colour = colour.ts ) )
    if ( !is.null( input$buttonMinMax ) ){
      if ( input$buttonMinMax == "Min" &&
           input$radioEvdStatistics == "GEV" ){
        gg.rl <- gg.rl + scale_y_reverse() 
      } else
        gg.rl <- gg.rl + ylim( plot.y.limits )
    }
    return( gg.rl )  } )
########################################################################
  
########################################################################
######################## Deseasonalization of the data #################
########################################################################
  ## since the deseasonalization will also be applied to a large
  ## variety of different
  ## stations in the leaflet plot, it will become a separate function
  deseasonalize.interactive <- function( x.xts ){
    ## Dropdown for deseasonalization method
    if ( is.null( x.xts ) ||
         is.null( input$selectDeseasonalize ) ||
         is.null( input$selectDataBase )
        ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( !is.null( input$selectDataBase ) ){
      if ( input$selectDataBase == "artificial data" ){
        ## For the artificial data there is no need for
        ## deseasonalization
        return( x.xts ) }
    }
    x.deseasonalized <- switch(
        input$selectDeseasonalize,
        "Anomalies" = anomalies( x.xts ),
        "decompose" = {
          ## Remove all NaN or decompose will not work.
          ## But anyway. Just removing the values won't make it
          ## run correctly. But the user is warned to remove the
          ## years.
          if ( any( is.na( x.xts ) ) ){
            x.no.nan <- na.omit( x.xts )
          } else {
            x.no.nan <- x.xts
          }
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
          ## Remove all NaN or stl will not work. But anyway.
          ## Just removing the values won't make it run correctly.
          ## But the user is warned to remove the years.
          if ( any( is.nan( x.xts ) ) ){
            x.no.nan <- na.omit( x.xts )
          } else {
            x.no.nan <- x.xts
          }
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
          ## Remove all NaN or stl will not work. But anyway.
          ## Just removing the values won't make it run correctly.
          ## But the user is warned to remove the years.
          if ( any( is.nan( x.xts ) ) ){
            x.no.nan <- na.omit( x.xts )
          } else {
            x.no.nan <- x.xts
          }
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
      ## I don't wanna any NaN in my time series. I some cases the
      ## deseasonalization methods themselved produces these. It's a
      ## dirty solution, but just omit them and inform the user
      x.deseasonalized <- na.omit( x.deseasonalized )
      shinytoastr::toastr_error(
                       "NaNs produced during the deseasonalization." )
    }
    return( x.deseasonalized ) }
########################################################################
  
########################################################################
################### Fitting of the GEV distribution ####################
########################################################################
  ## Since I will fit a couple of times with the chosen fitting
  ## parameters/algorithms, I will hard code it and just call it at
  ## the required points 
  fit.interactive <- function( x.kept, x.initial = NULL ){
    ## wait for the initialization
    if ( is.null( input$radioEvdStatistics ) ||
         is.null( input$selectOptimization ) ||
         is.null( input$buttonMinMax ) )
      return( NULL )
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
    } else {
      model <- "gpd"
    }
    ## When considering the minima instead of the maxima x*(-1)
    ## will be fitted and the location parameter will be multiplied
    ## by -1 afterwards
    if ( ( input$buttonMinMax == "Min" ) &&
         ( input$radioEvdStatistics == "GEV" ) ){
      x.kept <- x.kept*( -1 )
      if ( !is.null( x.initial ) )
        x.initial[ 1 ] <- -1* x.initial[ 1 ]
    }
    ## Check whether the supplied initial parameter combination can
    ## still be evaluated. This might fail, e.g. when changing between
    ## time series or minimal und maximal extremes.
    if ( is.nan( climex::likelihood( x.initial, x.kept,
                                    model = model ) ) ){
      shinytoastr::toastr_warning(
                       "Initial parameters can not be evaluated. They have been reseted during the fitting procedure!" )
      x.initial <- NULL
    } 
    if ( is.null( x.initial ) ){
        x.initial <- climex::likelihood.initials( x.kept,
                                                 model = model )
    } else {
      ## While changing the EVD statistics from "GEV" to "GP" the initial
      ## parameter combination has to be reset. This is nevertheless a
      ## little bit problematic since both evd.fitting() and
      ## initial.parameters() are labeled dirty during this procedure. So
      ## one can not really control which is evaluted first. But since the
      ## reseting of initial.parameters() would result in the default
      ## setting, we are save to use it in here too.
      if ( input$radioEvdStatistics == "GEV" &&
           length( x.initial ) != 3 )
        x.initial <- NULL
      if ( input$radioEvdStatistics == "GP" &&
           length( x.initial ) != 2 )
        x.initial <- NULL
    }
    if ( input$radioEvdStatistics == "GEV" ){
      ## Fits of GEV parameters to blocked data set
      x.fit.evd <- suppressWarnings( switch(
          input$selectOptimization,
          "Nelder-Mead" = fit.gev( x.kept, initial = x.initial,
                                  rerun = input$checkboxRerun,
                                  method = "Nelder-Mead",
                                  error.estimation = "none" ),
          "CG" = fit.gev( x.kept, initial = x.initial,
                         rerun = input$checkboxRerun,
                         method = "CG", error.estimation = "none" ),
          "BFGS" = fit.gev( x.kept, initial = x.initial,
                           rerun = input$checkboxRerun,
                           method = "BFGS", error.estimation = "none" ),
          "SANN" = fit.gev( x.kept, initial = x.initial,
                           rerun = input$checkboxRerun,
                           method = "SANN", error.estimation = "none" ),
          "dfoptim::nmk" = fit.gev( x.kept, initial = x.initial,
                                   rerun = input$checkboxRerun,
                                   method = "nmk",
                                   error.estimation = "none" ) ) ) 
      class( x.fit.evd ) <- c( "list", "climex.fit.gev" )
    } else {
      ## Fits of GPD parameters to blocked data set
      if ( is.null( input$sliderThreshold ) ){
        threshold <- max( x.kept )* .8
      } else {
        threshold <- input$sliderThreshold
      }
      suppressWarnings(
          x.fit.evd <- suppressWarnings( switch(
              input$selectOptimization,
              "Nelder-Mead" = fit.gpd( x.kept, initial = x.initial,
                                      threshold = threshold,
                                      rerun = input$checkboxRerun,
                                      method = "Nelder-Mead",
                                      error.estimation = "none" ),
              "CG" = fit.gpd( x.kept, initial = x.initial, 
                             threshold = threshold,
                             rerun = input$checkboxRerun,
                             method = "CG", error.estimation = "none" ),
              "BFGS" = fit.gpd( x.kept, initial = x.initial,
                               threshold = threshold,
                               rerun = input$checkboxRerun,
                               method = "BFGS",
                               error.estimation = "none" ),
              "SANN" = fit.gpd( x.kept, initial = x.initial,
                               threshold = threshold,
                               rerun = input$checkboxRerun,
                               method = "SANN",
                               error.estimation = "none" ),
              "dfoptim::nmk" = fit.gpd( x.kept, initial = x.initial,
                                       threshold = threshold,
                                       rerun = input$checkboxRerun,
                                       method = "nmk",
                                       error.estimation = "none" ) ) ) )
      class( x.fit.evd ) <- c( "list", "climex.fit.gpd" )
    }
    if ( ( input$buttonMinMax == "Min" ) &&
         ( input$radioEvdStatistics == "GEV" ) ){
      x.fit.evd$x <- x.fit.evd$x* ( -1 )
      x.fit.evd$par[ 1 ] <- x.fit.evd$par[ 1 ]* ( -1 )
    }
    return( x.fit.evd )
  }
  ## Fitting of the time series selected via a click on the map or
  ## the select form in the sidebar.
  ## For this time series it is possible to exclude individual points
  ## via clicking on them in the time series::remaining plot
  evd.fitting <- reactive( {
    x.block <- data.blocking( )[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.initial <- initial.parameters()
    if ( is.null( x.initial ) ){
      print( "the fitting will start as soon as the initial parameters are available" )
      return( NULL )
    }
    x.kept <- x.block[ reactive.values$keep.rows ]
    return( fit.interactive( x.kept, x.initial ) )
  } )
########################################################################
  
########################################################################
################ table containing fit statistics #######################
########################################################################
  ## Displaying of AIC, nllh, BIC and fitted parameters as well as
  ## the difference to the three last fits! (and highlight positive
  ## values with with green and negative with red)
  last.values <- last.1 <- last.1.aux <- last.1.int <- last.2 <-
    last.3 <- current.white <- rep( 0,  )
  output$tableStatistics <- renderUI({
    ## define the colour for increasing or decreasing values
    ## >0, <0, normal
    css.colours <- c( "#C53100",
                     "#0D8F20" )
    x.fit.evd <- evd.fitting( )
    if ( is.null( x.fit.evd ) )
      return( NULL )
      x.data <- data.blocking()
      x.block <- x.data[[ 1 ]]
      if ( is.null( x.block ) ){
        ## if the initialization has not finished yet just wait a
        ## little longer
        return( NULL )
      }
      if ( input$radioEvdStatistics == "GEV" ){
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
                                 x.fit.evd,
                                 error.estimation = "none",
                                 model = "gpd",
                                 threshold =
                                   input$sliderThreshold,
                                 total.length = x.data[[ 1 ]] ) )
      }
      ## negating the return level to get the correct results for
      ## the minium
      if ( !is.null( input$buttonMinMax ) ){
        if ( input$buttonMinMax == "Min" &&
             input$radioEvdStatistics == "GEV" )
          current[ 7 ] <- ( -1 )* current[ 7 ]
      }
      ## history of the statistics
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
          } else
            last.1.int[ ll ] <- paste( 
                as.character( format(  last.1.aux[ ll ], digits = 4 ) ),
                css.colours[ 1 ] )
        } else {
          last.1.int[ ll ] <- paste(
              as.character( format(  last.1.aux[ ll ], digits = 4 ) ),
              " ", css.colours[ 2 ] ) } }
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
        } }
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
      ## generate a html table with the 'pander' and the 'markdown'
      ## package
      x.html.table <- markdown::markdownToHTML(
                                    text = pander::pandoc.table.return(
                                                       x.table,
                                                       style =
                                                         "rmarkdown",
                                                       split.tables =
                                                         Inf ),
                                    fragment.only = TRUE )
      x.color.table <- climex:::color.table( x.html.table, css.colours ) })
########################################################################
  
########################################################################
################### Likelihood animation (tab2) ########################
########################################################################
  ## Fitting the MLE again with the algorithm of choice
  output$menuSliderLocationLim <- renderMenu( {
    x.fit.evd <- evd.fitting()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    ## Hide input when fitting the GPD
    if ( input$radioEvdStatistics == "GP" )
      return( NULL )
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    sliderInput( "sliderLocationLim", "Location sampling limits",
                round( x.fit.evd$par[ 1 ], 1 ) - 10,
                round( x.fit.evd$par[ 1 ] + 10, 1 ),
                c( round( x.fit.evd$par[ 1 ], 1 ) - 5,
                  round( x.fit.evd$par[ 1 ], 1 ) + 5 ) ) } )
  output$menuSliderScaleLim <- renderMenu( {
    x.fit.evd <- evd.fitting()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      x.fit.evd.scale <- x.fit.evd$par[ 2 ]
    } else {
      x.fit.evd.scale <- x.fit.evd$par[ 1 ]
    }
    sliderInput( "sliderScaleLim", "Scale sampling limits",
                round( max( 0, x.fit.evd.scale  - 10 ), 1 ),
                round( x.fit.evd.scale + 10, 1 ),
                c( round( max( 0, x.fit.evd.scale - 5 ), 1 ),
                  round( x.fit.evd.scale, 1 ) + 5 ) ) } )
  output$menuSliderShapeLim <- renderMenu( {
    x.fit.evd <- evd.fitting()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      x.fit.evd.shape <- x.fit.evd$par[ 3 ]
    } else {
      x.fit.evd.shape <- x.fit.evd$par[ 2 ]
    }
    sliderInput( "sliderShapeLim", "Shape sampling limits",
                round( x.fit.evd.shape - 1, 1 ),
                round( x.fit.evd.shape + 1, 1 ),
                c( round( x.fit.evd.shape, 1 ) - .3,
                  round( x.fit.evd.shape, 1 ) + .3  ) ) } )
  ## To enable the user to input her/his own custom initialization
  ## points for the optimization it needs two things: three
  ## numerical inputs chosing the climex::likelihood.initials as
  ## default and a reactive vector gluing all together
  output$inputInitialLocation <- renderMenu( {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      ## In order to fit the minimal extremes
      if ( ( !is.null( input$buttonMinMax ) ) &&
           ( input$buttonMinMax == "Min" ) )
        x.block <- x.block*( -1 )
    } else {
      model <- "gpd"
    }
    parameter.default <- climex::likelihood.initials( x.block,
                                                     model = model )
    numericInput( "initialLocation", "",
                 value = round( parameter.default[ 1 ], 4 ) ) } )
  output$inputInitialScale <- renderMenu( {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      ## In order to fit the minimal extremes
      if ( ( !is.null( input$buttonMinMax ) ) &&
           ( input$buttonMinMax == "Min" ) )
        x.block <- x.block*( -1 )
    } else {
      model <- "gpd"
    }
    parameter.default <- climex::likelihood.initials( x.block,
                                                     model = model )
    numericInput( "initialScale", "",
                 value = round( parameter.default[ 2 ], 4 ),
                 min = 0 ) } )
  output$inputInitialShape <- renderMenu( {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      ## In order to fit the minimal extremes
      if ( ( !is.null( input$buttonMinMax ) ) &&
           ( input$buttonMinMax == "Min" ) )
        x.block <- x.block*( -1 )
    } else {
      model <- "gpd"
    }
    parameter.default <- climex::likelihood.initials( x.block,
                                                     model = model )
    numericInput( "initialShape", "",
                 value = round( parameter.default[ 3 ], 4 ) ) } )
  ## Why I need this reactive content? Well, not really necessary
  ## but quite convenient
  initial.parameters <- reactive( {
    if ( is.null( input$initialLocation ) ||
         is.null( input$initialScale ) ||
         is.null( input$initialShape ) ){
      ## If the sliders arn't set yet, I will just use the default
      ## values
      x.block <- data.blocking()[[ 1 ]]
      if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
        ## if the initialization has not finished yet just wait a
        ## little longer
        return( NULL )
      }
      if ( input$radioEvdStatistics == "GEV" ){
        model <- "gev"
      } else {
        model <- "gpd"
      }
      return( climex::likelihood.initials( x.block, model = model ) )
    } else
      return( c( input$initialLocation, input$initialScale,
                input$initialShape ) ) } )
  cached.table.init <- NULL
  initial.parameters.likelihood <- reactive( {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.initial <- initial.parameters()
    input$tableDrawPoints
    x.fit.evd <- evd.fitting()
    par.init <- x.fit.evd$par
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      if ( is.null( input$sliderNumberInitialPoints ) ||
           is.null( input$sliderLocationLim ) ||
           is.null( input$sliderScaleLim ) ||
           is.null( input$sliderShapeLim ) ){
        return( c( NaN, NaN, NaN ) )
      }
    } else {
      model <- "gpd"
      if ( is.null( input$sliderNumberInitialPoints ) || 
           is.null( input$sliderScaleLim ) ||
           is.null( input$sliderShapeLim ) ){
        return( c( NaN, NaN ) )
      }
    }
    ## the first entry of the table should always be the actual
    ## point the optimization is starting from
    if ( input$radioEvdStatistics == "GEV" ){
      location <- c( x.initial[ 1 ],
                    round( stats::runif( (
                      input$sliderNumberInitialPoints - 1 ),
                      input$sliderLocationLim[ 1 ],
                      input$sliderLocationLim[ 2 ] ), 4 ) )
      scale <- c( x.initial[ 2 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderScaleLim[ 1 ],
                   input$sliderScaleLim[ 2 ] ), 4 ) )
      shape <- c( x.initial[ 3 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderShapeLim[ 1 ],
                   input$sliderShapeLim[ 2 ] ), 4 ) )
      table.init <- data.frame( location = location,
                               scale = scale, shape = shape )
    } else {
      scale <- c( x.initial[ 2 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderScaleLim[ 1 ],
                   input$sliderScaleLim[ 2 ] ), 4 ) )
      shape <- c( x.initial[ 3 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderShapeLim[ 1 ],
                   input$sliderShapeLim[ 2 ] ), 4 ) )
      table.init <- data.frame( scale = scale, shape = shape )
    }
    ## but we only want to have starting points which do not
    ## result in a NA
    if ( input$radioEvdStatistics == "GEV" ){
      while ( any( is.nan( apply(
          table.init, 1, climex::likelihood,
          x.in = x.block, model = model ) ) ) ){
            for ( ii in 1 : nrow( table.init ) ){
              if ( is.nan( climex::likelihood(
                                       as.numeric( table.init[ ii, ] ),
                                       x.in = x.block,
                                       model = model ) ) )
                table.init[ ii, ] <- c(
                    round( stats::runif( 1, input$sliderLocationLim[ 1 ],
                                        input$sliderLocationLim[ 2 ] ),
                          4 ),
                    round( stats::runif( 1, input$sliderScaleLim[ 1 ],
                                        input$sliderScaleLim[ 2 ] ),
                          4 ),
                    round( stats::runif( 1, input$sliderShapeLim[ 1 ],
                                        input$sliderShapeLim[ 2 ] ),
                          4 ) ) } }
    } else {
      while ( any( is.nan( apply(
          table.init, 1, climex::likelihood,
          x.in = x.block, model = model ) ) ) ){
            for ( ii in 1 : nrow( table.init ) ){
              if ( is.nan( climex::likelihood(
                                       as.numeric( table.init[ ii, ] ),
                                       x.in = x.block,
                                       model = model ) ) )
                table.init[ ii, ] <- c(
                    round( stats::runif( 1, input$sliderScaleLim[ 1 ],
                                        input$sliderScaleLim[ 2 ] ),
                          4 ),
                    round( stats::runif( 1, input$sliderShapeLim[ 1 ],
                                        input$sliderShapeLim[ 2 ] ),
                          4 ) ) } }
    }
    return( table.init ) } )
  output$tableInitialPoints <- renderDataTable( {
    initial.parameters <- initial.parameters.likelihood()
    if ( all( is.na( initial.parameters ) ) )
      return( NULL )
    initial.parameters$ID <- seq( 1, nrow( initial.parameters ) )
    ## round the number in the table
    return( initial.parameters ) },
    ## the drawCallback ensures that the width of the parent table
    ## is not set to a specific pixel number but to 100% percent.
    ## This ensures its correct rendering on mobile devices
    options = list( dom = 't', pageLength = 5,
                   drawCallback = I( "function( settings )
            {document.getElementById( 'tableInitialPoints' ).style.width = '100%';}") ) )
  ## Displaying of the heuristic estimates for a wiser picking of
  ## the limits
  output$tableHeuristicEstimates <- renderDataTable( {
    x.block <- data.blocking()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.fit.evd <- evd.fitting()
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
    } else {
      model <- "gpd"
    }
    x.mle.par <- x.fit.evd$par
    x.initial <- initial.parameters()
    x.suggested <- climex::likelihood.initials( x.block, model = model )
    if ( input$radioEvdStatistics == "GEV" ){
      x.df <- data.frame( parameter = c( "fitting results",
                                        "suggested initials" ),
                         location = c( round( x.mle.par[ 1 ], 4 ),
                                      round( x.suggested[ 1 ], 4 ) ),
                         scale = c( round( x.mle.par[ 2 ], 4 ),
                                   round( x.suggested[ 2 ], 4 ) ),
                         shape = c( round( x.mle.par[ 3 ], 4 ),
                                   round( x.suggested[ 3 ], 4 ) ) )
    } else {
      x.df <- data.frame( parameter = c( "fitting results",
                                        "suggested initials" ),
                         scale = c( round( x.mle.par[ 1 ], 4 ),
                                   round( x.suggested[ 1 ], 4 ) ),
                         shape = c( round( x.mle.par[ 2 ], 4 ),
                                   round( x.suggested[ 2 ], 4 ) ) )
    }            
    return( x.df ) },
    options = list( dom = 't',
                   drawCallback = I( "function( settings )
            {document.getElementById( 'tableHeuristicEstimates' ).style.width = '100%';}") ) )
  ## the two dummies to get the current width of the screen
  output$plotPlaceholder <- renderPlot({
    ttplot( x.block ) } )
  output$plotPlaceholderLeaflet <- renderPlot({
    ttplot( x.block ) } )
  output$loadingImage <- renderUI({
    if ( session$clientData$url_hostname != "localhost" &&
         session$clientData$url_hostname != "127.0.0.1" ){
      folder <- "/assets/"
    } else
      folder <- ""
    return( img( src = paste0( folder, "loading.gif" ),
                id = "loadingGif" ) ) } )
  output$loadingScript <- renderUI({
    if ( session$clientData$url_hostname != "localhost" &&
         session$clientData$url_hostname != "127.0.0.1" ){
      folder <- "/assets/"
    } else
      folder <- ""
    return( div( tags$head( singleton(
                          tags$script( src = paste0( folder,
                                                    "/loadingGif.js"  )
                                      ) ) ) ) ) } )
  output$drawLikelihoodAnimation <- renderUI( {
    ## This reactive content only depends on the action button
    ## because of the use of the isolate() functions.        
    ## Don't make the plot the first time I look at the tab
    if ( input$buttonDrawAnimation < 1 )
      return( NULL )
      isolate( { x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
          ## if the initialization has not finished yet just wait a
          ## little longer
          return( NULL )
        } } )
      isolate( initial.parameters <- initial.parameters.likelihood() )
      isolate( {
        if ( input$selectOptimization == "dfoptim::nmk" ){
          optimization.method <- "nmk"
        } else 
          optimization.method <- input$selectOptimization
        ## I use the plot "plotPlaceholder" to determine the width
        ## of the current box and adjust the pixel width of the png
        ## pictures
        session.width <- session$clientData$output_plotPlaceholder_width
        session.plot.width <- floor( session.width/ 3 )
        print( "starting animation......." )
        ## a temporary folder will be generate to harvest the images
        ## of the animation whenever the animation will be redone
        ## the old folder should be removed to avoid wasting memory
        ## of the server. Therefore the folder name has to be a global
        ## variable
        if ( !is.null( image.folder ) )
          unlink( image.folder, recursive = TRUE )
        ## if the shiny server is running on localhost it is run in
        ## the CLIMEX.PATH folder and the folder containing the images
        ## is constantly overwritten to prevent the script from
        ## occupying to much space. Due to a setwd in the climex()
        ## wrapper we are already in this folder
        if ( session$clientData$url_hostname == "localhost" ||
             session$clientData$url_hostname == "127.0.0.1"  ){
          working.folder <- paste0( CLIMEX.PATH, "app/www" )
          ## in case of the local session the variable image.folder
          ## was already set in the wrapper climex::climex()
        } else {
          ## I decided to make the variable image.folder a global
          ## one because in this way the folder addressed with it
          ## can be deleted the next time this function is called
          working.folder <- "/srv/shiny-server/assets" 
          image.folder <<- paste0(
              "/srv/shiny-server/assets/tmp/images_",
              as.numeric ( as.POSIXct( lubridate::now() ) ) )
        }
        dir.create( image.folder, recursive = TRUE )
        if ( input$radioEvdStatistics == "GEV" ){
          location.lim <- isolate( input$sliderLocationLim )
          model <- "gev"
        } else {
          location.lim <- NULL
          model <- "gpd"
        }
        climex:::animation.wrapper(
                     time.series = x.block,
                     starting.points = initial.parameters,
                     location.lim = location.lim,
                     scale.lim = isolate( input$sliderScaleLim ),
                     shape.lim = isolate( input$sliderShapeLim ),
                     optimization.method = optimization.method,
                     optimization.steps = isolate(
                         input$sliderOptimizationSteps ),
                     optimization.rerun = input$checkboxRerun,
                     height = 300, width = session.plot.width,
                     model = model, delay = 300,
                     loopMode = "loop",
                     image.folder = image.folder,
                     working.folder = working.folder )
        ## if the code is not running on localhost the shiny server
        ## won't find the animation.js script using its absolute path
        if ( session$clientData$url_hostname != "localhost" &&
             session$clientData$url_hostname != "127.0.0.1" ){
          working.folder <- sub( "/srv/shiny-server", "",
                                working.folder )
          animation.script <- "/assets/"
        } else {
          working.folder <- ""
          animation.script <- ""
        }
        return( div(
            class = "scianimator",
            style = "display: inline-block;",
            tags$script( src = paste0( animation.script,
                                      "jquery.scianimator.min.js" ) ),
            tags$script( src = paste0( working.folder,
                                      "/animation.js" ) ),
            div( id = "animationLocSc", class = "animationClimex" ),
            div( id = "animationLocSh", class = "animationClimex" ),
            div( id = "animationScSh", class = "animationClimex" ) ) )
      } ) } )
########################################################################
  selected.station <- callModule(
      climex:::leafletClimex, "leaflet", reactive.chosen,
      reactive( input$buttonMinMax ),
      reactive( input$radioEvdStatistics ),
      reactive( input$sliderYears ), data.blocking,
      evd.fitting,
      reactive( input$sliderThreshold ), fit.interactive,
      cleaning.interactive, deseasonalize.interactive,
      blocking.interactive, reactive( input$selectDataSource ) )
  
   ## })
}

##' @title The user interface for the \code{\link{climex}} function.
##'
##' @param selected Choose which tab is supposed to be selected when
##' starting the app
##'
##' @details Contains all the HTML codes.
##'
##' @return HTML code of the interface
##' @import shiny
##' @importFrom shinydashboard box
##' @importFrom shinydashboard dashboardPage
##' @importFrom shinydashboard dashboardHeader
##' @importFrom shinydashboard dashboardSidebar
##' @importFrom shinydashboard dashboardBody
##' @importFrom shinydashboard menuItemOutput
##' @importFrom shinydashboard menuItem
##' @importFrom shinydashboard renderMenu
##' @importFrom shinydashboard sidebarMenu
##' @importFrom shinydashboard tabItem
##' @importFrom shinydashboard tabItems
##' @importFrom shinydashboard tabBox
##' @importFrom dygraphs dygraphOutput
##' @importFrom htmltools includeCSS
##' @importFrom htmltools h2
##' @export
##' @author Philipp Mueller 
climex.ui <- function( selected = c( "Map", "General", "Likelihood" ) ){
  if ( missing( selected ) )
    selected <- "Map"
  selected <- match.arg( selected )
  dashboardPage(
    shinytoastr::useToastr(),
    header = dashboardHeader(
        title = a(
            "Climex",
            href = "https://github.com/theGreatWhiteShark/climex",
            id = "climexLink" )
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem( "Map", tabName = "tabMap",
                 icon = icon( "leaf", lib = "glyphicon" ),
                 selected = ifelse( selected == "Map", TRUE, FALSE ) ),
        menuItem( "General", tabName = "tabGeneral",
                 icon = icon( "stats", lib = "glyphicon" ),
                 selected = ifelse( selected == "General",
                                   TRUE, FALSE ) ),
        menuItem( "Likelihood", tabName = "tabLikelihood",
                 icon = icon( "ok-circle", lib = "glyphicon" ),
                 selected = ifelse( selected == "Likelihood",
                                   TRUE, FALSE ) ),
        climex:::sidebarDataBaseInput(),
        climex:::sidebarDataSourceInput(),
        climex:::sidebarDataTypeInput(),
        climex:::sidebarLoadingInput(),
        climex:::sidebarCleaningInput(),
        ## while plotting the images for the animation a gif
        ## should be shown
        div( uiOutput( "loadingScript" ), uiOutput( "loadingImage" ),
            id = "loadingWrapper" ) ) ),
    body = dashboardBody(
      ## shinyjs::useShinyjs(),
      includeCSS( paste0( system.file( "climex_app",
                                      package = "climex" ),
                         "/css/climex.css" ) ),
      includeCSS( paste0( system.file( "climex_app",
                                      package = "climex" ),
                         "/css/reset.css" ) ),
      includeCSS( paste0( system.file( "climex_app",
                                      package = "climex" ),
                         "/css/styles.css" ) ),
      includeCSS( paste0( system.file( "climex_app",
                                      package = "climex" ),
                         "/css/scianimator.css" ) ),
      tabItems(
        tabItem(
          tabName = "tabMap",
          tags$style( type = "text/css",
                     "#leaflet-map {height: calc(100vh - 80px) !important;}" ),
         climex:::leafletClimexUI( "leaflet" ) ),
        tabItem(
          tabName = "tabGeneral",
          ## In order guarantee the correct behavior of the rendering
          ## of the boxes and tables for smaller screen sizes too I
          ## assign customized classes to the boxes and reconfigure
          ## their ordering using CSS3 if the max-width is below a
          ## certain threshold
          fluidRow(
            box( title = h2( "GEV fit" ), status = "primary", 
              solidheader = TRUE, width = 8, id = "boxPlotFit",
              column( 9, plotOutput( "plotFitEvd" ) ),
              column( 3, plotOutput( "plotFitQQ", height = 140 ),
                     plotOutput( "plotFitQQ2", height = 140 ),
                     plotOutput( "plotFitReturnLevel",
                                height = 140 ) ) ),
            box( title = h2( "Options" ), width = 4,
              height = 550, background = "orange",
              id = "boxGevStatistics",
              radioButtons( "radioEvdStatistics", label = NULL,
                           inline = TRUE,
                           choices = c( "GEV", "GP" ),
                           selected = "GEV" ),
              climex:::generalExtremeExtractionInput(),
              radioButtons( "buttonMinMax", "Type of extreme",
                           inline = TRUE,
                           choices = c( "Max", "Min" ),
                           selected = "Max" ),
              selectInput( "selectDeseasonalize",
                          "Deseasonalization method",
                          choices = c( "Anomalies", "stl",
                                      "decompose",
                                      "deseasonalize::ds", "none" ),
                          selected = "Anomalies" ),
              selectInput( "selectOptimization", "Fitting routine",
                          choices = c( "Nelder-Mead", "CG",
                                      "BFGS", "SANN",
                                      "dfoptim::nmk" ),
                          selected = c( "Nelder-Mead" ) ) ) ),
          fluidRow(
            box( title = h2( "Results" ), width = 3,
              background = "orange", id = "boxGevResults",
              uiOutput( "tableStatistics", colHeaders = "provided" ) ),
            tabBox( title = h2( "Time series" ),
              selected = "Remaining", width = 9, id = "boxTimeSeries",
              tabPanel( "Pure",
                dygraphOutput( "plotTimeSeries", height = 250 ) ),
              tabPanel( "Deseasonalized",
                dygraphOutput( "plotDeseasonalized", height = 250 ) ),
              tabPanel( "Remaining",
                plotOutput( "plotBlocked", height = 250,
                  click = "plotBlockedClick",
                  brush = brushOpts( id = "plotBlockedBrush" ) ),
                actionButton( "excludeBlockedReset", "Reset" ),
                actionButton( "excludeBlockedToggle", "Brush" ) ) ) ) ),
        tabItem( tabName = "tabLikelihood",
          box( title =
              h2( "Starting points of the optimization routine" ),
            width = 8, status = "primary", id = "boxStartingPoints",
            dataTableOutput( "tableInitialPoints" ),
            ## a plot of height 0? Well, its actually a very nice
            ## trick since I need a width value for the generated
            ## pngs in the animation in pixel. But I really want to
            ## make app to be rendered nicely on different screen
            ## sizes. Via the session$clientData I can access the
            ## width and height of plots. Thus I can access the width
            ## of this specific box via the plotPlaceholder without
            ## seeing it at all.
            plotOutput( "plotPlaceholder", height = 0, width = '100%' ),
            htmlOutput( "drawLikelihoodAnimation" ) ),
          box( title = h2( "Options" ), width = 4,
            background = "orange", id = "boxHeuristic",
            dataTableOutput( "tableHeuristicEstimates" ),
            div( p( "actual initials", id = "tableInitialDescription" ),
                uiOutput( "inputInitialLocation" ),
                uiOutput( "inputInitialScale" ),
                uiOutput( "inputInitialShape" ),
                id = "initialTable" ),
            checkboxInput( "checkboxRerun",
                          "Rerun the optimization", value = TRUE ),
            sliderInput( "sliderNumberInitialPoints",
                        "Number of initial points", 1, 20, 5 ),
            uiOutput( "menuSliderLocationLim" ),
            uiOutput( "menuSliderScaleLim" ),
            uiOutput( "menuSliderShapeLim" ),
            sliderInput( "sliderOptimizationSteps",
                        "Which optimization steps", 0, 1, c( .1, .5 ) ),
            actionButton( "buttonDrawAnimation", "Start animation" ),
            actionButton( "tableDrawPoints", "Reset" ) ) ) ) ) )
}
