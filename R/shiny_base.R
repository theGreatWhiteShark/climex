##' @title Shiny app combining most of the tools used in extreme value analysis using GEV fitting via maximum likelihood.
##'
##' @details It possible to provide a time series of type xts to a list of elements of this type. This app need the its own css file.
##'
##' @param x.input Time series of type xts or list of such time series. Default = NULL
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
    ## will contain the folder in which the images of the animation can be found
    temp.folder <<- NULL
    if ( !is.null( x.input ) ){
        if ( any( class( x.input ) == c( "xts") ) ) {
            x.block <<- block( x.input )
            x.input <<- x.input
            type.input <<- "single"
        } else if ( class( x.input ) == "list" &&
                    all( unique( Reduce( c, lapply( x.input, class ) ) ) == c( "xts", "zoo" ) ) ){
            ## I don't want to have any NULL, NA or whatsoever in those classes
            x.block <<- x.input[[ 1 ]]
            x.input <<- x.input
            type.input <<- "list"
        } else
            warning( "climex::climex: Input has the wrong format!" )
    } else {
        x.input <<- stations.temp.max[[ 303 ]]
        x.block <<- block( anomalies( stations.temp.max[[ 303 ]] ) )
        type.input <<- "none"
    }
    shinyApp( climex.ui, climex.server )
}


##' @title Server-side part of the \code{\link{climex}} function.
##'
##' @details Since it grew organically most of the features are defined inside the function
##'
##' @return Function acting as the shiny server.
##' @import shiny
##' @import xts
##' @import dygraphs
##' @import ggplot2
##' @author Philipp Mueller 
climex.server <- function( input, output ){
####################################################################################
######################## Customizing the sidebar menu ##############################    
####################################################################################
    output$menu.select.data.base <- renderMenu( {
        if ( type.input != "none" ){
            selectInput( "select.data.base", "Data base",
                        choices = c( "input", "DWD", "artificial data" ), selected = "input" )
        } else
            selectInput( "select.data.base", "Data base",
                        choices = c( "input", "DWD", "artificial data" ), selected = "DWD" )
    } )
    output$menu.select.data.source.1 <- renderMenu( {
        file.loading()
        if ( !is.null( input$select.data.base ) ){
            if ( input$select.data.base == "DWD" ){
                if ( input$select.data.type == "Daily max. temp." ){
                    selectInput( "select.data.source", "Station",
                                choices = names( stations.temp.max ),
                                selected = "Potsdam" )
                } else if ( input$select.data.type == "Daily min. temp." ){
                    selectInput( "select.data.source", "Station",
                                choices = names( stations.temp.min ),
                                selected = "Potsdam" )
                } else if ( input$select.data.type == "Daily precipitation" ){
                    selectInput( "select.data.source", "Station",
                                choices = names( stations.prec ),
                                selected = "Potsdam" )
                }
            } else if ( input$select.data.base == "artificial data" ){
                sliderInput( "slider.artificial.data.location", "location", -30, 30, 1, round = 15 )
            } else if ( input$select.data.base == "input" ){
                if ( type.input == "single" ){
                    NULL
                } else if ( type.input == "list" ){
                    if ( !is.null( names( x.input ) ) ){
                        ## it's a named list
                        selectInput( "select.list.entry.name", "Entry", choices = names( x.input ),
                                    selected = names( x.input )[ 1 ] )
                    } else
                        selectInput( "select.list.entry.numerical", "Entry",
                                    choices = 1 : length( x.input ), selected = 1 )
                }
            }
        } else
            NULL } )
    output$menu.select.data.source.2 <- renderMenu( {
        if ( !is.null( input$select.data.base ) ){
            if ( input$select.data.base == "DWD" ){
                selectInput( "select.data.type", "Measured variable",
                            choices = c( "Daily max. temp.", "Daily min. temp.",
                                        "Daily precipitation" ),
                            selected = "Daily max. temp" )                        
            } else if ( input$select.data.base == "artificial data" ){
                sliderInput( "slider.artificial.data.scale", "scale", 0, 4, 0.8, round = -2 )
            } else if ( input$select.data.base == "input" ){
                NULL
            }
        } else
            NULL } )
    output$menu.select.data.source.3 <- renderMenu( {
        if ( !is.null( input$select.data.base ) ){
            if ( input$select.data.base == "DWD" ){
                NULL
            } else if ( input$select.data.base == "artificial data" ){
                sliderInput( "slider.artificial.data.shape", "shape", -1.5, 1.5, -0.25, round = -2 )
            } else if ( input$select.data.base == "input" ){
                fileInput( "file.input.selection", "Choose a .RData file" )
            }
        } else
            NULL } )
    output$slider.gev.statistics <- renderMenu( {
        x.deseasonalized <- deseasonalization()
        if ( input$radio.gev.statistics == "Blocks" ){
            sliderInput( "slider.box.length", "Box length in days:", 1, 365*3, 365 )
        } else {
         ##  if ( input$button.min.max == "Max" || is.null( input$button.min.max ) ){
                sliderInput( "slider.threshold.gev", "Threshold:",
                            round( min( x.deseasonalized, na.rm = TRUE ) ),
                            round( max( x.deseasonalized, na.rm = TRUE ) ),
                            round( 0.8* max( x.deseasonalized, na.rm = TRUE ) ) )
          ##   } else
        ##         sliderInput( "slider.threshold.gev", "Threshold:",
        ##                     round( min( x.deseasonalized, na.rm = TRUE ) ),
        ##                     round( max( x.deseasonalized, na.rm = TRUE ) ),
        ##                     round( 0.8* min( x.deseasonalized, na.rm = TRUE ) ) )
        }
    } )
    output$menu.data.cleaning <- renderMenu( {
        ## When applying the blocking method incomplete years distort the time series and
        ## have to be excluded. When using the threshold method on the other hand clusters
        ## are most likely to occure due to short range correlations. This has to be
        ## avoided by using declustering algorithms (which mainly picks the maximum of a
        ## specific cluster)
        if ( !is.null( input$radio.gev.statistics ) ) {
            if ( input$radio.gev.statistics == "Blocks" ){
                checkboxInput( "check.box.incomplete.years", "Remove incomplete years", FALSE )
            } else
                checkboxInput( "check.box.decluster", "Declustering of the data", FALSE )
        } else
            checkboxInput( "check.box.incomplete.years", "Remove incomplete years", FALSE )
    } )
#################################################################################### 
    
####################################################################################
#################### Data generation and selection #################################
####################################################################################
    file.loading <- reactive( {
        ## If no file is chosen, don't do anything
        if ( !is.null( input$file.input.selection$datapath ) ){
            ## load selected file
            load( input$file.input.selection$datapath, file.load.env <- new.env() )
            if ( length( ls( file.load.env ) ) > 1 ){
                ## more than one object was contained in the .RData file
                file.list <- list()
                for ( ll in ls( file.load.env ) )
                    file.list <- c( file.list, get( ll, envir = file.load.env ) )
                if ( !all( unique( lapply( file.list, class )[[ 1 ]] ) == c( "xts", "zoo" ) ) )
                    stop( "selected file has to contain objects of type xts or a list of those!" )
                x.input <<- file.list
                type.input <- "list"
            } else {
                file.input <- get( ls( file.load.env ), envir = file.load.env )
                if ( class( file.input ) == "list" ){
                    if ( !all( unique( lapply( file.input, class )[[ 1 ]] ) == c( "xts", "zoo" ) ) )
                        stop( "selected file has to contain objects of type xts or a list of those!" )
                    x.input <<- file.input
                    type.input <- "list"
                } else if ( any( class( file.input ) == "xts" ) ){
                    x.input <<- file.input
                    type.input <- "single"
                } else
                    stop( "selected file has to contain objects of type xts or a list of those!" )
            }
        }
    } )          
    data.selection <- reactive( {
        ## Selecting the data out of a pool of different possibilities or generate them
        ## artificially
        ## The cutting of the time series to a specific range is not sone here but in
        ## the generate.data function
        file.loading()
        if ( is.null( input$select.data.base ) ){
            x.xts <- x.input
        } else if ( input$select.data.base == "DWD" ){
            if ( is.null( input$select.data.source ) ){
                map.click <- input$leaflet.map_marker_click
                ## no station choosen yet. Using Potsdam as default
                if ( !is.null( map.click ) ){
                    data.selected <- data.chosen()
                    station.name <- as.character(
                        data.selected[[ 2 ]]$name[ which( data.selected[[ 2 ]]$latitude %in%
                                                          map.click$lat &
                                                          data.selected[[ 2 ]]$longitude %in%
                                                          map.click$lng ) ] )
                    if ( input$select.data.type == "Daily max. temp." ){
                    x.xts <- stations.temp.max[[ which( names( stations.temp.max ) %in%
                                                        station.name ) ]]
                    } else if ( input$select.data.type == "Daily min. temp." ){
                        x.xts <- stations.temp.min[[ which( names( stations.temp.min ) %in%
                                                            station.name ) ]]
                    } else if ( input$select.data.type == "Daily precipitation" ){
                        x.xts <- stations.prec[[ which( names( stations.prec ) %in%
                                                    station.name ) ]]
                    }
                } else {
                    if ( is.null( input$select.data.type ) ){
                        x.xts <- stations.temp.max[[ which( names( stations.temp.max ) == "Potsdam" ) ]]
                    } else if ( input$select.data.type == "Daily max. temp." ){
                        x.xts <- stations.temp.max[[ which( names( stations.temp.max ) == "Potsdam" ) ]]
                    } else if ( input$select.data.type == "Daily min. temp." ){
                        x.xts <- stations.temp.min[[ which( names( stations.temp.min ) == "Potsdam" ) ]]
                    } else if ( input$select.data.type == "Daily precipitation" ){
                        x.xts <- stations.prec[[ which( names( stations.prec ) == "Potsdam" ) ]]
                    } else
                        x.xts <- stations.temp.max[[ which( names( stations.temp.max ) == "Potsdam" ) ]]
                }
            } else {
                if ( input$select.data.type == "Daily max. temp." ){
                    x.xts <- stations.temp.max[[ which( names( stations.temp.max ) %in%
                                                        input$select.data.source ) ]]
                } else if ( input$select.data.type == "Daily min. temp." ){
                    x.xts <- stations.temp.min[[ which( names( stations.temp.min ) %in%
                                                        input$select.data.source ) ]]
                } else if ( input$select.data.type == "Daily precipitation" ){
                    x.xts <- stations.prec[[ which( names( stations.prec ) %in%
                                                input$select.data.source ) ]]
                }
            }
        } else if( input$select.data.base == "artificial data" ){
            ## For the artificial data set the length is determined by the Potsdam
            ## temperature series
            x.xts <- xts( extRemes::revd( length( stations.temp.max[[ 64 ]] ),
                                         loc = input$slider.artificial.data.location,
                                         scale = input$slider.artificial.data.scale,
                                         shape = input$slider.artificial.data.shape, type = "GEV" ),
                         order.by = index( stations.temp.max[[ 64 ]] ) )
        } else if ( input$select.data.base == "input" ){
            if ( type.input == "single" ){
                x.xts <- x.input
            } else if ( type.input == "list" ){
                if ( !is.null( names( x.input ) ) ){
                    x.xts <- x.input[[ which( names( x.input ) %in%
                                              input$select.list.entry.name ) ]]
                } else
                    x.xts <- x.input[[ as.numeric( input$select.list.entry.numerical ) ]]
            }
        }
        ## getting rid of artifacts
        x.xts[ which( x.xts == -999 ) ] <- NA
        if ( !is.null( input$check.box.incomplete.years ) ){
            if ( input$check.box.incomplete.years ){
            ## Remove all incomplete years from time series
                x.xts <- remove.incomplete.years( x.xts ) }
        }
        return( x.xts ) } )
    data.blocking <- reactive( {
        ## Decides if either the GEV distribution with block maxima or the Pareto
        ## distribution if threshold excedence should be considered.
        ## Import data set, cut it to the desired intervale, deseasonalize and block it
        x.xts <- data.selection( )
        x.deseasonalized <- deseasonalization( )
        ## Toggle if maxima of minima are going to be used
        if ( is.null( input$button.min.max ) ){
            block.mode <- "max"
        } else if ( input$button.min.max == "Max" ){
            block.mode <- "max"
        } else
            block.mode <- "min"
        if ( is.null( input$radio.gev.statistics ) ){
            ## While initialization input$radio.gev.statistics and input$slider.box.length are
            ## NULL. Therefore this is the fallback default x.block
            x.block <- block( x.deseasonalized, separation.mode = "years", block.mode = block.mode )
        } else if ( input$radio.gev.statistics == "Blocks" ){
            ## Box size as dynamic input parameter
            if ( is.null( input$slider.box.length ) ||  input$slider.box.length == 366 ||
                 input$slider.box.length == 365 ){
                x.block <- block( x.deseasonalized, separation.mode = "years",
                                 block.mode = block.mode )
            } else
                x.block <- block( x.deseasonalized, block.length = input$slider.box.length,
                                 block.mode = block.mode )
        } else if ( input$radio.gev.statistics == "Threshold" ){
            ## Due to "historical" reasons the vector containing the resulting values will
            ## still be called x.block. The "block.mode" and the corresponding
            ## "input$button.min.max" are still use full and decide if the values above
            ## or below the threshold are going to be extracted
            if ( is.null( input$slider.threshold.gev ) ){
                x.block <- x.deseasonalized[ x.deseasonalized >= max( x.deseasonalized )* .8 ]
            } else
                x.block <- x.deseasonalized[ x.deseasonalized >= input$slider.threshold.gev ]
            if ( !is.null( input$check.box.decluster ) ){
                if ( input$check.box.decluster )
                    x.block <- declustering( x.block, input$slider.threshold.gev )
            }           
        }
        x.block <<- x.block
        return( list( blocked.data = x.block, deseasonalized.data = x.deseasonalized,
                     pure.data = x.xts ) ) } )
####################################################################################
    
####################################################################################
##################### Interactive stuff and auxillary functions ####################
####################################################################################
    ## By doing this the value is temporary removed from the ts (after removal of
    ## incomplete years) and a new extremum is calculated for the correspondig year
    reactive.values <- reactiveValues(
        keep.rows = rep( TRUE, length( x.block[[ 1 ]] ) ) ) # which deleted
    ## Update the length whenever another data source is loaded
    generate.data <- reactive( {
        x.data <- data.blocking()
        reactive.values$keep.rows <- rep( TRUE, length( x.data[[ 1 ]] ) )
        return( x.data ) } )
    ## Toggle points that are clicked
    observeEvent( input$plot.blocked.click, {
        x.block <- generate.data()[[ 1 ]]
        df.block <- data.frame( date = index( x.block ), value = as.numeric( x.block ) )
        result <- nearPoints( df.block, input$plot.blocked.click, allRows = TRUE )
        reactive.values$keep.rows <- xor( reactive.values$keep.rows, result$selected_ ) } )
    ## Toggle points that are brushed
    observeEvent( input$exclude.blocked.toggle, {
        x.block <- generate.data()[[ 1 ]]
        df.block <- data.frame( date = index( x.block ), value = as.numeric( x.block ) )
        result <- brushedPoints( df.block, input$plot.blocked.brush, allRows = TRUE )
        reactive.values$keep.rows <- xor( reactive.values$keep.rows, result$selected_ ) } )
    ## Reset plot
    observeEvent( input$exclude.blocked.reset, {
        x.block <- generate.data()[[ 1 ]]
        reactive.values$keep.rows <- rep( TRUE, length( x.block ) ) } )
############################# declustering of the ts ###############################
    ## Inspired by extRemes::decluster.intervals and based on Ferro, C. A. T. and Segers, J.
    ## (2003). Inference for clusters of extreme values. _Journal of the Royal Statistical
    ## Society B_, *65*, 545-556.
    declustering <- function( x, threshold ){
        ## Caution: x is the full time series and not the blocked one!
        x.extremal.index <- extRemes::extremalindex( x, threshold, na.action = na.omit )
        ifelse( x.extremal.index[ 1 ] >= 1, cluster.size <- 0, cluster.size <-
                                                                   x.extremal.index[ 3 ] )
        ## cluster.size is the number of indices a two points have to be away from each
        ## other to belong to different indices
        n <- length( x )
        which.x.over.threshold <- x > threshold
        which.x.over.threshold <- na.omit( which.x.over.threshold )
        x.over.threshold <- x[ which.x.over.threshold ]
        n.over.threshold <- sum( which.x.over.threshold ) # amount of points over threshold
        index.x.over.threshold <- ( 1 : n )[
            na.omit( which.x.over.threshold ) ] # index of those points in the ts 'x'
        which.cluster <- rep( 1, n.over.threshold )
        x.result <- x
        ## number of indices the threshold exceedences are apart from each other
        x.distances <- diff( index.x.over.threshold ) 
        ## which point belongs to which cluster
        which.cluster[ 2 : n.over.threshold ] <- 1 + cumsum( x.distances > cluster.size ) 
        x.cluster <- split( x.over.threshold, which.cluster )
        x.cluster.max <- as.numeric( lapply( x.cluster, max ) )
        ## Only the maxima of the clusters survive
        x.over.threshold[ -which( x.over.threshold %in% x.cluster.max ) ] <- NA
        x.result[ which.x.over.threshold ] <- x.over.threshold
    }
####################################################################################
    
####################################################################################
########################## Plotting of the time series and fit #####################
####################################################################################    
    ## Displaying of the original, intermediate and final time series.
    colour.ts <- grDevices::rgb( 0.098, 0.098, 0.44 )
    colour.extremes <- grDevices::rgb( 1, 0.55, 0 )
    colour.ts.light <- "#7171EC"
    colour.extremes.light <- grDevices::rgb( 1, 0.9, 0.8 )
    function.get.y.label <- function( input ){
        if ( is.null( input$select.data.base ) ){
                y.label <- "temperature in °C"            
        } else if ( input$select.data.base == "artificial data" ){
            y.label <- "GEV sample"
        } else if ( input$select.data.base == "DWD" ){
            if ( is.null( input$select.data.type ) ){
                y.label <- "temperature in °C"
            } else if ( input$select.data.type == "Daily precipitation" ){
                y.label <- "precipitation in mm"
            } else {
                y.label <- "temperature in °C" }
        } else if ( input$select.data.base == "input" ){
            y.label <- "input"
        }
        return( y.label ) }
    ## Pure time series 
    output$plot.time.series <- renderDygraph( {
        x.data <- generate.data( )
        x.blocked <- x.data[[ 3 ]][ which( index( x.data[[ 3 ]] ) %in% index( x.data[[ 1 ]] ) ) ]
        plot.blocked <- x.data[[ 3 ]]
        plot.blocked[ !index( x.data[[ 3 ]] ) %in% index( x.blocked ) ] <- NA
        y.label <- function.get.y.label( input )
        bind.dy <- cbind( x.data[[ 3 ]], plot.blocked )
        names( bind.dy ) <- c( "pure ts", "annual maxima" )        
        dygraph( bind.dy, ylab = y.label ) %>%
            dySeries( "pure ts", color = colour.ts ) %>%
            dySeries( "annual maxima", color = colour.extremes, drawPoints = TRUE,
                     strokeWidth = 0, pointSize = 2 ) } )
    ## deseasonalized time series
    output$plot.deseasonalized <- renderDygraph( {
        x.data <- generate.data( )
        x.blocked <- x.data[[ 2 ]][ which( index( x.data[[ 2 ]] ) %in% index( x.data[[ 1 ]] ) ) ]
        plot.blocked <- x.data[[ 2 ]]
        plot.blocked[ !index( x.data[[ 2 ]] ) %in% index( x.blocked ) ] <- NA
        y.label <- function.get.y.label( input )
        bind.dy <- cbind( x.data[[ 2 ]], plot.blocked )
        names( bind.dy ) <- c( "deseasonalized ts", "annual maxima" )        
        dygraph( bind.dy, ylab = y.label ) %>%
            dySeries( "deseasonalized ts", color = colour.ts ) %>%
            dySeries( "annual maxima", color = colour.extremes, drawPoints = TRUE,
                     strokeWidth = 0, pointSize = 2 ) } )
    ## blocked time series
########## Using ggplot2 for an interactive excluding of points in x.block #########    
    ## Plot the result
    output$plot.blocked <- renderPlot( {
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.excluded <- x.block[ !reactive.values$keep.rows ]
        plot.kept <- data.frame( date = index( x.kept ), value = as.numeric( x.kept ) )
        plot.excluded <- data.frame( date = index( x.excluded ),
                                    value = as.numeric( x.excluded ) )
        y.label <- function.get.y.label( input )
        ggplot() + geom_line( data = plot.kept, aes( x = date, y = value ),
                             colour = colour.ts ) +
            geom_point( data = plot.kept, aes( x = date, y = value ), colour = colour.extremes,
                       fill = colour.extremes, size = 2, shape = 21 ) +
            geom_point( data = plot.excluded, aes( x = date, y = value ),
                       colour = colour.extremes,
                       fill = "white", size = 2, shape = 21 ) + ylab( y.label ) + theme_bw()
    } )
###### GEV fit and analysis plots ##################################################  
    output$plot.fit.gev <- renderPlot( {
        ## Plots the result of the fitted GEV
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        if ( !is.null( input$button.min.max ) ){
            if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" )
                x.kept <- ( -1 )* x.kept
        }
        x.lim <- c( max( x.kept ), min( x.kept ) )
        x.gev.fit <- fit.gev( )
        ## the amount of bins is changing whenever a single event is toggled. This is distracting.
        ## only if a certain amount of points are toggled (5%) a different number of bins shall
        ## be used. The number of boxes should be half the number of points in 5% steps.
        ## gg1.bins gives a factor which multiplied with the length of x.kept yields the number
        ## of blocks.
        gg1.bins <- ( ( ( length( x.kept ) - 1 )*100/ length( x.block ) )  %/% 5 )* 0.025
        ## for later usage: bins are at least this wide
        gg1.bin.width <- ( ( max( x.kept ) - min( x.kept ) )/( gg1.bins * length( x.kept ) ) )* 1.1
        if ( input$radio.gev.statistics == "Blocks" ){
            ## GEV
            ## determining the limits of the PDF plot
            threshold.pdf.plot <- 5E-4
            plot.range <- seq( x.gev.fit$par[ 1 ] - x.gev.fit$par[ 2 ]* 10,
                              x.gev.fit$par[ 1 ] + x.gev.fit$par[ 2 ]* 10, 0.01 )
            plot.data <- data.frame( x = plot.range, y = ismev::gev.dens( x.gev.fit$par,
                                                                         plot.range ) )
            plot.lim <- c(
                plot.data[[ 1 ]][ which.min(
                             abs( plot.data[[ 2 ]][ 1 : which.max( plot.data[[ 2 ]] ) ]-
                                  threshold.pdf.plot ) ) ],
                plot.data[[ 1 ]][ which.min( abs( plot.data[[ 2 ]][
                             which.max( plot.data[[ 2 ]] ) : length( plot.data[[ 2 ]] ) ] -
                             threshold.pdf.plot ) ) + which.max( plot.data[[ 2 ]] ) - 1 ] )
            if ( plot.lim[ 1 ] > x.lim[ 1 ] )
                plot.lim[ 1 ] <- x.lim[ 1 ] - abs( x.lim[ 1 ] )* 0.05 
            if ( plot.lim[ 2 ] < x.lim[ 2 ] )
                plot.lim[ 2 ] <- x.lim[ 2 ] + abs( x.lim[ 2 ] )* 0.05 
            plot.lim <- c( min( plot.lim[ 1 ], min( x.kept ) - gg1.bin.width ),
                          max( plot.lim[ 2 ], max( x.kept ) + gg1.bin.width ) )
        } else {
            ## Generalized Pareto
            ## Since the Pareto function does not have a location parameter
            if ( is.null( input$slider.threshold.gev ) ){
                threshold <- max( x.kept )* .8
            } else 
                threshold <- input$slider.threshold.gev
            plot.range <- seq( x.lim[ 2 ], x.lim[ 1 ]* 1.1, 0.01 )
            plot.data <- data.frame( x = plot.range,
                                    y = ismev::gpd.dens( x.gev.fit$par, threshold, plot.range ) )
            plot.data <- plot.data[ !is.na( plot.data[[ 2 ]] ), ]
            plot.data[ nrow( plot.data ) + 1, ] <- c( plot.data[[ 1 ]][ 1 ],
                                                     plot.data[[ 2 ]][ nrow( plot.data ) ] )
            plot.lim <- c( min( min( plot.range ), min( x.kept ) - gg1.bin.width ),
                          max( max( plot.range ), max( x.kept ) + gg1.bin.width ) )
        }
        ## splitting the plot.data$y in half and determining which index is closest to threshold
        x.label <- function.get.y.label( input )
        gg1 <- ggplot() + geom_histogram( data = x.kept, colour = colour.ts, alpha = 1,
                                         aes( x = as.numeric( x.kept ), y = ..density..,
                                             fill = colour.ts.light ), na.rm = TRUE,
                                         bins = gg1.bins* length( x.kept ) ) +
            geom_polygon( data = plot.data, alpha = 0.7, colour = colour.ts,
                         aes( x = x, y = y, fill = colour.extremes ) ) +            
            scale_fill_manual( values = c( colour.ts.light, colour.extremes ),
                              labels = c( "Histogram data", "Fitted distribution"  ) ) +
            theme_bw() + xlab( x.label ) + xlim( plot.lim ) +
            theme( legend.position = "none" )
        if ( is.null( input$button.min.max ) ){
            return( gg1 )
        } else if ( input$button.min.max == "Min" ){
            ## Adding a note when the minima are fitted
            ## upper point of the density plot
            y.lim.density <- max( density( x.kept )$y )
            ## upper point of the histogram (at least more or less since a different
            ## amount of breaks are used )
            y.lim.histogram <- max( hist( x.kept, plot = FALSE )$density )
            if ( input$radio.gev.statistics == "Threshold" ){
                plot.text <- data.frame( x = max( x.lim ),
                                        y = max( y.lim.density, y.lim.histogram )* 1.2,
                                        label = "No minimum extremes supported \n for the Generalized Pareto distribution!" )
            } else
                plot.text <- data.frame( x = max( x.lim ),
                                        y = max( y.lim.density, y.lim.histogram )* 1.2,
                                        label = "Since the minimal extremes are chosen the GEV distribution \n will be fitted to the negated time series" )
            gg1 <- gg1 + geom_label( data = plot.text, aes( x = x, y = y, label = label ),
                                         fill = "white", colour = "firebrick", fontface = "bold")
        }
        return( gg1 )
    } )
    output$plot.fit.qq <- renderPlot( {
        ## Quantile-quantile plot for fit statistics
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.gev.fit <- fit.gev()
        if ( input$radio.gev.statistics == "Blocks" ){
            if ( is.null( input$button.min.max ) ){
                plot.data <- data.frame(
                    model = extRemes::qevd( ppoints( length( x.kept ), 0 ),
                                           loc = x.gev.fit$par[ 1 ], scale = x.gev.fit$par[ 2 ],
                                           shape = x.gev.fit$par[ 3 ], type = "GEV" ),
                    empirical = sort( as.numeric( x.kept ) ) )
            } else if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" ){
                plot.data <- data.frame(
                    model = extRemes::qevd( ppoints( length( x.kept ), 0 ),
                                           loc = x.gev.fit$par[ 1 ], scale = x.gev.fit$par[ 2 ],
                                           shape = x.gev.fit$par[ 3 ], type = "GEV" ),
                    empirical = -1* sort( as.numeric( -x.kept ) ) )
            } else
                plot.data <- data.frame(
                    model = extRemes::qevd( ppoints( length( x.kept ), 0 ),
                                           loc = x.gev.fit$par[ 1 ], scale = x.gev.fit$par[ 2 ],
                                           shape = x.gev.fit$par[ 3 ], type = "GEV" ),
                    empirical = sort( as.numeric( x.kept ) ) )
        } else {
            if ( is.null( input$slider.threshold.gev ) ){
                threshold <- max( x.kept )* .8
            } else 
                threshold <- input$slider.threshold.gev
            plot.data <- data.frame(
                model = extRemes::qevd( ppoints( length( x.kept ), 0 ), scale = x.gev.fit$par[ 1 ], 
                                       shape = x.gev.fit$par[ 2 ], type = "GP",
                                       threshold = threshold ),
                empirical = sort( as.numeric( x.kept ) ) ) }
        plot.fit <- lm( model ~ empirical, plot.data )[[ 1 ]]
        gg.qq1 <- ggplot() + geom_point( data = plot.data, aes( x = model, y = empirical ),
                                        colour = colour.ts,
                                        shape = 1, size = 2, alpha = 0.8 ) +
            geom_abline( intercept = plot.fit[ 1 ], slope = -plot.fit[ 2 ], colour = colour.ts,
                        linetype = 2 ) +
            geom_abline( intercept = 0, slope = 1, colour = colour.extremes ) +
            theme_bw()
        if ( !is.null( input$button.min.max ) ){
            if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks")
                gg.qq1 <- gg.qq1 + scale_y_reverse()
        }
        return( gg.qq1 ) } )
    output$plot.fit.qq2 <- renderPlot( {
        ## Quantile-quantile plot for fit statistics with samples drawn from fitted GEV
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.gev.fit <- fit.gev()
        if ( input$radio.gev.statistics == "Blocks" ){
            sampled <- sort( extRemes::revd( length( x.kept ), loc = x.gev.fit$par[ 1 ],
                                            scale = x.gev.fit$par[ 2 ],
                                            shape = x.gev.fit$par[ 3 ], type = "GEV" ) )
            if ( !is.null( input$button.min.max ) ){
                if ( input$button.min.max == "Min" )
                    sampled <- -1* sampled
            }
        } else{
            sampled <- sort( extRemes::revd( length( x.kept ), scale = x.gev.fit$par[ 1 ],
                                            shape = x.gev.fit$par[ 2 ], type = "GEV",
                                            threshold = input$slider.threshold.gev ) )
        }
        if ( is.null( input$button.min.max ) ){
            empirical <- sort( as.numeric( x.kept ) )
        } else if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" ){
            empirical <- -1* sort( as.numeric( -x.kept ) )
        } else
            empirical <- sort( as.numeric( x.kept ) )
        length.e <- length( empirical )
        length.s <- length( sampled )
        ## inspired by extRemes::qqplot( plot.data$empirical, plot.data$sampled )
        ## function giving a linear interpolation 
        function.sampled.interpolate <- approxfun( seq( 0, 1, length = length( sampled ) ),
                                                  sort( sampled ), yleft = NA, yright = NA )
        if ( is.null( input$button.min.max ) ){
            period <- ( 1 : length( empirical ) - 1 )/ ( length( empirical ) - 1 )
        } else if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" ){
            period <- rev( ( 1 : length( empirical ) - 1 )/ ( length( empirical ) - 1 ) )
        } else
            period <- ( 1 : length( empirical ) - 1 )/ ( length( empirical ) - 1 )
        sampled.interpolate <- function.sampled.interpolate( period )
        sampled.ci.low <- function.sampled.interpolate(
            period - 1.36/ sqrt( length.e* length.s/ ( length.e + length.s ) ) )
        sampled.ci.high <- function.sampled.interpolate(
            period + 1.36/ sqrt( length.e* length.s/ ( length.e + length.s ) ) )
        plot.data <- data.frame( empirical = empirical, sampled = sampled.interpolate,
                                ci.low = sampled.ci.low, ci.high = sampled.ci.high )
        plot.fit <- lm( empirical ~ sampled, plot.data )[[ 1 ]]
        gg.qq2 <- ggplot() + geom_point( data = plot.data, aes( x = sampled, y = empirical ),
                                        colour = colour.ts, shape = 1, size = 2, alpha = 0.8,
                                        na.rm = TRUE ) +
            geom_line( data = plot.data, aes( x = sampled.ci.low, y = empirical ), linetype = 2,
                      colour = colour.extremes, na.rm = TRUE ) +
            geom_line( data = plot.data, aes( x = sampled.ci.high, y = empirical ), linetype = 2,
                      colour = colour.extremes, na.rm = TRUE ) +
            geom_abline( intercept = plot.fit[ 1 ], slope = plot.fit[ 2 ], colour = colour.ts,
                        linetype = 2 ) + theme_bw() +
            geom_abline( intercept = 0, slope = 1, colour = colour.extremes )
        if ( !is.null( input$button.min.max ) ){
            if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" )
                gg.qq2 <- gg.qq2 + scale_y_reverse() + scale_x_reverse()
        }
        return( gg.qq2 )        
    } )
    output$plot.fit.return.level <- renderPlot( {
        x.block <- generate.data()[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.gev.fit <- fit.gev()
        if ( input$select.optimization == "ismev::gev.fit" ){
            ## blame the extRemes package
            x.gev.fit <- gev.fit( x.kept, method = "Nelder-Mead" )
        }
        x.period <- c( 2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800 )
        if ( input$radio.gev.statistics == "Blocks" ){
            if ( is.null( input$button.min.max ) ){
                ## the true block maxima and their return levels will be calculated
                x.confidence.intervals <- extRemes::ci.fevd.mle( as.fevd( x.kept, x.gev.fit,
                                                                         type = "GEV"),
                                                                return.period = x.period )
                plot.data <- data.frame( x = -1/ log( ppoints( length( x.kept ), 0 ) ),
                                        y = sort( as.numeric( x.kept ) ) )
                plot.y.limits <- c( plot.data$y[ which.min( abs( plot.data$x - 1 ) ) ],
                                   max( x.confidence.intervals[ , 3 ] ) )
            } else if ( input$button.min.max == "Min" ){
                ## the time series will be negated and the results too to aquire the
                ## return levels of the minima
                x.confidence.intervals <- ( -1 )* extRemes::ci.fevd.mle( as.fevd( -x.kept, x.gev.fit,
                                                                                 type = "GEV"),
                                                                        return.period = x.period )
                plot.data <- data.frame( x = -1/ log( ppoints( length( x.kept ), 0 ) ),
                                        y = -1* sort( as.numeric( -x.kept ) ) )
                plot.y.limits <- c( min( x.confidence.intervals[ , 3 ] ),
                                   max( plot.data$y ) )
            } else {
                ## the true block maxima and their return levels will be calculated
                x.confidence.intervals <- extRemes::ci.fevd.mle( as.fevd( x.kept, x.gev.fit,
                                                                         type = "GEV"),
                                                                return.period = x.period )
                plot.data <- data.frame( x = -1/ log( ppoints( length( x.kept ), 0 ) ),
                                        y = sort( as.numeric( x.kept ) ) )
                plot.y.limits <- c( plot.data$y[ which.min( abs( plot.data$x - 1 ) ) ],
                                   max( x.confidence.intervals[ , 3 ] ) )
            } 
            plot.statistics <- data.frame( period = -1/ ( log( 1 - 1/ x.period ) ),
                                          level = as.numeric( x.confidence.intervals[ , 2 ] ),
                                          ci.low = as.numeric( x.confidence.intervals[ , 1 ] ), 
                                          ci.high = as.numeric( x.confidence.intervals[ , 3 ] ) )
        } else {
            ## the extRemes::ci.fevd.mle and the fevd object is accepts seems to be build in
            ## such a way that the maximum possible affort is necessary to use with non-native
            ## data. To circumvent this horrible programming style I will refit using extRemes::fevd
            ## well this just looks to awkward. Also the diagnostic plots in the ismev and the
            ## extRemes package look wrong for all data examples I just tried.
            ## I will leave this one blank.
            ## x.gpd.fit <- extRemes::fevd(  x.kept, threshold = input$slider.threshold.gev,
            ##                             type = "GP" )
            ## x.return.level <- extRemes::rlevd( x.period, type = "GP",
            ##                                   scale = x.gpd.fit$results$par[ 1 ],
            ##                                   shape = x.gpd.fit$results$par[ 2 ],
            ##                                   threshold = input$slider.threshold.gev )            
            ## x.confidence.intervals <- extRemes::ci.fevd.mle( x.gpd.fit, return.period = x.period )
            ## plot.statistics <- data.frame( period = x.period,
            ##                               level = as.numeric( x.confidence.intervals[ , 2 ] ),
            ##                               ci.low = as.numeric( x.confidence.intervals[ , 1 ] ), 
            ##                               ci.high = as.numeric( x.confidence.intervals[ , 3 ] ) )
            ## x.sort <- sort( as.numeric( x.kept ) )            
            ## plot.data <- data.frame( x = -1/ log( ppoints( length( x.kept ), 0 ) )[
            ##                                      x.sort > input$slider.threshold.gev ],
            ##                         y = x.sort[ x.sort > input$slider.threshold.gev ] )
            ## plot.data <- data.frame( x = x.period,
            ##                         y = input$slider.threshold.gev +
            ##                             x.gpd.fit$results$par[ 1 ]/ x.gpd.fit$results$par[ 2 ]* (
            ##                                 x.period^x.gpd.fit$results$par[ 2 ] - 1 ) )
            return( NULL )
        }
        gg.rl <- ggplot() + geom_point( data = plot.data, aes( x = x, y = y ), colour = colour.ts,
                                       shape = 1, size = 2, alpha = 0.8, na.rm = TRUE ) +
            geom_line( data = plot.statistics, aes( x = period, y = level ),
                      colour = colour.extremes, na.rm = TRUE ) +
            geom_line( data = plot.statistics, aes( x = period, y = ci.low ), linetype = 2,
                      colour = colour.extremes, na.rm = TRUE ) +
            geom_line( data = plot.statistics, aes( x = period, y = ci.high ), linetype = 2,
                      colour = colour.extremes, na.rm = TRUE ) + xlab( "return period [years]" ) +
            ylab( "return level" ) + theme_bw() + scale_x_log10( limits = c( 1, 1000 ) )
        if ( !is.null( input$button.min.max ) ){
            if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" ){
                gg.rl <- gg.rl + scale_y_reverse() 
            } else
                gg.rl <- gg.rl + ylim( plot.y.limits )
        }
        return( gg.rl )  } )
####################################################################################
    
####################################################################################
######################## Deseasonalization of the data #############################
####################################################################################
    deseasonalization <- reactive( {
        ## Dropdown for deseasonalization method
        x.xts <- data.selection( )
        if ( !is.null( input$select.data.base ) ){
            if ( input$select.data.base == "artificial data" ){
                ## For the artificial data there is no need for deseasonalization
                return( x.xts ) }
        }
        if ( input$select.deseasonalize == "Anomalies" ){
            x.deseasonalized <- anomalies( x.xts )
        } else if ( input$select.deseasonalize == "decompose" ){
            ## WARNING: only the seasonal component and not the trend was removed
            x.decomposed <- decompose( ts( as.numeric( x.xts ), frequency = 365.25 ) )
            x.deseasonalized <- x.xts - as.numeric( x.decomposed$seasonal )
        } else if ( input$select.deseasonalize == "stl" ){
            ## WARNING: only the seasonal component and not the trend was removed and
            ## a s window of 30 was used
            x.decomposed <- stl( ts( as.numeric( x.xts ), frequency = 365.25 ), 30 )
            x.deseasonalized <- x.xts - as.numeric( x.decomposed$time.series[ , 1 ] )
        } else if ( input$select.deseasonalize == "deseasonalize::ds" ){
            x.aux <- deseasonalize::ds( x.xts )$z
            x.deseasonalized <- xts( x.aux, order.by = index( x.xts ) )
        } else if ( input$select.deseasonalize == "none" ){
            x.deseasonalized <- x.xts
        } else {
            warning( "the decomponer function takes forever and X-13ARIMA is not implemented yet. Anomalies are use instead" )
            x.deseasonalized <- anomalies( x.xts ) }
        return( x.deseasonalized ) } )
####################################################################################
    
####################################################################################
######################## Fitting of the GEV distribution ###########################
####################################################################################
    fit.gev <- reactive( {
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        ## negating the time series to derive the statistics for the minima
        if ( !is.null( input$button.min.max ) ){
            if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" )
                x.kept <- ( -1 )* x.kept
        }
        ##! Drop-down menu to decide which fitting routine should be used
        if ( input$radio.gev.statistics == "Blocks" ){
            ## Fits of GEV parameters to blocked data set
            if ( input$select.optimization == "Nelder-Mead" ){
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "Nelder-Mead" ) )
            } else if ( input$select.optimization == "CG" ) {
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "CG" ) )
            } else if ( input$select.optimization == "BFGS" ) {
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "BFGS" ) )
            } else if ( input$select.optimization == "GenSA::GenSA" ) {
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "SANN" ) )
            } else if ( input$select.optimization == "ismev::gev.fit" ) {
                x.gev.fit <- ismev::gev.fit( x.kept, show = FALSE )
                x.gev.fit$value <- x.gev.fit$nllh
                x.gev.fit$par <- x.gev.fit$mle
                x.gev.fit$x <- x.kept
                ## very very dirty. but the extRemes package seems to be written to prevent
                ## nice interactions with its functions
                x.gev.fit$hessian <- gev.fit( x.kept, method = "Nelder-Mead",
                                             initial = as.numeric( x.gev.fit$par ) )$hessian
            } else if ( input$select.optimization == "extRemes::fevd" ) {
                x.gev.fit <- suppressWarnings( extRemes::fevd( x.kept )$results ) }
            class( x.gev.fit ) <- c("list", "climex.gev.fit")
        } else {
            ## Fits of GPD parameters to blocked data set
            if ( is.null( input$slider.threshold.gev ) ){
                threshold <- max( x.kept )* .8
            } else
                threshold <- input$slider.threshold.gev
            if ( input$select.optimization == "Nelder-Mead" ){
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, threshold, method = "Nelder-Mead", show = FALSE ) )
            } else if ( input$select.optimization == "CG" ) {
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, threshold, method = "CG", show = FALSE ) )
            } else if ( input$select.optimization == "BFGS" ) {
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, threshold, method = "BFGS", show = FALSE ) )
            } else if ( input$select.optimization == "SANN" ) {
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, threshold, method = "SANN", show = FALSE ) )
            } else {
                stop( "other fitting functions arn't supported yet!" )
            }
            ## For comparability.
            x.gev.fit$par <- x.gev.fit$mle
            names( x.gev.fit$par ) <- c( "scale", "shape" )
            x.gev.fit$convergence <- x.gev.fit$conv
            x.gev.fit$value <- x.gev.fit$nllh
            x.gev.fit$x <- x.kept
        }
        return( x.gev.fit )
    } )
####################################################################################
    
####################################################################################
######################### table containing fit statistics ##########################
####################################################################################
    ## Displaying of AIC, nllh, BIC and fitted parameters as well as the difference
    ## to the three last fits
    ## ! (and highlight positive values with with green and negative with red)
    last.values <- last.1 <- last.1.aux <- last.1.int <- last.2 <- last.3 <-
        current.white <- rep( 0,  )
    output$table.statistics <- renderUI( {
        ## define the colour for increasing or decreasing values
        css.colours <- c( "#C53100", "#0D8F20" ) # >0, <0, normal
        x.gev.fit <- fit.gev( )
        x.block <- generate.data( )[[ 1 ]]
        if ( input$radio.gev.statistics == "Blocks" ){
            current <- c( x.gev.fit$par[ 1 ], x.gev.fit$par[ 2 ], x.gev.fit$par[ 3 ],
                         x.gev.fit$value, aic( x.gev.fit ),
                         bic( x.gev.fit ), rlevd( x.gev.fit$par, error.estimation = "none" ) )
        } else 
            current <- c( 0, x.gev.fit$par[ 1 ], x.gev.fit$par[ 2 ],
                         x.gev.fit$value, aic( x.gev.fit ),
                         bic( x.gev.fit ), rlevd( x.gev.fit, error.estimation = "none" ) )
        ## negating the return level to get the correct results for the minium
        if ( !is.null( input$button.min.max ) ){
            if ( input$button.min.max == "Min" && input$radio.gev.statistics == "Blocks" )
                current[ 7 ] <- ( -1 )* current[ 7 ]
        }
        ## history of the statistics
        last.3 <<- last.2
        last.2 <<- last.1
        last.1.aux <- current - last.values
        ## For the fitted parameters any deviation of more than 1 percent is marked red
        for ( ll in 1 : length( x.gev.fit$par ) ){
            if ( all ( last.1.aux == 0 ) ){
                ## This happens right in the beginning on initialization
                ## The following prevents the output of coloured zeros
                last.1.int <- last.1.aux
                break
            } else if( abs( last.1.aux[ ll ] - current[ ll ] ) < 0.01* current[ ll ] ){
                    if ( last.1.aux[ ll ] > 0 ){
                        last.1.int[ ll ] <- paste0( 
                            "+", as.character( format(  last.1.aux[ ll ], digits = 4 ) ),
                            " ", css.colours[ 1 ] )
                    } else
                        last.1.int[ ll ] <- paste( 
                        as.character( format(  last.1.aux[ ll ], digits = 4 ) ),
                        css.colours[ 1 ] )
            } else {
                    last.1.int[ ll ] <- paste( as.character( format(  last.1.aux[ ll ],
                                                                    digits = 4 ) ),
                                              " ", css.colours[ 2 ] ) } }
        ## For the test statistic all changes to lower values are marked green
        for ( ll in ( length( x.gev.fit$par ) + 1 ) : length( last.1.aux ) ){
            if( last.1.aux[ ll ] > 0 ){
                last.1.int[ ll ] <- paste0(
                    "+", as.character( format(  last.1.aux[ ll ], digits = 4 ) ),
                    " ", css.colours[ 1 ] )
            } else if ( last.1.aux[ ll ] < 0 ){
                last.1.int[ ll ] <- paste( as.character( format(  last.1.aux[ ll ],
                                                                digits = 4 ) ),
                                          " ", css.colours[ 2 ] )
                } else {
                    last.1.int[ ll ] <- as.character( format(  last.1.aux[ ll ], digits = 4 ) )
                } }
        last.1 <<- last.1.int
        if ( all( last.values == 0 ) ){
            ## I don't want to see the statistics during the initialization
            last.1 <<- rep( 0, length( last.1 ) ) }
        last.values <<- current
        x.table <- data.frame( current = current, h_1 = last.1,
                                  h_2 = last.2, h_3 = last.3,
                              row.names = c( "location", "scale",
                                            "shape", "nllh", "AIC", "BIC", "rlevel" ) )
        ## generate a html table with the 'pander' and the 'markdown' package
        x.html.table <- markdown::markdownToHTML(
            text = pander::pandoc.table.return( x.table, style = "rmarkdown",
                                               split.tables = Inf ),
            fragment.only = TRUE )
        x.color.table <- color.table( x.html.table, css.colours ) })
####################################################################################
    
####################################################################################
######################## Likelihood animation (tab2) ###############################
####################################################################################
    ## Fitting the MLE again with the algorithm of choice
    output$menu.slider.location.lim <- renderMenu( {
        x.gev.fit <- fit.gev()
        x.block <- generate.data()[[ 1 ]]
        par.init <- likelihood.initials( x.block )
        sliderInput( "slider.location.lim", "Location limits", round( x.gev.fit$par[ 1 ], 1 ) - 10,
                    round( x.gev.fit$par[ 1 ] + 10, 1 ),
                    c( round( x.gev.fit$par[ 1 ], 1 ) - 5, round( x.gev.fit$par[ 1 ], 1 ) + 5 ) ) } )
    output$menu.slider.scale.lim <- renderMenu( {
        x.gev.fit <- fit.gev()
        x.block <- generate.data()[[ 1 ]]
        par.init <- likelihood.initials( x.block )
        sliderInput( "slider.scale.lim", "Scale limits",
                    round( max( 0, x.gev.fit$par[ 2 ]  - 10 ), 1 ),
                    round( x.gev.fit$par[ 2 ] + 10, 1 ),
                    c( round( max( 0, x.gev.fit$par[ 2 ] - 5 ), 1 ),
                      round( x.gev.fit$par[ 2 ], 1 ) + 5 ) ) } )
    output$menu.slider.shape.lim <- renderMenu( {
        x.gev.fit <- fit.gev()
        x.block <- generate.data()[[ 1 ]]
        par.init <- likelihood.initials( x.block )
            sliderInput( "slider.shape.lim", "Shape limits",
                        round( x.gev.fit$par[ 3 ] - 1, 1 ),
                        round( x.gev.fit$par[ 3 ] + 1, 1 ),
                        c( round( x.gev.fit$par[ 3 ], 1 ) - .3,
                          round( x.gev.fit$par[ 3 ], 1 ) + .3  ) ) } )
    mle.parameter.likelihood <- reactive( {
        x.block <- generate.data()[[ 1 ]]
        if ( input$select.optimization.procedure.likelihood == "dfoptim::nmk" ){
            suppressWarnings( par.mle <- try( nmk.modified( x = x.block )$par ) )
            if ( class( par.mle ) == "try-error" )
                par.mle <- gev.fit( x.block )$par
            return( par.mle )  } } )                
    cached.table.init <- NULL
    initial.parameters.likelihood <- reactive( {
        x.block <- generate.data()[[ 1 ]]
        input$table.draw.points
        par.init <- mle.parameter.likelihood()
        if ( is.null( input$slider.number.initial.points ) || is.null( input$slider.location.lim ) ||
             is.null( input$slider.scale.lim ) || is.null( input$slider.shape.lim ) ){
            return( c( NaN, NaN, NaN ) )
        }
        table.init <- data.frame( location = runif( input$slider.number.initial.points,
                                                   input$slider.location.lim[ 1 ],
                                                   input$slider.location.lim[ 2 ] ),
                                 scale = runif( input$slider.number.initial.points,
                                               input$slider.scale.lim[ 1 ],
                                               input$slider.scale.lim[ 2 ] ),
                                 shape = runif( input$slider.number.initial.points,
                                               input$slider.shape.lim[ 1 ],
                                               input$slider.shape.lim[ 2 ] ) )
        ## but we only want to have starting points which do not result in a NA
        while ( any( is.nan( apply( table.init, 1, likelihood, x.in = x.block ) ) ) ){
            for ( ii in 1 : nrow( table.init ) ){
                if ( is.nan( likelihood( as.numeric( table.init[ ii, ] ),
                                        x.in = x.block ) ) )
                    table.init[ ii, ] <- c( runif( 1, input$slider.location.lim[ 1 ],
                                                  input$slider.location.lim[ 2 ] ),
                                           runif( 1, input$slider.scale.lim[ 1 ],
                                                 input$slider.scale.lim[ 2 ] ),
                                           runif( 1, input$slider.shape.lim[ 1 ],
                                                 input$slider.shape.lim[ 2 ] ) ) } }
        return( table.init ) } )
    output$table.initial.points <- renderDataTable( {
        initial.parameters <- initial.parameters.likelihood()
        if ( all( is.na( initial.parameters ) ) )
            return( NULL )
        initial.parameters$ID <- seq( 1, nrow( initial.parameters ) )
        return( initial.parameters ) },
        options = list( dom = 't' ) )
    ## Displaying of the heuristic estimates for a wiser picking of the limits
    output$table.heuristic.estimates <- renderDataTable( {
        x.block <- generate.data()[[ 1 ]]
        x.mle.par <- mle.parameter.likelihood()
        x.mom <- likelihood.initials( x.block, type = "mom" )
        x.lmom <- likelihood.initials( x.block, type = "lmom" )
        x.df <- data.frame( estimate = c( "Method of moments", "Lmoments", "MLE" ),
                           loc = c( round( x.mom[ 1 ], 2 ),round( x.lmom[ 1 ], 2 ),
                                        round( x.mle.par[ 1 ], 2 ) ),
                           scale = c( round( x.mom[ 2 ], 2 ), round( x.lmom[ 2 ], 2 ),
                                     round( x.mle.par[ 2 ], 2 ) ),
                           shape = c( round( x.mom[ 3 ], 2 ), round( x.lmom[ 3 ], 2 ),
                                     round( x.mle.par[ 3 ], 3 ) ) )
        return( x.df ) },
        options = list( dom = 't' ) )
    output$draw.likelihood.animation <- renderUI( {
        ## This reactive content only depends on the action button because of
        ## the use of the isolate() functions
        ## Since the calculation of the animation is rather costly and the images have to
        ## be saved in different directories for the each specific client this option will
        ## be disabled as soon as the user running the script in Linux is 'shiny'. Therefore
        ## local deploying is still allowed
        if ( system2( "echo", args = "$USER", stdout = TRUE ) == "shiny" ){
            return( helpText( "Attention: calculating the animation of the optimization is rather costly. Therefore it is disabled in the current climex version. You can nevertheless deploy it using the app locally:\n require( climex )\n climex()\nSee the vignette of the package for further info. https://github.com/theGreatWhiteShark/climex/blob/master/vignettes/dwd_data_import.Rmd" ) )
        } else {
            input$button.draw.animation
            ## Don't make the plot the first time I look at the tab
            if ( input$button.draw.animation < 1 )
                return( NULL )
            isolate( initial.parameters <- initial.parameters.likelihood() )
            isolate( x.block <- generate.data()[[ 1 ]] )
            isolate( {
                if ( input$select.optimization.procedure.likelihood == "dfoptim::nmk" ){
                    optimization.function <- nmk.modified
                } else if ( input$select.optimization.procedure.likelihood == "dfoptim::nmk.modified"){
                    optimization.function <- nmk.modified
                }
                print( "starting animation......." )
                ## a temporary folder will be generate to harvest the images of the animation
                ## whenever the animation will be redone the old folder should be removed to avoid
                ## wasting memory of the server. Therefore the folder name has to be a global variable
                if ( !is.null( temp.folder ) )
                    unlink( temp.folder, recursive = TRUE )
                temp.folder <<- paste0( "/srv/shiny-server/assets/tmp/images_",
                                       as.numeric ( as.POSIXct( lubridate::now() ) ) )
                dir.create( temp.folder, recursive = TRUE )
                animation.wrapper( time.series = x.block, starting.points = initial.parameters,
                                  location.lim = isolate( input$slider.location.lim ),
                                  scale.lim = isolate( input$slider.scale.lim ),
                                  shape.lim = isolate( input$slider.shape.lim ),
                                  optimization.function = optimization.function,
                                  optimization.steps = isolate( input$slider.optimization.steps ),
                                  width = 300, height = 300, delay = 300, loopMode = "loop",
                                  folder = temp.folder )
                return( div( class = "scianimator", style = "display: inline-block;",
                            tags$script( src = "../assets/animation.js" ),
                            div( id = "animationLocSc", style = "display: inline-block;" ),
                            div( id = "animationLocSh", style = "display: inline-block;" ),
                            div( id = "animationScSh", style = "display: inline-block;" ) ) )
            } ) } } )
####################################################################################
    
####################################################################################
######################## Leaflet module (world map)  ###############################
####################################################################################
    ## This module provides an interactive map to display the locations of the individual stations.
    ## The user can choose individual stations by clicking at them. In addition a dialog will pop
    ## up telling the stations name, the length of the time series and the 20, 50 and 100 year
    ## return level calculated with the setting in the
    ## basic map without station positions        
    ## select only those stations in Germany with a certain minimum number of years
    data.chosen <- reactive( {
        if ( !is.null( input$select.data.base ) && input$select.data.base == "DWD" ){
            if ( is.null( input$select.data.type ) ){
                selection <- Reduce( c, lapply( stations.temp.max, function( x )
                    length( unique( lubridate::year( x ) ) ) ) ) >= input$slider.map
                stations.selected <- stations.temp.max[ selection ]
            } else if ( input$select.data.type == "Daily max. temp." ){
                selection <- Reduce( c, lapply( stations.temp.max, function( x )
                    length( unique( lubridate::year( x ) ) ) ) ) >= input$slider.map
                stations.selected <- stations.temp.max[ selection ]
            } else if ( input$select.data.type == "Daily min. temp." ){
                selection <- Reduce( c, lapply( stations.temp.min, function( x )
                    length( unique( lubridate::year( x ) ) ) ) ) >= input$slider.map
                stations.selected <- stations.temp.min[ selection ]
            } else if  ( input$select.data.type == "Daily precipitation" ){
                selection <- Reduce( c, lapply( stations.prec, function( x )
                    length( unique( lubridate::year( x ) ) ) ) ) >= input$slider.map
                stations.selected <- stations.prec[ selection ]
            } 
            position.selected <- station.positions[ selection,  ]
            list( stations.selected, position.selected )
        } else
            NULL
    } )
    ## create custom markers
    blue.icon <-  makeIcon( iconUrl = paste0( system.file( "climex_app", package = "climex" ),
                                             "/www/marker-icon.png" ),
                           iconWidth = 25, iconHeight = 41, iconAnchorX = 12.5, iconAnchorY = 41,
                           shadowUrl = paste0( system.file( "climex_app", package = "climex" ),
                                              "/www/marker-shadow.png" ), shadowWidth = 41,
                           shadowHeight = 41, shadowAnchorX = 12.5, shadowAnchorY = 41 )
                           
    red.icon <-  makeIcon( iconUrl = paste0( system.file( "climex_app", package = "climex" ),
                                             "/www/select-marker.png" ),
                           iconWidth = 25, iconHeight = 41, iconAnchorX = 12.5, iconAnchorY = 41,
                           shadowUrl = paste0( system.file( "climex_app", package = "climex" ),
                                              "/www/marker-shadow.png" ), shadowWidth = 41,
                          shadowHeight = 41, shadowAnchorX = 12.5, shadowAnchorY = 41 )
    
    output$leaflet.map <- renderLeaflet( {
        leaflet() %>% fitBounds( 5, 46, 13, 55 ) %>%
            addTiles( "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                     attribution = '<code> Kartendaten: © <a href="https://openstreetmap.org/copyright">OpenStreetMap</a>-Mitwirkende, SRTM | Kartendarstellung: © <a href="http://opentopomap.org">OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a> </code>' ) } )
    
    observe( {
        data.selected <- data.chosen()
        if ( !is.null( data.selected ) ){
            leafletProxy( "leaflet.map" ) %>%
                clearGroup( group = "stations" )
            leafletProxy( "leaflet.map" ) %>%
                addMarkers( data = data.selected[[ 2 ]], group = "stations", lng = ~longitude,
                           icon = blue.icon, lat = ~latitude,
                           options = popupOptions( closeButton = FALSE ) )
        } } )
    output$menu.select.data.source.1 <- renderMenu( {        
        if ( !is.null( input$select.data.base ) ){
            if ( input$select.data.base == "DWD" ){
                map.click <- input$leaflet.map_marker_click
                data.selected <- data.chosen()
                if ( is.null( data.selected ) || is.null( map.click ) ){
                    ## no data was chosen yet. Setting the Potsdam station as default
                    if ( is.null( input$select.data.type ) ){
                        selectInput( "select.data.source", "Station",
                                    choices = names( stations.temp.max ), selected = "Potsdam" )
                    } else if ( input$select.data.type == "Daily max. temp." ){
                        selectInput( "select.data.source", "Station",
                                    choices = names( stations.temp.max ), selected = "Potsdam" )
                    } else if ( input$select.data.type == "Daily min. temp." ){
                        selectInput( "select.data.source", "Station",
                                    choices = names( stations.temp.min ), selected = "Potsdam" )
                    } else if ( input$select.data.type == "Daily precipitation" ){
                        selectInput( "select.data.source", "Station",
                                    choices = names( stations.prec ), selected = "Potsdam" )
                    }
                } else {
                    station.name <- as.character(
                        data.selected[[ 2 ]]$name[ which(
                                                data.selected[[ 2 ]]$latitude %in% map.click$lat &
                                                data.selected[[ 2 ]]$longitude %in% map.click$lng ) ] )
                    if ( input$select.data.type == "Daily max. temp." ){
                        selectInput( "select.data.source", "Station",
                                    choices = names( stations.temp.max ),
                                    selected = station.name )
                    } else if ( input$select.data.type == "Daily min. temp." ){
                        selectInput( "select.data.source", "Station",
                                    choices = names( stations.temp.min ),
                                    selected = station.name )
                    } else if ( input$select.data.type == "Daily precipitation" ){
                        selectInput( "select.data.source", "Station",
                                    choices = names( stations.prec ),
                                    selected = station.name )
                    }
                }
            } else if ( input$select.data.base == "artificial data" ){
                sliderInput( "slider.artificial.data.location", "location", -30, 30, 1, round = 15 )
            } else if ( input$select.data.base == "input" ){
                if ( type.input == "single" ){
                    NULL
                } else if ( type.input == "list" ){
                    if ( !is.null( names( x.input ) ) ){
                        ## it's a named list
                        selectInput( "select.list.entry.name", "Entry", choices = names( x.input ),
                                    selected = names( x.input )[ 1 ] )
                    } else
                        selectInput( "select.list.entry.numerical", "Entry",
                                    choices = 1 : length( x.input ), selected = 1 )
                }
            }
        } else
            NULL } )
    ## observe( {
    ##     map.click <- input$leaflet.map_marker_click
    ##     if ( is.null( map.click ) )
    ##         return()
    ##     data.selected <- data.chosen()
    ##     if ( is.null( data.selected ) )
    ##         return()
    ##     station.name <- as.character(
    ##         data.selected[[ 2 ]]$name[ which( data.selected[[ 2 ]]$latitude %in% map.click$lat &
    ##                                           data.selected[[ 2 ]]$longitude %in% map.click$lng ) ] )
    ##     x.gev.fit <- fit.gev()
    ##     x.rlevd <- rlevd( x.gev.fit, return.period = c( 100, 50, 20 ) )
    ##     x.popup <- paste0( "<b>", station.name, "</b>", "<br/>",
    ##                       "100y return level: ", x.rlevd[ 1 ], "<br/>",
    ##                       "50y return level: ", x.rlevd[ 2 ], "<br/>",
    ##                       "20y return level: ", x.rlevd[ 3 ] )
        
    ##     leafletProxy( "leaflet.map" ) %>%
    ##             clearGroup( group = "selected" )
    ##     leafletProxy( "leaflet.map" ) %>%
    ##         addMarkers( data = map.click, group = "selected", icon = red.icon, popup = x.popup,
    ##                    lng = ~lng, lat = ~lat, options = popupOptions( closeButton = FALSE ) ) } )

    output$tableMap <- renderTable( {
        map.click <- input$leaflet.map_marker_click
        if ( is.null( map.click ) )
            return( NULL )
        data.selected <- data.chosen()
        if ( is.null( data.selected ) )
            return( NULL )
        station.name <- as.character(
            data.selected[[ 2 ]]$name[ which( data.selected[[ 2 ]]$latitude %in% map.click$lat &
                                              data.selected[[ 2 ]]$longitude %in% map.click$lng ) ] )
        
        leafletProxy( "leaflet.map" ) %>%
                clearGroup( group = "selected" )
        leafletProxy( "leaflet.map" ) %>%
            addMarkers( data = map.click, group = "selected", icon = red.icon, lng = ~lng, lat = ~lat )
        ## calculate the GEV fit and various return levels
        x.gev.fit <- fit.gev()
        if ( input$button.min.max == "Max" ){
            x.rlevd <- rlevd( x.gev.fit, return.period = c( 100, 50, 20 ) )
        } else
            x.rlevd <- ( -1 )* rlevd( x.gev.fit, return.period = c( 100, 50, 20 ) )
        ## paste0( "<b>", station.name, "</b>", "<br/>", "100y return level: ", x.rlevd[ 1 ], "<br/>",
        ##        "50y return level: ", x.rlevd[ 2 ], "<br/>", "20y return level: ", x.rlevd[ 3 ] )
        x.df <- data.frame( names = c( "100y return level", "50y return level",
                                          "20y return level" ),
                           x.rlevd, row.names = NULL )
        colnames( x.df ) <- c( station.name, "" )
        x.df
     }, rownames = FALSE, digits = 3, width = 220 )
}

##' @title The user interface for the \code{\link{climex}} function.
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
##' @author Philipp Mueller 
climex.ui <- function(){
    dashboardPage( 
        dashboardHeader( 
            title = "Climex"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem( "Map", tabName = "tabMap", icon = icon( "leaf", lib = "glyphicon" ) ),
                menuItem( "General", tabName = "tabGeneral", icon = icon( "bar-chart" ) ),
                menuItem( "Likelihood", tabName = "tabLikelihood",
                         icon = icon( "wrench", lib = "glyphicon" ) ),
                menuItemOutput( "menu.select.data.base" ),
                menuItemOutput( "menu.select.data.source.1" ),
                menuItemOutput( "menu.select.data.source.2" ),
                menuItemOutput( "menu.select.data.source.3" ),
                menuItemOutput( "menu.data.cleaning" ) ) ),
        dashboardBody(
            ## shinyjs::useShinyjs(),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/climex.css" ) ),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/reset.css" ) ),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/styles.css" ) ),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/scianimator.css" ) ),
            tags$head( tags$script( src = paste0( system.file( "climex_app", package = "climex" ),
                               "/js/jquery.scianimator.min.js" ) ) ),
            tabItems(
                tabItem(
                    tabName = "tabMap",
                    tags$style( type = "text/css", "#leaflet.map {height: calc(100vh - 80px) !important;}" ),
                    leafletOutput( "leaflet.map", width = "100%", height = 1000 ),
                    absolutePanel( top = 49, right = 10,
                                  sliderInput( "slider.map", "Minimal length (years)",
                                              0, 155, value = 65, step = 1, width = 215 ),
                                  tableOutput( "tableMap" ) ) ),
                tabItem(
                    tabName = "tabGeneral",
                    fluidRow(
                        box( title = h2( "GEV fit" ), status = "primary", solidheader = TRUE, width = 8,
                            column( 9, plotOutput( "plot.fit.gev" ) ),
                            column( 3, plotOutput( "plot.fit.qq", height = 140 ),
                                   plotOutput( "plot.fit.qq2", height = 140 ),
                                   plotOutput( "plot.fit.return.level",
                                              height = 140 ) ) ),
                        box( title = h2( "Options" ), collapsible = TRUE,
                            width = 4, height = 550, background = "orange",
                            radioButtons( "radio.gev.statistics", label = NULL, inline = TRUE,
                                         choices = c( "Blocks", "Threshold" ), selected = "Blocks" ),
                            menuItemOutput( "slider.gev.statistics" ),
                            radioButtons( "button.min.max", "Type of extreme", inline = TRUE,
                                         choices = c( "Max", "Min" ), selected = "Max" ),
                            selectInput( "select.deseasonalize", "Deseasonalization method",
                                        choices = c( "Anomalies", "stl", "decompose",
                                                    "deseasonalize::ds", "none" ),
                                        selected = "Anomalies" ),
                            selectInput( "select.optimization", "Optimization/Fitting routine",
                                        choices = c( "Nelder-Mead", "CG", "BFGS", "GenSA::GenSA",
                                                    "ismev::gev.fit", "extRemes::fevd" ),
                                        selected = c( "Nelder-Mead" ) ) ) ),
                    fluidRow(
                        box( title = h2( "Results" ), width = 3, background = "orange",
                            uiOutput( "table.statistics", colHeaders = "provided" ) ),
                        tabBox( title = h2( "Time series" ), selected = "Blocked ts", width = 9,
                               tabPanel( "Pure ts", dygraphOutput( "plot.time.series", height = 250 ) ),
                               tabPanel( "Deseasonalized ts", dygraphOutput( "plot.deseasonalized",
                                                                            height = 250 ) ),
                               tabPanel( "Blocked ts",
                                        plotOutput( "plot.blocked", height = 250,
                                                   click = "plot.blocked.click",
                                                   brush = brushOpts( id = "plot.blocked.brush" ) ),
                                        actionButton( "exclude.blocked.reset", "Reset" ),
                                        actionButton( "exclude.blocked.toggle", "Brush" ) ) ) ) ),
                tabItem( tabName = "tabLikelihood",                                  
                        box( title = h2( "Likelihood of the time series and optimization routes of different initial conditions" ),
                            width = 8, status = "primary", dataTableOutput( "table.initial.points" ),
                            actionButton( "table.draw.points", "Reset" ),
                            htmlOutput( "draw.likelihood.animation" ) ),
                        box( width = 4, background = "orange",
                            dataTableOutput( "table.heuristic.estimates" ),
                            sliderInput( "slider.number.initial.points",
                                        "Number of initial points", 1, 20, 5 ),
                            uiOutput( "menu.slider.location.lim" ),
                            uiOutput( "menu.slider.scale.lim" ), uiOutput( "menu.slider.shape.lim" ),
                            selectInput( "select.optimization.procedure.likelihood",
                                        "Optimization procedure",
                                        choices = c( "dfoptim::nmk", "dfoptim::nmk.modified" ),
                                        selected = "dfoptim::nmk" ),
                            sliderInput( "slider.optimization.steps", "Which optimization steps", 0, 1,
                                        c( .1, .5 ) ),
                            actionButton( "button.draw.animation", "Start animation" ) ) ) ) ) )
}
