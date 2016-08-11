## since we do just local loading and no uploading we don't have to care about it at the moment
options( shiny.maxRequestSize = 1000* 1024^2 )
shinyServer( function( input, output ){
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
        if ( input$select.data.base == "DWD" ){
            selectInput( "select.data.source", "Station",
                        choices = names( stations.temp.max ),
                        selected = "Potsdam" )
        } else if ( input$select.data.base == "artificial data" ){
            sliderInput( "slider.artificial.data.location", "location", -30, 30, 1, round = 15 )
        } else if ( input$select.data.base == "input" ){
            if ( type.input == "single" ){
                selectInput( "select.list.aux", "", "" )
            } else if ( type.input == "list" ){
                if ( !is.null( names( x.input ) ) ){
                    ## it's a named list
                    selectInput( "select.list.entry.name", "Entry", choices = names( x.input ),
                                selected = names( x.input )[ 1 ] )
                } else
                    selectInput( "select.list.entry.numerical", "Entry",
                                choices = 1 : length( x.input ), selected = 1 )
            }
        } } )
    output$menu.select.data.source.2 <- renderMenu( {
        if ( input$select.data.base == "DWD" ){
            selectInput( "select.data.type", "Measured variable",
                        choices = c( "Daily max. temp.", "Daily min. temp.", "Daily precipitation" ),
                        selected = "Daily max. temp" )                        
        } else if ( input$select.data.base == "artificial data" ){
            sliderInput( "slider.artificial.data.scale", "scale", 0, 4, 0.8, round = -2 )
        } else if ( input$select.data.base == "input" ){
            selectInput( "select.list.aux", "", "" )
        }
    } )
    output$menu.select.data.source.3 <- renderMenu( {
        if ( input$select.data.base == "DWD" ){
            selectInput( "select.list.aux", "", "" )
        } else if ( input$select.data.base == "artificial data" ){
            sliderInput( "slider.artificial.data.shape", "shape", -1.5, 1.5, -0.25, round = -2 )
        } else if ( input$select.data.base == "input" ){
            fileInput( "file.input.selection", "Choose a .RData file" )
        } } )
    output$slider.gev.statistics <- renderMenu( {
        x.deseasonalized <- deseasonalization()
        if ( input$radio.gev.statistics == "blocks" ){
            sliderInput( "slider.box.length", "Box length in days:", 1, 365*3, 365 )
        } else 
            sliderInput( "slider.threshold.gev", "threshold:",
                        round( min( x.deseasonalized, na.rm = TRUE ) ),
                        round( max( x.deseasonalized, na.rm = TRUE ) ),
                        round( 0.8* max( x.deseasonalized, na.rm = TRUE ) ) )
    } )
    output$menu.data.cleaning <- renderMenu( {
        ## When applying the blocking method incomplete years distort the time series and
        ## have to be excluded. When using the threshold method on the other hand clusters
        ## are most likely to occure due to short range correlations. This has to be
        ## avoided by using declustering algorithms (which mainly picks the maximum of a
        ## specific cluster)
        if ( input$radio.gev.statistics == "blocks" ){
            checkboxInput( "check.box.incomplete.years", "Remove incomplete years", FALSE )
        } else
            checkboxInput( "check.box.decluster", "Declustering of the data", FALSE )
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
        if ( input$select.data.base == "DWD" ){
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
        } else if( input$select.data.base == "artificial data" ){
            ## For the artificial data set the length is determined by the Potsdam
            ## temperature series
            x.xts <- xts( revd( length( stations.temp.max[[ 64 ]] ),
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
        if ( input$check.box.incomplete.years ){
            ## Remove all incomplete years from time series
            x.xts <- remove.incomplete.years( x.xts ) }
        return( x.xts ) } )
    data.blocking <- reactive( {
        ## Decides if either the GEV distribution with block maxima or the Pareto
        ## distribution if threshold excedence should be considered.
        ## Import data set, cut it to the desired intervale, deseasonalize and block it
        x.xts <- data.selection( )
        x.deseasonalized <- deseasonalization( )
        ## Toggle if maxima of minima are going to be used
        ifelse( input$button.min.max == "max", block.mode <- "max", block.mode <- "min")
        if ( input$radio.gev.statistics == "blocks" ){
            ## Box size as dynamic input parameter        
            if ( input$slider.box.length == 366 || input$slider.box.length == 365 ){
                x.block <- block( x.deseasonalized, separation.mode = "years",
                                 block.mode = block.mode )
            } else
                x.block <- block( x.deseasonalized, block.length = input$slider.box.length,
                                 block.mode = block.mode )
        } else if ( input$radio.gev.statistics == "threshold" ){
            ## Due to "historical" reasons the vector containing the resulting values will
            ## still be called x.block. The "block.mode" and the corresponding
            ## "input$button.min.max" are still use full and decide if the values above
            ## or below the threshold are going to be extracted
            if ( input$check.box.decluster )
                x.deseasonalized <- declustering( x.deseasonalized, input$slider.threshold.gev )
            if ( block.mode == "max" ){
                x.block <- x.deseasonalized[ x.deseasonalized >= input$slider.threshold.gev ]
            } else
                x.block <- x.deseasonalized[ x.deseasonalized <= input$slider.threshold.gev ]
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
    colour.ts <- rgb( 0.098, 0.098, 0.44 )
    colour.extremes <- rgb( 1, 0.55, 0 )
    colour.ts.light <- "#7171EC"
    colour.extremes.light <- rgb( 1, 0.9, 0.8 )
    function.get.y.label <- function( input ){
        if ( input$select.data.base == "artificial data" ){
            y.label <- "GEV sample"
        } else if ( input$select.data.base == "DWD" ){
            if ( input$select.data.type == "Daily precipitation" ){
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
        x.lim <- c( max( x.kept ), min( x.kept ) )
        x.gev.fit <- fit.gev( )
        if ( input$radio.gev.statistics == "blocks" ){
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
        } else {
            ## Generalized Pareto
            ## Since the Pareto function does not have a location parameter
            plot.range <- seq( x.lim[ 2 ], x.lim[ 1 ]* 1.1, 0.01 )
            plot.data <- data.frame( x = plot.range,
                                    y = ismev::gpd.dens( x.gev.fit$par,
                                                        input$slider.threshold.gev,
                                                        plot.range ) )
            plot.data <- plot.data[ !is.na( plot.data[[ 2 ]] ), ]
            plot.data[ nrow( plot.data ) + 1, ] <- c( plot.data[[ 1 ]][ 1 ],
                                                     plot.data[[ 2 ]][ nrow( plot.data ) ] )
            plot.lim <- c( min( plot.range ), max( plot.range ) )
        }
        ## splitting the plot.data$y in half and determining which index is closest to threshold
        x.label <- function.get.y.label( input )
        ggplot() + geom_histogram( data = x.kept, colour = colour.ts, alpha = 1,
                                  aes( x = as.numeric( x.kept ), y = ..density..,
                                      fill = colour.ts.light ) ) +
            geom_polygon( data = plot.data, alpha = 0.7, colour = colour.ts,
                         aes( x = x, y = y, fill = colour.extremes ) ) +            
            scale_fill_manual( values = c( colour.ts.light, colour.extremes ),
                              labels = c( "Histogram data", "Fitted distribution"  ) ) +
            theme_bw() + xlab( x.label ) + xlim( plot.lim ) +
            theme( legend.position = "none" )
        return( last_plot() )
    } )
    output$plot.fit.qq <- renderPlot( {
        ## Quantile-quantile plot for fit statistics
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.gev.fit <- fit.gev( )
        if ( input$radio.gev.statistics == "blocks" ){
            plot.data <- data.frame(
                model = extRemes::qevd( ppoints( length( x.kept ), 0 ),
                                       loc = x.gev.fit$par[ 1 ], scale = x.gev.fit$par[ 2 ],
                                       shape = x.gev.fit$par[ 3 ], type = "GEV" ),
                empirical = sort( as.numeric( x.kept ) ) )
        } else {
            plot.data <- data.frame(
                model = extRemes::qevd( ppoints( length( x.kept ), 0 ),
                                       scale = x.gev.fit$par[ 1 ],
                                       shape = x.gev.fit$par[ 2 ], type = "GP",
                                       threshold = input$slider.threshold.gev ),
                empirical = sort( as.numeric( x.kept ) ) ) }
        plot.fit <- lm( model ~ empirical, plot.data )[[ 1 ]]
        ggplot() + geom_point( data = plot.data, aes( x = model, y = empirical ),
                              colour = colour.ts,
                              shape = 1, size = 2, alpha = 0.8 ) +
            geom_abline( intercept = plot.fit[ 1 ], slope = plot.fit[ 2 ], colour = colour.ts,
                        linetype = 2 ) +
            geom_abline( intercept = 0, slope = 1, colour = colour.extremes ) +
            theme_bw() } )
    output$plot.fit.qq2 <- renderPlot( {
        ## Quantile-quantile plot for fit statistics with samples drawn from fitted GEV
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.gev.fit <- fit.gev( )
        if ( input$radio.gev.statistics == "blocks" ){
            sampled <- sort( extRemes::revd( length( x.kept ), loc = x.gev.fit$par[ 1 ],
                                            scale = x.gev.fit$par[ 2 ],
                                            shape = x.gev.fit$par[ 3 ], type = "GEV" ) )
        } else
            sampled <- sort( extRemes::revd( length( x.kept ), scale = x.gev.fit$par[ 1 ],
                                            shape = x.gev.fit$par[ 2 ], type = "GEV",
                                            threshold = input$slider.threshold.gev ) )
        empirical <- sort( as.numeric( x.kept ) )
        length.e <- length( empirical )
        length.s <- length( sampled )
        ## inspired by extRemes::qqplot( plot.data$empirical, plot.data$sampled )
        ## function giving a linear interpolation 
        function.sampled.interpolate <- approxfun( seq( 0, 1, length = length( sampled ) ),
                                                  sort( sampled ), yleft = NA, yright = NA )
        period <- ( 1 : length( empirical ) - 1 )/ ( length( empirical ) - 1 )
        sampled.interpolate <- function.sampled.interpolate( period )
        sampled.ci.low <- function.sampled.interpolate(
            period - 1.36/ sqrt( length.e* length.s/ ( length.e + length.s ) ) )
        sampled.ci.high <- function.sampled.interpolate(
            period + 1.36/ sqrt( length.e* length.s/ ( length.e + length.s ) ) )
        plot.data <- data.frame( empirical = empirical, sampled = sampled.interpolate,
                                ci.low = sampled.ci.low, ci.high = sampled.ci.high )
        plot.fit <- lm( sampled ~ empirical, plot.data )[[ 1 ]]
        ggplot() + geom_point( data = plot.data, aes( x = sampled, y = empirical ),
                              colour = colour.ts, shape = 1, size = 2, alpha = 0.8 ) +
            geom_line( data = plot.data, aes( x = sampled.ci.low, y = empirical ), linetype = 2,
                      colour = colour.extremes ) +
            geom_line( data = plot.data, aes( x = sampled.ci.high, y = empirical ), linetype = 2,
                      colour = colour.extremes ) +
            geom_abline( intercept = plot.fit[ 1 ], slope = plot.fit[ 2 ], colour = colour.ts,
                        linetype = 2 ) +
            geom_abline( intercept = 0, slope = 1, colour = colour.extremes ) +
            theme_bw() } )
    output$plot.fit.return.level <- renderPlot( {
        ## Return level plot
        x.block <- generate.data( )[[ 1 ]]
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.gev.fit <- fit.gev( )
        x.period <- c( 2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800 )
        if ( input$radio.gev.statistics == "blocks" ){
            x.return.level <- extRemes::rlevd( x.period, type = "GEV", loc = x.gev.fit$par[ 1 ],
                                              scale = x.gev.fit$par[ 2 ],
                                              shape = x.gev.fit$par[ 3 ] )
            x.confidence.intervals <- extRemes::ci.fevd.mle( as.fevd( x.kept, x.gev.fit,
                                                                     type = "GEV"),
                                                            return.period = x.period )
            plot.statistics <- data.frame( period = -1/ ( log( 1 - 1/ x.period ) ),
                                          level = as.numeric( x.confidence.intervals[ , 2 ] ),
                                          ci.low = as.numeric( x.confidence.intervals[ , 1 ] ), 
                                          ci.high = as.numeric( x.confidence.intervals[ , 3 ] ) )
        } else
            return( NULL )
        plot.data <- data.frame( x = -1/ log( ppoints( length( x.kept ), 0 ) ),
                                y = sort( as.numeric( x.kept ) ) )
        ggplot() + geom_point( data = plot.data, aes( x = x, y = y ), colour = colour.ts,
                              shape = 1,
                              size = 2, alpha = 0.8 ) +
            geom_line( data = plot.statistics, aes( x = period, y = level ),
                      colour = colour.extremes ) +
            geom_line( data = plot.statistics, aes( x = period, y = ci.low ), linetype = 2,
                      colour = colour.extremes ) +
            geom_line( data = plot.statistics, aes( x = period, y = ci.high ), linetype = 2,
                      colour = colour.extremes ) + xlab( "return period [years]" ) +
            ylab( "return level" ) + theme_bw() + scale_x_log10( limits = c( 1, 1000 ) ) +
            ylim( c( plot.data$y[ which.min( abs( plot.data$x - 1 ) ) ],
                    max( plot.statistics$ci.high ) ) ) } )
####################################################################################
    
####################################################################################
######################## Deseasonalization of the data #############################
####################################################################################
    deseasonalization <- reactive( {
        ## Dropdown for deseasonalization method
        x.xts <- data.selection( )
        if ( input$select.data.base == "artificial data" ){
            ## For the artificial data there is no need for deseasonalization
            return( x.xts ) }
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
        ##! Drop-down menu to decide which fitting routine should be used
        if ( input$radio.gev.statistics == "blocks" ){
            ## Fits of GEV parameters to blocked data set
            if ( input$select.optimization == "Nelder-Mead" ){
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "Nelder-Mead" ) )
            } else if ( input$select.optimization == "CG" ) {
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "CG" ) )
            } else if ( input$select.optimization == "BFGS" ) {
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "BFGS" ) )
            } else if ( input$select.optimization == "SANN" ) {
                x.gev.fit <- suppressWarnings( gev.fit( x.kept, method = "SANN" ) )
            } else if ( input$select.optimization == "ismev::gev.fit" ) {
                x.gev.fit <- suppressWarnings( ismev::gev.fit( x.kept  ) )
                x.gev.fit$convergence <- x.gev.fit$conv
                x.gev.fit$value <- x.gev.fit$nllh
                x.gev.fit$par <- x.gev.fit$mle
            } else if ( input$select.optimization == "extRemes::fevd" ) {
                x.gev.fit <- suppressWarnings( extRemes::fevd( x.kept )$results ) }
            class( x.gev.fit ) <- c("list", "climex.gev.fit")
        } else {
            if ( input$button.min.max == "min" ){
                ## Apply GP fit to all values below a certain threshold
                x.kept <- -1* x.kept }
            ## Fits of GPD parameters to blocked data set
            if ( input$select.optimization == "Nelder-Mead" ){
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, input$slider.threshold.gev,
                                   method = "Nelder-Mead", show = FALSE ) )
            } else if ( input$select.optimization == "CG" ) {
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, input$slider.threshold.gev,
                                   method = "CG", show = FALSE ) )
            } else if ( input$select.optimization == "BFGS" ) {
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, input$slider.threshold.gev,
                                   method = "BFGS", show = FALSE ) )
            } else if ( input$select.optimization == "SANN" ) {
                x.gev.fit <- suppressWarnings(
                    ismev::gpd.fit( x.kept, input$slider.threshold.gev,
                                   method = "SANN", show = FALSE ) )
            } else {
                stop( "other fitting functions arn't supported yet!" )
            }
            ## For comparability.
            names( x.gev.fit$par ) <- c( "scale", "shape" )
            x.gev.fit$convergence <- x.gev.fit$conv
            x.gev.fit$value <- x.gev.fit$nllh
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
            x.block <- generate.data( )[[ 1 ]]
            x.gev.fit <- fit.gev( )
            if ( input$radio.gev.statistics == "blocks" ){
                current <- c( x.gev.fit$par[ 1 ], x.gev.fit$par[ 2 ], x.gev.fit$par[ 3 ],
                             x.gev.fit$value, aic( x.gev.fit ),
                             bic( x.gev.fit, x.block ), rlevd( x.gev.fit$par ) )
            } else 
                current <- c( 0, x.gev.fit$par[ 1 ], x.gev.fit$par[ 2 ],
                             x.gev.fit$value, aic( x.gev.fit ),
                             bic( x.gev.fit, x.block ), rlevd( x.gev.fit$par ) )
            ## history of the statistics
            last.3 <<- last.2
            last.2 <<- last.1
            last.1.aux <- current - last.values
            ## For the fitted parameters any deviation of more than 5 percent is marked red
            for ( ll in 1 : length( x.gev.fit$par ) ){
                if( abs( last.1.aux[ ll ] ) > 0.05* current[ ll ] ){
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
            x.html.table <- markdownToHTML(
                text = pandoc.table.return( x.table, style = "rmarkdown",
                                           split.tables = Inf ),
                fragment.only = TRUE )
            x.color.table <- color.table( x.html.table, css.colours ) })
####################################################################################
    
####################################################################################
######################## Likelihood animation (tab2) ###############################
####################################################################################
    ## Fitting the MLE again with the algorithm of choice
    output$menu.slider.location.lim <- renderMenu( {
        x.gev.fit <- fit.gev( )
        x.block <- generate.data()[[ 1 ]]
        par.init <- likelihood.initials( x.block )
        if ( input$slider.number.initial.points != 1 ){
            sliderInput( "slider.location.lim", "Location offset",
                        round( x.gev.fit$par[ 1 ] - 10, 1 ),
                        round( x.gev.fit$par[ 1 ] + 10, 1 ), round( x.gev.fit$par[ 1 ], 1 ) )
        } else
            numericInput( "numeric.location.set", "Location value",
                         value = par.init[ 1 ] ) } )
    output$menu.slider.scale.lim <- renderMenu( {
        x.gev.fit <- fit.gev()
        x.block <- generate.data()[[ 1 ]]
        par.init <- likelihood.initials( x.block )
        if ( input$slider.number.initial.points != 1 ){
            ifelse( x.gev.fit$par[ 2 ] - 5 < 0, scale.min <- 0,
                   scale.min <- x.gev.fit$par[ 2 ] - 5 )
            ifelse( x.gev.fit$par[ 2 ] - 2 < 0, scale.set <- 0,
                   scale.set <- x.gev.fit$par[ 2 ] - 2 )
            sliderInput( "slider.scale.lim", "Scale limits", round( scale.min, 1 ),
                        round( x.gev.fit$par[ 2 ] + 5, 1 ),
                        c( round( scale.set, 1 ), round( x.gev.fit$par[ 2 ], 1 ) + 2 ) )
        } else
            numericInput( "numeric.scale.set", "Scale value", par.init[ 2 ] ) } )
    output$menu.slider.shape.lim <- renderMenu( {
        x.gev.fit <- fit.gev()
        x.block <- generate.data()[[ 1 ]]
        par.init <- likelihood.initials( x.block )
        if ( input$slider.number.initial.points != 1 ){
            sliderInput( "slider.shape.lim", "Shape limits",
                        round( x.gev.fit$par[ 3 ] - 0.7, 1 ),
                        round( x.gev.fit$par[ 3 ] + 0.7, 1 ),
                        c( round( x.gev.fit$par[ 3 ], 1 ) - .3,
                          round( x.gev.fit$par[ 3 ], 1 ) + .3  ) )
        } else
            numericInput( "numeric.shape.set", "Shape value", par.init[ 3 ] ) } )
    mle.parameter.likelihood <- reactive( {
        x.block <- generate.data()[[ 1 ]]
        if ( input$select.optimization.procedure.likelihood == "dfoptim::nmk" ){
            par.mle <- try( nmk.modified( x = x.block )$par )
            if ( class( par.mle ) == "try-error" )
                par.mle <- gev.fit( x.block )$par
            return( par.mle )  } } )                
    cached.table.init <- NULL
    initial.parameters.likelihood <- reactive( {
        x.block <- generate.data()[[ 1 ]]
        input$table.draw.points
        par.init <- mle.parameter.likelihood()
        if ( input$slider.number.initial.points != 1 ){
            table.init <- data.frame( location = rep( input$slider.location.lim,
                                                     input$slider.number.initial.points ),
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
                        table.init[ ii, ] <- c( input$slider.location.lim,
                                               runif( 1, input$slider.scale.lim[ 1 ],
                                                     input$slider.scale.lim[ 2 ] ),
                                               runif( 1, input$slider.shape.lim[ 1 ],
                                                     input$slider.shape.lim[ 2 ] ) ) } }
            return( table.init )
        } else {
            table.init <- data.frame( location = input$numeric.location.set,
                                     scale = input$numeric.scale.set,
                                     shape = input$numeric.shape.set )
            if ( is.nan( likelihood( as.numeric( table.init ), x.block ) ) )
                stop( "The chosen parameter combination is not definied for the likelihood function of the chosen time series!" ) }
        return( table.init ) } )
    output$table.initial.points <- renderDataTable( {
        initial.parameters <- initial.parameters.likelihood()
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
                           location = c( round( x.mom[ 1 ], 2 ),round( x.lmom[ 1 ], 2 ),
                                        round( x.mle.par[ 1 ], 2 ) ),
                           scale = c( round( x.mom[ 2 ], 2 ), round( x.lmom[ 2 ], 2 ),
                                     round( x.mle.par[ 2 ], 2 ) ),
                           shape = c( round( x.mom[ 3 ], 2 ), round( x.lmom[ 3 ], 2 ),
                                     round( x.mle.par[ 3 ], 3 ) ) )
        return( x.df ) },
        options = list( dom = 't' ) )
    output$draw.likelihood.animation <- renderUI( {
        ## This reactive content only depends on the action button because of the use of the isolate() functions
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
            likelihood.gui( time.series = x.block,
                           starting.points = initial.parameters,
                           scale.lim = isolate( input$slider.scale.lim ),
                           shape.lim = isolate( input$slider.shape.lim ),
                           optimization.function = optimization.function,
                           optimization.steps =
                               isolate( input$slider.optimization.steps) ) } ) } ) }
    )
####################################################################################
   
