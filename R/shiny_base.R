##' @title Shiny app combining most of the tools used in extreme value analysis using GEV fitting via maximum likelihood.
##'
##' @details It possible to provide a time series of type \pkg{xts} to a list of elements of this type. This app need the its own css file. This function will be exclusively called when the climex app is run on localhost. In order to assure its correct behavior the necessary file/folder structure in the CLIMEX.PATH (global variable which has to be set beforehand; see vignette for details) will be check and if necesary generated. The input time series is accepted in three different formats
##' \enumerate{
##'  \item As a single time series in the formatted using the \pkg{xts} package 
##'  \item As a named list of various xts time series 
##'  \item As a list containing two elements;  a list of xts time series and a data.frame with the columns \emph{longitude}, \emph{latitude}, \emph{altitude}, \emph{name} corresponding to the stations geo-coordinates, height and name 
##' }
##'
##' @param x.input Time series of one of the formats mentioned in the details section. 
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
    ## will contain the folder in which the images of the animation can be found
    image.folder <<- paste0( CLIMEX.PATH, "app/www/images" )
    ## this is unfortunately necessary since some JavaScript scripts are written out and
    ## some images are plotted which have to be accessible within the shiny app
    ## If there are not the appropriate files present to run the shiny app, the apps as well
    ## as the folder structure has to be generated
    if ( !dir.exists( paste0( CLIMEX.PATH, "app" ) ) )
        dir.create( paste0( CLIMEX.PATH, "app" ) )
    if ( !dir.exists( paste0( CLIMEX.PATH, "app/www" ) ) )
        dir.create( paste0( CLIMEX.PATH, "app/www" ) )
    ## In order to display the animation the app needs the jquery scianimator. Since it is not able
    ## to access a system file on its own the wrapper climex() will copy it to the apps folder
    if ( !dir.exists( paste0( CLIMEX.PATH, "app/www/jquery.scianimator.min.js" ) ) ){
        file.copy( paste0( system.file( "climex_app", package = "climex" ),
                          "/js/jquery.scianimator.min.js" ),
                  to = paste0( CLIMEX.PATH, "app/www/jquery.scianimator.min.js" ), overwrite = TRUE )
    }
    ## source of the gif: http://i.imgur.com/seuaOqf.gif
    ## since this is a non-commercial product there shouldn't be any problems
    ## http://imgur.com/tos
    file.copy( paste0( system.file( "climex_app", package = "climex" ), "/js/loadingGif.js" ),
              to = paste0( CLIMEX.PATH, "app/www/loadingGif.js" ), overwrite = TRUE ) 
    file.copy( paste0( system.file( "climex_app", package = "climex" ), "/res/loading.gif" ),
              to = paste0( CLIMEX.PATH, "app/www/loading.gif" ) )      
    writeLines( "shinyUI( climex.ui() )", con = paste0( CLIMEX.PATH, "app/ui.R" ) )
    writeLines( "shinyServer( climex.server )", con = paste0( CLIMEX.PATH, "app/server.R" ) )
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
##' @details Since it grew organically most of the features are defined inside this function. Okay, why did I decided to define the climex.server and the climex.ui function? Since in this way I can provide a local and a server side variant of the code which is maintained in the same place. I do not really like the idea of installing the newest version of the code from Github and than copy/pasting the code in some files somewhere in /srv/shiny-server.
##'
##' @return Function acting as the shiny server.
##' @import shiny
##' @import xts
##' @import dygraphs
##' @import ggplot2
##' @export
##' @author Philipp Mueller 
climex.server <- function( input, output, session ){
####################################################################################
######################## Customizing the sidebar menu ##############################    
####################################################################################
    ## So everything in my code is always of the style this.is.a.name. So why do I use the
    ## camel case thisIsAName for the shiny objects? Well, since CSS file do not support
    ## the point separator.
    ##
    ## For now there are three different data sources you can choose:
    ## the input provided when calling climex::climex() and running it on
    ## localhost, the station data of the German weather service (DWD) and
    ## artificial data sampled from a GEV distribution
    output$menuSelectDataBase <- renderMenu( {
        if ( !is.null( x.input ) ){
            selectInput( "selectDataBase", "Data base",
                        choices = c( "input", "DWD", "artificial data" ), selected = "input" )
        } else
            selectInput( "selectDataBase", "Data base",
                        choices = c( "input", "DWD", "artificial data" ), selected = "DWD" )
    } )
    ## Using this drop down menu one can select the specific type of input
    ## data to use, since the DWD data base provides different
    ## measurements recorded at the individual stations.
    output$menuSelectDataSource1 <- renderMenu( {
        ## If artificial data was choosen as the input source, display
        ## a slider for the location parameter of the parent GEV
        ## distribution
        if ( !is.null( input$selectDataBase ) &&
                  input$selectDataBase == "artificial data" ){
            return( sliderInput( "sliderArtificialDataLocation",
                                "location", -30, 30, 1, round = 15 ) )
        }
        ## just show the stations which amount of years is larger than the
        ## specified number (slider)
        ## list(stations.selected, position.selected). This already does
        ## the look-up in file.loading(), input$selectDataBase and
        ## input$selectDataType so no need to repeat these steps.
        data.selected <- data.chosen() 
        ## Since this is not a crucial element and the other elements have
        ## a fallback to the Potsdam time series we can just wait until
        ## input$selectDataBase and input$sliderMap( used in data.chosen )
        ## are initialized 
        if ( !is.null( input$selectDataBase ) &&
                  !is.null( data.selected ) ){
            ## use the leaflet map to choose a individual station
            if ( !is.null( input$leafletMap_marker_click ) ){
                map.click <- input$leafletMap_marker_click
                ## picked station
                station.name <- as.character(
                    data.selected[[ 2 ]]$name[ which(
                                            data.selected[[ 2 ]]$latitude
                                            %in% map.click$lat &                                                      data.selected[[ 2 ]]$longitude
                                            %in% map.click$lng ) ] )
            } else {
                ## if not just use the Potsdam station for the DWD data
                ## or the first station in the list for every other source
                if ( input$selectDataBase == "DWD" ){
                    station.name <- "Potsdam"
                } else
                    station.name <- as.character(
                        data.selected[[ 2 ]]$name[ 1 ] )
            }
            ## export drop-down menu
            selectInput( "selectDataSource", "Station",
                        choices = as.character(  data.selected[[ 2 ]]$name ),
                        selected = as.character( station.name ) )
        } else
            NULL } )
    output$menuSelectDataSource2 <- renderMenu( {
        if ( !is.null( input$selectDataBase ) ){
            if ( input$selectDataBase == "DWD" ){
                selectInput( "selectDataType", "Measured variable",
                            choices = c( "Daily max. temp.", "Daily min. temp.",
                                        "Daily precipitation" ),
                            selected = "Daily max. temp" )                        
            } else if ( input$selectDataBase == "artificial data" ){
                sliderInput( "sliderArtificialDataScale", "scale", 0, 4, 0.8, round = -2 )
            } else if ( input$selectDataBase == "input" ){
                NULL
            }
        } else
            NULL } )
    ## Display either the shape slider or the option to load a .RData file from
    ## disk (only on localhost)
    output$menuSelectDataSource3 <- renderMenu( {
        if ( is.null( input$selectDataBase ) )
            return( NULL )
        if ( input$selectDataBase == "artificial data" ){
            sliderInput( "sliderArtificialDataShape", "shape", -1.5, 1.5, -0.25, round = -2 )
        } else if ( input$selectDataBase == "input" && ( session$clientData$url_hostname == "localhost" ||                                                                  session$clientData$url_hostname == "127.0.0.1"  )){
            ## due to security concerns only allow the file selection on
            ## localhost
            fileInput( "fileInputSelection", "Choose a .RData file" )
        } else
            return( NULL ) } )
    output$sliderGevStatistics <- renderMenu( {
        x.xts <- data.selection()
        x.deseasonalized <- deseasonalize.interactive( x.xts )
        if ( is.null( x.deseasonalized ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        if ( input$radioGevStatistics == "Blocks" ){
            sliderInput( "sliderBoxLength", "Box length in days", 1, 365*3, 365 )
        } else {
            ##  if ( input$buttonMinMax == "Max" || is.null( input$buttonMinMax ) ){
            sliderInput( "sliderThresholdGev", "Threshold",
                        round( min( x.deseasonalized, na.rm = TRUE ) ),
                        round( max( x.deseasonalized, na.rm = TRUE ) ),
                        round( 0.8* max( x.deseasonalized, na.rm = TRUE ) ) )
            ##   } else
            ##         sliderInput( "sliderThresholdGev", "Threshold:",
            ##                     round( min( x.deseasonalized, na.rm = TRUE ) ),
            ##                     round( max( x.deseasonalized, na.rm = TRUE ) ),
            ##                     round( 0.8* min( x.deseasonalized, na.rm = TRUE ) ) )
        }
    } )
    output$menuDataCleaning <- renderMenu( {
        ## When applying the blocking method incomplete years distort the time series and
        ## have to be excluded. When using the threshold method on the other hand clusters
        ## are most likely to occure due to short range correlations. This has to be
        ## avoided by using declustering algorithms (which mainly picks the maximum of a
        ## specific cluster)
        if ( !is.null( input$radioGevStatistics ) ) {
            if ( input$radioGevStatistics == "Blocks" ){
                checkboxInput( "checkBoxIncompleteYears", "Remove incomplete years", FALSE )
            } else
                checkboxInput( "checkBoxDecluster", "Declustering of the data", FALSE )
        } else
            checkboxInput( "checkBoxIncompleteYears", "Remove incomplete years", FALSE )
    } )
#################################################################################### 
    
####################################################################################
#################### Data generation and selection #################################
####################################################################################
    file.loading <- reactive( {
        ## If no file is chosen, don't do anything
        if ( is.null( input$fileInputSelection$datapath ) )
            return( NULL )
        ## load selected file
        load( input$fileInputSelection$datapath, file.load.env <- new.env() )
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
        toastr_warning( "Sorry but this feature is implemented for the format in which the argument x.input is accepted in climex::climex only! Please do the conversion and formatting in R beforehand and just save a .RData containing a single object" )
        return( NULL )
    } )       
    data.selection <- reactive( {
        ## Selecting the data out of a pool of different possibilities or generate them
        ## artificially
        data.selected <- data.chosen()
        if ( is.null( data.selected ) || is.null( input$selectDataSource ) ){
            ## as long as the menus are not initialized yet, just assign
            ## the default time series.
            ## x.xts <- x.input
            return( NULL )
        } else if( input$selectDataBase == "artificial data" ){
            ## The length of the artificial time series is determined by
            ## the number of years chosen via the input$sliderMap slider
            ## in the leaflet tab
            x.xts <- xts( extRemes::revd( input$sliderMap,
                                         loc = input$sliderArtificialDataLocation,
                                         scale = input$sliderArtificialDataScale,
                                         shape = input$sliderArtificialDataShape, type = "GEV" ),
                         order.by = index( stations.temp.max[[ 64 ]] ) )
        } else {
            ## In all other cases the possible choices are contained in
            ## the data.selected object and are agnostic of the data base
            ## /input chosen
            if ( any( class( data.selected[[ 1 ]] ) == "xts" ) ){
                x.xts <- data.selected[[ 1 ]]
            } else {
                ## There is a bug when switching from one data base into
                ## another: since the input$selectDataSource needs a
                ## little bit longer to update it tries to access a value
                ## in here which is might not present in the newly
                ## selected one. If this happens, just select the first
                ## element instead
                if ( !any(  names( data.selected[[ 1 ]] ) == input$selectDataSource ) ){
                    x.xts <- data.selected[[ 1 ]][[ 1 ]]
                    shinytoastr::toastr_info(
                        "Please check the selected station after changing the data base!" ) 
                } else
                    x.xts <- data.selected[[ 1 ]][[ which( names( data.selected[[ 1 ]] ) == input$selectDataSource ) ]]
            }
        }
        return( cleaning.interactive( x.xts ) )
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
        return( x.xts )
    }
    data.blocking <- reactive( {
        ## Decides if either the GEV distribution with block maxima or the Pareto
        ## distribution if threshold excedence should be considered.
        ## Import data set, cut it to the desired intervale, deseasonalize and block it
        x.xts <- data.selection()
        if ( is.null( x.xts ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        x.deseasonalized <- deseasonalize.interactive( x.xts )
        x.block <- blocking.interactive( x.deseasonalized )
        return( list( blocked.data = x.block, deseasonalized.data = x.deseasonalized,
                     pure.data = x.xts ) ) } )
    ## wrapper blocking time series according to the set slider etc. values
    blocking.interactive <- function( x.xts ){
        ## Toggle if maxima of minima are going to be used
        if ( is.null( input$buttonMinMax ) ){
            block.mode <- "max"
        } else if ( input$buttonMinMax == "Max" ){
            block.mode <- "max"
        } else
            block.mode <- "min"
        if ( is.null( input$radioGevStatistics ) ){
            ## While initialization input$radioGevStatistics and input$sliderBoxLength are
            ## NULL. Therefore this is the fallback default x.block
            x.block <- block( x.xts, separation.mode = "years", block.mode = block.mode )
        } else if ( input$radioGevStatistics == "Blocks" ){
            ## Box size as dynamic input parameter
            if ( is.null( input$sliderBoxLength ) ||  input$sliderBoxLength == 366 ||
                                                          input$sliderBoxLength == 365 ){
                x.block <- block( x.xts, separation.mode = "years",
                                 block.mode = block.mode )
            } else
                x.block <- block( x.xts, block.length = input$sliderBoxLength,
                                 block.mode = block.mode )
        } else if ( input$radioGevStatistics == "Threshold" ){
            ## Due to "historical" reasons the vector containing the resulting values will
            ## still be called x.block. The "block.mode" and the corresponding
            ## "input$buttonMinMax" are still use full and decide if the values above
            ## or below the threshold are going to be extracted
            if ( is.null( input$sliderThresholdGev ) ){
                x.block <- x.xts[ x.xts >= max( x.xts )* .8 ]
            } else
                x.block <- x.xts[ x.xts >= input$sliderThresholdGev ]
            if ( !is.null( input$checkBoxDecluster ) ){
                if ( input$checkBoxDecluster )
                    x.block <- declustering( x.block, input$sliderThresholdGev )
            }
            return( x.block )
        }
    }
####################################################################################
    
####################################################################################
##################### Interactive stuff and auxillary functions ####################
####################################################################################
    ## By doing this the value is temporary removed from the ts (after removal of
    ## incomplete years) and a new extremum is calculated for the correspondig year
    reactive.values <- reactiveValues( keep.rows = NULL )
    observe( {
        x.data <- data.blocking()
        ## use the x.block variable to update the reactive value keep.row (containing
        ## a listing of all the points of the actual time series which are used during
        ## the fitting procedure)
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
        df.block <- data.frame( date = index( x.block ), value = as.numeric( x.block ) )
        result <- nearPoints( df.block, input$plotBlockedClick, allRows = TRUE )
        reactive.values$keep.rows <- xor( reactive.values$keep.rows, result$selected_ ) } )
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
        df.block <- data.frame( date = index( x.block ), value = as.numeric( x.block ) )
        result <- brushedPoints( df.block, input$plotBlockedBrush, allRows = TRUE )
        reactive.values$keep.rows <- xor( reactive.values$keep.rows, result$selected_ ) } )
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
############################# declustering of the ts ###############################
    ## Inspired by extRemes::decluster.intervals and based on Ferro, C. A. T. and Segers, J.
    ## (2003). Inference for clusters of extreme values. _Journal of the Royal Statistical
    ## Society B_, *65*, 545-556.
    declustering <- function( x, threshold ){
        ## Caution: x is the full time series and not the blocked one!
        x.extremal.index <- extRemes::extremalindex( x, threshold, na.action = stats::na.omit )
        ifelse( x.extremal.index[ 1 ] >= 1, cluster.size <- 0, cluster.size <-
                                                                   x.extremal.index[ 3 ] )
        ## cluster.size is the number of indices a two points have to be away from each
        ## other to belong to different indices
        n <- length( x )
        which.x.over.threshold <- x > threshold
        which.x.over.threshold <- stats::na.omit( which.x.over.threshold )
        x.over.threshold <- x[ which.x.over.threshold ]
        n.over.threshold <- sum( which.x.over.threshold ) # amount of points over threshold
        index.x.over.threshold <- ( 1 : n )[
            stats::na.omit( which.x.over.threshold ) ] # index of those points in the ts 'x'
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
        if ( is.null( input$selectDataBase ) ){
            y.label <- "temperature in °C"            
        } else if ( input$selectDataBase == "artificial data" ){
            y.label <- "GEV sample"
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
    output$plotDeseasonalized <- renderDygraph( {
        x.data <- data.blocking( )
        if ( is.null( x.data ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
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
    output$plotBlocked <- renderPlot( {
        x.block <- data.blocking( )[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
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
                                  fill = "white", size = 2, shape = 21 ) + ylab( y.label ) + theme_bw() +
                       theme( axis.title = element_text( size = 17, colour = colour.ts ),
                             axis.text = element_text( size = 13, colour = colour.ts ) )
    } )
###### GEV fit and analysis plots ##################################################  
    output$plotFitGev <- renderPlot( {
        ## Plots the result of the fitted GEV
        x.block <- data.blocking( )[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        x.kept <- x.block[ reactive.values$keep.rows ]
        if ( !is.null( input$buttonMinMax ) ){
            if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" )
                x.kept <- ( -1 )* x.kept
        }
        x.lim <- c( max( x.kept ), min( x.kept ) )
        x.fit.gev <- gev.fitting( )
        if ( is.null( x.fit.gev ) )
            return( NULL )
        ## the amount of bins is changing whenever a single event is toggled. This is distracting.
        ## only if a certain amount of points are toggled (5%) a different number of bins shall
        ## be used. The number of boxes should be half the number of points in 5% steps.
        ## gg1.bins gives a factor which multiplied with the length of x.kept yields the number
        ## of blocks.
        gg1.bins <- ( ( ( length( x.kept ) - 1 )*100/ length( x.block ) )  %/% 5 )* 0.025
        ## for later usage: bins are at least this wide
        gg1.bin.width <- ( ( max( x.kept ) - min( x.kept ) )/( gg1.bins * length( x.kept ) ) )* 1.1
        if ( input$radioGevStatistics == "Blocks" ){
            ## GEV
            ## determining the limits of the PDF plot
            threshold.pdf.plot <- 5E-4
            plot.range <- seq( x.fit.gev$par[ 1 ] - x.fit.gev$par[ 2 ]* 10,
                              x.fit.gev$par[ 1 ] + x.fit.gev$par[ 2 ]* 10, 0.01 )
            plot.data <- data.frame( x = plot.range, y = ismev::gev.dens( x.fit.gev$par,
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
            if ( is.null( input$sliderThresholdGev ) ){
                threshold <- max( x.kept )* .8
            } else 
                threshold <- input$sliderThresholdGev
            plot.range <- seq( x.lim[ 2 ], x.lim[ 1 ]* 1.1, 0.01 )
            plot.data <- data.frame( x = plot.range,
                                    y = ismev::gpd.dens( x.fit.gev$par, threshold, plot.range ) )
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
                          theme( legend.position = "none" ) +
                          theme( axis.title = element_text( size = 17, colour = colour.ts ),
                                axis.text = element_text( size = 13, colour = colour.ts ) )
        if ( is.null( input$buttonMinMax ) ){
            return( gg1 )
        } else if ( input$buttonMinMax == "Min" ){
            ## Adding a note when the minima are fitted
            ## upper point of the density plot
            y.lim.density <- max( stats::density( x.kept )$y )
            ## upper point of the histogram (at least more or less since a different
            ## amount of breaks are used )
            y.lim.histogram <- max( graphics::hist( x.kept, plot = FALSE )$density )
            if ( input$radioGevStatistics == "Threshold" ){
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
    output$plotFitQQ <- renderPlot( {
        ## Quantile-quantile plot for fit statistics
        x.block <- data.blocking( )[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.fit.gev <- gev.fitting()
        if ( is.null( x.fit.gev ) )
            return( NULL )
        if ( input$radioGevStatistics == "Blocks" ){
            if ( is.null( input$buttonMinMax ) ){
                plot.data <- data.frame(
                    model = extRemes::qevd( stats::ppoints( length( x.kept ), 0 ),
                                           loc = x.fit.gev$par[ 1 ], scale = x.fit.gev$par[ 2 ],
                                           shape = x.fit.gev$par[ 3 ], type = "GEV" ),
                    empirical = sort( as.numeric( x.kept ) ) )
            } else if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" ){
                plot.data <- data.frame(
                    model = extRemes::qevd( stats::ppoints( length( x.kept ), 0 ),
                                           loc = x.fit.gev$par[ 1 ], scale = x.fit.gev$par[ 2 ],
                                           shape = x.fit.gev$par[ 3 ], type = "GEV" ),
                    empirical = -1* sort( as.numeric( -x.kept ) ) )
            } else
                plot.data <- data.frame(
                    model = extRemes::qevd( stats::ppoints( length( x.kept ), 0 ),
                                           loc = x.fit.gev$par[ 1 ], scale = x.fit.gev$par[ 2 ],
                                           shape = x.fit.gev$par[ 3 ], type = "GEV" ),
                    empirical = sort( as.numeric( x.kept ) ) )
        } else {
            if ( is.null( input$sliderThresholdGev ) ){
                threshold <- max( x.kept )* .8
            } else 
                threshold <- input$sliderThresholdGev
            plot.data <- data.frame(
                model = extRemes::qevd( stats::ppoints( length( x.kept ), 0 ), scale = x.fit.gev$par[ 1 ], 
                                       shape = x.fit.gev$par[ 2 ], type = "GP",
                                       threshold = threshold ),
                empirical = sort( as.numeric( x.kept ) ) ) }
        plot.fit <- stats::lm( model ~ empirical, plot.data )[[ 1 ]]
        gg.qq1 <- ggplot() + geom_point( data = plot.data, aes( x = model, y = empirical ),
                                        colour = colour.ts,
                                        shape = 1, size = 2, alpha = 0.8 ) +
                             geom_abline( intercept = plot.fit[ 1 ], slope = -plot.fit[ 2 ], colour = colour.ts,
                                         linetype = 2 ) +
                             geom_abline( intercept = 0, slope = 1, colour = colour.extremes ) +
                             theme_bw() +
                             theme( axis.title = element_text( size = 15, colour = colour.ts ),
                                   axis.text = element_text( size = 12, colour = colour.ts ) )
        if ( !is.null( input$buttonMinMax ) ){
            if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks")
                gg.qq1 <- gg.qq1 + scale_y_reverse()
        }
        return( gg.qq1 ) } )
    output$plotFitQQ2 <- renderPlot( {
        ## Quantile-quantile plot for fit statistics with samples drawn from fitted GEV
        x.block <- data.blocking( )[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.fit.gev <- gev.fitting()
        if ( is.null( x.fit.gev ) )
            return( NULL )
        if ( input$radioGevStatistics == "Blocks" ){
            sampled <- sort( extRemes::revd( length( x.kept ), loc = x.fit.gev$par[ 1 ],
                                            scale = x.fit.gev$par[ 2 ],
                                            shape = x.fit.gev$par[ 3 ], type = "GEV" ) )
            if ( !is.null( input$buttonMinMax ) ){
                if ( input$buttonMinMax == "Min" )
                    sampled <- -1* sampled
            }
        } else{
            sampled <- sort( extRemes::revd( length( x.kept ), scale = x.fit.gev$par[ 1 ],
                                            shape = x.fit.gev$par[ 2 ], type = "GEV",
                                            threshold = input$sliderThresholdGev ) )
        }
        if ( is.null( input$buttonMinMax ) ){
            empirical <- sort( as.numeric( x.kept ) )
        } else if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" ){
            empirical <- -1* sort( as.numeric( -x.kept ) )
        } else
            empirical <- sort( as.numeric( x.kept ) )
        length.e <- length( empirical )
        length.s <- length( sampled )
        ## inspired by extRemes::qqplot( plot.data$empirical, plot.data$sampled )
        ## function giving a linear interpolation 
        function.sampled.interpolate <- stats::approxfun( seq( 0, 1, length = length( sampled ) ),
                                                  sort( sampled ), yleft = NA, yright = NA )
        if ( is.null( input$buttonMinMax ) ){
            period <- ( 1 : length( empirical ) - 1 )/ ( length( empirical ) - 1 )
        } else if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" ){
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
        plot.fit <- stats::lm( empirical ~ sampled, plot.data )[[ 1 ]]
        gg.qq2 <- ggplot() + geom_point( data = plot.data, aes( x = sampled, y = empirical ),
                                        colour = colour.ts, shape = 1, size = 2, alpha = 0.8,
                                        na.rm = TRUE ) +
                             geom_line( data = plot.data, aes( x = sampled.ci.low, y = empirical ), linetype = 2,
                                       colour = colour.extremes, na.rm = TRUE ) +
                             geom_line( data = plot.data, aes( x = sampled.ci.high, y = empirical ), linetype = 2,
                                       colour = colour.extremes, na.rm = TRUE ) +
                             geom_abline( intercept = plot.fit[ 1 ], slope = plot.fit[ 2 ], colour = colour.ts,
                                         linetype = 2 ) + theme_bw() +
                             geom_abline( intercept = 0, slope = 1, colour = colour.extremes )+
                             theme( axis.title = element_text( size = 15, colour = colour.ts ),
                                   axis.text = element_text( size = 12, colour = colour.ts ) )
        if ( !is.null( input$buttonMinMax ) ){
            if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" )
                gg.qq2 <- gg.qq2 + scale_y_reverse() + scale_x_reverse()
        }
        return( gg.qq2 )        
    } )
    output$plotFitReturnLevel <- renderPlot( {
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        x.kept <- x.block[ reactive.values$keep.rows ]
        x.fit.gev <- gev.fitting()
        if ( is.null( x.fit.gev ) )
            return( NULL )
        x.period <- c( 2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800 )
        if ( input$radioGevStatistics == "Blocks" ){
            if ( is.null( input$buttonMinMax ) ){
                ## the true block maxima and their return levels will be calculated
                x.confidence.intervals <- extRemes::ci.fevd.mle( climex:::as.fevd( x.kept, x.fit.gev,
                                                                                  type = "GEV"),
                                                                return.period = x.period )
                plot.data <- data.frame( x = -1/ log( stats::ppoints( length( x.kept ), 0 ) ),
                                        y = sort( as.numeric( x.kept ) ) )
                plot.y.limits <- c( plot.data$y[ which.min( abs( plot.data$x - 1 ) ) ],
                                   max( x.confidence.intervals[ , 3 ] ) )
            } else if ( input$buttonMinMax == "Min" ){
                ## the time series will be negated and the results too to aquire the
                ## return levels of the minima
                x.confidence.intervals <- ( -1 )*
                                              extRemes::ci.fevd.mle( climex:::as.fevd( -x.kept, x.fit.gev, type = "GEV"),
                                                                    return.period = x.period )
                plot.data <- data.frame( x = -1/ log( stats::ppoints( length( x.kept ), 0 ) ),
                                        y = -1* sort( as.numeric( -x.kept ) ) )
                plot.y.limits <- c( min( x.confidence.intervals[ , 3 ] ),
                                   max( plot.data$y ) )
            } else {
                ## the true block maxima and their return levels will be calculated
                x.confidence.intervals <- extRemes::ci.fevd.mle( climex:::as.fevd( x.kept, x.fit.gev,
                                                                                  type = "GEV"),
                                                                return.period = x.period )
                plot.data <- data.frame( x = -1/ log( stats::ppoints( length( x.kept ), 0 ) ),
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
            ## x.gpd.fit <- extRemes::fevd(  x.kept, threshold = input$sliderThresholdGev,
            ##                             type = "GP" )
            ## x.return.level <- extRemes::rlevd( x.period, type = "GP",
            ##                                   scale = x.gpd.fit$results$par[ 1 ],
            ##                                   shape = x.gpd.fit$results$par[ 2 ],
            ##                                   threshold = input$sliderThresholdGev )            
            ## x.confidence.intervals <- extRemes::ci.fevd.mle( x.gpd.fit, return.period = x.period )
            ## plot.statistics <- data.frame( period = x.period,
            ##                               level = as.numeric( x.confidence.intervals[ , 2 ] ),
            ##                               ci.low = as.numeric( x.confidence.intervals[ , 1 ] ), 
            ##                               ci.high = as.numeric( x.confidence.intervals[ , 3 ] ) )
            ## x.sort <- sort( as.numeric( x.kept ) )            
            ## plot.data <- data.frame( x = -1/ log( ppoints( length( x.kept ), 0 ) )[
            ##                                      x.sort > input$sliderThresholdGev ],
            ##                         y = x.sort[ x.sort > input$sliderThresholdGev ] )
            ## plot.data <- data.frame( x = x.period,
            ##                         y = input$sliderThresholdGev +
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
                                      colour = colour.extremes, na.rm = TRUE ) + xlab( "return period" ) +
                            ylab( "return level" ) + theme_bw() + scale_x_log10( limits = c( 1, 1000 ) )+
                            theme( axis.title = element_text( size = 15, colour = colour.ts ),
                                  axis.text = element_text( size = 12, colour = colour.ts ) )
        if ( !is.null( input$buttonMinMax ) ){
            if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" ){
                gg.rl <- gg.rl + scale_y_reverse() 
            } else
                gg.rl <- gg.rl + ylim( plot.y.limits )
        }
        return( gg.rl )  } )
####################################################################################
    
####################################################################################
######################## Deseasonalization of the data #############################
####################################################################################
    ## since the deseasonalization will also be applied to a large variety of different
    ## stations in the leaflet plot, it will become a separate function
    deseasonalize.interactive <- function( x.xts ){
        ## Dropdown for deseasonalization method
        if ( is.null( x.xts ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        if ( !is.null( input$selectDataBase ) ){
            if ( input$selectDataBase == "artificial data" ){
                ## For the artificial data there is no need for deseasonalization
                return( x.xts ) }
        }
        ## if not all elements are initialized yet, define some defaults
        select.deseasonalize <- ifelse( is.null( input$selectDeseasonalize ), "Anomalies",
                                       input$selectDeseasonalize )
        x.deseasonalized <- switch(
            select.deseasonalize,
            "Anomalies" = anomalies( x.xts ),
            "decompose" = {
                shinytoastr::toastr_warning(
                    "WARNING: only the seasonal component and not the trend was removed" )
                if ( any( is.na( x.xts ) ) ){
                    ## There are missing values in the time series. This
                    ## is lethal for the decompose function. So the
                    ## incomplete years have to be removed
                    shinytoastr::toastr_info(
                        "Incomplete years were removed internally" )
                    x.xts <- climex::remove.incomplete.years( x.xts )
                }
                x.decomposed <- stats::decompose( stats::ts( as.numeric( x.xts ),
                                              frequency = 365.25 ) ) 
                x.xts - as.numeric( x.decomposed$seasonal ) },
            "stl" = {
                shinytoastr::toastr_warning(
                    "WARNING: only the seasonal component and not the trend was removed and a s window of 30 was used" )
                x.decomposed <- stats::stl( stats::ts( as.numeric( x.xts ),
                                        frequency = 365.25 ), 30 )
                x.xts - as.numeric( x.decomposed$time.series[ , 1 ] ) }, 
            "deseasonalize::ds" = {
                x.aux <- deseasonalize::ds( x.xts )$z
                xts( x.aux, order.by = index( x.xts ) ) },
            "none" = x.xts,
            { shinytoastr::toastr_warning(
                  "the decomponer function takes forever and X-13ARIMA is not implemented yet. Anomalies are use instead" ) 
                anomalies( x.xts ) } )
        return( x.deseasonalized ) }
####################################################################################
    
####################################################################################
######################## Fitting of the GEV distribution ###########################
####################################################################################
    ## Since I will fit a couple of times with the chosen fitting parameters/algorithms
    ## I will hard code it and just call it at the required points 
    fit.interactive <- function( x.kept, x.initial = likelihood.initials( x.kept ) ){
        ## wait for the initialization
        if ( is.null( input$radioGevStatistics ) || is.null( input$selectOptimization ) )
            return( NULL )
        ##! Drop-down menu to decide which fitting routine should be used
        if ( input$radioGevStatistics == "Blocks" ){
            ## Fits of GEV parameters to blocked data set
            x.fit.gev <- suppressWarnings( switch(
                input$selectOptimization,
                "Nelder-Mead" = fit.gev( x.kept, initial = x.initial, rerun = input$checkboxRerun,
                                        method = "Nelder-Mead", error.estimation = "none" ),
                "CG" = fit.gev( x.kept, initial = x.initial, rerun = input$checkboxRerun,
                               method = "CG", error.estimation = "none" ),
                "BFGS" = fit.gev( x.kept, initial = x.initial, rerun = input$checkboxRerun,
                                 method = "BFGS", error.estimation = "none" ),
                "SANN" = fit.gev( x.kept, initial = x.initial, rerun = input$checkboxRerun,
                                 method = "SANN", error.estimation = "none" ),
                "dfoptim::nmk" = fit.gev( x.kept, initial = x.initial, rerun = input$checkboxRerun,
                                         method = "nmk", error.estimation = "none" ) ) ) 
            class( x.fit.gev ) <- c( "list", "climex.fit.gev" )
        } else {
            ## Fits of GPD parameters to blocked data set
            if ( is.null( input$sliderThresholdGev ) ){
                threshold <- max( x.kept )* .8
            } else
                threshold <- input$sliderThresholdGev
            suppressWarnings(
                x.fit.gev <- ismev::gpd.fit( x.kept, threshold,
                                            show = FALSE,
                                            method =
                                                input$selectionOptimization ) )
            ## For comparability.
            x.fit.gev$par <- x.fit.gev$mle
            names( x.fit.gev$par ) <- c( "scale", "shape" )
            x.fit.gev$convergence <- x.fit.gev$conv
            x.fit.gev$value <- x.fit.gev$nllh
            x.fit.gev$x <- x.kept
        }
        return( x.fit.gev )
    }
    ## Fitting of the time series selected via a click on the map or the select form in the sidebar
    ## For this time series it is possible to exclude individual points via clicking on them in the
    ## Time series::remaining plot
    gev.fitting <- reactive( {
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
        ## negating the time series to derive the statistics for the minima
        if ( !is.null( input$buttonMinMax ) ){
            if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" )
                x.kept <- ( -1 )* x.kept
        }
        return( fit.interactive( x.kept, x.initial ) )
    } )
    ## the purpose of this function is to supply a data.frame containing the 50/100/500 year return
    ## level of all selected stations. Lets see how fast it will be. Maybe I will just calculate one
    ## return level per station.
    ## This will be calculated on demand (as soon as the user clicks the corresponding form)
    calculate.chosen.return.levels <- reactive( {
        data.selected <- data.chosen()
        return.level.year <- input$sliderMapReturnLevel # selected return level
        ## wait for initialization
        if ( is.null( input$sliderMapReturnLevel ) || is.null( data.selected ) )
            return( NULL )
        ## if no geo-coordinates are provided for the time series, don't calculate the return levels
        if ( any( is.na( c( data.selected[[ 2 ]]$longitude, data.selected[[ 2 ]]$latitude ) ) ) )
            return( NULL )
        ## clean the stations
        data.cleaned <- lapply( data.selected[[ 1 ]], cleaning.interactive )
        ## deseasonalize them
        data.deseasonalized <- lapply( data.cleaned, deseasonalize.interactive )
        ## block them
        data.blocked <- lapply( data.deseasonalized, blocking.interactive )
        ## calculate the return level and append it to the data.selected[[ 2 ]] data.frame
        data.selected[[ 2 ]]$return.level <- Reduce( c, lapply( data.blocked, function( x )
            climex::return.level( fit.interactive( x ), return.level.year ) ) )
        return( data.selected[[ 2 ]] )
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
    output$tableStatistics <- renderUI( {
        ## define the colour for increasing or decreasing values
        css.colours <- c( "#C53100", "#0D8F20" ) # >0, <0, normal
        x.fit.gev <- gev.fitting( )
        if ( is.null( x.fit.gev ) )
            return( NULL )
        x.block <- data.blocking( )[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        if ( input$radioGevStatistics == "Blocks" ){
            current <- c( x.fit.gev$par[ 1 ], x.fit.gev$par[ 2 ], x.fit.gev$par[ 3 ],
                         x.fit.gev$value, climex:::aic( x.fit.gev ), climex:::bic( x.fit.gev ),
                         climex:::return.level( x.fit.gev$par, error.estimation = "none" ) )
        } else 
            current <- c( 0, x.fit.gev$par[ 1 ], x.fit.gev$par[ 2 ],
                         x.fit.gev$value, climex:::aic( x.fit.gev ), climex:::bic( x.fit.gev ),
                         climex:::return.level( x.fit.gev, error.estimation = "none" ) )
        ## negating the return level to get the correct results for the minium
        if ( !is.null( input$buttonMinMax ) ){
            if ( input$buttonMinMax == "Min" && input$radioGevStatistics == "Blocks" )
                current[ 7 ] <- ( -1 )* current[ 7 ]
        }
        ## history of the statistics
        last.3 <<- last.2
        last.2 <<- last.1
        last.1.aux <- current - last.values
        ## For the fitted parameters any deviation of more than 1 percent is marked red
        for ( ll in 1 : length( x.fit.gev$par ) ){
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
        for ( ll in ( length( x.fit.gev$par ) + 1 ) : length( last.1.aux ) ){
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
        x.color.table <- climex:::color.table( x.html.table, css.colours ) })
####################################################################################
    
####################################################################################
######################## Likelihood animation (tab2) ###############################
####################################################################################
    ## Fitting the MLE again with the algorithm of choice
    output$menuSliderLocationLim <- renderMenu( {
        x.fit.gev <- gev.fitting()
        if ( is.null( x.fit.gev ) )
            return( NULL )
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        par.init <- likelihood.initials( x.block )
        sliderInput( "sliderLocationLim", "Location sampling limits",
                    round( x.fit.gev$par[ 1 ], 1 ) - 10,
                    round( x.fit.gev$par[ 1 ] + 10, 1 ),
                    c( round( x.fit.gev$par[ 1 ], 1 ) - 5, round( x.fit.gev$par[ 1 ], 1 ) + 5 ) ) } )
    output$menuSliderScaleLim <- renderMenu( {
        x.fit.gev <- gev.fitting()
        if ( is.null( x.fit.gev ) )
            return( NULL )
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        par.init <- likelihood.initials( x.block )
        sliderInput( "sliderScaleLim", "Scale sampling limits",
                    round( max( 0, x.fit.gev$par[ 2 ]  - 10 ), 1 ),
                    round( x.fit.gev$par[ 2 ] + 10, 1 ),
                    c( round( max( 0, x.fit.gev$par[ 2 ] - 5 ), 1 ),
                      round( x.fit.gev$par[ 2 ], 1 ) + 5 ) ) } )
    output$menuSliderShapeLim <- renderMenu( {
        x.fit.gev <- gev.fitting()
        if ( is.null( x.fit.gev ) )
            return( NULL )
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        par.init <- likelihood.initials( x.block )
        sliderInput( "sliderShapeLim", "Shape sampling limits",
                    round( x.fit.gev$par[ 3 ] - 1, 1 ),
                    round( x.fit.gev$par[ 3 ] + 1, 1 ),
                    c( round( x.fit.gev$par[ 3 ], 1 ) - .3,
                      round( x.fit.gev$par[ 3 ], 1 ) + .3  ) ) } )
    ## To enable the user to input her/his own custom initialization points for the optimization
    ## it needs two things: three numerical inputs chosing the climex::likelihood.initials as
    ## default and a reactive vector gluing all together
    output$inputInitialLocation <- renderMenu( {
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        parameter.default <- climex::likelihood.initials( x.block )
        numericInput( "initialLocation", "", value = round( parameter.default[ 1 ], 4 ) ) } )
    output$inputInitialScale <- renderMenu( {
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        parameter.default <- climex::likelihood.initials( x.block )
        numericInput( "initialScale", "", value = round( parameter.default[ 2 ], 4 ), min = 0 ) } )
    output$inputInitialShape <- renderMenu( {
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        parameter.default <- climex::likelihood.initials( x.block )
        numericInput( "initialShape", "", value = round( parameter.default[ 3 ], 4 ) ) } )
    ##Why I need this reactive content? Well, not really necessary but quite convenient
    initial.parameters <- reactive( {
        if ( is.null( input$initialLocation ) | is.null( input$initialScale ) |
                 is.null( input$initialShape ) ){
            ## If the sliders arn't set yet, I will just use the default values
            x.block <- data.blocking()[[ 1 ]]
            if ( is.null( x.block ) ){
                ## if the initialization has not finished yet just wait a
                ## little longer
                return( NULL )
            }
            return( climex::likelihood.initials( x.block ) )
        } else
            return( c( input$initialLocation, input$initialScale, input$initialShape ) ) } )
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
        x.fit.gev <- gev.fitting()
        par.init <- x.fit.gev$par
        if ( is.null( input$sliderNumberInitialPoints ) || is.null( input$sliderLocationLim ) ||
                 is.null( input$sliderScaleLim ) || is.null( input$sliderShapeLim ) ){
            return( c( NaN, NaN, NaN ) )
        }
        ## the first entry of the table should always be the actual point the optimization
        ## is starting from
        table.init <- data.frame(
            location = c( x.initial[ 1 ],
                         round( stats::runif( ( input$sliderNumberInitialPoints - 1 ),
                                             input$sliderLocationLim[ 1 ],
                                             input$sliderLocationLim[ 2 ] ), 4 ) ),
            scale = c( x.initial[ 2 ],
                      round( stats::runif( ( input$sliderNumberInitialPoints - 1 ),
                                          input$sliderScaleLim[ 1 ],
                                          input$sliderScaleLim[ 2 ] ), 4 ) ),
            shape = c( x.initial[ 3 ],
                      round( stats::runif( ( input$sliderNumberInitialPoints - 1 ),
                                          input$sliderShapeLim[ 1 ],
                                          input$sliderShapeLim[ 2 ] ), 4 ) ) )
        ## but we only want to have starting points which do not result in a NA
        while ( any( is.nan( apply( table.init, 1, likelihood, x.in = x.block ) ) ) ){
            for ( ii in 1 : nrow( table.init ) ){
                if ( is.nan( likelihood( as.numeric( table.init[ ii, ] ),
                                        x.in = x.block ) ) )
                    table.init[ ii, ] <- c( round( stats::runif( 1, input$sliderLocationLim[ 1 ],
                                                         input$sliderLocationLim[ 2 ] ), 4 ),
                                           round( stats::runif( 1, input$sliderScaleLim[ 1 ],
                                                        input$sliderScaleLim[ 2 ] ), 4 ),
                                           round( stats::runif( 1, input$sliderShapeLim[ 1 ],
                                                        input$sliderShapeLim[ 2 ] ), 4 ) ) } }
        return( table.init ) } )
    output$tableInitialPoints <- renderDataTable( {
        initial.parameters <- initial.parameters.likelihood()
        if ( all( is.na( initial.parameters ) ) )
            return( NULL )
        initial.parameters$ID <- seq( 1, nrow( initial.parameters ) )
        ## round the number in the table
        return( initial.parameters ) },
        ## the drawCallback ensures that the width of the parent table is not set to a specific
        ## pixel number but to 100% percent. This ensures its correct rendering on mobile devices
        options = list( dom = 't', pageLength = 5,
                       drawCallback = I( "function( settings )
            {document.getElementById( 'tableInitialPoints' ).style.width = '100%';}") ) )
    ## Displaying of the heuristic estimates for a wiser picking of the limits
    output$tableHeuristicEstimates <- renderDataTable( {
        x.block <- data.blocking()[[ 1 ]]
        if ( is.null( x.block ) ){
            ## if the initialization has not finished yet just wait a
            ## little longer
            return( NULL )
        }
        x.fit.gev <- gev.fitting()
        x.mle.par <- x.fit.gev$par
        x.initial <- initial.parameters()
        x.suggested <- likelihood.initials( x.block )
        x.df <- data.frame( parameter = c( "fitting results", "suggested initials" ),
                           location = c( round( x.mle.par[ 1 ], 4 ), round( x.suggested[ 1 ], 4 ) ),
                           scale = c( round( x.mle.par[ 2 ], 4 ), round( x.suggested[ 2 ], 4 ) ),
                           shape = c( round( x.mle.par[ 3 ], 4 ), round( x.suggested[ 3 ], 4 ) ) )
        return( x.df ) },
        options = list( dom = 't',
                       drawCallback = I( "function( settings )
            {document.getElementById( 'tableHeuristicEstimates' ).style.width = '100%';}") ) )
    output$plotPlaceholder <- renderPlot({
        ttplot( x.block ) } )
    output$loadingImage <- renderUI({
        if ( session$clientData$url_hostname != "localhost" &&
             session$clientData$url_hostname != "127.0.0.1" ){
            folder <- "/assets/"
        } else
            folder <- ""
        return( img( src = paste0( folder, "loading.gif" ), id = "loadingGif" ) ) } )
    output$loadingScript <- renderUI({
        if ( session$clientData$url_hostname != "localhost" &&
             session$clientData$url_hostname != "127.0.0.1" ){
            folder <- "/assets/"
        } else
            folder <- ""
        return( div( tags$head( singleton(
            tags$script( src = paste0( folder, "/loadingGif.js"  ) ) ) ) ) ) } )
    output$drawLikelihoodAnimation <- renderUI( {
        ## This reactive content only depends on the action button because of
        ## the use of the isolate() functions        
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
            ## I use the plot "plotPlaceholder" to determine the width of the current box and adjust
            ## the pixel width of the png pictures
            session.width <- session$clientData$output_plotPlaceholder_width
            session.plot.width <- floor( session.width/ 3 )
            print( "starting animation......." )
            ## a temporary folder will be generate to harvest the images of the animation
            ## whenever the animation will be redone the old folder should be removed to avoid
            ## wasting memory of the server. Therefore the folder name has to be a global variable
            if ( !is.null( image.folder ) )
                unlink( image.folder, recursive = TRUE )
            ## if the shiny server is running on localhost it is run in the CLIMEX.PATH
            ## folder and the folder containing the images is constantly overwritten
            ## to prevent the script from occupying to much space. Due to a setwd in
            ## the climex() wrapper we are already in this folder
            if ( session$clientData$url_hostname == "localhost" ||
                 session$clientData$url_hostname == "127.0.0.1"  ){
                working.folder <- paste0( CLIMEX.PATH, "app/www" )
                ## in case of the local session the variable image.folder was already set in the
                ## wrapper climex::climex()
            } else {
                ## I decided to make the variable image.folder a global one because in this way
                ## the folder addressed with it can be deleted the next time this function is
                ## called
                working.folder <- "/srv/shiny-server/assets" 
                image.folder <<- paste0( "/srv/shiny-server/assets/tmp/images_",
                                        as.numeric ( as.POSIXct( lubridate::now() ) ) )
            }
            dir.create( image.folder, recursive = TRUE )
            climex:::animation.wrapper(
                time.series = x.block,
                starting.points = initial.parameters,
                location.lim = isolate( input$sliderLocationLim ),
                scale.lim = isolate( input$sliderScaleLim ),
                shape.lim = isolate( input$sliderShapeLim ),
                optimization.method = optimization.method,
                optimization.steps = isolate( input$sliderOptimizationSteps ),
                optimization.rerun = input$checkboxRerun,
                width = session.plot.width, height = 300, delay = 300,
                loopMode = "loop",
                image.folder = image.folder, working.folder = working.folder )
            ## if the code is not running on localhost the shiny server won't find
            ## the animation.js script using its absolute path
            if ( session$clientData$url_hostname != "localhost" &&
                 session$clientData$url_hostname != "127.0.0.1" ){
                working.folder <- sub( "/srv/shiny-server", "", working.folder )
                animation.script <- "/assets/"
            } else {
                working.folder <- ""
                animation.script <- ""
            }
            return( div( class = "scianimator", style = "display: inline-block;",
                        tags$script( src = paste0( animation.script, "jquery.scianimator.min.js" ) ),
                        tags$script( src = paste0( working.folder, "/animation.js" ) ),
                        div( id = "animationLocSc", class = "animationClimex" ),
                        div( id = "animationLocSh", class = "animationClimex" ),
                        div( id = "animationScSh", class = "animationClimex" ) ) )
        } ) } )
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
    ##
    ## This functions extracts all stations containing more than a
    ## specified number of years of data
    data.chosen <- reactive( {
        if ( is.null( input$selectDataBase ) || is.null( input$sliderMap ) )
            return( NULL )
        ## the generation of the artificial data is handled in the
        ## data.selection reactive function
        if ( input$selectDataBase == "DWD" ){
            if ( is.null( input$selectDataType ) )
                return( NULL )
            selection.list <- switch( input$selectDataType,
                                     "Daily max. temp." = stations.temp.max,
                                     "Daily min. temp." = stations.temp.min,
                                     "Daily precipitation" = stations.prec )
            ## to also cope the possibility of importing such position data
            positions.all <- station.positions
        } else if ( input$selectDataBase == "input" ){
            file.loading()
            if ( any( class( x.input ) == "xts" ) ){
                ## to assure compatibility
                aux <- list( x.input,
                            data.frame( longitude = NA, latitude = NA,
                                       altitude = NA, name = "1" ) )
                names( aux[[ 1 ]] ) <- c( "1" )
                ## adding a dummy name which is going to be displayed in
                ## the sidebar
                return( aux )
            } else {
                ## two cases are accepted here: a list containing stations xts
                ## time series of contain such a list and a data.frame specifying
                ## the stations positions
                if ( class( x.input ) == "list" &&
                                                   class( x.input[[ 1 ]] ) == "list" ){
                    selection.list <- x.input[[ 1 ]]
                    ## I will assume the second element of this list is a
                    ## data.frame containing the coordinated, height and name of
                    ## the individual stations
                    positions.all <- x.input[[ 2 ]]
                } else {
                    ## Just an ordinary list of xts elements
                    selection.list <- x.input
                    ##  dummy names
                    if ( is.null( names( selection.list ) ) )
                        names( selection.list ) <- as.character(
                                                   seq( 1, length( selection.list ) ) ) 
                    ## create a dummy
                    positions.all <- data.frame(
                        longitude = rep( NA, length( selection.list ) ),
                        latitude = rep( NA, length( selection.list ) ),
                        altitude = rep( NA, length( selection.list ) ),
                        name = names( selection.list ) )
                }
            }
        }
        ## select time series with sufficient length 
        selection <- Reduce( c, lapply( selection.list, function( x )
            length( unique( lubridate::year( x ) ) ) ) ) >= input$sliderMap
        stations.selected <- selection.list[ selection ]
        positions.selected <- positions.all[ selection,  ]
        ## first element contains a list of all selected stations
        ## second element contains a data.frame with the longitude, latitude,
        ## altitude and name of each selected station
        return( list( stations.selected, positions.selected ) )
    } )
    ## create custom markers.
    ## This is essentially the same marker but with different colors. The selected one
    ## should be colored red and all the others blue. 
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
    ## Create the underlying map containing the Openstreetmap tile. This is the
    ## fundamental layer and all the markers will be added on top of it.
    output$leafletMap <- renderLeaflet( {
        leaflet() %>% fitBounds( 5, 46, 13, 55 ) %>%
            addTiles( "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                     attribution = '<code> Kartendaten: © <a href="https://openstreetmap.org/copyright">OpenStreetMap</a>-Mitwirkende, SRTM | Kartendarstellung: © <a href="http://opentopomap.org">OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a> </code>' ) } )
    ## Depending on the number of minimal years and the selected data source markers
    ## will be placed at the geo-coordinates of the individual stations. 
    observe( {
        data.selected <- data.chosen()
        if ( !is.null( data.selected ) ){
            if ( any( is.na( c( data.selected[[ 2 ]]$longitude,
                               data.selected[[ 2 ]]$latitude ) ) ) ){
                ## I am dealing with either a placeholder or a compromised
                ## data.frame. Anyway, the leaflet map can not handle it
                return( NULL )
            }
            leafletProxy( "leafletMap" ) %>%
                clearGroup( "stations" ) %>%
                addMarkers( data = data.selected[[ 2 ]], group = "stations", lng = ~longitude,
                           icon = blue.icon, lat = ~latitude,
                           options = popupOptions( closeButton = FALSE ) )
        } } )
    observe( {
        ## the calculation of all the return levels of the stations just takes too long. I put it
        ## in a different observe object and the only way to start the calculation will be using a
        ## button
        if ( is.null( input$buttonDrawMarkers ) || input$buttonDrawMarkers < 1 )
            return( NULL )
        isolate( data.return.levels <- calculate.chosen.return.levels() )
        if ( !is.null( data.return.levels ) ){
            if ( any( is.na( c( data.return.levels$longitude,
                               data.return.levels$latitude ) ) ) ){
                ## I am dealing with either a placeholder or a compromised
                ## data.frame. Anyway, the leaflet map can not handle it
                return( NULL )
            }
            ## range of the return levels
            color.max <- max( data.return.levels$return.level )
            color.min <- min( data.return.levels$return.level )
            ## create a palette for the return levels of the individual circles
            palette <- colorNumeric( c( "navy", "skyblue", "limegreen", "yellow",
                                       "darkorange", "firebrick4" ), c( color.min, color.max ) )
            map.leaflet <- leafletProxy( "leafletMap" )
            map.leaflet <- clearGroup( map.leaflet, "returns" )
            map.leaflet <- addCircleMarkers(
                map.leaflet, data = data.return.levels,
                group = "returns", lng = ~longitude,
                color = ~palette( return.level ), lat = ~latitude,
                options = popupOptions( closeButton = FALSE ),
                fillOpacity = .8 )
            ## layer control to turn the return level layer on and off
            map.leaflet <- addLayersControl( map.leaflet, baseGroups = c( "stations", "returns" ),
                                            position = "bottomright",
                                            options = layersControlOptions( collapsed = FALSE ) )
            map.leaflet <- addLegend( map.leaflet, pal = palette, values = c( color.min, color.max ),
                                     title = "[years]", layerId = "leafletLegend",
                                     orientation = "horizontal" )
            return( map.leaflet )
        } } )
    output$tableMap <- renderTable( {
        data.selected <- data.chosen()
        if ( is.null( data.selected ) ||
                 is.null( input$leafletMap_marker_click) )
            return( NULL )
        map.click <- input$leafletMap_marker_click
        station.name <- as.character(
            data.selected[[ 2 ]]$name[ which( data.selected[[ 2 ]]$latitude %in% map.click$lat &
                                                                                     data.selected[[ 2 ]]$longitude %in% map.click$lng ) ] )
        leafletProxy( "leafletMap" ) %>%
            clearGroup( group = "selected" )
        leafletProxy( "leafletMap" ) %>%
            addMarkers( data = map.click, group = "selected", icon = red.icon, lng = ~lng, lat = ~lat )
        ## calculate the GEV fit and various return levels
        x.fit.gev <- gev.fitting()
        if ( is.null( x.fit.gev ) )
            return( NULL )
        if ( input$buttonMinMax == "Max" ){
            x.return.level <- climex:::return.level( x.fit.gev, return.period = c( 100, 50, 20 ) )
        } else
            x.return.level <- ( -1 )* climex:::return.level( x.fit.gev, return.period = c( 100, 50, 20 ) )
        ## paste0( "<b>", station.name, "</b>", "<br/>", "100y return level: ", x.return.level[ 1 ], "<br/>",
        ##        "50y return level: ", x.return.level[ 2 ], "<br/>", "20y return level: ", x.return.level[ 3 ] )
        x.df <- data.frame( names = c( "100y return level", "50y return level",
                                      "20y return level" ),
                           x.return.level, row.names = NULL )
        colnames( x.df ) <- c( station.name, "" )
        x.df
    }, rownames = FALSE, digits = 3, width = 220 )
}

##' @title The user interface for the \code{\link{climex}} function.
##'
##' @param selected Choose which tab is supposed to be selected when starting the app
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
            title = a( "Climex", href = "https://github.com/theGreatWhiteShark/climex",
                      id = "climexLink" )
        ),
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem( "Map", tabName = "tabMap",
                         icon = icon( "leaf", lib = "glyphicon" ),
                         selected = ifelse( selected == "Map",
                                           TRUE, FALSE ) ),
                menuItem( "General", tabName = "tabGeneral",
                         icon = icon( "stats", lib = "glyphicon" ),
                         selected = ifelse( selected == "General",
                                           TRUE, FALSE ) ),
                menuItem( "Likelihood", tabName = "tabLikelihood",
                         icon = icon( "ok-circle", lib = "glyphicon" ),
                         selected = ifelse( selected == "Likelihood",
                                           TRUE, FALSE ) ),
                menuItemOutput( "menuSelectDataBase" ),
                menuItemOutput( "menuSelectDataSource1" ),
                menuItemOutput( "menuSelectDataSource2" ),
                menuItemOutput( "menuSelectDataSource3" ),
                menuItemOutput( "menuDataCleaning" ),
                ## while plotting the images for the animation a gif should be shown
                div( uiOutput( "loadingScript" ), uiOutput( "loadingImage" ),
                    id = "loadingWrapper" ) ) ),
        body = dashboardBody(
            ## shinyjs::useShinyjs(),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/climex.css" ) ),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/reset.css" ) ),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/styles.css" ) ),
            includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                               "/css/scianimator.css" ) ),
            tabItems(
                tabItem(
                    tabName = "tabMap",
                    tags$style( type = "text/css", "#leafletMap {height: calc(100vh - 80px) !important;}" ),
                    leafletOutput( "leafletMap", width = "100%", height = 1000 ),
                    ## 50px is the thickness of the top navigation bar
                    absolutePanel( top = 50, right = 0, id = "leafletBox",
                                  sliderInput( "sliderMap", "Minimal length [years]",
                                              0, 155, value = 65, step = 1 ),
                                  tableOutput( "tableMap" ) ),
                    ## lift it a little but upwards so one can still see the card licensing
                    absolutePanel( bottom = 32, right = 0, id = "leafletMarkerBox",
                                  sliderInput( "sliderMapReturnLevel", "Return level [years]",
                                              30, 1000, value = 100 ),
                                  actionButton( "buttonDrawMarkers", "Calculate return levels" ) ) ),
                tabItem(
                    tabName = "tabGeneral",
                    ## In order guarantee the correct behavior of the rendering of the boxes and
                    ## tables for smaller screen sizes too I assign customized classes to the
                    ## boxes and reconfigure their ordering using CSS3 if the
                    ## max-width is below a certain threshold
                    fluidRow(
                        box( title = h2( "GEV fit" ),
                                    status = "primary", 
                                    solidheader = TRUE, width = 8,
                                    id = "boxPlotFit",
                                    column( 9, plotOutput( "plotFitGev" ) ),
                                    column( 3, plotOutput( "plotFitQQ", height = 140 ),
                                           plotOutput( "plotFitQQ2", height = 140 ),
                                           plotOutput( "plotFitReturnLevel",
                                                      height = 140 ) ) ),
                        box( title = h2( "Options" ), width = 4,
                                    height = 550, background = "orange",
                                    id = "boxGevStatistics",
                            radioButtons( "radioGevStatistics", label = NULL, inline = TRUE,
                                         choices = c( "Blocks", "Threshold" ), selected = "Blocks" ),
                            menuItemOutput( "sliderGevStatistics" ),
                            radioButtons( "buttonMinMax", "Type of extreme", inline = TRUE,
                                         choices = c( "Max", "Min" ), selected = "Max" ),
                            selectInput( "selectDeseasonalize", "Deseasonalization method",
                                        choices = c( "Anomalies", "stl", "decompose",
                                                    "deseasonalize::ds", "none" ),
                                        selected = "Anomalies" ),
                            selectInput( "selectOptimization", "Fitting routine",
                                        choices = c( "Nelder-Mead", "CG", "BFGS", "SANN",
                                                    "dfoptim::nmk" ),
                                        selected = c( "Nelder-Mead" ) ) ) ),
                    fluidRow(
                        box( title = h2( "Results" ), width = 3,
                            background = "orange", id = "boxGevResults",
                            uiOutput( "tableStatistics", colHeaders = "provided" ) ),
                        tabBox( title = h2( "Time series" ),
                               selected = "Remaining", width = 9,
                               id = "boxTimeSeries",
                               tabPanel( "Pure", dygraphOutput( "plotTimeSeries", height = 250 ) ),
                               tabPanel( "Deseasonalized", dygraphOutput( "plotDeseasonalized",
                                                                         height = 250 ) ),
                               tabPanel( "Remaining",
                                        plotOutput( "plotBlocked", height = 250,
                                                   click = "plotBlockedClick",
                                                   brush = brushOpts( id = "plotBlockedBrush" ) ),
                                        actionButton( "excludeBlockedReset", "Reset" ),
                                        actionButton( "excludeBlockedToggle", "Brush" ) ) ) ) ),
                tabItem( tabName = "tabLikelihood",
                        box( title = h2( "Starting points of the optimization routine" ),
                            width = 8, status = "primary",
                            id = "boxStartingPoints",
                            dataTableOutput( "tableInitialPoints" ),
                ## a plot of height 0? Well, its actually a very nice trick since I need
                ## a width value for the generated pngs in the animation in pixel. But
                ## I really want to make app to be rendered nicely on different screen
                ## sizes. Via the session$clientData I can access the width and height of
                ## plots. Thus I can access the width of this specific box via the
                ## plotPlaceholder without seeing it at all.
                            plotOutput( "plotPlaceholder", height = 0,
                                       width = '100%' ),
                            htmlOutput( "drawLikelihoodAnimation" ) ),
                        box( title = h2( "Options" ), width = 4,
                                    background = "orange", id = "boxHeuristic",
                                    dataTableOutput( "tableHeuristicEstimates" ),
                            div( p( "actual initials", id = "tableInitialDescription" ),
                                uiOutput( "inputInitialLocation" ),
                                uiOutput( "inputInitialScale" ), uiOutput( "inputInitialShape" ),
                                id = "initialTable" ),
                            checkboxInput( "checkboxRerun", "Rerun the optimization", value = TRUE ),
                            sliderInput( "sliderNumberInitialPoints",
                                        "Number of initial points", 1, 20, 5 ),
                            uiOutput( "menuSliderLocationLim" ),
                            uiOutput( "menuSliderScaleLim" ), uiOutput( "menuSliderShapeLim" ),
                            sliderInput( "sliderOptimizationSteps", "Which optimization steps", 0, 1,
                                        c( .1, .5 ) ),
                            actionButton( "buttonDrawAnimation", "Start animation" ),
                            actionButton( "tableDrawPoints", "Reset" ) ) ) ) ) )
}
