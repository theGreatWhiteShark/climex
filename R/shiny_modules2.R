##' @title Adds font color to a HTML table.
##'
##' @details In each element of the table where the font should be colored, the corresponding color has to be added via a paste( as.character( table[ x, y ] ), "#0000FF" ).
##'
##' @param x.html.table HTML table.
##' @param css.colours Character vector containing the colors in hex.
##' @param style Additional style tags for the output table.
##'
##' @seealso \code{\link{pander::pandoc.table.return}} and \code{\link{markdown::markdownToHTML}}
##'
##' @return Same format as input.
##' @author Philipp Mueller 
color.table <- function( x.html.table, css.colours, style = "table-condensed table-bordered" ){
    x.html.table.split <- stringr::str_split( x.html.table, "\n" )[[ 1 ]]
  ids <- paste0( "\"center\"><font color='", css.colours, "'>" )
  for ( ii in 1 : length( css.colours ) ){
      locations <- grep( css.colours[[ ii ]], x.html.table.split )
      x.html.table.split[ locations ] <- gsub( css.colours[ ii ], "</font>",
                                         x.html.table.split[ locations ], fixed = TRUE ) 
      x.html.table.split[ locations ] <- gsub( "\"center\">", ids[ ii ],
                                              x.html.table.split[ locations ] ) }
  x.html.table <- paste( x.html.table.split, collapse = "\n" )
  Encoding( x.html.table ) <- "UTF-8"
  return( list( htmltools::tags$script( sprintf( '$( "table" ).addClass( "table %s" );', style ) ),
               htmltools::HTML( x.html.table ) ) )
}

##' @title Displays the contour plots of the GEV likelihood function of a time series and the optimization routes for a bunch of provided initial points.
##'
##' @details Three orthogonal 2D plots are done for the negative log-likelihood of the GEV function intersecting in the actual result of the default optimization. Caution: the trajectories will move out of the planes and so the precise position of the trajectory might be misleading. But the overall goal is to check for local minima. A bunch of images will be generated in the provided folder. 
##'
##' @param time.series Vector of block maxima.
##' @param starting.points Data.frame of the starting points where each one is contained in the single row and the columns are spanned by location, scale and shape.
##' @param location.lim Region of the location parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot.
##' @param scale.lim Region of the scale parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot.
##' @param shape.lim Region of the shape parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot.
##' @param optimization.function Function use for preforming the GEV fit. This must be a function providing the list element "x.update" containing the parameters evaluated at each step of the optimization procedure and a list element "par" containing a vector of the GEV parameter estimates. For now only the function nmk.modified is available. Default = nmk.modified.
##' @param optimization.steps Vector containing two numbers from 0 to 1 specifying the start and the end point of the optimization. Since the number of steps is unknown beforehand it will be chosen relativley to the total number of steps. Default = c( 0, 1 ).
##' @param width of both the images and the form with the playback options in pixel.
##' @param height of both the images and the form with the playback options in pixel.
##' @param colors List of colors used to generate the plots.
##' @param image.folder where the generated pictures should be saved.
##'
##' @family shiny
##'
##' @import ggplot2
##' @import shiny
##' @importFrom animation ani.options
##' @importFrom animation saveHTML
##' @return Opens a HTML widget showing the animation of the optimization routine.
##' @author Philipp Mueller 
plot.animation <- function( time.series, starting.points, location.lim = NULL, scale.lim = NULL,
                           shape.lim = NULL, optimization.function = climex:::nmk.modified,
                           optimization.steps = c( .1, .5 ), height = 300, width = 300,
                           colors = list( plane.low = "skyblue1", plane.high = "#191970",
                                         plane.contour = "white", path.low = "yellow",
                                         path.high = "darkred", path.true = "black" ),
                           image.folder = "images" ){
    ## conversion to ensure functionality
    if ( class( starting.points ) == "numeric" )
        starting.points <- data.frame( location = starting.points[ 1 ], scale = starting.points[ 2 ],
                                      shape = starting.points[ 3 ] )
    ## MLE estimates of the provided time.series
    time.series.par <- gev.fit( x = time.series )$par
    ## Initial parameters of the fitting routine
    time.series.initials <- likelihood.initials( time.series )
    ## Calculation of the likelihood surface/space
    ## This is now replaced by the likelihood_GEV.cpp file in /src. inline was just not fast enough
    ## C++ function calculating the likelihood for every parameter combination
    ## calcNllh <- inline::cxxfunction( signature( parameters = "numeric", xin = "numeric" ),
    ##                                     plugin = "RcppArmadillo", body = '
    ## Rcpp::DataFrame parametersDataframe( parameters );
    ## Rcpp::NumericVector xVector( xin );
    ## Rcpp::NumericVector loc = parametersDataframe[ "location" ];
    ## Rcpp::NumericVector sc = parametersDataframe[ "scale" ];
    ## Rcpp::NumericVector sh = parametersDataframe[ "shape" ];
    ## Rcpp::NumericVector L(loc.size());
    ## Rcpp::NumericVector y( xVector.size() );
    ## for ( int ii = 0; ii < sc.size(); ii++ ){
    ##     y = 1 + ( ( xVector - loc[ii])/sc[ii] )*sh[ii];    
    ##     L[ii] =  log( sc[ii] )* xVector.size() + sum( pow( y, -1/sh[ii] ) ) + sum( log( y ) )*(1/sh[ii] + 1 ) ;
    ## }                                          
    ## return Rcpp::wrap( L ); ')
    ## the likelihood will be calculated along three planes spanning the 3D likelihood space
    ## and all intersecting each other in the resulting point of the original fit (time.series.par)
    number.of.points <- 100
    ## setting the limits of the GEV parameters if not provided yet
    if ( is.null( location.lim ) )
        location.lim <- c( time.series.par[ 1 ] - 5, time.series.par[ 1 ] + 5 )
    if ( is.null( scale.lim ) )
        scale.lim <- c( max( 0, time.series.par[ 2 ] - 5 ), time.series.par[ 2 ] + 5 )
    if ( is.null( shape.lim ) )
        shape.lim <- c( time.series.par[ 3 ] - .7, time.series.par[ 3 ] + .7 )
    calculate.plane <- function( var1.lim, var2.lim, const, const.position ){
        ## only likelihoods which are not plane.threshold values bigger than the minimum will
        ## be displayed
        threshold <- 1E3
        var1.range <- seq( var1.lim[ 1 ], var1.lim[ 2 ],, number.of.points )
        var2.range <- seq( var2.lim[ 1 ], var2.lim[ 2 ],, number.of.points )
        plane.aux <- expand.grid( var1.range, var2.range )
        if ( const.position == 1 ){
            plane <- data.frame( location = const, scale = plane.aux[ , 1 ], shape = plane.aux[ , 2 ],
                                row.names = NULL )
        } else if ( const.position == 2 ){
            plane <- data.frame( location = plane.aux[ , 1 ], scale = const, shape = plane.aux[ , 2 ],
                                row.names = NULL )
        } else {
            plane <- data.frame( location = plane.aux[ , 1 ], scale = plane.aux[ , 2 ], shape = const,
                                row.names = NULL )
        }
        plane$likelihood <- plane$likelihood.lower <- .Call( 'likelihood_GEV', PACKAGE = 'climex',
                                                            plane, time.series )
        ## there will be a harder cut-off (lower) for the contour plot
        plane.threshold <- min( plane$likelihood, na.rm = TRUE ) + threshold
        plane$likelihood[ plane$likelihood > plane.threshold ] <- plane.threshold
        plane$likelihood.lower[ plane$likelihood.lower > plane.threshold ] <- plane.threshold
        return( plane )
    }
    plane.loc.sc <- calculate.plane( location.lim, scale.lim, time.series.par[ 3 ], 3 )
    plane.loc.sh <- calculate.plane( location.lim, shape.lim, time.series.par[ 2 ], 2 )
    plane.sc.sh <- calculate.plane( scale.lim, shape.lim, time.series.par[ 1 ], 1 )
    ## Every element of this list contains one trajetory of the optimization.
    suppressWarnings(
        trajectories <- apply( starting.points, 1, function( par ){
            optimization.function( par = as.numeric( par ), x = time.series )$x.updates } ) )
    ## Choose the range in which the optimization is plotted
    step.max <- max( Reduce( rbind, trajectories )$step )
    step.begin <- round( optimization.steps[ 1 ]* step.max )
    if ( step.begin < ( nrow( starting.points ) + 1 ) )
        step.begin <- nrow( starting.points + 1 )
    step.end <- round( optimization.steps[ 2 ]* step.max )
    ## Extract the segments according to the provided optimization.steps
    list.segments <- lapply( trajectories, function( x ) {
        return( data.frame( location.start = x$location[ step.begin : ( step.end - 1 ) ],
                           location.end = x$location[ ( step.begin + 1 ) : step.end ],
                           scale.start = x$scale[ step.begin : ( step.end - 1 ) ],
                           scale.end = x$scale[ ( step.begin + 1 ) : step.end ],
                           shape.start = x$shape[ step.begin : ( step.end - 1 ) ],
                           shape.end = x$shape[ ( step.begin + 1 ) : step.end ],
                           step = x$step[ step.begin : ( step.end - 1 ) ] ) ) } )
    ## Plotting the trajectory by adding new layers to the plot
    segments.plot <- data.frame(
        path = Reduce( rbind, list.segments ),
        id = factor( Reduce( c, lapply( seq( 1, nrow( starting.points ) ), function( x )
            rep( x, nrow( list.segments[[ x ]] ) ) ) ) ) )
    ## New approach: just displaying specific number of points every time with a opacity
    ## increasing with the time that pasted.
    plot.plane <- function( plane, col1, col2 ){
        ggplot() + geom_raster( data = plane, na.rm = TRUE,
                               aes_string( x = names( plane )[ col1 ], y = names( plane )[ col2 ],
                                          fill = "likelihood" ) ) +
            geom_contour( data = plane, colour = colors$plane.contour, na.rm = TRUE,
                         aes_string( x = names( plane[ col1 ] ), y = names( plane )[ col2 ],
                                    z = "likelihood.lower" ) ) +
            scale_fill_gradient2( low = colors$plane.low, high = colors$plane.high,
                                   na.value = "white", trans = "log" )
        return( last_plot() )
    } 
    ## plane.name gives an extension to the .png files identifying the 2D section of the likelihood
    ## space. col1 and col2 specify which of the dimensions should be taken. 1 = location,
    ## 2 = scale, 3 = shape
    plot.trajectories <- function( segments, gg.plane, plane.name, col1, col2, x.lim, y.lim ){
        ## just plot number.plot.points a time and increase the alpha value for points further in
        ## the past
        number.plot.points <- 5
        true.end <- data.frame( location = time.series.par[ 1 ], scale = time.series.par[ 2 ],
                               shape = time.series.par[ 3 ] )
        ## The first trajectory belongs to the true starting points of the optimization and
        ## should have a distinct color
        color.points <- c( colors$path.true,
                          colorRampPalette( colors = c( colors$path.low, colors$path.high ) )(
            nrow( starting.points ) - 1 ) )
        ## plotting of the individual png files containing the likelihood plane and a segment
        ## of the trajectory. Those files are going to be concatenated to the animation
        individual.plots <- function( segment, id ){
            png( filename = paste0( image.folder, "/plane_", plane.name, id, ".png" ),
                width = width, height = height )
            ## here I assume that the entries in segment are ordered according to their id
            gg.plot <- gg.plane + geom_segment(
                                      data = segment,
                                      aes_string( x = names( segment )[ col1* 2 - 1 ],
                                                 xend = names( segment)[ col1* 2 ], alpha = "path.step",
                                                 y = names( segment )[ col2* 2 - 1 ],
                                                 yend = names( segment )[ col2* 2 ], colour = "id" ),
                                      arrow = arrow( length = unit( 0.3, "cm" ) ) ) +
                ## for better highlighting of the positions
                geom_point( data = segment,
                           aes_string( x = names( segment )[ col1* 2 - 1 ], alpha = "path.step",
                                      y = names( segment )[ col2* 2 - 1 ], colour = "id" ) ) +
                ## true end point
                geom_point( data = true.end, size = 2, shape = 21,
                           aes_string( x = names( true.end )[ col1 ],
                                      y = names( true.end )[ col2 ] ) ) +
                xlim( x.lim ) + ylim( y.lim ) + scale_alpha( guide = FALSE ) +
                scale_colour_manual( values = color.points ) + theme_bw() +
                guides( fill = guide_legend( title = "nllh" ) )
                ## ggsave( filename = paste0( folder, "/plane_", plane.name, id, ".png" ),
            ##        device = "png", width = width, height = height, units = "cm", dpi = 500 )
            print( gg.plot )
            dev.off()
            invisible()
        }
        ## iterate through the steps and just hand those rows over corresponding to a
        ## specific range of steps
        for ( ii in step.begin : ( step.end - number.plot.points + 1 ) ){
            print( paste( "likelihood.gui: plotting frame", ii, "of",
                         step.end - number.plot.points + 1  ) )
            individual.plots( segments[ ii <= segments$path.step & segments$path.step <= ii + 4, ], ii )
        }
        invisible()
    }
    ## actual plotting
    plot.trajectories( segments.plot, plot.plane( plane.loc.sc, 1, 2 ), "loc.sc", 1, 2,
                      x.lim = location.lim, y.lim = scale.lim )
    plot.trajectories( segments.plot, plot.plane( plane.loc.sh, 1, 3 ), "loc.sh", 1, 3,
                      x.lim = location.lim, y.lim = shape.lim )
    plot.trajectories( segments.plot, plot.plane( plane.sc.sh, 2, 3 ), "sc.sh", 2, 3,
                      x.lim = scale.lim, y.lim = shape.lim )
}

##' @title Help function filling the JavaScript template which produces the likelihood animation.
##' @details All the images as well as the JavaScript script will be produced in a folder in the /tmp/ directory, while the template will be extracted from the climex app system files. A huge thank towards Brent Ertz for his awesome scianimator jquery plugin \url{https://github.com/brentertz/scianimator} and Yihui Xie who's animation R package was the basis for this code.
##'
##' @param time.series Blocked data for which the GEV fit is performed
##' @param starting.points of the optimization.
##' @param location.lim Range of the location parameter in which the likelihood surface will be calculated.
##' @param scale.lim Range of the scale parameter in which the likelihood surface will be calculated.
##' @param shape.lim Range of the shape parameter in which the likelihood surface will be calculated.
##' @param optimization.function For now only a internal hack of the dfoptim::nmk function is available. This is not really pleasant since it is not the original function of the optimization. I should expand this soon.
##' @param optimization.steps Since the first updates contain a lot of jumps and the last ones almost no deviations in the GEV parameters the range of the trajectories can be limited.
##' @param width of both the images and the form with the playback options in pixel.
##' @param height of both the images and the form with the playback options in pixel.
##' @param delay Time for which the individual pictures stay visible during the animation in ms. Default = 300.
##' @param loopMode Scianimator parameter declaring what will happen after a full run through all the images. Default = "loop".
##' @param image.folder where the generated pictures should be saved.
##' @param working.folder the overall folder containing the generated pictures, JavaScript files etc.
##'
##' @family shiny
##'
##' @return An invisible copy of the generated JavaScript file.
##' @seealso \code{\link{plot.animation}}
##' @author Philipp Mueller 
animation.wrapper <- function( time.series, starting.points, location.lim, scale.lim, shape.lim,
                  optimization.function = climex:::nmk.modified, optimization.steps,
                  width, height, delay = 300, loopMode = "loop", image.folder, working.folder ){
    ## load the JavaScript template file
    template <- readLines( paste0( system.file( "climex_app", package = "climex" ),
                                  "/js/template2.js" ) )
    ## Creating a new folder for the animation images
    ## if ( dir.exists(image.folder ) )
    ##     unlink( paste0( image.folder, "/" ), recursive = TRUE )
    ## dir.create( image.folder )
    ## create the images
    plot.animation( time.series, starting.points, location.lim, scale.lim, shape.lim,
                   optimization.function, optimization.steps, height, width, image.folder = image.folder )
    ## get the image names including the folder name and write them in the JavaScript template
    files.all <- Reduce( c, lapply( list.files( image.folder ), function( x )
        paste0( image.folder, "/", x ) ) )
    ## I try to link the /tmp folder in the assets of the shiny-server to make the images
    ## accessible to the client
    if ( substr( working.folder, 1, 17 ) == "/srv/shiny-server" ){
        ## If the server is not running of localhost the client is not able to see the folders using
        ## the full path
        files.all <- sub( "/srv/shiny-server/assets/tmp", "/assets/tmp", files.all )
    } else
        files.all <- sub( CLIMEX.PATH, "", files.all )
    template[ grep( "%imgLocSc", template ) ] <-
        sub( "%imgLocSc", paste0( "'", grep( "loc.sc", files.all, value = TRUE ), "'",
                                 collapse = ", " ), template[ grep( "%imgLocSc", template ) ] )
    template[ grep( "%imgLocSh", template ) ] <-
        sub( "%imgLocSh", paste0( "'", grep( "loc.sh", files.all, value = TRUE ), "'",
                                 collapse = ", " ), template[ grep( "%imgLocSh", template ) ] )
    template[ grep( "%imgScSh", template ) ] <-
        sub( "%imgScSh", paste0( "'", grep( "sc.sh", files.all, value = TRUE ), "'",
                               collapse = ", " ), template[ grep( "%imgScSh", template ) ] )
    ## write the other parameters to the template too
    ## So unfortunately the ggsave function does not accept width values of the unit "pixel" so
    ## I have to hard code this fellow here
    template[ grep( "%width", template ) ] <- sub( "%width", 300,
                                                  template[ grep( "%width", template ) ] )
    template[ grep( "%delay", template ) ] <- sub( "%delay", delay,
                                                  template[ grep( "%delay", template ) ] )
    template[ grep( "%loop", template ) ] <- sub( "%loop", loopMode,
                                                 template[ grep( "%loop", template ) ] )
    ## write the results to a JavaScript file
    writeLines( template, con = paste0( working.folder, "/animation.js" ) )
    print( "I'm here right now" )
    print( getwd() )
    invisible( template )
}

#' @useDynLib climex
#' @importFrom Rcpp sourceCpp
NULL
