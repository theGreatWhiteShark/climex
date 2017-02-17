##' @title Displays the contour plots of the GEV likelihood function of a time series and the optimization routes for a bunch of provided initial points.
##'
##' @details Three orthogonal 2D plots are done for the negative log-likelihood of the GEV function intersecting in the actual result of the default optimization. Caution: An optimization only be displayed for optimization.method == 'nmk' and the trajectories will move out of the planes and so the precise position of the trajectory might be misleading. But the overall goal is to check for local minima. A bunch of images will be generated in the provided folder. Since the likelihood values cover quite some orders of magnitude they will be cut 1E3 above the minimal value. Also mind the differing height value: for a plot containing the legend (in this version of the script it is just the last one) the height value is increased to also cover the additional legend.
##'
##' @param time.series Vector of block maxima.
##' @param starting.points Data.frame of the starting points where each one is contained in the single row and the columns are spanned by location, scale and shape.
##' @param location.lim Region of the location parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot.
##' @param scale.lim Region of the scale parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot.
##' @param shape.lim Region of the shape parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot.
##' @param optimization.method For fitting the time.series using the provided
##' starting.points the fit.gev() function of this package will be used. This
##' parameter determines the 'method' argument. Caution: only for the 'nmk'
##' method all the updates and therefore an animation can be displayed. The
##' other methods from the stats::optim() function are not that straight forward
##' to modify since they link a lot of different libraries and I do not want to
##' make an R fork just to get the animation going. Default = 'nmk'.
##' @param optimization.steps Vector containing two numbers from 0 to 1 specifying the start and the end point of the optimization. Since the number of steps is unknown beforehand it will be chosen relativley to the total number of steps. Default = c( 0, 1 ).
##' @param optimization.rerun Flag deciding if to rerun the optimization at the
##' parameters determined by the first run. Default = TRUE.
##' @param height of both the images and the form with the playback options in pixel.
##' @param width of both the images and the form with the playback options in pixel.
##' @param model Whether to calculate the likelihood for the GEV or GP distribution. Default = "gev".
##' @param colors List of colors used to generate the plots.
##' @param image.folder where the generated pictures should be saved.
##'
##' @family shiny
##'
##' @import ggplot2
##' @import shiny
##' @return Opens a HTML widget showing the animation of the optimization routine.
##' @author Philipp Mueller 
plot.animation <- function( time.series, starting.points,
                           location.lim = NULL, scale.lim = NULL,
                           shape.lim = NULL, optimization.method = 'nmk',
                           optimization.steps = c( .1, .5 ),
                           optimization.rerun = TRUE,
                           height = 300, width = 300,
                           model = c( "gev", "gpd" ),
                           colors = list( plane.low = "#eaeafa",
                                         plane.high = "#191970",
                                         plane.contour = "white",
                                         path.low = "yellow",
                                         path.high = "darkred",
                                         path.true = "black" ),
                           image.folder = "images" ){
  ## conversion to ensure functionality
  if ( class( starting.points ) == "numeric" )
    starting.points <- data.frame( location = starting.points[ 1 ],
                                  scale = starting.points[ 2 ],
                                  shape = starting.points[ 3 ] )
  ## MLE estimates of the provided time.series
  time.series.par <- fit.gev( x = time.series )$par
  ## Initial parameters of the fitting routine
  time.series.initials <- likelihood.initials( time.series )
  ## the likelihood will be calculated along three planes spanning
  ## the 3D likelihood space and all intersecting each other in the
  ## resulting point of the original fit (time.series.par)
  number.of.points <- 100
  ## setting the limits of the GEV parameters if not provided yet
  if ( is.null( location.lim ) )
    location.lim <- c( time.series.par[ 1 ] - 5,
                      time.series.par[ 1 ] + 5 )
  if ( is.null( scale.lim ) )
    scale.lim <- c( max( 0, time.series.par[ 2 ] - 5 ),
                   time.series.par[ 2 ] + 5 )
  if ( is.null( shape.lim ) )
    shape.lim <- c( time.series.par[ 3 ] - .7,
                   time.series.par[ 3 ] + .7 )
  calculate.plane <- function( var1.lim, var2.lim, const,
                              const.position ){
    ## only likelihoods which are not plane.threshold values bigger
    ## than the minimum will be displayed
    threshold <- 1E3
    var1.range <- seq( var1.lim[ 1 ], var1.lim[ 2 ],, number.of.points )
    var2.range <- seq( var2.lim[ 1 ], var2.lim[ 2 ],, number.of.points )
    plane.aux <- expand.grid( var1.range, var2.range )
    if ( const.position == 1 ){
      plane <- data.frame( location = const, scale = plane.aux[ , 1 ],
                          shape = plane.aux[ , 2 ], row.names = NULL )
    } else if ( const.position == 2 ){
      plane <- data.frame( location = plane.aux[ , 1 ], scale = const,
                          shape = plane.aux[ , 2 ], row.names = NULL )
    } else {
      plane <- data.frame( location = plane.aux[ , 1 ],
                          scale = plane.aux[ , 2 ], shape = const,
                          row.names = NULL )
    }
    plane$likelihood <- plane$likelihood.lower <- .Call(
                            'likelihood_GEV', PACKAGE = 'climex',
                            plane, time.series )
    ## there will be a harder cut-off (lower) for the contour plot
    plane.threshold <- min( plane$likelihood, na.rm = TRUE ) + threshold
    plane$likelihood[ plane$likelihood > plane.threshold ] <-
      plane.threshold
    plane$likelihood.lower[ plane$likelihood.lower > plane.threshold ] <-
      plane.threshold
    return( plane )
  }
  plane.loc.sc <- calculate.plane( location.lim, scale.lim,
                                  time.series.par[ 3 ], 3 )
  plane.loc.sh <- calculate.plane( location.lim, shape.lim,
                                  time.series.par[ 2 ], 2 )
  plane.sc.sh <- calculate.plane( scale.lim, shape.lim,
                                 time.series.par[ 1 ], 1 )
  ## Every element of this list contains one trajetory of the
  ## optimization.
  suppressWarnings(
      trajectories <- apply( starting.points, 1, function( par ){
        climex::fit.gev( x = time.series, initial = as.numeric( par ),
                        error.estimation = "none",
                        rerun = optimization.rerun,
                        method = optimization.method )$updates } ) )
  ## Now there are two options: either there are the whole trajectories
  ## containing all the updates of the optimization
  ## (optimization.method = 'nmk' ) or just the beginning and end point
  ## in the parameter space. In the former case there has to be an
  ## animation. In the later one just a single image for each plane
  ## in the 3D parameter space.
  if ( optimization.method == "nmk" ){
    ## Choose the range in which the optimization is plotted
    step.max <- max( Reduce( rbind, trajectories )$step )
    step.begin <- round( optimization.steps[ 1 ]* step.max )
    if ( step.begin < ( nrow( starting.points ) + 1 ) )
      step.begin <- nrow( starting.points + 1 )
    step.end <- round( optimization.steps[ 2 ]* step.max )
    ## Extract the segments according to the provided optimization.steps
    list.segments <- lapply( trajectories, function( x ) {
      return( data.frame(
          location.start = x$location[ step.begin : ( step.end - 1 ) ],
          location.end = x$location[ ( step.begin + 1 ) : step.end ],
          scale.start = x$scale[ step.begin : ( step.end - 1 ) ],
          scale.end = x$scale[ ( step.begin + 1 ) : step.end ],
          shape.start = x$shape[ step.begin : ( step.end - 1 ) ],
          shape.end = x$shape[ ( step.begin + 1 ) : step.end ],
          step = x$step[ step.begin : ( step.end - 1 ) ] ) ) } )
    ## Plotting the trajectory by adding new layers to the plot
    segments.plot <- data.frame(
        path = Reduce( rbind, list.segments ),
        id = factor( Reduce(
            c, lapply( seq( 1, nrow( starting.points ) ), function( x )
              rep( x, nrow( list.segments[[ x ]] ) ) ) ) ) )
    ## it is not really useful to see individual trajectories
    ## disappearing. So the NA in segments.plot will be replaced by
    ## the last finite value.
    for ( ii in as.numeric( unique( segments.plot$id ) ) ){
      if ( any( Reduce(
          c, lapply( segments.plot[ segments.plot$id == ii, ],
                    is.na ) ) ) ){
        segments.values <- segments.plot[ segments.plot$id == ii, ]
        ## the following variable contains the content of the last
        ## row without any NA
        segments.last.values <- as.numeric( segments.plot[
            segments.plot$path.step == (
              max( segments.plot$path.step[ segments.plot$id == ii ],
                  na.rm = TRUE ) - 1 ) & segments.plot$id == ii &
              !is.na( segments.plot$path.step ), ] )
        ## filling all NA
        for ( rr in 1 : nrow( segments.values ) ){
          if ( any( is.na( segments.values[ rr, ] ) ) )
            segments.values[ rr, ] <- segments.last.values
        }
        ## but for the step number this not really makes any sense
        segments.values$path.step <- seq(
            min( segments.values$path.step ),
            nrow( segments.values) - 1 +
            min( segments.values$path.step ) )
        segments.plot[ segments.plot$id == ii, ] <- segments.values
      }
    }
  } else {
    ## just a arrow for each optimization route
    segments.plot <- Reduce( rbind, lapply( trajectories, function( x )
      data.frame( location.start = x$location[ 1 ],
                 location.end = x$location[ 2 ],
                 scale.start = x$scale[ 1 ],
                 scale.end = x$scale[ 2 ],
                 shape.start = x$shape[ 1 ],
                 shape.end = x$shape[ 2 ] ) ) )
    segments.plot$id <- factor( seq( 1, length( trajectories ) ) )
  }
  ## New approach: just displaying specific number of points every
  ## time with a opacity increasing with the time that pasted.
  plot.plane <- function( plane, col1, col2 ){
    ggplot() +
      geom_raster( data = plane, na.rm = TRUE,
                  aes_string( x = names( plane )[ col1 ],
                             y = names( plane )[ col2 ],
                             fill = "likelihood" ) ) +
      geom_contour( data = plane, colour = colors$plane.contour,
                   na.rm = TRUE, aes_string( x = names( plane[ col1 ] ),
                                            y = names( plane )[ col2 ],
                                            z = "likelihood.lower" ) ) +
      scale_fill_gradient2( low = colors$plane.low,
                           high = colors$plane.high,
                           na.value = "white", trans = "log",
                           label = function( x ) {
                             options( digits = 2 );
                             format( x, scientific = TRUE ) } ) +
      theme_bw() +
      theme( axis.title = element_text( size = 15, colour = "#191970" ),
            axis.text = element_text( size = 12, colour = "#191970" ),
            axis.line = element_line( colour = "#191970" ),
            panel.grid.major = element_line( colour = "#FFFFFF" ),
            panel.grid.minor = element_line( colour = "#FFFFFF" ),
            legend.title = element_text( size = 15, colour = "#191970" ),
            legend.text = element_text( size = 12, colour = "#191970" ),
            legend.position = "bottom", legend.direction = "horizontal"
            )
    return( last_plot() )
  } 
  ## plane.name gives an extension to the .png files identifying the
  ## 2D section of the likelihood space. col1 and col2 specify which
  ## of the dimensions should be taken. 1 = location, 2 = scale,
  ## 3 = shape
  plot.trajectories.nmk <- function( segments, gg.plane, plane.name,
                                    col1, col2, x.lim, y.lim,
                                    plot.legend = 0 ){
    ## just plot number.plot.points a time and increase the alpha
    ## value for points further in the past
    number.plot.points <- 5
    true.end <- data.frame( location = time.series.par[ 1 ],
                           scale = time.series.par[ 2 ],
                           shape = time.series.par[ 3 ] )
    ## The first trajectory belongs to the true starting points of
    ## the optimization and should have a distinct color
    color.points <- c( colors$path.true,
                      grDevices::colorRampPalette(
                                     colors = c( colors$path.low,
                                                colors$path.high ) )(
                                       nrow( starting.points ) - 1 ) )
    ## plotting of the individual png files containing the likelihood
    ## plane and a segment of the trajectory. Those files are going
    ## to be concatenated to the animation
    individual.plots <- function( segment, id, plot.legend = 0 ){
      ## if the legend should be printed as well the height value is
      ## increased by 15% to keep the symmetry while containing the
      ## additional legend
      if ( plot.legend > 0 ){
        height.plot <- height* 1.14
      } else
        height.plot <- height
      grDevices::png( filename = paste0( image.folder, "/plane_",
                                        plane.name, id, ".png" ),
                     width = width, height = height.plot )
      ## here I assume that the entries in segment are ordered
      ## according to their id
      gg.plot <- gg.plane +
        geom_segment(
            data = segment,
            aes_string( x = names( segment )[ col1* 2 - 1 ],
                       xend = names( segment)[ col1* 2 ],
                       alpha = "path.step",
                       y = names( segment )[ col2* 2 - 1 ],
                       yend = names( segment )[ col2* 2 ],
                       colour = "id" ),
            arrow = arrow( length = unit( 0.3, "cm" ) ) ) +
        ## for better highlighting of the positions
        geom_point( data = segment,
                   aes_string( x = names( segment )[ col1* 2 - 1 ],
                              alpha = "path.step",
                              y = names( segment )[ col2* 2 - 1 ],
                              colour = "id" ) ) +
        ## true end point
        geom_point( data = true.end, size = 2, shape = 21,
                   aes_string( x = names( true.end )[ col1 ],
                              y = names( true.end )[ col2 ] ) ) +
        xlim( x.lim ) + ylim( y.lim ) + scale_alpha( guide = FALSE ) +
        scale_colour_manual( values = color.points ) +
        theme( legend.box = "vertical", legend.box.just = "bottom" ) +
        guides( fill = guide_legend( title = "Likelihood",
                                    title.position = "top" ) )
      ## depending on the position there is a different legend shown
      ## or none (where the navigation tool resides)
      if ( plot.legend == 0 ){
        gg.plot <- gg.plot + theme( legend.position = "none" )
      } else if ( plot.legend == 1 ){
        gg.plot <- gg.plot + guides( fill = FALSE )
      } else if ( plot.legend == 2 )
        gg.plot <- gg.plot + guides( colour = FALSE )
      print( gg.plot )
      grDevices::dev.off()
      invisible()
    }
    ## iterate through the steps and just hand those rows over
    ## corresponding to a specific range of steps
    for ( ii in step.begin : ( step.end - number.plot.points + 1 ) ){
      print( paste( "likelihood.gui: plotting frame", ii, "of",
                   step.end - number.plot.points + 1  ) )
      individual.plots( segments[ ii <= segments$path.step &
                                  segments$path.step <= ( ii + 4 ), ],
                       ii, plot.legend )
    }
    invisible()
  }
  ## plane.name gives an extension to the .png files identifying the
  ## 2D section of the likelihood space. col1 and col2 specify which
  ## of the dimensions should be taken. 1 = location, 2 = scale,
  ## 3 = shape
  plot.trajectories.single <- function( segments, gg.plane,
                                       plane.name, col1, col2,
                                       x.lim, y.lim,
                                       plot.legend = 0 ){
    ## The first trajectory belongs to the true starting points of the
    ## optimization and should have a distinct color
    color.points <- c( colors$path.true,
                      grDevices::colorRampPalette(
                                     colors = c( colors$path.low,
                                                colors$path.high ) )(
                                       nrow( starting.points ) - 1 ) )
    ## if the legend should be printed as well the height value is
    ## increased by 15% to keep the symmetry while containing the
    ## additional legend
    if ( plot.legend > 0 ){
      height.plot <- height* 1.14
    } else
      height.plot <- height
    grDevices::png( filename = paste0( image.folder, "/plane_",
                                      plane.name, ".png" ),
                   width = width, height = height.plot )
    ## here I assume that the entries in segment are ordered according
    ## to their id
    gg.plot <- gg.plane +
      geom_segment(
          data = segments,
          aes_string( x = names( segments )[ col1* 2 - 1 ],
                     xend = names( segments )[ col1* 2 ], 
                     y = names( segments )[ col2* 2 - 1 ],
                     yend = names( segments )[ col2* 2 ],
                     colour = "id" ),
          arrow = arrow( length = unit( 0.3, "cm" ) ) ) +
      ## for better highlighting of the positions
      geom_point( data = segments, shape = 1, size = 3,
                 aes_string( x = names( segments )[ col1* 2 - 1 ],
                            y = names( segments )[ col2* 2 - 1 ],
                            colour = "id" ) ) +
      xlim( x.lim ) + ylim( y.lim ) +
      scale_colour_manual( values = color.points ) +
      theme( legend.box = "vertical", legend.box.just = "bottom" ) +
      guides( fill = guide_legend( title = "Likelihood",
                                  title.position = "top" ) )
    ## depending on the position there is a different legend shown
    ## or none (where the navigation tool resides)
    if ( plot.legend == 0 ){
      gg.plot <- gg.plot + theme( legend.position = "none" )
    } else if ( plot.legend == 1 ){
      gg.plot <- gg.plot + guides( fill = FALSE )
    } else if ( plot.legend == 2 )
      gg.plot <- gg.plot + guides( colour = FALSE )
    print( gg.plot )
    grDevices::dev.off()
    invisible()
  }
  ## actual plotting
  if ( optimization.method == "nmk" ){
    plot.trajectories.nmk( segments.plot,
                          plot.plane( plane.loc.sc, 1, 2 ), "loc.sc",
                          1, 2, x.lim = location.lim,
                          y.lim = scale.lim,
                          plot.legend = 1 )
    plot.trajectories.nmk( segments.plot,
                          plot.plane( plane.loc.sh, 1, 3 ), "loc.sh",
                          1, 3, x.lim = location.lim,
                          y.lim = shape.lim,
                          plot.legend = 0 )
    plot.trajectories.nmk( segments.plot,
                          plot.plane( plane.sc.sh, 2, 3 ), "sc.sh",
                          2, 3, x.lim = scale.lim,
                          y.lim = shape.lim,
                          plot.legend = 2 )
  } else {
    plot.trajectories.single( segments.plot,
                             plot.plane( plane.loc.sc, 1, 2 ), "loc.sc",
                             1, 2, x.lim = location.lim,
                             y.lim = scale.lim, plot.legend = 1 )
    plot.trajectories.single( segments.plot,
                             plot.plane( plane.loc.sh, 1, 3 ), "loc.sh",
                             1, 3, x.lim = location.lim,
                             y.lim = shape.lim, plot.legend = 0 )
    plot.trajectories.single( segments.plot,
                             plot.plane( plane.sc.sh, 2, 3 ), "sc.sh",
                             2, 3, x.lim = scale.lim,
                             y.lim = shape.lim, plot.legend = 2 )
  }
}

##' @title Help function filling the JavaScript template which produces the likelihood animation.
##' @details All the images as well as the JavaScript script will be produced in a folder in the /tmp/ directory, while the template will be extracted from the climex app system files. A huge thank towards Brent Ertz for his awesome scianimator jquery plugin \url{https://github.com/brentertz/scianimator} and Yihui Xie who's animation R package was the basis for this code. The warning messages "Removed x rows ..." appear since a couple of trajectories are outside the plotted area.
##'
##' @param time.series Blocked data for which the GEV fit is performed
##' @param starting.points of the optimization.
##' @param location.lim Range of the location parameter in which the likelihood surface will be calculated.
##' @param scale.lim Range of the scale parameter in which the likelihood surface will be calculated.
##' @param shape.lim Range of the shape parameter in which the likelihood surface will be calculated.
##' @param optimization.method For fitting the time.series using the provided
##' starting.points the fit.gev() function of this package will be used. This
##' parameter determines the 'method' argument. Caution: only for the 'nmk'
##' method all the updates and therefore an animation can be displayed. The
##' other methods from the stats::optim() function are not that straight forward
##' to modify since they link a lot of different libraries and I do not want to
##' make an R fork just to get the animation going. Default = 'nmk'.
##' @param optimization.steps Since the first updates contain a lot of jumps and the last ones almost no deviations in the GEV parameters the range of the trajectories can be limited.
##' @param optimization.rerun Flag deciding if to rerun the optimization at the
##' parameters determined by the first run. Default = TRUE.
##' @param height of both the images and the form with the playback options in pixel.
##' @param width of both the images and the form with the playback options in pixel.
##' @param model Whether to calculate the likelihood for the GEV or GP distribution. Default = "gev".
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
animation.wrapper <- function( time.series, starting.points,
                              location.lim, scale.lim, shape.lim,
                              optimization.method = "nmk",
                              optimization.steps,
                              optimization.rerun = TRUE,
                              height, width, model = c( "gev", "gpd" ),
                              delay = 300, loopMode = "loop",
                              image.folder, working.folder ){
  ## load the JavaScript template file
  template <- readLines( paste0( system.file( "climex_app",
                                             package = "climex" ),
                                "/js/template2.js" ) )
  ## Creating a new folder for the animation images
  ## if ( dir.exists(image.folder ) )
  ##     unlink( paste0( image.folder, "/" ), recursive = TRUE )
  ## dir.create( image.folder )
  ## create the images
  climex:::plot.animation( time.series, starting.points, location.lim,
                          scale.lim, shape.lim, optimization.method,
                          optimization.steps, height = height,
                          width = width, model = model,
                          image.folder = image.folder )
  ## get the image names including the folder name and write them
  ## in the JavaScript template
  files.all <- Reduce( c, lapply( list.files( image.folder ),
                                 function( x )
                                   paste0( image.folder, "/", x ) ) )
  ## I try to link the /tmp folder in the assets of the shiny-server
  ## to make the images accessible to the client
  if ( substr( working.folder, 1, 17 ) == "/srv/shiny-server" ){
    ## If the server is not running of localhost the client is not
    ## able to see the folders using the full path
    files.all <- sub( "/srv/shiny-server/assets/tmp", "/assets/tmp",
                     files.all )
  } else
    files.all <- sub( paste0( CLIMEX.PATH, "app/www/" ), "", files.all )
  template[ grep( "%imgLocSc", template ) ] <-
    sub( "%imgLocSc", paste0( "'", grep( "loc.sc", files.all,
                                        value = TRUE ), "'",
                             collapse = ", " ),
        template[ grep( "%imgLocSc", template ) ] )
  template[ grep( "%imgLocSh", template ) ] <-
    sub( "%imgLocSh", paste0( "'", grep( "loc.sh", files.all,
                                        value = TRUE ), "'",
                             collapse = ", " ),
        template[ grep( "%imgLocSh", template ) ] )
  template[ grep( "%imgScSh", template ) ] <-
    sub( "%imgScSh", paste0( "'", grep( "sc.sh", files.all,
                                       value = TRUE ), "'",
                            collapse = ", " ),
        template[ grep( "%imgScSh", template ) ] )
  ## write the other parameters to the template too
  ## So unfortunately the ggsave function does not accept width
  ## values of the unit "pixel" so I have to hard code this fellow
  ## here
  template[ grep( "%width", template ) ] <- sub(
      "%width", 300, template[ grep( "%width", template ) ] )
  template[ grep( "%delay", template ) ] <- sub(
      "%delay", delay, template[ grep( "%delay", template ) ] )
  template[ grep( "%loop", template ) ] <- sub(
      "%loop", loopMode, template[ grep( "%loop", template ) ] )
  ## write the results to a JavaScript file
  writeLines( template, con = paste0( working.folder, "/animation.js" ) )
  invisible( template )
}

#' @useDynLib climex
#' @importFrom Rcpp sourceCpp
NULL
