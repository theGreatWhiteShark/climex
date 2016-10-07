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

##' @title Displays a contour plot of the likelihood function of a time series and the optimization routes for a bunch of provided initial points.
##'
##' @details A 2D plot is done in the scale and in the shape plane of the negative log-likelihood. The GEV fit is done by the chosen optimization routine and the plane of the optimal location parameter is chosen for the plot. If any other should be used instead use 'location.offset'. But the trajectories will move out of this plane and so the contour plot is not valid anymore. This function will produce a folder "images" containing the individual images of the optimization. Also a html file containing the animation will be produced. The random initial points are plotted colourful and the heuristic estimate of the starting point will always be plotted in black as well. If the number of starting.points is reduced to 1 another mode of visualization will be triggered. Then the likelihood plane will be updated in every step. So one is able to determine the influence of the location parameter on the fit as well.
##'
##' @param time.series Vector of block maxima.
##' @param starting.points Data.frame of the starting points where each one is contained in the single row and the columns are spanned by location, scale and shape. If NULL 7 random points are drawn randomly in the region spanned by scale.lim and shape.lim with the parameter value of the location plane. Default = NULL
##' @param scale.lim Region of the scale parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot. If NULL the MLE estimate using the time series will be calculated an the limits will be set to +/- 5 this value. Default = NULL.
##' @param shape.lim Region of the shape parameter for which the likelihood function is going to be evaluated and plotted as a density plot and a contour plot. If NULL the MLE estimate using the time series will be calculated an the limits will be set to +/- 5 this value. Default = NULL.
##' @param optimization.function Function use for preforming the GEV fit. This must be a function providing the list element "x.update" containing the parameters evaluated at each step of the optimization procedure and a list element "par" containing a vector of the GEV parameter estimates. For now only the function nmk.modified is available. Default = nmk.modified.
##' @param location.offset Use only if another location plane should be drawn. Default = NULL
##' @param optimization.steps Vector containing two numbers from 0 to 1 specifying the start and the end point of the optimization. Since the number of steps is unknown beforehand it will be chosen relativley to the total number of steps. Default = c( 0, 1 ).
##' @param likelihood.minimum The true minimum of the likelihood function. This should be supplied if gev.fit( x ) doesn't find the optimum in the first run. Default = NULL.
##'
##' @family shiny
##'
##' @import ggplot2
##' @import shiny
##' @importFrom animation ani.options
##' @importFrom animation saveHTML
##' @return Opens a HTML widget showing the animation of the optimization routine.
##' @author Philipp Mueller 
likelihood.gui <- function( time.series, starting.points = NULL, scale.lim = NULL, shape.lim = NULL,
                              location.offset = NULL, optimization.function = nmk.modified,
                              optimization.steps = c( .1, .5 ), likelihood.minimum = NULL ){
    ## conversion to ensure functionality
    if ( class( starting.points ) == "numeric" )
        starting.points <- data.frame( location = starting.points[ 1 ], scale = starting.points[ 2 ],
                                      shape = starting.points[ 3 ] )
    ## MLE estimates of the provided time.series
    if ( is.null( likelihood.minimum ) ){
        time.series.par <- gev.fit( x = time.series )$par
    } else
        time.series.par <- likelihood.minimum
    ## Initial parameters of the fitting routine
    time.series.initials <- likelihood.initials( time.series )
    ## Calculation of the likelihood surface/space
    ## Inspired by the likelihood.plot function but redone
    ## C++ function calculating the likelihood for every parameter combination
    calcNllh <- inline::cxxfunction( signature( parameters = "numeric", xin = "numeric" ),
                                    plugin = "RcppArmadillo", body = '
Rcpp::DataFrame parametersDataframe( parameters );
Rcpp::NumericVector xVector( xin );
Rcpp::NumericVector loc = parametersDataframe[ "location" ];
Rcpp::NumericVector sc = parametersDataframe[ "scale" ];
Rcpp::NumericVector sh = parametersDataframe[ "shape" ];
Rcpp::NumericVector L(loc.size());
Rcpp::NumericVector y( xVector.size() );
for ( int ii = 0; ii < sc.size(); ii++ ){
    y = 1 + ( ( xVector - loc[ii])/sc[ii] )*sh[ii];    
    L[ii] =  log( sc[ii] )* xVector.size() + sum( pow( y, -1/sh[ii] ) ) + sum( log( y ) )*(1/sh[ii] + 1 ) ;
}                                          
return Rcpp::wrap( L );')
    ## 2D likelihood
    ## some more point because its just the plane this time
    number.of.points <- 200
    ## setting the limits of the scale and shape parameter if not provided yet
    if ( is.null( scale.lim ) )
        scale.lim <- c( time.series.par[ 2 ] - 5, time.series.par[ 2 ] + 5 )
    if ( scale.lim[ 1 ] < 0 )
        scale.lim[ 1 ] <- 0
    if ( is.null( shape.lim ) )
        shape.lim <- c( time.series.par[ 3 ] - .7, time.series.par[ 3 ] + .7 )
    scale.range <- seq( scale.lim[ 1 ], scale.lim[ 2 ],, number.of.points )
    shape.range <- seq( shape.lim[ 1 ], shape.lim[ 2 ],, number.of.points )
    par.plane <- expand.grid( scale.range, shape.range )
    names( par.plane ) <- c( "scale", "shape" )
    par.plane$location <- rep( time.series.par[ 1 ], nrow( par.plane ) )
    likelihood.plane <- calcNllh( par.plane, time.series )
    ## Maybe introducing a cutoff in the likelihood values make the contour plot work again
    ## In here there was a bug. Sometimes (bigger negative shape parameter) the
    ## neg.log.likelihood is going negative. Therefore no point is below max*2
    min.likelihood <- min( likelihood.plane, na.rm = TRUE )
    if ( min.likelihood < 0 ){
        max.likelihood <- min.likelihood/ 3
        max.likelihood.low <- min.likelihood/ 2
    } else {
        max.likelihood <- min.likelihood* 3
        max.likelihood.low <- min.likelihood* 2
    }
    par.plane$likelihood <- par.plane$likelihood.low <- par.plane$likelihood.full <-
        likelihood.plane
    par.plane$likelihood[ par.plane$likelihood > max.likelihood ] <- max.likelihood
    ## A lower one for the contour plot
    par.plane$likelihood.low[ par.plane$likelihood.low > max.likelihood.low ] <- max.likelihood.low
    gg.plane <-
        ggplot() + geom_raster( data = par.plane, aes( x = scale, y = shape, fill = likelihood ),
                               na.rm = TRUE ) +
        geom_contour( data = par.plane, aes( x = scale, y = shape, z = likelihood.low ),
                     colour = "white", na.rm = TRUE )
    ## calculation of the starting points
    if ( is.null( starting.points ) ){
        ## setting up the fixed (location) parameter of the plotted plane
        number.examples.plane <- 7
        if ( is.null( location.offset ) )
            location.offset <- time.series.par[ 1 ]            
        starting.points <- data.frame( location = rep( location.offset, number.examples.plane ),
                                      scale = runif( number.examples.plane,
                                                    scale.lim[ 1 ], scale.lim[ 2 ] ),
                                      shape = runif( number.examples.plane,
                                                    shape.lim[ 1 ], shape.lim[ 2 ] ) )
        ## Check that likelihood functions can be evaluated at all the initial points
        for ( ee in 1 : nrow( starting.points ) ){
            while ( is.na( likelihood( as.numeric( starting.points[ ee, ] ), time.series ) ) )
                starting.points[ ee, ] <- c( location.offset,
                                            runif( 1, scale.lim[ 1 ], scale.lim[ 2 ] ),
                                            runif( 1, shape.lim[ 1 ], shape.lim[ 2 ]  ) ) } }
    ## Adding the heuristic initial estimate at the last position. But only when more than
    ## one point is supplied
    if ( nrow( starting.points ) > 1 ){
        starting.points[ nrow( starting.points ) + 1, ] <- likelihood.initials( time.series )
        type.last <- factor( nrow( starting.points ) ) }
    ## Every element of this list contains one trajetory of the optimization.
    examples.trajectories <- list()
    for ( ee in 1 : nrow( starting.points ) )
        suppressWarnings( examples.trajectories[[ ee ]] <- optimization.function(
                              par = as.numeric( starting.points[ ee, ] ),
                              x = time.series )$x.updates )
    ## Plotting the trajectory by adding new layers to the plot
    trajectories <- Reduce( rbind, examples.trajectories )
    trajectories$types <- rep( 0, nrow( trajectories ) )
    counter <- 1
    for ( ee in 1 : length( examples.trajectories ) ){
        trajectories$type[ counter : ( counter + nrow( examples.trajectories[[ ee ]] ) - 1 ) ] <- ee
        counter <- counter + nrow( examples.trajectories[[ ee ]] )
    }
    ## If the location parameter varies more than 10% with respect to the parameter of the plane
    ## the point and the segment are printed red and not orange. If fill == 0 plot orange
    trajectories$fill <- rep( 0, nrow( trajectories ) )
    trajectories$fill[ abs( trajectories$location - time.series.par[ 1 ] ) >=
                       time.series.par[ 1 ]* 0.1 ] <- 1
    trajectories$fill <- factor( trajectories$fill )
    trajectories$type <- factor( trajectories$type )
    trajectories$scale.begin <- c( NA, trajectories$scale[ 1 : ( nrow( trajectories ) - 1 ) ] )
    trajectories$shape.begin <- c( NA, trajectories$shape[ 1 : ( nrow( trajectories ) - 1 ) ] )
    ## Choose the range in which the optimization is plotted
    step.max <- max( trajectories$step )
    step.begin <- round( optimization.steps[ 1 ]* step.max )
    if ( step.begin < ( nrow( starting.points ) + 1 ) )
        step.begin <- nrow( starting.points + 1 )
    step.end <- round( optimization.steps[ 2 ]* step.max )
    ## New approach: just displaying specific number of points every time with a opacity
    ## increasing with the time that pasted.
    plot.trajectories <- function( trajectories, gg.plane ){
        number.plot.points <- 5
        for ( ii in step.begin : step.end ){
            segment <- data.frame( x = 0, xend = 0, y = 0, yend = 0, fill = 0, type = 0, step = 0 )
            counter.segment <- 1
            for ( ss in ( ii - number.plot.points ) : ii ){
                length.additional <- length( trajectories$scale.begin[ trajectories$step == ss ] )
                segment[ ( nrow( segment ) + 1 ) : ( nrow( segment ) + length.additional ), ] <-
                    cbind( trajectories$scale.begin[ trajectories$step == ss ],
                          trajectories$scale[ trajectories$step == ss ],
                          trajectories$shape.begin[ trajectories$step == ss ],
                          trajectories$shape[ trajectories$step == ss ],
                          trajectories$fill[ trajectories$step == ss ],
                          trajectories$type[ trajectories$step == ss ],
                          rep( counter.segment, length.additional ) )
                counter.segment <- counter.segment + 1
            }
            print( paste( "likelihood.gui: plotting frame", ii, "of", step.end ) )
            ## Subset the data. All the random initial points have different colours but the
            ## heuristic estimate for the time.series is plotted always in black
            segment <- segment[ -1, ] #getting rid of the first row containing only artifacts
            if ( nrow( segment[ segment$type != type.last, ] ) != 0 ){
                ggseg.random <- gg.plane +
                    geom_segment( data = segment[ segment$type != type.last, ],
                                 aes( x = x, xend = xend, alpha = step, yend = yend,
                                     colour = type, y = y),
                                 arrow = arrow( length = unit( 0.3, "cm" ) ) ) +
                    geom_point( data = segment[ segment$type != type.last, ], size = 2, shape = 21,
                               aes( x = xend, y = yend, colour = type, shape = fill, alpha = step ) )
            } else
                ggseg.random <- gg.plane
            if ( nrow(  segment[ segment$type == type.last, ] ) != 0 ){
                ggseg.heuristic <- ggseg.random +
                    geom_segment( data = segment[ segment$type == type.last, ], colour = "black",
                                 aes( x = x, xend = xend, alpha = step, yend = yend, y = y ), 
                                                arrow = arrow( length = unit( 0.3, "cm" ) ) ) +
                    geom_point( data = segment[ segment$type == type.last, ],
                               aes( x = xend, y = yend, shape = fill, alpha = step ),
                               size = 2, shape = 21, colour = "black" )
            } else
                ggseg.heuristic <- ggseg.random
            suppressWarnings(
                gg.plot <- ggseg.heuristic + xlim( scale.lim ) + ylim( shape.lim ) +
                scale_shape_manual( labels = c( TeX( "minor $\\Delta\\mu$" ),
                                               TeX( "major $\\Delta\\mu$" ) ), values = c( 1, 4 ) ) +
                scale_colour_gradientn( colours = rev( RColorBrewer::brewer.pal( 7, "Blues" ) ),
                                 na.value = "white", trans = "log" ) +
                scale_alpha( guide = FALSE ) )
            
            suppressWarnings( print( gg.plot ) )
        }
        return( last_plot() )
    }
    ## Second version for following one single trajectory and replotting and recalculating the
    ## likelihood plane in every step
    plot.follow.trajectory <- function( trajectories, scale.lim, shape.lim, time.series ){
        number.of.points <- 200
        scale.range <- seq( scale.lim[ 1 ], scale.lim[ 2 ],, number.of.points )
        shape.range <- seq( shape.lim[ 1 ], shape.lim[ 2 ],, number.of.points )
        par.plane <- expand.grid( scale.range, shape.range )
        names( par.plane ) <- c( "scale", "shape" )
        par.plane$location <- rep( time.series.par[ 1 ], nrow( par.plane ) )            
        likelihood.plane.mle <- calcNllh( par.plane, time.series )
        ## if there are negative parts of the likelihood, introduce an offset so its still
        ## possible to plot it in a logarithmic way.
        ## Maybe introducing a cutoff in the likelihood values make the contour plot work again
        ## This is set for all the optimization steps to provied a more uniform picture
        min.likelihood <- min( likelihood.plane.mle, na.rm = TRUE )
        max.likelihood <- 1E8
        ## if there are negative parts of the likelihood, introduce an offset so its still
        ## possible to plot it in a logarithmic way.
        if ( min.likelihood < 0 ){
            max.likelihood.low <- -min.likelihood* 2
            likelihood.plane.mle <- likelihood.plane.mle - min.likelihood + 1
        } else
            max.likelihood.low <- min.likelihood* 2
        par.plane$likelihood <- par.plane$likelihood.low <- likelihood.plane.mle
        par.plane$likelihood[ par.plane$likelihood > max.likelihood ] <- max.likelihood
        ## A lower one for the contour plot
        par.plane$likelihood.low[ par.plane$likelihood.low > max.likelihood.low ] <-
            max.likelihood.low
        number.of.displayed.steps <- 5
        for ( ii in ( step.begin + number.of.displayed.steps ) : step.end ){
            par.plane$location <- rep( trajectories$location[ ii ], nrow( par.plane ) )            
            likelihood.plane <- calcNllh( par.plane, time.series )
            min.likelihood <- min( likelihood.plane, na.rm = TRUE )
            if ( min.likelihood < 0 ){
                max.likelihood.low <- -min.likelihood* 2
                likelihood.plane <- likelihood.plane - min.likelihood + 1
            } else
                max.likelihood.low <- min.likelihood* 2
            par.plane$likelihood <- par.plane$likelihood.low <- likelihood.plane
            par.plane$likelihood[ par.plane$likelihood > max.likelihood ] <- max.likelihood
            ## A lower one for the contour plot
            par.plane$likelihood.low[ par.plane$likelihood.low > max.likelihood.low ] <-
                max.likelihood.low
            print( paste( "likelihood.gui: plotting frame", ii, "of", step.end ) )
            gg.plane <-
                ggplot() + geom_raster( data = par.plane, na.rm = TRUE,
                                       aes( x = scale, y = shape, fill = likelihood ) ) +
                geom_contour( data = par.plane, aes( x = scale, y = shape, z = likelihood.low ),
                             colour = rgb( 1, .55, 0 ), na.rm = TRUE, alpha = .8 ) +
                geom_segment( data = trajectories[ ( ii - number.of.displayed.steps ) : ii, ], 
                             aes( x = scale.begin, xend = scale, alpha = step, yend = shape,
                                 y = shape.begin ), colour = "orange",
                             arrow = arrow( length = unit( 0.3, "cm" ) ) ) +
                geom_point( data = trajectories[ ( ii - number.of.displayed.steps ) : ii, ],
                           size = 2, shape = 21,
                           aes( x = scale, y = shape, alpha = step ), colour = "orange" ) +
                geom_point( data = data.frame( x = time.series.par[ 2 ], y = time.series.par[ 3 ] ),
                           aes( x = x, y = y ), colour = "white", size = 2, shape = 13 ) +
                xlim( scale.lim ) + ylim( shape.lim ) +
                ggtitle( paste( "location", trajectories$location[ ii ] ) ) +
                scale_shape_manual( labels = c( latex2exp::TeX( "minor $\\Delta\\mu$" ),
                                               latex2exp::TeX( "major $\\Delta\\mu$" ) ),
                                   values = c( 1, 4 ) ) +
                scale_fill_gradientn( colours = rev( RColorBrewer::brewer.pal( 7, "Blues" ) ),
                                     na.value = "white", trans = "log" ) +
                scale_alpha( guide = FALSE )
            suppressWarnings( print( gg.plane ) )
        }
        return( last_plot() )
    }
    ## This function saves all the produced images in the ./images/ folder and generates an
    ## animation out of it. But there is a problem: the UI is always lunched in a separate
    ## window as a html widget independent of the R process. So its unfortunately not
    ## compatible with my climex-shiny app.

    ## Before I create new plots I have to delete the existing ones!
    if ( dir.exists( "images" ) )
        unlink( "images/", recursive = TRUE )
    if ( nrow( starting.points ) > 1 ){
        saveHTML( expr = { ani.options( interval = 0.3 )
            plot.trajectories( trajectories, gg.plane ) },
            img.name = "grad.evolution", htmlfile = "grad.evolution.html", ani.height = 850, 
            ani.width = 850, title = "Evolution of the Optimization for different starting points", 
            description = "Each color corresponds to a distinct starting point whilst the black one is the heuristic one." )
    } else {
        saveHTML( expr = { ani.options( interval = 0.4 )
            plot.follow.trajectory( trajectories, scale.lim, shape.lim, time.series ) },
            img.name = "grad.evolution", htmlfile = "grad.evolution.html", ani.height = 850, 
            ani.width = 850, title = "Evolution of the Optimization for a specific starting point", 
            description = "Each color corresponds to a distinct starting point whilst the black one is the heuristic one." )
    }
    ui <- fluidPage(
        titlePanel( "Grad.evolution demo" ),
        sidebarLayout(
            sidebarPanel(
                sliderInput( 'slider.steps.animation', 'Steps', min = 1, max = 10, value=1, 
                            animate = animationOptions( interval = 0, loop = TRUE ) ) ),
            mainPanel( uiOutput( "optimization.plots" ) ) ) )
    server <- function( input, output, session ){
        imgurl <- reactive( {
            slider.input = input$slider.steps.animation
            return( paste0( "./images/grad.evolution", slider.input, ".png" ) ) } )
        output$optimization.plots <- renderUI( {
            tags$div(
                tags$img( src = imgurl() )
            ) } ) }
    shinyApp( ui, server )
}

