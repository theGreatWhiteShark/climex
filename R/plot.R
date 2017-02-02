##' @title This function takes several plot of the ggplot2 and arranges them on one page.
##' @details Well, it is not really related to extreme value fitting and I am aware of the fact that its quite bad style to export auxiliary functions. But this one is just so handy. If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom. ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects). Imports the grid library.
##'
##' @param main Title of the overall plot.
##' @param tt.title Same as 'main'.
##' @param ... objects for ggplot.
##' @param plotlist alternative way to pass ggplot objects.
##' @param cols Number of columns in layout. Default = 1.
##' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
##'
##' @family plot
##' @author Paul Teetor
##' @examples
##' anomalies.potsdam <- anomalies( temp.potsdam )
##' multiplot( anomalies.potsdam, temp.potsdam, cols = 2, main = "Difference between the pure data of the daily maximum temperatures of the Potsdam station and their anomalies" )
##' @return print object.
##' @export
##' @import ggplot2
##' @import grid
multiplot <- function( tt.title = main, main = NULL, ..., plotlist = NULL, cols = 1,
                      layout = NULL ) {
    sink( "/dev/null" )

    ## Make a list from the ... arguments and plotlist
    plots <- c( list( ... ), plotlist )

    numPlots = length( plots )

    ## If layout is NULL, then use 'cols' to determine layout
    if ( is.null( layout ) ) {
        ## Make the panel
        ## ncol: Number of columns of plots
        ## nrow: Number of rows needed, calculated from # of cols
        layout <- matrix( seq( 1, cols* ceiling ( numPlots/ cols ) ),
                         ncol = cols, nrow = ceiling( numPlots/ cols ) )
    }
    if ( numPlots == 1 ) {
        print( plots[[ 1 ]] )   
    } else {
    ## Set up the page
        grid.newpage()
        if ( !is.null( tt.title ) ){
            pushViewport( viewport( layout = grid.layout(
                                        nrow( layout ) + 1, ncol( layout ),
                                        heights = unit( rep( 1, nrow( layout ) + 1 ),
                                                       c( "lines", rep( "null",
                                                                       nrow( layout) ) ) ) ) ) )
        } else
            pushViewport( viewport( layout = grid.layout( nrow( layout ), ncol( layout ) ) ) )

        ## Make each plot, in the correct location
        if ( !is.null( tt.title ) ){
            grid.text( tt.title, vp = viewport( layout.pos.row = 1,
                                           layout.pos.col = c( 1, ncol( layout ) ) ) )
            for (i in 1:numPlots) {
                ## Get the i,j matrix positions of the regions that contain this subplot
                matchidx <- as.data.frame( which( layout == i, arr.ind = TRUE) )
                print( plots[[ i ]], vp = viewport( layout.pos.row = matchidx$row + 1,
                                                   layout.pos.col = matchidx$col ) )
            }
        } else {
            for (i in 1:numPlots) {
                ## Get the i,j matrix positions of the regions that contain this subplot
                matchidx <- as.data.frame( which( layout == i, arr.ind = TRUE ) )
                print( plots[[ i ]], vp = viewport( layout.pos.row = matchidx$row,
                                                   layout.pos.col = matchidx$col ) )
            }
        }
    }
    sink()
    invisible( last_plot() )
}

##' @title Plotting a xts time series with ggplot2 in a convenient format.
##'
##' @details Plots all objects of class xts. With the main argument a title can be provided.
##'
##' @param data.input Time series which should be visualized.
##' @param ... Additional parameters for the multiplot function.
##'
##' @family plot
##' @export
##' @import xts
##' @import ggplot2
##' @return Nothing. The multiplot function is run in the last step.
##' @author Philipp Mueller
ttplot <- function( data.input, main = "Time series", ylab = NULL, x.df = NULL ){
    if ( !is.xts( data.input ) )
        stop( "Please supply an object of class 'xts'!" )
    if ( is.null( ylab ) )
        ylab <- names( data.input )
    if ( is.null( ylab ) )
        ylab <- ""
    ## Checks if the time series has daily values or if it is blocked. In the latter case the
    ## create values can not be provided as the input of the x axis because ggplot2 does
    ## not know how to scale them.
    x.time.unit <- index( data.input )[[ 2 ]] - index( data.input )[[ 1 ]]
    if ( class( x.time.unit ) != "difftime" || attributes( x.time.unit )$units != "days" ) {
        x.df <- data.frame( date = as.numeric( index( data.input ) ),
                           value = as.numeric( data.input ) ) }
    else
        x.df <- data.frame( date = index( data.input ), value = as.numeric( data.input ) )
    ggplot( data = x.df, aes( x = date, y = value ) ) +
        geom_point( colour = "darkorange" ) + geom_line( colour = "navy" ) +
        ggtitle( main ) + xlab( "Time" ) + ylab( ylab ) +
        theme_bw() + theme( panel.grid.major = element_line( colour = "grey75" ),
                           panel.grid.minor = element_line( colour = "grey80" ) )
    return( last_plot() )
}

##' @title Generates three 2D slices of the likelihood of a provided time series.
##'
##' @details The likelihood will be plotted around its optimal value. To determine it the time series will be fitted via \code{\link{fit.gev}}. Its also possible to provide another point the likelihood will be centered around using 'center'.
##'
##' @param time.series Data for which the likelihood function is going to be calculated.
##' @param location.lim Boundaries of the plotted likelihood region. Default = +/- 2.5 times the minimum value.
##' @param scale.lim Boundaries of the plotted likelihood region. Default = +/- 2.5 times the minimum value.
##' @param shape.lim Boundaries of the plotted likelihood region. Default = +/- 2.5 times the minimum value.
##' @param center It set this point (3D) will be the new center of the likelihood plot. Default = NULL
##' @param initial Initial parameters which can be provided for the fit of the time series.
##' @param main Title for the multiplot. Default = NULL.
##' @param true.minima Provide the true minima of the neg log-likelihood function if fit.gev does not provide it in the first run. Default = NULL.
##'
##' @import ggplot2
##' @return returns a multiplot of planes through the negative log-likelihood of the GEV function calculated using the input time series.
##' @author Philipp Mueller 
likelihood.plot <- function( time.series, location.lim = NULL, scale.lim = NULL, shape.lim = NULL,
                               center = NULL, initial = NULL, main = NULL, true.minima = NULL ){
    ## determining the center and the limits
    if ( is.null( true.minima ) ){
        time.series.mle <- fit.gev( x = time.series, initial = initial )$par
    } else
        time.series.mle <- true.minima
    if ( is.null( center ) )
        center <- time.series.mle
    if ( is.null( location.lim ) )
        location.lim <- c( center[ 1 ] - time.series.mle[ 1 ]* .5,
                          center[ 1 ] + time.series.mle[ 1 ]* .5 )
    if ( is.null( scale.lim ) )
        scale.lim <- c( center[ 2 ] - time.series.mle[ 2 ]* .5,
                       center[ 2 ] + time.series.mle[ 2 ]* .5 )
    if( scale.lim[ 1 ] < 0 )
        scale.lim[ 1 ] <- 0
    if ( is.null( shape.lim ) )
        shape.lim <- c( center[ 3 ] - time.series.mle[ 3 ]* .5,
                       center[ 3 ] + time.series.mle[ 3 ]* .5 )
    ## Calculation of the likelihood surface/space
    ## Inspired by the likelihood.plot function but redone
    ## C++ function calculating the likelihood for every parameter combination
    calcNllh <- inline::cxxfunction( methods::signature( parameters = "numeric", xin = "numeric" ),
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
    location.range <- seq( location.lim[ 1 ], location.lim[ 2 ], , number.of.points )
    scale.range <- seq( scale.lim[ 1 ], scale.lim[ 2 ], , number.of.points )
    shape.range <- seq( shape.lim[ 1 ], shape.lim[ 2 ], , number.of.points )
    loc.sc.plane <- data.frame( expand.grid( location.range, scale.range )[ , 1 ],
                               expand.grid( location.range, scale.range )[ , 2 ],
                               rep( center[ 3 ], number.of.points^ 2 ) )
    loc.sh.plane <- data.frame( expand.grid( location.range, shape.range )[ , 1 ],
                               rep( center[ 2 ], number.of.points^ 2 ),
                               expand.grid( location.range, shape.range )[ , 2 ] )
    sc.sh.plane <- data.frame( rep( center[ 1 ], number.of.points^ 2 ),
                              expand.grid( scale.range, shape.range )[ , 1 ],
                              expand.grid( scale.range, shape.range )[ , 2 ] )
    plot.plane <- function( par.plane, par.plane.names, time.series = time.series ){
        names( par.plane ) <- c( "location", "scale", "shape" )
        likelihood.plane <- calcNllh( par.plane, time.series )
        ## Maybe introducing a cutoff in the likelihood values make the contour plot work again
        likelihood.plane[ likelihood.plane > 1E8 ] <- 1E8       
        min.likelihood <- min( likelihood.plane, na.rm = TRUE )
        if ( min.likelihood < 0 ){
            max.likelihood.low <- -min.likelihood* 1.5
            ## comment the next line to have a cutoff at 0 in the likelihood surface plot
            likelihood.plane <- likelihood.plane - min.likelihood + 0.01
            warning( "In some regions the likelihood is negative. An offset is used in order to still plot it in a logarithmic way" )
        } else
            max.likelihood.low <- min.likelihood* 1.5
        par.plane$likelihood <- par.plane$likelihood.low <- likelihood.plane
        par.plane$likelihood.low[ par.plane$likelihood.low >
                                  max.likelihood.low ] <- max.likelihood.low
        names( par.plane ) <- c( par.plane.names, "likelihood.low", "likelihood" )
        gg.plane <-
            ggplot() + geom_raster( data = par.plane, aes( x = x, y = y, fill = likelihood ),
                                   na.rm = TRUE ) +
            geom_contour( data = par.plane,
                         aes( x = x, y = y, z = likelihood.low ), colour = grDevices::rgb( 1, .55, 0 ),
                         na.rm = TRUE ) +
            scale_fill_gradientn( colours = rev( RColorBrewer::brewer.pal( 7, "Blues" ) ),
                                 na.value = "white", trans = "log" ) +
            theme_bw()
        return( last_plot() ) 
    }
    plot.center <- data.frame( x = center[ 1 ], y = center[ 2 ], z = center[ 3 ] )
    multiplot( plotlist = list(
                   plot.plane( loc.sh.plane,
                              par.plane.names = c( "x", "a", "y" ),
                              time.series ) +
                   geom_point( data = plot.center, aes( x = x, y = z ), shape = 4 ) +
                   xlab( "location" ) + ylab( "shape" ),
                   plot.plane( sc.sh.plane,
                              par.plane.names = c( "a", "x", "y" ),
                              time.series ) +
                   geom_point( data = plot.center, aes( x = y, y = z ), shape = 4 ) +
                   xlab( "scale" ) + ylab( "shape" ),
                   plot.plane( loc.sc.plane,
                              par.plane.names = c( "x", "y", "a" ),
                              time.series  ) +
                   geom_point( data = plot.center, aes( x = x, y = y ), shape = 4 ) +
                   xlab( "location" ) + ylab( "scale" ) ),
              main = main )
}

##' @title Plots the GEV function fitted using \code{\link{fit.gev}}
##'
##' @details Uses ggplot2
##'
##' @param x Fitted GEV object.
##'
##' @export
##' @import ggplot2
##' @return ggplot2 object.
plot.climex.fit.gev <- function( x ){
    x.data <- x$x
    x.lim <- c( max( x.data, na.rm = TRUE ), min( x.data, na.rm = TRUE ) )
    threshold.pdf.plot <- 5E-4
    plot.gev.range <- seq( x$par[ 1 ] - x$par[ 2 ]* 10, x$par[ 1 ] + x$par[ 2 ]* 10, 0.01 )
    plot.gev.data <- data.frame( x.plot = plot.gev.range,
                                y.plot = ismev::gev.dens( x$par, plot.gev.range ) )
    plot.gev.lim <- c( plot.gev.data[[ 1 ]][
        which.min( abs( plot.gev.data[[ 2 ]][ 1 : which.max( plot.gev.data[[ 2 ]] ) ] -
                        threshold.pdf.plot ) ) ],
        plot.gev.data[[ 1 ]][ which.min( ( plot.gev.data[[ 2 ]][ which.max( plot.gev.data[[ 2 ]] )
                                                   : length( plot.gev.data[[ 2 ]] ) ] -
                              threshold.pdf.plot ) ) +
                         which.max( plot.gev.data[[ 2 ]] ) - 1 ] )
    if ( plot.gev.lim[ 1 ] > x.lim[ 1 ] )
        plot.gev.lim[ 1 ] <- x.lim[ 1 ] - abs( x.lim[ 1 ] )* 0.05 
    if ( plot.gev.lim[ 2 ] < x.lim[ 2 ] )
        plot.gev.lim[ 2 ] <- x.lim[ 2 ] + abs( x.lim[ 2 ] )* 0.05
    
    ggplot() + geom_histogram( data = data.frame( x = x.data ),
                              colour = grDevices::rgb( .098, .098, .44 ), alpha = 1,
                              aes( x = x, y = ..density.., fill = "#7171EC" ) ) +
        geom_polygon( data = plot.gev.data, alpha = 0.7, colour = grDevices::rgb( .098, .098, .44 ),
                     aes( x = x.plot, y = y.plot, fill = grDevices::rgb( 1, .55, 0 ) ) ) +            
        scale_fill_manual( values = c( "#7171EC", grDevices::rgb( 1, .55, 0 ) ),
                          labels = c( "Histogram", "Fitted GEV"  ) ) +
        theme_bw() + xlim( plot.gev.lim ) + ylab( "Density" ) +
        theme( legend.title = element_blank() )
    return( last_plot() )
}
