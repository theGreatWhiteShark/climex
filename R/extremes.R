##' @title block
##' @description Separates the input into blocks of equal size and returns the maximum or minimum of the block as result.
##'
##' @details If 'separation.mode' is set to "years" the data is separated according to its time stamps. If not the size of a block is determined by the 'block.length' parameter or calculated via the 'block.number' parameter. This is done for data of class 'xts'. For calculating the mean of the blocks have a look at the \code{\link{stats::ave}} function.
##' 
##' @param input.bulk Provided data. Preferably of class 'xts' but also backward compatible.
##' @param block.number Specifies the number of blocks the input data is going to be separated in.
##' @param block.length Length of the blocks. For the sake of simplicity the last block is not forced to match the length of the other plots.
##' @param block.mode This parameter determines if the maximum "max" or the minimum "min" of a block shall be returned. Default: "max".
##' @param separation.mode "years" is used to split the data according to its date values instead.
##' @return Of class 'xts' if a 'xts' object was provided or of class 'numeric' else.
##' @family extremes
##' @author Philipp Mueller
##' @examples
##' block( temp.potsdam )
block <- function( input.bulk, block.number = round( nrow( input.bulk )/ 50 ), block.length = NULL,
                  block.mode = c( "max", "min" ),
                  separation.mode = c( "weeks", "months", "years", "quarterly"  ) ){
    UseMethod( "block" )
}
block.xts <- function( input.bulk, block.number = round( length( input.bulk )/ 50 ),
                      block.length = NULL, block.mode = c( "max", "min" ),
                      separation.mode = c( "years", "none" ) ){
    ## Initializing. The 'block.length' is the most important parameter
    if ( !missing( block.length ) || !missing( block.number ) ){
        separation.mode <- "none"
        if ( !is.null( block.length ) ){
            block.number <- floor( length( input.bulk )/ block.length ) + 1
        } else {
            ## separation according to the number of blocks is used
            if ( block.number <= 1 )
                stop( "Provide a block number greater than 1!" )
            if ( block.number* 5 > length( input.bulk ) )
                warning( "There are less than five times data points than actual blocks. This are many of blocks." )
            block.length <- length( input.bulk )/ ( block.number ) + 1/ ( block.number ) }
    } else
        separation.mode <- "years"
    if ( missing( block.mode ) )
        block.mode <- "max"
    block.mode <- match.arg( block.mode )
    ## according to the desired block.length the data is now separated into snippets and those
    ## are saved inside a list
    if ( separation.mode == "years" ){
        input.index <- data.frame( value = input.bulk, index = year( input.bulk ),
                                  row.names = index( input.bulk ) )
    } else {
        ## All data belonging to the same block share the same index value
        input.index <- data.frame( value = input.bulk,
                                  index =  floor( ( seq( 1 : length( input.bulk ) ) - 1 )/
                                                  block.length ) + 1,
                                  row.names = index( input.bulk ) )        
    }
    input.blocked <- split( input.index,input.index$index )
    ## Extract the maxima or minima from the blocked data
    if ( block.mode == "max" ){
        input.extremes <- Reduce( rbind, lapply( input.blocked, function( x ){
            data.frame( date = row.names( x )[ which.max( x[[ 1 ]] ) ],
                       value = x[ which.max( x[[ 1 ]] ), 1 ] ) } ) )
    } else 
        input.extremes <- Reduce( rbind, lapply( input.blocked, function( x ){
            data.frame( date = row.names( x )[ which.min( x[[ 1 ]] ) ],
                       value = x[ which.min( x[[ 1 ]] ), 1 ] ) } ) )
    extremes.xts <- xts( input.extremes[[ 2 ]] , order.by = as.Date( input.extremes[[ 1 ]] ) )
    return( extremes.xts )
}
block.default <- function( input.bulk, block.number = round( length( input.bulk )/ 50 ),
                          block.length = NULL, block.mode = c( "max", "min" ),
                          separation.mode = c( "weeks", "months", "years", "quarterly" ) ){
    ## In this method for numerical data the time based separation.modes are not allowed anymore.
    if ( !missing( block.length ) || !missing( block.number ) ){
        if ( !is.null( block.length ) ){
            block.number <- floor( length( input.bulk )/ block.length ) + 1
        } else {
            ## separation according to the number of blocks is used
            if ( block.number <= 1 )
                stop( "Provide a block number greater than 1!" )
            if ( block.number* 5 > length( input.bulk ) )
                warning( "There are less than five times data points than actual blocks. This are many of blocks." )
            block.length <- length( input.bulk )/ ( block.number ) + 1/ ( block.number ) }        
    } else
        stop( "No block length or number of blocks provided to block" )
    if ( missing( block.mode ) )
        block.mode <- "max"
    block.mode <- match.arg( block.mode )
    ## according to the desired block.length the data is now separated into snippets and those
    ## are saved inside a list
    ## All data belonging to the same block share the same index value
    input.index <- data.frame( value = input.bulk,
                              index =  floor( ( seq( 1 : length( input.bulk ) ) - 1 )/
                                              block.length ) + 1 )
    input.blocked <- split( input.index,input.index$index )
    ## Extract the maxima or minima from the blocked data
    if ( block.mode == "max" ){
        input.extremes <- Reduce( c, lapply( input.blocked, function( x ){
             x[ which.max( x[[ 1 ]] ), 1 ] } ) )
    } else 
        input.extremes <- Reduce( c, lapply( input.blocked, function( x ){
            x[ which.min( x[[ 1 ]] ), 1 ] } ) )
    return( input.extremes )
}

##' @title find.block.length
##' @description Finding the appropriate block length before applying GEV fitting. 
##'
##' @details Function fits GEV using gev.fit() after applying various block lengths and returns summary statistics. After a sufficient high block length the parameters of the distribution should converge. Function is implemented for inputs of the classes "xts" and "ts".
##' 
##' @param x Time series of class "xts" or "ts"
##' @param size.low Smallest block length applied to the data. Default = 10.
##' @param size.high Blggest block length applied to the data. Default = 400.
##' @param return.period Argument of the extRemes::return.level() function specifying the return period of the event the return level shall be calculated for. Default = 100.
##' @param plot Plots the results. Default = TRUE
##' @param main Title of the plot.
##' @param ... Additional arguments applied for extRemes::fevd() 
##'
##' @return Data.frame containing the summary statistic of the analysis:
##' \itemize{
##'  \item{ block = specific block length. }
##'  \item{ loc = MLE estimate for the mean location parameter }
##'  \item{ se.loc = MLE estimate for the standard deviation of the location parameter. }
##'  \item{ sca= MLE estimate for the mean scale parameter }
##'  \item{ se.sca = MLE estimate for the standard deviation of the scale parameter. }
##'  \item{ sha = MLE estimate for the mean shape parameter }
##'  \item{ se.sha = MLE estimate for the standard deviation of the shape parameter. }
##'  \item{ nllh = Negative log-likelihood of the applied fit. }
##'  \item{ AIC = Akaike information criterion of the applied fit. }
##'  \item{ BIC = Bayesian information criterion of the applied fit. }
##' }   
##' @author Philipp Mueller
find.block.length <- function( x, size.low = 20, size.high = 400, return.period = 100,
                              plot = TRUE, main = NULL ){
    ## Computes statistics for different block length and returns a table and a plot displaying
    ## the results. This functions is made to help deciding for an appropriate block length. 
    ## Possible values of the return level
    
    ## Indicating whether the evaluation of the confidence intervals of the return levels already has failed.  
    err.ret <- FALSE
    if ( any( class( x ) == "bulk" ) ) {
        x.data <- x[[ 2 ]]
    } else 
        x.data <- as.numeric( x )
        
    x.gev.fit <- gev.fit( x.data, show = FALSE )
    ## Initiation of the matrix containing all the information
    gev.sum <- data.frame( block = 1, loc = x.gev.fit$mle[ 1 ], 
                          se.loc = x.gev.fit$se[ 1 ], scale = x.gev.fit$mle[ 2 ], 
                          se.sca = x.gev.fit$se[ 2 ], shape = x.gev.fit$mle[ 3 ],
                          se.sha = x.gev.fit$se[ 3 ], nllh = x.gev.fit$nllh,
                          AIC = aic( x.gev.fit ), BIC = bic( x.gev.fit ),
                          stringsAsFactors = FALSE )
    
    bb.count <- 1
    for ( bb in seq( size.low, size.high, , 20 ) ){
        ## sometimes it just does not work
        bb.gev.fit <- gev.fit( block( x, block.length = bb ), show = FALSE )
        gev.sum[ bb.count, ] <- c( bb,  bb.gev.fit$mle[ 1 ], bb.gev.fit$se[ 1 ], 
                                  bb.gev.fit$mle[ 2 ], bb.gev.fit$se[ 2 ], 
                                  bb.gev.fit$mle[ 3 ], bb.gev.fit$se[ 3 ],
                                  bb.gev.fit$nllh, aic( bb.gev.fit ),
                                  bic( bb.gev.fit ) )
        bb.count <- bb.count + 1
    }

    if ( is.null( main ) )
        main <- "Summary statistics versus block length"
    if ( plot ) {
        p1 <- ggplot( data = gev.sum, aes( x = block ) ) +
            geom_line( aes( y = AIC ), colour = "darkorange2" ) +
            geom_point( aes( y = AIC, colour = "darkorange2" ) ) +
            geom_line( aes( y = BIC ), colour = "navy" ) +
            geom_point( aes( y = BIC, colour = "navy" ) ) +
            scale_colour_manual( values = c( "darkorange2", "navy" ), 
                                labels = c( "AIC", "BIC" ) ) +
            theme_bw() + theme( panel.grid.major = element_line(  colour = "grey75" ),
                               panel.grid.minor = element_line( colour = "grey80" ),
                               legend.title = element_blank() ) +
            ylab( "Statistics of the fits to data of different block length" ) +
            xlab( "block length" ) +
            ggtitle( "Return levels versus block length" )
        p2 <- ggplot( data = gev.sum, aes( x = block ) ) +
            geom_line( aes( y = loc ), colour = "firebrick1" ) +
            geom_errorbar( aes( y = loc,
                               ymin = loc - se.loc/2,
                               ymax = loc + se.loc/2,
                               colour = "firebrick4" ) ) +
            geom_line( aes( y = scale ), colour = "orange1" ) +
            geom_errorbar( aes( y = scale,
                               ymin = scale - se.sca/2,
                               ymax = scale + se.sca/2,
                               colour = "orange4" ) ) +
            geom_line( aes( y = shape ), colour = "royalblue1" ) +
            geom_errorbar( aes( y = shape,
                               ymin = shape - se.sha/2,
                               ymax = shape + se.sha/2,
                               colour = "royalblue4" ) ) +
            scale_colour_manual( values = c( "firebrick4", "orange4", "royalblue4" ),
                                labels = c( "loc", "scale", "shape" ) ) +
            xlab( "block length" ) + ylab( "" ) +
            theme_bw() + theme( panel.grid.major = element_line(  colour = "grey75" ),
                               panel.grid.minor = element_line( colour = "grey80" ),
                               legend.title = element_blank() ) +
            ggtitle( main )
        multiplot( plotlist = list( p1, p2 ), cols = 1 )
    }
    return( gev.sum )
}
##' @title decluster
##' @description Decluster point over threshold data used for GP fitting.
##'
##' @details Inspired by the decluster algorithm used in the extRemes package
##'
##' @param x Time series
##' @param threshold Has to be set sufficient high to fulfill the asymptotic condition for the GP distribution.
##'
##' @return Declustered values above the provided threshold (xts).
##' @author Philipp Mueller 
decluster <- function( x, threshold ){
    ## Caution: x is the full time series and not the blocked one!
    x.extremal.index <- extRemes::extremalindex( x, threshold, na.action = stats::na.omit )
    ifelse( x.extremal.index[ 1 ] >= 1, cluster.size <- 0, cluster.size <- x.extremal.index[ 3 ] )
    ## cluster.size is the number of indices a two points have to be away from each other to belong to different indices
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

##' @title rlevd
##' @description Calculation of the return levels.
##'
##' @details Uses the extRemes::rlevd function at its core but also can handle multiple versions of the parameter input (as numeric, or direct outputs of various fitting procedures), is capable of calculating numerous return levels at once and also calculates the errors of the return levels. For the errors the ML fit is using the option hessian=TRUE (if not done already) or uses a Monte Carlo based approach. If no fitting object is provided, no errors will be calculated. The calculation of the error only works for objects fitted by gev.fit and not for ones fitted by foreign packages.
##'
##' @param x Parameter input. Class numeric, climex.gev.fit, gev.fit (ismev) or fevd (extRemes)
##' @param return.period Numeric vector of the return periods the return levels should be calculated at. Default = 100.
##' @param error.estimation Method of calculating the standard errors of the return levels. Using option "MLE" it is calculated using the Delta method and the MLE of the GEV parameters. Alternative one can use Monte Carlo simulations with "MC" for which monte.carlo.sample.size samples of the same size as x will be drawn from a GEV distribution constituted by the obtained MLE of the GEV parameters of x. The standard error is then calculated via the square of the variance of the calculated return levels. Sometimes the inversion of the hessian fails (since the are some NaN in the hessian) (which is also the reason why the ismev package occasionally does not work). Option "none" just skips the calculation of the error and return just a numeric value of the estimate. To avoid broken dependencies  with existing code this option will be default. Default = "none".
##' @param monte.carlo.sample.size Number of samples used to obtain the Monte Carlo estimate of the standard error of the fitting. Default = 1000
##'
##' @return If error.estimation == "none" a numerical vector containing the estimates of the return levels will be returned. Else a list containing the estimates and their standard errors will be returned.
##' @examples
##' fit.results <- gev.fit( block( anomalies( temp.potsdam ) ) )
##' rlevd( fit.results, return.period = c( 10, 50, 100 ), error.estimation = "MLE" )
rlevd <- function( x, return.period = 100, error.estimation = c( "none", "MC", "MLE" ),
                  monte.carlo.sample.size = 1000 ){
    if ( any( class( x ) == "climex.gev.fit" ) ){
        return.levels <- Reduce( c, lapply( return.period, function( y )
            as.numeric( extRemes::rlevd( y, x$par[ 1 ], x$par[ 2 ], x$par[ 3 ] ) ) ) )
    } else if ( class( x ) == "gev.fit" || class( x ) == "gum.fit" ){
        return.levels <- Reduce( c, lapply( return.period, function( y )
            as.numeric( extRemes::rlevd( y, x$mle[ 1 ], x$mle[ 2 ], x$mle[ 3 ] ) ) ) )
    } else if ( class( x ) == "fevd" ){
        return.levels <- Reduce( c, lapply( return.period, function( y )
            as.numeric( extRemes::rlevd( y, x$results$par[ 1 ], x$results$par[ 2 ],
                                    x$results$par[ 3 ] ) ) ) )
    } else if ( class( x ) == "numeric" ){
        return.levels <- Reduce( c, lapply( return.period, function( y )
            as.numeric( extRemes::rlevd( y, x[ 1 ], x[ 2 ], x[ 3 ] ) ) ) )
    } else
        stop( "as.numeric( rlevd is not implemented for this class of input values!" )

    if ( error.estimation == "none" || class( x ) == "numeric" ){
        return( return.levels )
    } else if ( error.estimation == "MLE" && !any( is.nan( x$hessian ) ) ){
        if ( !any( names( x ) == "hessian" ) ){
            x.aux <- stats::optim( x$par, likelihood, x = x$x, hessian = TRUE )
            x.aux$x <- x$x
            x <- x.aux
        }
        ## Calculating the errors using the MLE
        error.covariance <- solve( x$hessian ) 
        ## Delta method for the return level
        parameter.estimate <- x$par
        ## Formula according to Stuart Coles p. 56
        errors <- data.frame( a = 0 )
        for ( rr in 1 : length( return.period ) ){
            yp <- -log( 1 - 1/return.period[ rr ] )
            scale <- parameter.estimate[ 2 ]
            shape <- parameter.estimate[ 3 ]
            dz <- c( 1, -shape^{ -1 }* ( 1 - yp^{ -shape } ),
                    scale* shape^{ -2 }* ( 1 - yp^{ -shape } ) -
                                 scale* shape^{ -1 }* yp^{ -shape }* log( yp ) )
            errors <- cbind( errors, dz %*% error.covariance %*% dz )
        }
        errors <- errors[ , -1 ]
        names( errors ) <- paste0( return.period, ".rlevel" )
    } else {
        parameter.estimate <- x$par
        ## Draw a number of samples and fit the GEV parameters for all of them
        samples.list <- lapply( 1 : monte.carlo.sample.size, function( y )
            extRemes::revd( length( x$x ), parameter.estimate[ 1 ], parameter.estimate[ 2 ],
                           parameter.estimate[ 3 ], type = "GEV" ) )
        samples.fit <- lapply( samples.list, function( y )
            stats::optim( likelihood.initials( y ), likelihood, x = y,
                  method = "Nelder-Mead" )$par )
        errors <- data.frame( a = 0 )
        for ( rr in 1 : length( return.period ) )
            errors <- cbind( errors, sqrt( stats::var( Reduce( c, lapply( samples.fit, function( z )
                rlevd( z, return.period = return.period[ rr ] ) ) ) ) ) )
        errors <- errors[ , -1 ]
        names( errors ) <-paste0( return.period, ".rlevel" )
    }
    return( list( return.levels = return.levels, errors = errors ) )
}
        
