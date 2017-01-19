##' @title Performing a fit to the GEV function.
##'
##' @details Custom fitting of the GEV function including the estimation of the initial conditions. Per default the optimization is run twice to prevent getting trapped in local minima of the negative log-likelihood function. The output is of optim style. But the errors of the fitting are also provided as well as the estimates and the errors of some specified return levels. I had some problems with the optim implementation of the simulated annealing implementation. The optimization just kept its value and nothing happened. That's why I switched to the \code{\link{GenSA}} package. More for convenience and due to its use in the animation of the climex shiny app I also introduced a slightly modified version of the dfoptim package's 'nmk' function. But I do not recommend using it since they produce slightly worse results and seem to be numerical more unstable.
##'
##' @param x Blocked time series to which the GEV distribution should be fitted.
##' @param initial Initial values for the GEV parameters. Has to be provided as 3x1 vector. If NULL the parameters are estimated with the function \code{\link{likelihood.initials}}. Default = NULL
##' @param rerun The optimization will be started again using the results of the first optimization run. If the "Nelder-Mead" algorithm is used for optimization (as it is here) this can be useful to escape local minima. When choosing simulated annealing as method the rerun will be skipped. Provide a different number of runs directly to the GenSA function via ... instead. Default = TRUE
##' @param optim.function Function which is going to be optimized. Default: \code{\link{likelihood}}
##' @param gradient.function If NULL a finite difference method is invoked. Default: \code{\link{likelihood.gradient}}
##' @param error.estimation Method for calculating the standard errors of the fitted results. Using the option "MLE" the errors of the GEV parameters will be calculated as the square roots of the diagonal elements of the inverse of the hessian matrix calculated with the MLE of the GEV parameters. The standard error of the return level is calculated using the Delta method and the MLE of the GEV parameters. Alternative one can use Monte Carlo simulations with "MC" for which 1000 samples of the same size as x will be drawn from a GEV distribution constituted by the obtained MLE of the GEV parameters of x. The standard error is then calculated via the square of the variance of all fitted GEV parameters and calculated return levels. Sometimes the inversion of the hessian fails (since the are some NaN in the hessian) (which is also the reason why the ismev package occasionally does not work). In such cases the Monte Carlo method is used. Option "none" just skips the calculation of the error. Default = "none".
##' @param method Through the argument 'method' (which is passed to the optim function) the optimization algorithm is chosen. The default one is the "Nelder-Mead".
##' @param monte.carlo.sample.size Number of samples used to obtain the Monte Carlo estimate of the standard error of the fitting. Default = 1000
##' @param return.period Quantiles at which the return level is going to be evaluated. Class "numeric". Default = 100.
##' @param ... Additional arguments for the optim() or GenSA::GenSA() function. Depending on the chosen method.
##' 
##' @family optimization
##' 
##' @return Output of the optim function with class == c( "list", "climex.fit.gev" )
##' \itemize{
##'  \item{ par = MLE of the GEV parameters }
##'  \item{ value = Value of the negative log-likelihood evaluated at the MLE }
##'  \item{ counts = Number of function evaluations during the optimization }
##'  \item{ convergence = Specifies the state of the convergence (see \code{\link{optim}})) }
##'  \item{ message = see \code{\link{optim}} }
##'  \item{ hessian = Hessian or the observed information matrix evaluated at the MLE }
##'  \item{ return.level = Estimate of the return levels at the provided return periods }
##'  \item{ se = Standard error of the GEV parameters and the return levels }
##'  \item{ x = Original time series }
##'  \item{ updates = A data.frame either the first and last step ( !'nmk' )
##'         containing all the optimization steps visited during the call. }
##' }
##' @author Philipp Mueller
##' @export
##' @import xts
##' @examples
##' potsdam.anomalies <- anomalies( temp.potsdam )
##' potsdam.blocked <- block( potsdam.anomalies )
##' fit.gev( potsdam.blocked )
fit.gev <- function( x, initial = NULL, rerun = TRUE, optim.function = likelihood,
                    gradient.function = likelihood.gradient,
                    error.estimation = c( "none", "MLE", "MC" ),
                    method = c( "Nelder-Mead", "BFGS", "CG", "SANN", "nmk" ),
                    monte.carlo.sample.size = 1000, return.period = 100, ... ){
    ## Since there are some problems with the simulated annealing algorithm I intersect the
    ## method argument and switch to another package if necessary
    if ( missing( method ) )
        method <- "Nelder-Mead"
    method <- match.arg( method )    
    ## Default values if no initial parameters are supplied
    if ( is.null( initial ) )
        initial <- likelihood.initials( x )
    if ( is.null( error.estimation ) )
        error.estimation <- "none"
    error.estimation <- match.arg( error.estimation )
    ## if the error.estimation is not required, do not calculate the hessian
    if ( error.estimation == "none" ){
        hessian.calculate <- FALSE
    } else
        hessian.calculate <- TRUE
    ## Optimization
    if ( method %in% c( "Nelder-Mead", "BFGS", "CG" ) ){
        suppressWarnings(
            res.optim <- stats::optim( initial, optim.function, gr = gradient.function, x = x,
                                      hessian = hessian.calculate, method = method, ... ) )
    } else if ( method == "SANN" ){
        ## Since this implementation didn't yielded nice results I switch to another package
        aux <- GenSA::GenSA( as.numeric( initial ), optim.function, lower = c( -Inf, 0, -Inf ),
                            upper = c( Inf, Inf, Inf ), x = x, ... )
        ## return a NaN when not optimizing instead of the initial parameters
        if ( sum( aux$par %in% initial ) > 1 ){
            ## more than one parameter value remained unchanged
            aux$value <- NaN
        }
        res.optim <- list( par = aux$par, value = aux$value, counts = aux$counts,
                          hessian = if ( hessian.calculate ){
                                        numDeriv::hessian( optim.function, x = aux$par, x.in = x )
                                    } else NULL,
                          convergence = 0, message = NULL )
    } else if ( method == "nmk" ){
        ## The benefit of the nmk method is that its code base is written in R. So
        ## I could easily modify it and display the progress of the optimization.
        ## In principle I would recommend using the default optim procedures.
        aux <- dfoptim::nmk( par = initial, fn = likelihood, x = x, MODIFIED = TRUE,
                            WARNINGS = FALSE )
        res.optim <- list( par = aux$par, value = aux$value, counts = aux$feval,
                          convergence = 0, message = NULL, updates = aux$x.updates,
                          hessian = if ( hessian.calculate ){
                                        numDeriv::hessian( optim.function, x = aux$par, x.in = x )
                                    } else NULL )
        res.optim$counts[ 2 ] <- NA
        names( res.optim$counts ) <- c( "function", "gradient" )   
    }
    if ( rerun && method != "SANN" ){
        ## Rerunning the optimization for the simulated annealing makes no sense.
        ## Just increase the number of iterations for this method.
        if ( method %in% c( "Nelder-Mead", "BFGS", "CG" ) ){
            suppressWarnings(
                res.optim.rerun <- try( stats::optim( par = res.optim$par, fn = optim.function,
                                                     gr = gradient.function, x = x,
                                                     hessian = hessian.calculate, method = method,
                                                     ... ), silent = TRUE ) )
        } else if ( method == "nmk" ){
            aux <- dfoptim::nmk( par = res.optim$par, fn = likelihood, x = x, MODIFIED = TRUE,
                                WARNINGS = FALSE )
            res.optim.rerun <- list( par = aux$par, value = aux$value, counts = aux$feval,
                                    convergence = 0, message = NULL, updates = aux$x.updates,
                                    hessian = if ( hessian.calculate ){
                                                  numDeriv::hessian( optim.function, x = aux$par,
                                                                    x.in = x )
                                        } else NULL )
            res.optim.rerun$counts[ 2 ] <- NA
            ## When rerun the whole path should be present in the updates
            res.optim.rerun$updates$step <- seq(
                res.optim$updates$step[ nrow( res.optim$updates ) ],
                res.optim$updates$step[ nrow( res.optim$updates ) ] +
                nrow( res.optim.rerun$updates ) - 1 )
            res.optim.rerun$updates <- rbind( res.optim$updates, res.optim.rerun$updates )
            names( res.optim.rerun$counts ) <- c( "function", "gradient" )
        }
        if ( class( res.optim.rerun ) == "try-error" ){
            warning( "Rerun failed. Be sure to use the Nelder-Mead method of optimization." )
        } else
            res.optim <- res.optim.rerun
    }
    ## if no updates element is available add the start and end parameter pair.
    ## This is just a poor approximation of the optimization route but I can
    ## do at this moment (without rewriting and linking the whole internal
    ## calls of optim )
    if ( is.null( res.optim$updates ) )
        res.optim$updates <- data.frame( location = c( initial[ 1 ], res.optim$par[ 1 ] ),
                                        scale = c( initial[ 2 ], res.optim$par[ 2 ] ),
                                        shape = c( initial[ 3 ], res.optim$par[ 3 ] ),
                                        step = c( 1, res.optim$counts[ 1 ] ) )
    if ( error.estimation != "none" ){
        error.covariance <- try( solve( res.optim$hessian ) )
        if ( class( error.covariance ) == "try-error" || error.estimation == "MC" ||
             any( is.nan( res.optim$hessian ) ) ){
            parameter.estimate <- res.optim$par
            number.of.samples <- 1000
            ## Draw a number of samples and fit the GEV parameters for all of them
            samples.list <- lapply( 1 : number.of.samples, function( y )
                extRemes::revd( length( x ), parameter.estimate[ 1 ], parameter.estimate[ 2 ],
                               parameter.estimate[ 3 ], type = "GEV" ) )
            ## If e.g. via the BFGS method way to big shape parameter are estimated the guessing of the initial parameters for the optimization won't work anymore since the sampled values are way to big (e.g. 1E144)
            suppressWarnings( 
                samples.fit <- try( lapply( samples.list, function( y )
                    stats::optim( likelihood.initials( y ), optim.function, x = y,
                                 method = "Nelder-Mead" )$par ) ) )
            if ( class( samples.fit ) == "try-error" ){
                errors <- c( NaN, NaN, NaN, NaN )
            } else {
                errors <- data.frame( sqrt( stats::var( Reduce( rbind, samples.fit )[ , 1 ] ) ),
                                     sqrt( stats::var( Reduce( rbind, samples.fit )[ , 2 ] ) ),
                                     sqrt( stats::var( Reduce( rbind, samples.fit )[ , 3 ] ) ) )
                for ( rr in 1 : length( return.period ) )
                    errors <- cbind( errors,
                                    sqrt( stats::var(
                                        Reduce( c, lapply( samples.fit, function( z )
                                            return.level( z,
                                                         return.period = return.period[ rr ] ) ) ) ) ) )
            }
            names( errors ) <- c( "location", "scale", "shape", paste0( return.period, ".rlevel" ) )
        } else {
            ## Calculating the errors using the MLE
            errors.aux <- sqrt( diag( error.covariance ) ) # GEV parameters
            errors <- data.frame( errors.aux[ 1 ], errors.aux[ 2 ], errors.aux[ 3 ] )
            ## Delta method for the return level
            parameter.estimate <- res.optim$par
            ## Formula according to Stuart Coles p. 56
            for ( rr in 1 : length( return.period ) ){
                yp <- -log( 1 - 1/return.period[ rr ] )
                scale <- parameter.estimate[ 2 ]
                shape <- parameter.estimate[ 3 ]
                dz <- c( 1, -shape^{ -1 }* ( 1 - yp^{ -shape } ),
                        scale* shape^{ -2 }* ( 1 - yp^{ -shape } ) -
                                     scale* shape^{ -1 }* yp^{ -shape }* log( yp ) )
                errors <- cbind( errors, dz %*% error.covariance %*% dz )
            }
            names( errors ) <- c( "location", "scale", "shape", paste0( return.period, ".rlevel" ) )
        }
        res.optim$se <- errors
    }
    ## Naming of the resulting fit parameter (necessary for a correct conversion with as.fevd)
    names( res.optim$par ) <- c( "location", "scale", "shape" )
    ## introducing a new data type for handling fits done with climex
    class( res.optim ) <- c( "list", "climex.fit.gev" )

    ## adding the return levels
    res.optim$return.level <- Reduce( c, lapply( return.period,
                                                function( y ) return.level( res.optim, y ) ) )
    names( res.optim$return.level ) <- paste0( return.period, ".rlevel" )
    res.optim$x <- x
    return( res.optim )
}

##' @title Calculated the negative log likelihood of the GEV function.
##'
##' @details This function is only meant to work with constant parameters and no covariats. x.in is not called "x" anymore since the call grad( func = likelihood, x = parameters, ... ) wouldn't be possible.
##'
##' @param parameters Vector containing the location, scale and shape parameter. If NULL the \code{\link{likelihood.initials}} is used. Default = NULL
##' @param x.in Time series.
##' @param verbose Display debugging information.
##' 
##' @family optimization
##'
##' @export
##' @return Numerical value of the negative log likelihood
##' @author Philipp Mueller
likelihood <- function( parameters = NULL, x.in, verbose = FALSE ){
    if ( is.null( parameters ) ){
        initials <- likelihood.initials( x.in )
        scale <- initials[ 2 ]
        location <- initials[ 1 ]
        shape <- initials[ 3 ]
    } else {
        ## extracting parameters (for the sake of convenience)
        parameters <- as.numeric( parameters )
        location <- parameters[ 1 ]
        scale <- parameters[ 2 ]
        shape <- parameters[ 3 ]
    }
    ## reparametrization
    gamma <- shape/ scale
    alpha <- 1/ shape + 1
    y <- x.in - location

    negloglikelihood <- numeric( 1 )
    suppressWarnings( {
        z <- 1 + y* gamma
        negloglikelihood <- length( x.in )* log( scale ) + alpha* sum( log( z ) ) +
            sum( z^{ -1/ shape } )
        names( negloglikelihood ) <- NULL
    } )
    if ( verbose )
        print( paste( "The negloglikelihood is", negloglikelihood, "\n" ) )    
    return( negloglikelihood )
}

##' @title Calculates the gradient of the negative log likelihood of the GEV function.
##'
##' @details Like \code{\{link{likelihood}} the quantity min.shape switches between the evaluation according to a Gumbel or a Frechet or Weibull like function. In the case of the Gumbel like type the gradient of the shape parameter is set to zero.  
##'
##' @param parameters Vector containing the location, scale and shape parameter.
##' @param x.in Time series.
##' 
##' @family optimization
##'
##' @return Numerical vector containing the derivative of the negative log likelihood in (location, scale, shape) direction.
##' @author Philipp Mueller
likelihood.gradient <- function( parameters, x.in ){
    ## extracting parameters (for the sake of convenience)
    if ( class( parameters ) == "list" )
        parameters <- as.numeric( parameters )
    location <- parameters[ 1 ]
    scale <- parameters[ 2 ]
    if ( length( parameters ) == 3 ){
        shape <- parameters[ 3 ]
    } else
        shape <- 0

    ## reparametrization
    gamma <- shape/ scale
    alpha <- 1/ shape + 1
    y <- x.in - location

    gradient <- numeric( 3 )

    ## Weibull or Frechet
    z <- 1 + y* gamma
    gradient[ 1 ] <- sum( z^{-alpha}/ scale ) - sum( alpha* gamma/ z )
    gradient[ 2 ] <- length( x.in )/ scale - sum( alpha* shape* y/ ( scale^2 * z ) ) +
        sum( z^{ - alpha }*y/ scale^2 )
    gradient[ 3 ] <- sum( alpha* y/ ( scale* z ) ) - sum( log( z )/ shape^ 2 ) +
        sum( z^{ -1/ shape }* log( z )/ shape^ 2 - y/ ( scale* shape* z^alpha ) )
    return( gradient )
}

##' @title Estimates the initial GEV parameters of a time series.
##'
##' @details Two main methods are used for the estimation: the L-moments method of Hosking & Wallis implemented in the extRemes::initializer.lmoments() function and an estimation using the first two moments of the Gumbel distribution. For the later one a modification was added: By looking at skewness of the distribution and with respect to some heuristic thresholds a shape parameter between -.4 and .2 is assigned. Warning: both methods do not work for samples with diverging (or pretty big) mean or variance. For this reason the restrict argument is included. If the estimates are bigger than the corresponding restrict.thresholds, they will be replaced by this specific value.
##'
##' @param x Time series/numeric.
##' @param type Which method should be used to calculate the initial parameters. "best" combines all methods and return the result with the least likelihood. "mom" - method of moments returns an approximation according to the first two moments of the Gumbel distribution. "lmom" - returns an estimate according to the Lmoments method. Default = "best"
##' @param modified If TRUE my changes in the heuristics will be applied. Like using the skewness etc. Default = FALSE
##' @param restrict If TRUE the estimates will be minimum of the two different approximations and restrict.thresholds. This should prevent artifacts due to diverging means and variances. Default = FALSE
##' @param restrict.thresholds Maximal value for each of the estimates. Default = c( 50, 50, 5 )
##'
##' @family optimization
##'
##' @export
##' @return Numerical vector containing the c( location, scale, shape ) estimate.
##' @author Philipp Mueller
likelihood.initials <- function( x, type = c( "best", "mom", "lmom" ), modified = TRUE, restrict = FALSE, restrict.thresholds = c( 1000, 500, 3 ) ){
    if ( missing( type ) )
        type <- "best"
    type <- match.arg( type )
    ## Method of moments
    sc.init <- sqrt( 6* stats::var( x ) )/ pi
    loc.init <- mean( x ) - 0.57722* sc.init
    if ( modified ){
        x.skewness <- moments::skewness( x )
        ## When, for some reason, the time series consists of just a sequence of one unique number the calculation of the skewness returns NaN and the function throws an error
        if ( is.na( x.skewness ) )
            x.skewness <- 0
        if ( x.skewness >= .7 && x.skewness <= 1.6 ){
            sh.init <- 0.001
        } else if( x.skewness < .7 ){
            sh.init <- -.1
            if ( x.skewness < .1 ){
                sh.init <- -.2775
                if ( x.skewness < -.2 ){
                sh.init <- -.4
                if ( x.skewness < -1 )
                    sh.init <- -1.5 # Some arbitrary high value. Didn't checked it.
                }
            }
        } else if ( x.skewness > 1.4 ){
            sh.init <- .2
            if ( x.skewness > 3.4 )
                sh.init <- 1.5 # Some arbitrary high value. Didn't checked it.
        }
    } else
        sh.init <- 0.00001
    if ( type == "mom" )
        return( c( loc.init, sc.init, sh.init ) )    
    ## Approximationg using the Lmoments method of Hosking, Wallis and Wood (1985)
    initial.lmom <- try( as.numeric( extRemes::initializer.lmoments( x, "gev" ) ), silent = TRUE )
    if ( class( initial.lmom ) == "try-error" )
        initial.lmom <- c( Inf, Inf, Inf )
    if ( type == "lmom" )
        return( c( initial.lmom ) )

    ## When the initial parameters are too big due to a diverging mean or variance, some heuristics are used for approximation
    if ( restrict ){
        loc.restrict <- stats::median( x )
        ## Approximation using the mode of the PDF for the location parameter and using the formula for the median of the Gumbel distribution.
        sc.restrict <- abs( ( stats::median( x ) - mode( x ) )/ log( log( 2 ) ) )
        sh.restrict <- sh.init # using my skewness approach
        if ( sc.init > restrict.thresholds[ 2 ] )
            sc.init <- sc.restrict
        if ( abs( loc.init ) > restrict.thresholds[ 1 ] )
            loc.init <- loc.restrict
        if ( abs( initial.lmom[ 1 ] ) > restrict.thresholds[ 1 ] )
            initial.lmom[ 1 ] <- loc.restrict
        if ( initial.lmom[ 2 ] > restrict.thresholds[ 2 ] )
            initial.lmom[ 2 ] <- sc.restrict
        if ( abs( initial.lmom[ 3 ] ) > restrict.thresholds[ 3 ] )
            initial.lmom[ 3 ] <- sh.restrict
    }

    initial.gum1 <- c( loc.init, sc.init, sh.init )   
    initial.gum2 <- c( loc.init, sc.init, sh.init + stats::rnorm( 1, sd = 0.5 ) ) 
    initial.gum3 <- c( loc.init, sc.init, sh.init + stats::rnorm( 1, sd = 0.5 ) ) 
    initial.default1 <- c( loc.init, sc.init, 0.1 )
    initial.default2 <- c( loc.init, sc.init, 1E-8 )
    
    initials <- list( initial.gum1, initial.gum2, initial.gum3, 
                     initial.lmom, initial.default1, initial.default2 )
    suppressWarnings( initials.likelihood <- lapply( initials, likelihood, x.in = x ) )
    
    if ( !all( is.nan( as.numeric( initials.likelihood ) ) ) ){
        ## Returning the set of initial parameters which is yielding the lowest negative log-likelihood
        return( initials[[ which.min( initials.likelihood ) ]] )
    } else
        ## Well, what to do now?
        stop( "The GEV likelihood couldn't be evaluated at all of the suggested initials parameter positions" )
}
