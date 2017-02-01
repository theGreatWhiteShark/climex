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
                climex:::revd( length( x ), location = parameter.estimate[ 1 ],
                              scale = parameter.estimate[ 2 ],
                              shape = parameter.estimate[ 3 ], model = "gev" ) )
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
                                                function( y ) climex::return.level( res.optim, y ) ) )
    names( res.optim$return.level ) <- paste0( return.period, ".rlevel" )
    res.optim$x <- x
    return( res.optim )
}

##' @title Performing a fit to the GPD function.
##'
##' @details Custom fitting of the generalized Pareto distribution (GPD) function including the estimation of the initial conditions. Per default the optimization is run twice to prevent getting trapped in local minima of the negative log-likelihood function. The output is of optim style. But the errors of the fitting are also provided as well as the estimates and the errors of some specified return levels. I had some problems with the optim implementation of the simulated annealing implementation. The optimization just kept its value and nothing happened. That's why I switched to the \code{\link{GenSA}} package. More for convenience and due to its use in the animation of the climex shiny app I also introduced a slightly modified version of the dfoptim package's 'nmk' function. But I do not recommend using it since they produce slightly worse results and seem to be numerical more unstable.
##'
##' @param x Blocked time series to which the GPD distribution should be fitted.
##' @param initial Initial values for the GPD parameters. Has to be provided as 2x1 vector. If NULL the parameters are estimated with the function \code{\link{likelihood.initials}}. Default = NULL
##' @param threshold Optional threshold for the GPD model. If present it will be added to the return level to produce a value which fits to underlying time series. Default = NULL.
##' @param rerun The optimization will be started again using the results of the first optimization run. If the "Nelder-Mead" algorithm is used for optimization (as it is here) this can be useful to escape local minima. When choosing simulated annealing as method the rerun will be skipped. Provide a different number of runs directly to the GenSA function via ... instead. Default = TRUE
##' @param optim.function Function which is going to be optimized. Default: \code{\link{likelihood}}
##' @param gradient.function If NULL a finite difference method is invoked. I'm not really sure why but I obtained more consistent results using the finite difference method instead of the derived formula of the GPD likelihood gradient. To use the later one provide \code{\link{likelihood.gradient}}. Default = NULL.
##' @param error.estimation Method for calculating the standard errors of the fitted results. Using the option "MLE" the errors of the GPD parameters will be calculated as the square roots of the diagonal elements of the inverse of the hessian matrix calculated with the MLE of the GPD parameters. The standard error of the return level is calculated using the Delta method and the MLE of the GPD parameters. Alternative one can use Monte Carlo simulations with "MC" for which 1000 samples of the same size as x will be drawn from a GPD constituted by the obtained MLE of the GPD parameters of x. The standard error is then calculated via the square of the variance of all fitted GPD parameters and calculated return levels. Sometimes the inversion of the hessian fails (since the are some NaN in the hessian) (which is also the reason why the ismev package occasionally does not work). In such cases the Monte Carlo method is used. Option "none" just skips the calculation of the error. Default = "none".
##' @param method Through the argument 'method' (which is passed to the optim function) the optimization algorithm is chosen. The default one is the "Nelder-Mead".
##' @param monte.carlo.sample.size Number of samples used to obtain the Monte Carlo estimate of the standard error of the fitting. Default = 1000
##' @param return.period Quantiles at which the return level is going to be evaluated. Class "numeric". Default = 100.
##' @param total.length Total number of observations in the time series the exceedance were obtained from. This argument is needed to calculate the standard error of the return level via the delta method of the MLE. Default = NULL.
##' @param ... Additional arguments for the optim() or GenSA::GenSA() function. Depending on the chosen method.
##' 
##' @family optimization
##' 
##' @return Output of the optim function with class == c( "list", "climex.fit.gpd" )
##' \itemize{
##'  \item{ par = MLE of the GPD parameters }
##'  \item{ value = Value of the negative log-likelihood evaluated at the MLE }
##'  \item{ counts = Number of function evaluations during the optimization }
##'  \item{ convergence = Specifies the state of the convergence (see \code{\link{optim}})) }
##'  \item{ message = see \code{\link{optim}} }
##'  \item{ hessian = Hessian or the observed information matrix evaluated at the MLE }
##'  \item{ return.level = Estimate of the return levels at the provided return periods }
##'  \item{ se = Standard error of the GPD parameters and the return levels }
##'  \item{ x = Original time series }
##'  \item{ updates = A data.frame either the first and last step ( !'nmk' )
##'         containing all the optimization steps visited during the call. }
##' }
##' @author Philipp Mueller
##' @export
##' @import xts
##' @examples
##' potsdam.anomalies <- anomalies( temp.potsdam )
##' potsdam.extremes <- threshold( potsdam.anomalies, threshold = 10, decluster = TRUE )
##' fit.gpd( potsdam.extremes )
fit.gpd <- function( x, initial = NULL, threshold = NULL, rerun = TRUE,
                    optim.function = likelihood, gradient.function = NULL,                    
                    error.estimation = c( "none", "MLE", "MC" ),
                    method = c( "Nelder-Mead", "BFGS", "CG", "SANN", "nmk" ),
                    monte.carlo.sample.size = 1000, return.period = 100,
                    total.length = NULL, ... ){
    if ( missing( method ) )
        method <- "Nelder-Mead"
    method <- match.arg( method )    
    ## Default values if no initial parameters are supplied
    if ( is.null( initial ) )
        initial <- likelihood.initials( x, model = "gpd" )
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
                                      hessian = hessian.calculate, method = method,
                                      model = "gpd", ... ) )
    } else if ( method == "SANN" ){
        ## Since this implementation didn't yielded nice results I switch to another package
        aux <- GenSA::GenSA( as.numeric( initial ), optim.function, lower = c( 0, -Inf ),
                            upper = c( Inf, Inf ), x = x, model = "gpd", ... )
        ## return a NaN when not optimizing instead of the initial parameters
        if ( sum( aux$par %in% initial ) > 1 ){
            ## more than one parameter value remained unchanged
            aux$value <- NaN
        }
        res.optim <- list( par = aux$par, value = aux$value, counts = aux$counts,
                          hessian = if ( hessian.calculate ){
                                        numDeriv::hessian( optim.function, x = aux$par, x.in = x,
                                                          model = "gpd", ... )
                                    } else NULL,
                          convergence = 0, message = NULL )
    } else if ( method == "nmk" ){
        ## The benefit of the nmk method is that its code base is written in R. So
        ## I could easily modify it and display the progress of the optimization.
        ## In principle I would recommend using the default optim procedures.
        aux <- dfoptim::nmk( par = initial, fn = likelihood, x = x, MODIFIED = TRUE,
                            WARNINGS = FALSE, model = "gpd", ... )
        res.optim <- list( par = aux$par, value = aux$value, counts = aux$feval,
                          convergence = 0, message = NULL, updates = aux$x.updates,
                          hessian = if ( hessian.calculate ){
                                        numDeriv::hessian( optim.function, x = aux$par, x.in = x,
                                                          model = "gpd", ... )
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
                                                     model = "gpd", ... ), silent = TRUE ) )
        } else if ( method == "nmk" ){
            aux <- dfoptim::nmk( par = res.optim$par, fn = likelihood, x = x, MODIFIED = TRUE,
                                WARNINGS = FALSE, model = "gpd", ... )
            res.optim.rerun <- list( par = aux$par, value = aux$value, counts = aux$feval,
                                    convergence = 0, message = NULL, updates = aux$x.updates,
                                    hessian = if ( hessian.calculate ){
                                                  numDeriv::hessian( optim.function, x = aux$par,
                                                                    x.in = x )
                                        } else NULL, model = "gpd", ... )
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
    ## adding the time series and threshold to the result
    res.optim$x <- x
    res.optim$threshold <- threshold
    ## For an adequate calculation of the return level
    if ( is.null( total.length ) )
        warning( "The estimation of the return level of the GP distribution does need the total length 'total.length' of the time series the exceedance are extracted from! Please supply it or use the Monte Carlo approach!" ) 
    ##
    ## Regarding the animation in the climex web app
    ##
    ## if no updates element is available add the start and end parameter pair.
    ## This is just a poor approximation of the optimization route but I can
    ## do at this moment (without rewriting and linking the whole internal
    ## calls of optim )
    if ( is.null( res.optim$updates ) )
        res.optim$updates <- data.frame( scale = c( initial[ 1 ], res.optim$par[ 1 ] ),
                                        shape = c( initial[ 2 ], res.optim$par[ 2 ] ),
                                        step = c( 1, res.optim$counts[ 1 ] ) )
    ##
    ## Error estimation
    ##
    if ( error.estimation != "none" ){
        error.covariance <- try( solve( res.optim$hessian ) )
        if ( class( error.covariance ) == "try-error" || error.estimation == "MC" ||
             any( is.nan( res.optim$hessian ) ) ){
            parameter.estimate <- res.optim$par
            number.of.samples <- 1000
            ## Draw a number of samples and fit the GPD parameters for all of them
            samples.list <- lapply( 1 : number.of.samples, function( y )
                climex:::revd( length( x ), scale = parameter.estimate[ 1 ],
                              shape = parameter.estimate[ 2 ], model = "gpd",
                              silent = TRUE ) )
            ## If e.g. via the BFGS method way to big shape parameter are estimated the guessing of the initial parameters for the optimization won't work anymore since the sampled values are way to big (e.g. 1E144)
            suppressWarnings( 
                samples.fit <- try( lapply( samples.list, function( y )
                    stats::optim( likelihood.initials( y, model = "gpd" ),
                                 optim.function, x = y,
                                 method = "Nelder-Mead", model = "gpd", ... )$par ) ) )
            if ( class( samples.fit ) == "try-error" ){
                errors <- c( NaN, NaN, NaN )
            } else {
                errors <- data.frame( sqrt( stats::var( Reduce( rbind, samples.fit )[ , 1 ] ) ),
                                     sqrt( stats::var( Reduce( rbind, samples.fit )[ , 2 ] ) ) )
                for ( rr in 1 : length( return.period ) )
                    errors <- cbind(
                        errors,
                        sqrt( stats::var(
                            Reduce( c, lapply( samples.fit, function( z )
                                return.level( z, return.period = return.period[ rr ],
                                             error.estimation = "none", model = "gpd",
                                             threshold = threshold,
                                             total.length = total.length,
                                             original.time.series = x )
                                            ) ) ) ) )
            }
            names( errors ) <- c( "scale", "shape", paste0( return.period, ".rlevel" ) )
        } else {
            ## Calculating the errors using the MLE
            errors.aux <- sqrt( diag( error.covariance ) ) # GPD parameters
            errors <- data.frame( errors.aux[ 1 ], errors.aux[ 2 ] )
            ## Delta method for the return level
            parameter.estimate <- res.optim$par
            ## Formula according to Stuart Coles p. 82
            if ( is.null( total.length ) ){
                errors <- cbind( errors, rep( NaN, length( return.period ) ) )
            } else {
                for ( rr in 1 : length( return.period ) ){
                    zeta <- length( x )/ total.length # probability of an exceedance
                    ## m-observation return level = return.period* the mean number of
                    ## exceedance per year. This way the unit of the provided return level
                    ## and its error are  not 'per observation' but 'per year'.
                    ## In this step we harness the power of the 'xts' package
                    m <- return.period* mean( apply.yearly( x, function( y ) length( y ) ) )
                    ## In addition the uncertainty of zeta has to be part of the error
                    ## covariance matrix
                    error.covariance.2 <- matrix( 0, 3, 3 )
                    error.covariance.2[ 1 , 1 ] <- zeta*( 1 - zeta )/ total.length
                    error.covariance.2[ 2 : 3, 2 : 3 ] <- error.covariance
                    scale <- parameter.estimate[ 1 ]
                    shape <- parameter.estimate[ 2 ]
                    dz <- c( scale* m^shape* zeta^{ shape - 1 },
                            shape^{ -1 }* ( ( m* zeta )^shape - 1 ),
                            -scale* shape^{ -2 }* ( ( m* zeta )^ shape - 1 ) +
                                          scale* shape^{ -1 }* ( m* zeta )^shape* log( m* zeta ) )
                    errors <- cbind( errors, dz %*% error.covariance.2 %*% dz )
                }
            }
            names( errors ) <- c( "scale", "shape", paste0( return.period, ".rlevel" ) )
        }
        res.optim$se <- errors
    }
    names( res.optim$par ) <- c( "scale", "shape" )
    ## introducing a new data type for handling fits done with climex
    class( res.optim ) <- c( "list", "climex.fit.gpd" )

    ## adding the return levels
    res.optim$return.level <- Reduce( c, lapply( return.period, function( y )
        climex::return.level( res.optim, y, error.estimation = "none", model = "gpd",
                     threshold = threshold, total.length = total.length ) ) )
    names( res.optim$return.level ) <- paste0( return.period, ".rlevel" )
    return( res.optim )
}

##' @title Calculated the negative log likelihood of the GEV or GPD function.
##'
##' @details This function is only meant to work with constant parameters and no covariats. x.in is not called "x" anymore since the call grad( func = likelihood, x = parameters, ... ) wouldn't be possible.
##'
##' @param parameters Vector containing the location, scale and shape parameter for the GEV or the scale and shape parameter for the GPD. If NULL the \code{\link{likelihood.initials}} is used. Default = NULL
##' @param x.in Time series.
##' @param model Determining whether to calculate the initial parameters of the GEV or GPD function. Default = "gev"
##' 
##' @family optimization
##'
##' @export
##' @return Numerical value of the negative log likelihood.
##' @author Philipp Mueller
likelihood <- function( parameters = NULL, x.in, model = c( "gev", "gpd" ) ){
    if ( missing( model ) )
        model <- "gev"
    model <- match.arg( model )
    if ( all( class( parameters ) != "numeric" ) )
        stop( "likelihood: please provide the parameters as a numerical vector!" )
    ## Getting rid of names
    parameters <- as.numeric( parameters )
    ## Verify input
    if ( model == "gev" && length( parameters ) != 3 )
        stop( "likelihood: to calculate the likelihood of the GEV function please provide the following parameters: c( location, scale, shape )" )
    if ( model == "gpd" && length( parameters ) != 2 )
        stop( "likelihood: to calculate the likelihood of the GP function please provide the following parameters: c( scale, shape )" )
    if ( is.null( parameters ) ){
        initials <- likelihood.initials( x.in, model = model )
        if ( model == "gev" ){
            scale <- initials[ 2 ]
            location <- initials[ 1 ]
            shape <- initials[ 3 ]
        } else {
            scale <- initials[ 1 ]
            shape <- initials[ 2 ]
        }
    } else {
        if ( model == "gev" ){
            ## extracting parameters (for the sake of convenience)
            parameters <- as.numeric( parameters )
            location <- parameters[ 1 ]
            scale <- parameters[ 2 ]
            shape <- parameters[ 3 ]
        } else {
            scale <- parameters[ 1 ]
            shape <- parameters[ 2 ]
        }
    }
    ## reparametrization
    alpha <- 1/ shape + 1
    gamma <- shape/ scale
    if ( model == "gev" ){
        y <- x.in - location
    } else
        y <- x.in
    z <- 1 + y* gamma

    if ( model == "gev" ){
        suppressWarnings( {
            negloglikelihood <- length( x.in )* log( scale ) + alpha* sum( log( z ) ) +
                sum( z^{ -1/ shape } )
        } )
    } else {
        suppressWarnings( {
            negloglikelihood <- length( x.in )* log( scale ) +
                alpha* sum( log( z ) )
        } )
    }
    names( negloglikelihood ) <- NULL
    return( negloglikelihood )
}

##' @title Calculates the gradient of the negative log likelihood of the GEV or GPD function.
##'
##' @details 
##'
##' @param parameters Vector containing the location, scale and shape parameter for the GEV model or the scale and shape parameter for the GPD one.
##' @param x.in Time series or numerical vector containing the extreme events.
##' @param model Determining whether to calculate the initial parameters of the GEV or GPD function. Default = "gev".
##' 
##' @family optimization
##'
##' @return Numerical vector containing the derivative of the negative log likelihood in (location, scale, shape for GEV) or (scale, shape for GPD) direction.
##' @author Philipp Mueller
likelihood.gradient <- function( parameters, x.in, model = c( "gev", "gpd" ) ){
    if ( missing( model ) )
        model <- "gev"
    model <- match.arg( model )
    if ( all( class( parameters ) != "numeric" ) )
        stop( "likelihood.gradient: please provide the parameters as a numerical vector!" )
    ## Getting rid of names
    parameters <- as.numeric( parameters )
    ## Verify input
    if ( model == "gev" && length( parameters ) != 3 )
        stop( "likelihood.gradient: to calculate the likelihood of the GEV function please provide the following parameters: c( location, scale, shape )" )
    if ( model == "gpd" && length( parameters ) != 2 )
        stop( "likelihood.gradient: to calculate the likelihood of the GP function please provide the following parameters: c( scale, shape )" )
    if ( model == "gev" ){
        location <- parameters[ 1 ]
        scale <- parameters[ 2 ]
        shape <- parameters[ 3 ]
    } else {
        scale <- parameters[ 1 ]
        shape <- parameters[ 2 ]
    }

    ## reparametrization
    gamma <- shape/ scale
    alpha <- 1/ shape + 1
    if ( model == "gev" ){
        y <- x.in - location
    } else
        y <- x.in
    z <- 1 + y* gamma

    ## Calculating the gradient
    if ( model == "gev" ){
        gradient <- numeric( 3 )
        gradient[ 1 ] <- sum( z^{-alpha}/ scale ) - sum( alpha* gamma/ z )
        gradient[ 2 ] <- length( x.in )/ scale - sum( alpha* shape* y/ ( scale^2 * z ) ) +
            sum( z^{ - alpha }*y/ scale^2 )
        gradient[ 3 ] <- sum( alpha* y/ ( scale* z ) ) - sum( log( z )/ shape^ 2 ) +
            sum( z^{ -1/ shape }* log( z )/ shape^ 2 - y/ ( scale* shape* z^alpha ) )
    } else {
        gradient <- numeric( 2 )
        gradient[ 1 ] <- -length( x.in )/scale + alpha* shape* sum( x.in/ ( z* scale^2) )
        gradient[ 2 ] <- 1/ shape^2* sum( log( z ) ) - alpha/ scale* sum( x.in/ z )
    }            
    return( gradient )
}

##' @title Estimates the initial GEV or GPD parameters of a time series.
##'
##' @details Two main methods are used for the estimation: the L-moments method of Hosking & Wallis implemented in the extRemes::initializer.lmoments() function and an estimation using the first two moments of the Gumbel distribution. For the later one a modification was added: By looking at skewness of the distribution and with respect to some heuristic thresholds a shape parameter between -.4 and .2 is assigned. Warning: both methods do not work for samples with diverging (or pretty big) mean or variance. For this reason the restrict argument is included. If the estimates are bigger than the corresponding restrict.thresholds, they will be replaced by this specific value.
##'
##' @param x Time series/numeric.
##' @param model Determining whether to calculate the initial parameters of the GEV or GPD function. Default = "gev"
##' @param type Which method should be used to calculate the initial parameters. "best" combines all methods, in addition samples 100 different shape values around the one determined by type 'mom' and returns the result with the least likelihood. "mom" - method of moments returns an approximation according to the first two moments of the Gumbel distribution. "lmom" - returns an estimate according to the Lmoments method. Default = "best"
##' @param modified Determines if the skewness is getting used to determine the initial shape parameter. Default = TRUE.
##'
##' @family optimization
##'
##' @export
##' @return Numerical vector containing the c( location, scale, shape ) estimates for method = "gev" or the c( scale, shape ) estimates for method = "gpd".
##' @author Philipp Mueller
likelihood.initials <- function( x, model = c( "gev", "gpd" ),
                                type = c( "best", "mom", "lmom" ), modified = TRUE ){
    if ( missing( model ) )
        model <- "gev"
    model <- match.arg( model )
    if ( missing( type ) )
        type <- "best"
    type <- match.arg( type )
    if ( model == "gev" ){
        ## Method of moments
        sc.init <- sqrt( 6* stats::var( x ) )/ pi
        loc.init <- mean( x ) - 0.57722* sc.init
        if ( modified ){
            x.skewness <- moments::skewness( x )
            ## When, for some reason, the time series consists of just a sequence of
            ## one unique number the calculation of the skewness returns NaN and the
            ## function throws an error
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
        ## Approximationg using the Lmoments method of Hosking, Wallis and Wood (1985).
        ## Therefore I will use the interior of the extRemes:::initializer.lmoments function
        lambda <- try( Lmoments::Lmoments( x ),
                            silent = TRUE )
        if ( class( lambda ) == "try-error" ){
            initial.lmom <- c( Inf, Inf, Inf )
        } else {
            tau3 <- lambda[ 3 ]/ lambda[ 2 ]
            co <- 2/ ( 3 + tau3 ) - log( 2 )/ log( 3 )
            kappa <- 7.8590* co + 2.9554* co^2
            g <- gamma( 1 + kappa )
            sigma <- ( lambda[ 2 ]* kappa )/( ( 1 - 2^( -kappa ) )* g )
            mu <- lambda[ 1 ] - ( sigma/ kappa )*( 1 - g )
            xi <- -kappa
            initial.lmom <- c( mu, sigma, xi )
        }
        if ( type == "lmom" )
            return( initial.lmom )

        initial.gum1 <- c( loc.init, sc.init, sh.init )   
        initial.gum2 <- c( loc.init, sc.init, sh.init + stats::rnorm( 1, sd = 0.5 ) ) 
        initial.gum3 <- c( loc.init, sc.init, sh.init + stats::rnorm( 1, sd = 0.5 ) ) 
        initial.default1 <- c( loc.init, sc.init, 0.1 )
        initial.default2 <- c( loc.init, sc.init, 1E-8 )
        ## Instead of taking just a default shape parameter, pick a bunch of them
        ## and query for the one resulting in the lowest negative log-likelihood
        sh.init.vector <- c( sh.init + stats::rnorm( 100, sd = .5 ), .1, 1e-8, sh.init )
        parameter.vector <- rbind(
            data.frame( location = rep( loc.init, length( sh.init.vector ) ),
                       scale = rep( sc.init, length( sh.init.vector ) ),
                       shape = sh.init.vector ),
            initial.lmom )
        suppressWarnings( initials.likelihood <- apply(
                              parameter.vector, 1, function( ss )
                                  climex::likelihood( c( ss[ 1 ], ss[ 2 ], ss[ 3 ] ),
                                                     x.in = x, model = "gev" ) ) )
    } else {
        ## Approximationg using the Lmoments method
        ## Since I decided to not calculate the threshold inside the fitting (or even
        ## inside this function - for the sake of the Linux principle) I will use the
        ## interior of the extRemes:::initializer.lmoments function
	lambda <- try( Lmoments::Lmoments( x ), silent = TRUE )
        
        if ( class( lambda ) == "try-error" ){
            initial.lmom <- c( Inf, Inf, Inf )
        } else {
            tau2 <- lambda[ 2 ]/ lambda[ 1 ]
            sigma <- lambda[ 1 ]*( 1/tau2 - 1 )
            kappa <- 1/tau2 - 2
            xi <- -kappa
            initial.lmom <- c( sigma, xi )
	}
        if ( type == "lmom" )
            return( initial.lmom )

        ## Approximation using the method of moments
        sc.init <- sqrt( var( x ) )
        sh.init <- .1
        if ( type == "mom" )
            return( c( sc.init, sh.init ) )

        ## Instead of taking just a default shape parameter, pick a bunch of them
        ## and query for the one resulting in the lowest negative log-likelihood
        sh.init.vector <- c( sh.init + stats::rnorm( 100, sd = .5 ), .1, 1e-8, sh.init )
        parameter.vector <- rbind(
            data.frame( scale = rep( sc.init, length( sh.init.vector ) ),
                       shape = sh.init.vector ),
            initial.lmom )
        suppressWarnings( initials.likelihood <- apply(
                              parameter.vector, 1, function( ss )
                                  climex::likelihood( c( ss[ 1 ], ss[ 2 ] ),
                                                     x.in = x, model = "gpd" ) ) )

    }
    if ( !all( is.nan( as.numeric( initials.likelihood ) ) ) ){
        ## Returning the set of initial parameters which is yielding the
        ## lowest negative log-likelihood
        ## the second value will tend to fluctuate a lot!
        return( as.numeric( parameter.vector[ which.min( initials.likelihood ), ] ) )
    } else
        ## Well, what to do now?
        stop( "The GEV likelihood couldn't be evaluated at all of the suggested initials parameter positions" )
}
