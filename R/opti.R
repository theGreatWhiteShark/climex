##' @title Robust maximum-likelihood fit of the GEV distribution
##'
##' @description This function fits the Generalized Extreme Value (GEV)
##' distribution to the supplied data, which has to be composed of
##' block maxima (preferably without trend and correlations). The
##' determination of the starting point for the optimization and the
##' calculation of the return level and the all the corresponding
##' estimates of the fitting errors will be done internally.
##'
##' @details The optimization is performed using the augmented Lagrangian
##' method. Within this framework the log-likelihood function of the GEV
##' distribution gets augmented with N+2 constraints, where N is the
##' number of points in the time series. N+1 of those constraints ensure
##' the log-likelihood (containing two logarithms) to be always defined.
##' The remaining constraints ensures for the shape parameter to be
##' always bigger than -1 for the maximum likelihood to be defined in the
##' first place. The penalty in the log-likelihood function is the sum of
##' all squared constrain violations plus an additional term linear in
##' the constraint violation to ensure well-conditioning. Using this
##' penalty term the problem becomes unconstrained again and can be
##' solved using \code{\link{stats::optim}}. After each of those inner
##' routines the weighting parameter of the penalty is being increased
##' until some convergence conditions are fulfilled.
##'
##' Since it usually takes just four to five outer iterations this
##' functions needs only double the time a pure call to the stats::optim
##' function would need.
##'
##' The negative log-likelihood of the Gumbel distribution is just fitted
##' if the shape parameter is exactly equal to zero.
##'
##' If the user instead wants to fit just the Gumbel distribution and
##' not the entire GEV distribution, the shape parameter of the
##' `initial' has to be set to 0. But in practice this is strongly
##' discouraged since it will yield inferior results.
##'
##' I found the Nelder-Mead method to be more robust to starting
##' points more far away from the global optimum. This also holds
##' for the inner routine of the augmented Lagrangian method. Since
##' other routines, like CG and BFGS only cause problems in the
##' extreme value analysis, there won't be an option to choose them
##' in this package.
##'
##' @param x Blocked time series to which the GEV distribution should
##' be fitted.
##' @param initial Initial values for the GEV parameters. Has to be
##'   provided as 3x1 vector. If NULL the parameters are estimated
##'   using \code{\link{likelihood.initials}}. If the shape parameter
##'   is set to 0 the exponential distribution instead of the GP one
##'   is fitted. But this its strongly discouraged to do so! Default =
##'   NULL.
##' @param likelihood.function Function, which is going to be optimized.
##' Default: \code{\link{likelihood}}
##' @param gradient.function If NULL a finite difference method is
##' invoked. Default: \code{\link{likelihood.gradient}}
##' @param error.estimation Method for calculating the standard errors of
##' the fitted results. Using the option "MLE" the errors of the GEV
##' parameters will be calculated as the square roots of the diagonal
##' elements of the inverse of the hessian matrix calculated with the MLE
##' of the GEV parameters. The standard error of the return level is
##' calculated using the Delta method and the maximum likelihood
##' estimates (MLE) of the GEV parameters.
##' 
##' Alternative one can use Monte Carlo simulations with "MC" for which
##' monte.carlo.sample.size samples of the same size as x will be drawn
##' from a GEV distribution constituted by the obtained MLE of the GEV
##' parameters of x. The standard error is then calculated via the square
##' of the variance of all fitted GEV parameters and calculated return
##' levels.
##'
##' Sometimes the inversion of the hessian fails (since the are some NaN
##' in the hessian) (which is also the reason why the ismev package
##' occasionally does not work). In such cases the Monte Carlo method is
##' used. Option "none" just skips the calculation of the error.
##' Default = "none".
##' @param monte.carlo.sample.size Number of samples used to obtain the
##' Monte Carlo estimate of the standard error of the fitting.
##' Default = 100.
##' @param return.period Quantiles at which the return level is going to
##' be evaluated. Class "numeric". Default = 100.
##' @param silent Determines whether or not warning messages shall be
##' displayed and results shall be reported. Default = TRUE.
##' @param ... Additional arguments for the optim() function.
##' 
##' @family optimization
##' 
##' @return Output of the optim function with class == c( "list",
##' "climex.fit.gev" )
##' \itemize{
##'  \item{ par = MLE of the GEV parameters }
##'  \item{ value = Value of the negative log-likelihood evaluated
##' at the MLE }
##'  \item{ counts = Number of evaluations of the likelihood
##' function and its gradient during optimization (inner routine) }
##'  \item{ outer.iteration = Number of updates of the penalty and
##' the Lagrangian parameter to fine-tune the impact of the
##' constraints on the optimization (outer routine) }
##'  \item{ return.level = Estimate of the return levels at the
##' provided return periods }
##'  \item{ se = Standard error of the GEV parameters and the return
##' levels }
##'  \item{ x = Original time series }
##'  \item{ control = Parameter and options used during optimization }
##' }
##' @author Philipp Mueller
##' @export
##' @importFrom xts xts
##' @importFrom alabama auglag
##' @examples
##' potsdam.anomalies <- anomalies( temp.potsdam )
##' potsdam.blocked <- block( potsdam.anomalies )
##' fit.gev( potsdam.blocked )
fit.gev <- function( x, initial = NULL,
                    likelihood.function = likelihood,
                    gradient.function = likelihood.gradient,
                    error.estimation = c( "MLE", "MC", "none" ),
                    monte.carlo.sample.size = 100, return.period = 100,
                    silent = TRUE, ... ){
  ## Default values if no initial parameters were supplied
  if ( is.null( initial ) )
    initial <- likelihood.initials( x, model = "gev" )
  if ( is.null( error.estimation ) )
    error.estimation <- "MLE"
  error.estimation <- match.arg( error.estimation )
  ## Optimization
  ## 
  ## I found the Nelder-Mead method to be more robust to starting
  ## points more far away from the global optimum. This also holds
  ## for the inner routine of the augmented Lagrangian method. Since
  ## other routines, like CG and BFGS only cause problems in the
  ## extreme value analysis, there won't be an option to choose them
  ## in this package.

  if ( as.numeric( initial[ 3 ] ) != 0 ){
    ## Fitting the negative log-likelihood of the GEV distribution.
    ## The augmented Lagrangian method allowing non-linear constraints
    ## will be used in here. The code depends of the 'alabama' package.
    ## In principle I could also integrate the routine in here, but let's
    ## stick to the Linux principle.
    ## The Rsolnp package did not performed as well as the alabama one.
    ## It takes two orders of magnitude longer and gets stuck for
    ## certain initial parameter combinations.
    ## If the shape parameter is exactly equal to zero and the
    ## likelihood function switches to the Gumbel distribution, the
    ## constraints involving the shape parameter become true and are
    ## redundant anyway.
    ##
    ## The auglag optimization function seems to produce results
    ## reliable only up to the 5E-4 and I can't see why this is
    ## happening. Adding various additional options and tolerances to
    ## both auglag and optim doesn't change the matter. Since these
    ## deviations are minor ones and the actual MLE estimates of the GEV
    ## and GP parameters are way bigger, I will just leave it this
    ## way. 
    suppressWarnings(
        res.alabama <- auglag(
            par = initial, fn = likelihood.function,
            gr = gradient.function,
            hin = function( parameters, x.in, ... ){
              return( as.numeric(
                  c( parameters[ 2 ] - .03,
                    .95 + parameters[ 3 ]* ( x.in - parameters[ 1 ])/
                    parameters[ 2 ],
                    parameters[ 3 ] + .95 ) ) ) },
            control.outer = list( trace = !silent,
                                 method = "Nelder-Mead" ),
            x.in = x, model = "gev" ) )
  } else {
    ## If the shape parameter in the initial parameter combination was
    ## set to zero, the users want to fit to the pure Gumbel
    ## distribution instead to the GEV distribution. Some changes to
    ## the auglag call are necessary since the algorithm can 'escape'
    ## the Gumbel likelihood if it encounters it during the very
    ## beginning of the optimization.
    suppressWarnings(
        res.alabama <- auglag(
            par = initial[1:2], fn =  function( parameters, ... ){
              likelihood( c( as.numeric( parameters ), 0 ), ... ) },
            gr = function( parameters, ... ){
              likelihood.gradient(
                  c( as.numeric( parameters ), 0 ), ... )[ 1 : 2 ] },
            hin = function( parameters, x.in, ... ){
              return( parameters[ 2 ] - .03 ) },
            control.outer = list( trace = !silent,
                                 method = "Nelder-Mead" ),
            x.in = x, model = "gev" ) )
    ## Add a couple of zeros for the shape parameter to ensure
    ## compatibility to the other parts of the code.
    res.alabama$par <- c( res.alabama$par, 0 )
    res.alabama$gradient <- c( res.alabama$gradient, 0 )
    dummy.hessian <- matrix( rep( 0, 9 ), nrow = 3, ncol = 3 )
    dummy.hessian[ 1 : 2, 1 : 2 ] <- res.alabama$hessian
    res.alabama$hessian <- dummy.hessian
  }
  ## There is no need for the user to deal with all the outputs of the
  ## auglag function. So let's reduce them. 
  res.optim <- list( par = res.alabama$par,
                    value = res.alabama$value,
                    gradient = res.alabama$gradient,
                    counts = res.alabama$counts,
                    outer.iterations = res.alabama$outer.iterations,
                    control = list(
                        initial = initial,
                        return.period = return.period,
                        likelihood.function = likelihood.function,
                        gradient.function = gradient.function,
                        hessian = res.alabama$hessian,
                        monte.carlo.sample.size =
                          monte.carlo.sample.size,
                        error.estimation = error.estimation ) )
  ## Calculate the estimate of the fitting error of the GEV parameters
  ## and the return levels.
  if ( error.estimation != "none" ){
    ## The error of the fitted parameters are the roots of the
    ## diagonal entries of the estimated information matrix. The later
    ## one can be obtained by inverting the hessian. But this obtained
    ## hessian may be bad conditioned, so I will try to invert it. If
    ## this is not possible the Monte Carlo-based method will be used
    ## instead.
    ##
    ## If the shape parameter is exactly zero and the Gumbel
    ## distribution was fitted, the third row and column were just
    ## augmented by 0.
    if ( res.optim$par[ 3 ] != 0 ){
      error.covariance <- try( solve( res.optim$control$hessian ),
                              silent = silent )
    } else {
      ## Omit the augmentation
      error.covariance <- try( solve(
          res.optim$control$hessian[ 1 : 2, 1 : 2 ] ),
          silent = silent )
      ## Augment the result again to ensure compatibility
      if ( class( error.covariance ) != "try-error" ){
        dummy.matrix <- matrix( rep( 0, 9 ), nrow = 3, ncol = 3 )
        dummy.matrix[ 1 : 2, 1 : 2 ] <- error.covariance
        error.covariance <- dummy.matrix
      }
    }
    if ( class( error.covariance ) == "try-error" ||
         error.estimation == "MC" ||
         any( is.nan( res.optim$control$hessian ) ) ){
      parameter.estimate <- res.optim$par
      number.of.samples <- monte.carlo.sample.size
      ## Draw a number of samples and fit the GEV parameters for all
      ## of them
      samples.list <- lapply( 1 : number.of.samples, function( y )
        climex:::revd( length( x ), location = parameter.estimate[ 1 ],
                      scale = parameter.estimate[ 2 ],
                      shape = parameter.estimate[ 3 ], model = "gev" ) )
      suppressWarnings(
          samples.fit <- try( lapply( samples.list, function ( y )
            auglag( par = likelihood.initials( y, model = "gev" ),
                   fn = likelihood.function,
                   gr = gradient.function, 
                   hin = function( parameters, x.in, ... ){
                     return( as.numeric(
                         c( parameters[ 2 ] - .03,
                           .95 + parameters[ 3 ]*
                           ( x.in - parameters[ 1 ])/
                           parameters[ 2 ],
                           parameters[ 3 ] + .95 ) ) ) },
                   control.outer = list( trace = FALSE,
                                        method = "Nelder-Mead" ),
                   x.in = y, model = "gev" )$par ) ) )
      if ( class( samples.fit ) == "try-error" ){
        errors <- c( NaN, NaN, NaN, rep( NaN, length( return.period ) ) )
      } else {
        errors <- data.frame(
            sqrt( stats::var( Reduce( rbind, samples.fit )[ , 1 ] ) ),
            sqrt( stats::var( Reduce( rbind, samples.fit )[ , 2 ] ) ),
            sqrt( stats::var( Reduce( rbind, samples.fit )[ , 3 ] ) ) )
        for ( rr in 1 : length( return.period ) )
          errors <- cbind(
              errors,
              sqrt( stats::var( Reduce( c, lapply( samples.fit, (
                function( z )
                  climex::return.level(
                              z,
                              return.period = return.period[ rr ]
                          )$return.level ) ) ) ) ) )
      }
      errors <- as.numeric( errors )
      names( errors ) <- c( "location", "scale", "shape",
                           paste0( return.period, ".rlevel" ) )
    } else {
      ## Calculating the errors using the MLE
      errors.aux <- sqrt( diag( error.covariance ) ) # GEV parameters
      errors <- data.frame( errors.aux[ 1 ], errors.aux[ 2 ],
                           errors.aux[ 3 ] )
      ## Delta method for the return level
      parameter.estimate <- res.optim$par
      ## Formula according to Stuart Coles p. 56
      for ( rr in 1 : length( return.period ) ){
        yp <- -log( 1 - 1/return.period[ rr ] )
        scale <- parameter.estimate[ 2 ]
        shape <- parameter.estimate[ 3 ]
        if ( shape != 0 ){
          ## GEV distribution
          dz <- c( 1, -shape^{ -1 }* ( 1 - yp^{ -shape } ),
                  scale* shape^{ -2 }* ( 1 - yp^{ -shape } ) -
                  scale* shape^{ -1 }* yp^{ -shape }* log( yp ) )
          errors <- cbind( errors, dz %*% error.covariance %*% dz )
        } else {
          ## Gumbel distribution
          dz <- c( 1, -log( yp ) )
          errors <- cbind( errors,
                          dz %*% error.covariance[ 1 : 2, 1 : 2 ] %*%
                          dz )
        }
      }
      names( errors ) <- c( "location", "scale", "shape",
                           paste0( return.period, ".rlevel" ) )
    }
    res.optim$se <- errors
  } else {
    res.optim$se <- rep( NaN, ( 3 + length( return.period ) ) )
  }
  ## Naming of the resulting fit parameter (necessary for a correct
  ## conversion with as.fevd)
  names( res.optim$par ) <- c( "location", "scale", "shape" )
  ## introducing a new data type for handling fits done with climex
  class( res.optim ) <- c( "list", "climex.fit.gev" )
  
  ## adding the return levels
  res.optim$return.level <- Reduce(
      c, lapply( return.period,
                function( y )
                  climex::return.level( res.optim, y
                                       )$return.level ) )
  names( res.optim$return.level ) <- paste0( return.period, ".rlevel" )
  res.optim$x <- x

  if ( !silent )
    summary( res.optim )
  return( res.optim )
}

##' @title Robust maximum-likelihood fit of the GPD distribution
##'
##' @description This function fits the Generalized Pareto distribution
##' (GPD) to the supplied data, which have to be threshold exceedances
##' with the corresponding threshold already subtracted. The
##' determination of the starting point for the optimization and the
##' calculation of the return level and the all the corresponding
##' estimates of the fitting errors will be done internally.
##'
##' @details The optimization is performed using the augmented Lagrangian
##' method. Within this framework the log-likelihood function of the GPD
##' gets augmented with N+2 constraints, where N is the
##' number of points in the time series. N+1 of those constraints ensure
##' the log-likelihood (containing two logarithms) to be always defined.
##' The remaining constraints ensures for the shape parameter to be
##' always bigger than -1 for the maximum likelihood to be defined in the
##' first place. The penalty in the log-likelihood function is the sum of
##' all squared constrain violations plus an additional term linear in
##' the constraint violation to ensure well-conditioning. Using this
##' penalty term the problem becomes unconstrained again and can be
##' solved using \code{\link{stats::optim}}. After each of those inner
##' routines the weighting parameter of the penalty is being increased
##' until some convergence conditions are fulfilled.
##'
##' Since it usually takes just four to five outer iterations this
##' functions needs only double the time a pure call to the stats::optim
##' function would need.
##'
##' The 'total.length' argument refers to the length of the original time
##' series before the thresholding was applied. If present it will be used
##' to calculate the maximum likelihood estimate of the probability of an
##' observation to be a threshold exceedance (necessary to determine the
##' estimation errors for the calculated return levels). Else an
##' estimator based on mean number of exceedances per year will be
##' used.
##'
##'
##' If the user instead wants to fit just the exponential distribution
##' and not the entire GP distribution, the shape parameter of the
##' `initial' has to be set to 0. But in practice this is strongly
##' discouraged since it will yield inferior results.
##' 
##' I found the Nelder-Mead method to be more robust to starting
##' points more far away from the global optimum. This also holds
##' for the inner routine of the augmented Lagrangian method. Since
##' other routines, like CG and BFGS only cause problems in the
##' extreme value analysis, there won't be an option to choose them
##' in this package.
##' 
##' @param x Threshold exceedances with the threshold already subtracted.
##' @param initial Initial values for the GPD parameters. Has to be
##'   provided as 2x1 vector. If NULL the parameters are estimated
##'   with the function \code{\link{likelihood.initials}}. If the
##'   shape parameter is set to 0 the exponential distribution instead
##'   of the GP one is fitted. But this its strongly discouraged to do
##'   so! Default = NULL
##' @param threshold Optional threshold used to extract the exceedances
##' x from the original series. If present it will be added to the
##' return level to produce a value which fits to underlying time series.
##' Default = NULL.
##' @param likelihood.function Function which is going to be optimized.
##' Default: \code{\link{likelihood}}
##' @param gradient.function If NULL a finite difference method is
##' invoked. To use the derived formula from the GPD likelihood gradient
##' provide \code{\link{likelihood.gradient}}.
##' Default = climex:::likelihood.gradient.
##' @param error.estimation Method for calculating the standard errors of
##' the fitted results. Using the option "MLE" the errors of the GPD
##' parameters will be calculated as the square roots of the diagonal
##' elements of the inverse of the hessian matrix calculated with the MLE
##' of the GPD parameters. The standard error of the return level is
##' calculated using the Delta method and the maximum likelihood
##' estimates (MLE) of the GPD parameters.
##' 
##' Alternative one can use Monte Carlo simulations with "MC" for which
##' monte.carlo.sample.size samples of the same size as x will be drawn
##' from a GPD distribution constituted by the obtained MLE of the GPD
##' parameters of x. The standard error is then calculated via the square
##' of the variance of all fitted GPD parameters and calculated return
##' levels.
##'
##' Sometimes the inversion of the hessian fails (since the are some NaN
##' in the hessian) (which is also the reason why the ismev package
##' occasionally does not work). In such cases the Monte Carlo method is
##' used. Option "none" just skips the calculation of the error.
##' Default = "MLE".
##' @param monte.carlo.sample.size Number of samples used to obtain the
##' Monte Carlo estimate of the standard error of the fitting.
##' Default = 100.
##' @param return.period Quantiles at which the return level is going to
##' be evaluated. Class "numeric". Default = 100.
##' @param total.length Uses the maximum likelihood estimator to
##'   calculate the probability of a measurement to be an
##'   exceedance. Else an estimate based on the mean number of
##'   exceedances in the available years (time stamps of the class
##'   'xts' time series) will be used. Default = NULL. 
##' @param silent Determines whether or not warning messages shall be
##' displayed and results shall be reported. Default = TRUE.
##' @param ... Additional arguments for the optim() function.
##' 
##' @family optimization
##' 
##' @return Output of the optim function with class ==
##' c( "list", "climex.fit.gpd" )
##' \itemize{
##'  \item{ par = MLE of the GPD parameters }
##'  \item{ value = Value of the negative log-likelihood
##' evaluated at the MLE }
##'  \item{ counts = Number of evaluations of the likelihood
##' function and its gradient during optimization (inner routine) }
##'  \item{ outer.iteration = Number of updates of the penalty and
##' the Lagrangian parameter to fine-tune the impact of the
##' constraints on the optimization (outer routine) }
##'  \item{ return.level = Estimate of the return levels at the provided
##' return periods }
##'  \item{ se = Standard error of the GPD parameters and the return
##' levels }
##'  \item{ x = Threshold exceedances }
##'  \item{ threshold = Value which had to be exceeded }
##'  \item{ control = Parameter and options used during optimization }
##' }
##' @author Philipp Mueller
##' @export
##' @importFrom xts xts
##' @importFrom alabama auglag
##' @examples
##' potsdam.anomalies <- anomalies( temp.potsdam )
##' potsdam.extremes <- threshold( potsdam.anomalies, threshold = 10,
##'                                decluster = TRUE )
##' fit.gpd( potsdam.extremes )
fit.gpd <- function( x, initial = NULL, threshold = NULL,
                    likelihood.function = likelihood,
                    gradient.function = climex:::likelihood.gradient,                    
                    error.estimation = c( "MLE", "MC", "none" ),
                    monte.carlo.sample.size = 100, return.period = 100,
                    total.length = NULL, silent = TRUE, ... ){
  ## Default values if no initial parameters are supplied
  if ( is.null( initial ) )
    initial <- likelihood.initials( x, model = "gpd" )
  if ( is.null( error.estimation ) )
    error.estimation <- "MLE"
  error.estimation <- match.arg( error.estimation )
  ## Optimization
  ## 
  ## I found the Nelder-Mead method to be more robust to starting
  ## points more far away from the global optimum. This also holds
  ## for the inner routine of the augmented Lagrangian method. Since
  ## other routines, like CG and BFGS only cause problems in the
  ## extreme value analysis, there won't be an option to choose them
  ## in this package.

  if ( as.numeric( initial[ 2 ] ) != 0 ){
    ## Maximization of the negative log-likelihood of the GP
    ## distribution.
    ## The augmented Lagrangian method allowing non-linear constraints
    ## will be used in here. The code depends of the 'alabama' package.
    ## In principle I could also integrate the routine in here, but let's
    ## stick to the Linux principle.
    ## The Rsolnp package did not performed as well as the alabama one.
    ## It takes two orders of magnitude longer and gets stuck for
    ## certain initial parameter combinations.
    ## For shape parameter equal to zero only the first constraint is
    ## relevant and the other ones can not be violated anymore. So no
    ## need to remove them.
    ##
    ## The auglag optimization function seems to produce results
    ## reliable only up to the 5E-4 and I can't see why this is
    ## happening. Adding various additional options and tolerances to
    ## both auglag and optim doesn't change the matter. Since these
    ## deviations are minor ones and the actual MLE estimates of the GEV
    ## and GP parameters are way bigger, I will just leave it this
    ## way. 
    suppressWarnings(
        res.alabama <- auglag(
            par = initial, fn = likelihood.function,
            gr = gradient.function,
            hin = function( parameters, x.in, ... ){
              return( as.numeric(
                  c( parameters[ 1 ] - .03,
                    .95 + parameters[ 2 ]* ( x.in )/ parameters[ 1 ],
                    parameters[ 2 ] + .95 ) ) ) },
            control.outer = list( trace = !silent,
                                 method = "Nelder-Mead" ),
            x.in = x, model = "gpd" ) )
  } else {
    ## If the shape parameter in the initial parameter combination was
    ## set to zero, the users want to fit to the pure exponential
    ## distribution instead to the GP distribution. Some changes to
    ## the auglag call are necessary since the algorithm can 'escape'
    ## the exponential likelihood if it encounters it during the very
    ## beginning of the optimization.
    suppressWarnings(
        res.alabama <- auglag(
            par = initial[ 1 ], fn =  function( parameters, ... ){
              likelihood( c( as.numeric( parameters ), 0 ), ... ) },
            gr = function( parameters, ... ){
              likelihood.gradient(
                  c( as.numeric( parameters ), 0 ), ... )[ 1 ] },
            hin = function( parameters, x.in, ... ){
              return( parameters[ 1 ] - .03 ) },
            control.outer = list( trace = !silent,
                                 method = "Nelder-Mead" ),
            x.in = x, model = "gpd" ) )
    ## Add a couple of zeros for the shape parameter to ensure
    ## compatibility to the other parts of the code.
    res.alabama$par <- c( res.alabama$par, 0 )
    res.alabama$gradient <- c( res.alabama$gradient, 0 )
    dummy.hessian <- matrix( rep( 0, 4 ), nrow = 2, ncol = 2 )
    dummy.hessian[ 1 ] <- res.alabama$hessian
    res.alabama$hessian <- dummy.hessian
  }
  ## There is no need for the user to deal with all the outputs of the
  ## auglag function. So let's reduce them. 
  res.optim <- list( par = res.alabama$par,
                    value = res.alabama$value,
                    gradient = res.alabama$gradient,
                    counts = res.alabama$counts,
                    outer.iterations = res.alabama$outer.iterations,
                    x = x,
                    threshold = threshold,
                    control = list(
                        initial = initial,
                        likelihood.function = likelihood.function,
                        gradient.function = gradient.function,
                        hessian = res.alabama$hessian,
                        monte.carlo.sample.size =
                          monte.carlo.sample.size,
                        error.estimation = error.estimation,
                        total.length = total.length,
                        return.period = return.period ) )
  ## For an adequate calculation of the return level
  ## Error estimation
  
  if ( error.estimation != "none" ){
    ## If the shape parameter is exactly zero and the Gumbel
    ## distribution was fitted, the third row and column were just
    ## augmented by 0.
    if ( res.optim$par[ 2 ] != 0 ){
      error.covariance <- try( solve( res.optim$control$hessian ),
                              silent = silent )
    } else {
      ## Omit the augmentation
      error.covariance <- try( solve(
          res.optim$control$hessian[ 1 ] ),
          silent = silent )
      ## Augment the result again to ensure compatibility
      if ( class( error.covariance ) != "try-error" ){
        dummy.matrix <- matrix( rep( 0, 4 ), nrow = 2, ncol = 2 )
        dummy.matrix[ 1 ] <- error.covariance
        error.covariance <- dummy.matrix
      }
    }
    if ( class( error.covariance ) == "try-error" ||
         error.estimation == "MC" ||
         any( is.nan( res.optim$control$hessian ) ) ){
      parameter.estimate <- res.optim$par
      number.of.samples <- monte.carlo.sample.size
      ## Draw a number of samples and fit the GPD parameters for all
      ## of them.
      samples.list <- lapply( 1 : number.of.samples, function( y )
        climex:::revd( length( x ), scale = parameter.estimate[ 1 ],
                      shape = parameter.estimate[ 2 ], model = "gpd",
                      silent = TRUE ) )
      
      suppressWarnings(
          samples.fit <- try( lapply( samples.list, function( y )
            auglag( par = likelihood.initials( y, model = "gpd" ),
                   fn = likelihood.function,
                   gr = gradient.function,
                   hin = function( parameters, x.in, ... ){
                     return( as.numeric(
                         c( parameters[ 1 ] - .03,
                           .95 + parameters[ 2 ]*
                           ( x.in )/ parameters[ 1 ],
                           parameters[ 2 ] + .95 ) ) ) },
                   control.outer = list( trace = FALSE,
                                        method = "Nelder-Mead" ),
                   x.in = y, model = "gpd" )$par ) ) )
      
      if ( class( samples.fit ) == "try-error" ){
        errors <- c( NaN, NaN, NaN )
      } else {
        errors <- data.frame(
            sqrt( stats::var( Reduce( rbind, samples.fit )[ , 1 ] ) ),
            sqrt( stats::var( Reduce( rbind, samples.fit )[ , 2 ] ) ) )
        for ( rr in 1 : length( return.period ) )
          errors <- cbind(
              errors,
              sqrt( stats::var( Reduce(
                               c,
                               lapply( samples.fit,
                                      function( z )
                                        return.level(
                                            z,
                                            return.period =
                                              return.period[ rr ],
                                            error.estimation = "none",
                                            model = "gpd",
                                            threshold = threshold,
                                            total.length =
                                              total.length,
                                            thresholded.time.series =
                                              x, 
                                            silent = silent
                                        )$return.level ) ) ) ) )
      }
      names( errors ) <- c( "scale", "shape",
                           paste0( return.period, ".rlevel" ) )
    } else {
      ## Calculating the errors using the MLE
      ## The supplied 'return.period' are of the unit 'per year'.
      ## In order to be used with the GP return levels they have to
      ## be transformed in 'per observation (of the original series)'.
      if ( !is.null( total.length ) ){
        ## The maximum likelihood estimate of the probability of an
        ## exceedance to occur per year will be used.
        zeta <- length( x )/ total.length
        m <- return.period* 365.25* zeta
      } else {
        ## m-observation return level = return.period* the mean number of
        ## exceedance per year. This way the unit of the provided return
        ## level and its error are  not 'per observation' but 'per year'.
        ## In this step we harness the power of the 'xts' package
        m <- return.period*
          mean( apply.yearly( x, function( y ) length( y ) ) )
        zeta <- NULL
      }
      errors.aux <- sqrt( diag( error.covariance ) ) # GPD parameters
      errors <- data.frame( errors.aux[ 1 ], errors.aux[ 2 ] )
      ## Delta method for the return level
      parameter.estimate <- res.optim$par
      ## Formula according to Stuart Coles p. 82
      for ( rr in 1 : length( return.period ) ){
        scale <- parameter.estimate[ 1 ]
        shape <- parameter.estimate[ 2 ]
        if ( is.null( zeta ) ){
          ## Calculate the exceedance probability
          zeta <- m[ rr ]/ ( return.period[ rr ]* 365.25 )
        }
        if ( shape != 0 ){
          ## GP distribution
          dz <- c( scale* m^shape* zeta^{ shape - 1 },
          ( m[ rr ]^ shape - 1 )/ shape,
          -scale* shape^{ -2 }*( m[ rr ]^shape - 1 ) +
          scale/shape*m[ rr ]^shape* log( m[ rr ] ) )
          ## Generate a dummy variance matrix to incorporate the
          ## uncertainty of zeta.
          error.matrix <- matrix( rep( 0, 9 ), nrow = 3, ncol = 3 )
          if ( !is.null( total.length ) ){
            ## If the total length of the underlying series BEFORE
            ## thresholding is provided, we are glad to use it.
            error.matrix[ 1, 1 ] <- zeta*( 1 - zeta )/ total.length
          } else {
            ## If not we have to estimate it using the MLE of zeta
            ## number of exceedances/ total length.
            error.matrix[ 1, 1 ] <- zeta^2*( 1 - zeta )/ length( x )
          }
          error.matrix[ 2 : 3, 2 : 3 ] <- error.covariance 
          errors <- cbind( errors, dz %*% error.matrix %*% dz )
        } else {
          ## Exponential distribution
          dz <- c( scale/ zeta, log( m[ rr ] ) )
          ## Generate a dummy variance matrix to incorporate the
          ## uncertainty of zeta.
          error.matrix <- matrix( rep( 0, 4 ), nrow = 2, ncol = 2 )
          if ( !is.null( total.length ) ){
            ## If the total length of the underlying series BEFORE
            ## thresholding is provided, we are glad to use it.
            error.matrix[ 1, 1 ] <- zeta*( 1 - zeta )/ total.length
          } else {
            ## If not we have to estimate it using the MLE of zeta
            ## number of exceedances/ total length.
            error.matrix[ 1, 1 ] <- zeta^2*( 1 - zeta )/ length( x )
          }
          error.matrix[ 2, 2 ] <- as.numeric( errors[ 1 ] )
          errors <- cbind( errors, dz %*% error.matrix %*% dz )
        }
      }
      names( errors ) <- c( "scale", "shape",
                           paste0( return.period, ".rlevel" ) )
    }
    res.optim$se <- errors
  } else {
    ## No error estimation required.
    res.optim$se <- rep( NA, length( return.period ) + 2 )
  }
  names( res.optim$par ) <- c( "scale", "shape" )
  ## introducing a new data type for handling fits done with climex
  class( res.optim ) <- c( "list", "climex.fit.gpd" )

  ## adding the return levels
  res.optim$return.level <- Reduce(
      c, lapply( return.period, function( y )
        climex::return.level( res.optim, y, error.estimation = "none",
                             model = "gpd", threshold = threshold,
                             total.length = total.length,
                             silent = silent )$return.level ) )
  names( res.optim$return.level ) <- paste0( return.period, ".rlevel" )
  return( res.optim )
}

##' @title Calculated the negative log likelihood of the GEV or GPD
##' function.
##'
##' @details This function is only meant to work with constant parameters
##' and no covariats. x.in is not called "x" anymore since the call
##' grad( func = likelihood, x = parameters, ... ) wouldn't be possible.
##'
##' @param parameters Vector containing the location, scale and shape
##' parameter for the GEV or the scale and shape parameter for the GPD.
##' If NULL the \code{\link{likelihood.initials}} is used. Default = NULL
##' @param x.in Time series.
##' @param model Determining whether to calculate the initial parameters
##' of the GEV or GPD function. Default = "gev"
##' 
##' @family optimization
##'
##' @export
##' @return Numerical value of the negative log likelihood.
##' @author Philipp Mueller
likelihood <- function( parameters = NULL, x.in,
                       model = c( "gev", "gpd" ) ){
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

  suppressWarnings( {
    if ( model == "gev" ){
      if ( shape == 0 ){
        ## Using the Gumbel distribution. But only when the shape parameter
        ## is exactly 0
        negloglikelihood <- length( x.in )*log( scale ) +
          sum( y )/ scale + sum( exp( -y/ scale ) )
      } else {
        negloglikelihood <- length( x.in )* log( scale ) +
          alpha* sum( log( z ) ) + sum( z^{ -1/ shape } )
      }
    } else {
      if ( shape == 0 ){
        ## Again: just for a shape exactly equal to 0
        negloglikelihood <- length( x.in )* log( scale ) +
          sum( y )/scale 
      } else {
        negloglikelihood <- length( x.in )* log( scale ) +
          alpha* sum( log( z ) )
      }
    }
  } )
  names( negloglikelihood ) <- NULL
  return( negloglikelihood )
}

##' @title Calculated the augmented negative log likelihood of the
##' GEV or GPD function.
##'
##' @description This function uses the \code{\link{likelihood}}
##' function and adds the linear constraints used in
##' \code{\link{fit.gev}} and \code{\link{fit.gpd}} to produce the
##' augmented Lagrangian version of the GEV or GP negative
##' log-likelihood function. 
##'
##' @details A convenience function not used by the fitting routines.
##'
##' It is only meant to work with constant parameters
##' and no covariates.
##'
##' 'x.in' is not called "x" anymore since the call
##' grad( func = likelihood, x = parameters, ... ) wouldn't be possible.
##'
##' @param parameters Vector containing the location, scale and shape
##' parameter for the GEV or the scale and shape parameter for the GPD.
##' If NULL the \code{\link{likelihood.initials}} is used. Default = NULL
##' @param x.in Time series.
##' @param model Determining whether to calculate the initial parameters
##' of the GEV or GPD function. Default = "gev"
##' @param lagrangian.multiplier Lagrangian multipliers used to weight
##' the linear contribution of the constraints. In most cases all of them
##' are zero, since optimization of the GEV/GP likelihood usually doesn't
##' take place inside a region of constraint violations. When supplying
##' this parameter it has to have the same length as present number of
##' constraints: number of points in x.in + 2.
##' Default = 0 for all constraints.
##' @param penalty.parameter Penalty parameter used to weight the
##' quadratic contribution of the constraints. In the end of a typical
##' constrained GEV or GP optimization this parameter is 1000.
##' Default = 1000.
##' 
##' @family optimization
##'
##' @export
##' @return Numerical value of the augmented negative log likelihood.
##' @author Philipp Mueller
likelihood.augmented <- function( parameters, x.in,
                                 model = c( "gev", "gpd" ),
                                 lagrangian.multiplier =
                                   rep( 0, length( x.in ) + 2 ),
                                 penalty.parameter = 1000 ){
  if ( missing( model ) )
    model <- "gev"
  ## Defining the constraints
  constraints <- function( parameters, x.in, model ){
    if ( model == "gev" ){
      return( as.numeric(
          c( parameters[ 2 ] - .03,
            .95 + parameters[ 3 ]*
            ( x.in - parameters[ 1 ])/ parameters[ 2 ],
            parameters[ 3 ] + .95 ) ) )
    } else {
      return( as.numeric(
          c( parameters[ 1 ] - .03,
            .95 + parameters[ 2 ]*
            ( x.in )/ parameters[ 1 ],
            parameters[ 2 ] + .95 ) ) )
      
    } }
  ## Right here I will use the code of the alabama::auglag2 function.
  constraint.violation <- constraints( parameters, x.in, model )
  constraint.violation.threshold <- constraint.violation
  ## Mark a constraint as inactive if its value is smaller than
  ## the initial Lagrangian parameter divided by the scale
  ## parameter for the penalty (Lagrangian) term. In addition
  ## replace all values of the constraint function exceeding this
  ## threshold by the threshold itself.
  inactive <- ( 1 : length( constraint.violation ) )[
    ( constraint.violation > lagrangian.multiplier[
                                 1 : length( constraint.violation ) ]/
      penalty.parameter ) ]
  constraint.violation.threshold[ inactive ] <-
    lagrangian.multiplier[ inactive ] / penalty.parameter
  ## Augmenting the Lagrangian to penalize constraint violations
  ## by squaring the infeasibilites and an explicit estimate of the
  ## Lagrange multipliers to avoid a systematic perturbation.
  ## Nocedal P. 514; (17.36)
  return( as.numeric(
      climex::likelihood( parameters, x.in = x.in, model = model ) -
      sum( lagrangian.multiplier* constraint.violation.threshold ) + 
      penalty.parameter/2 * sum( constraint.violation.threshold *
                                 constraint.violation.threshold ) ) )
}


##' @title Calculates the gradient of the negative log likelihood of the
##' GEV or GPD function.
##'
##' @details 
##'
##' @param parameters Vector containing the location, scale and shape
##' parameter for the GEV model or the scale and shape parameter for the
##' GPD one.
##' @param x.in Time series or numerical vector containing the extreme
##' events.
##' @param model Determining whether to calculate the initial parameters
##' of the GEV or GPD function. Default = "gev".
##' 
##' @family optimization
##'
##' @return Numerical vector containing the derivative of the negative
##' log-likelihood in (location, scale, shape for GEV) or (scale,
##' shape for GPD) direction.
##' @author Philipp Mueller
likelihood.gradient <- function( parameters, x.in,
                                model = c( "gev", "gpd" ) ){
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

  ## Calculating the gradient
  if ( model == "gev" ){
    gradient <- numeric( 3 )
    if ( shape == 0 ){
      ## If the shape parameter is identical to zero, use the gradient
      ## of the Gumbel distribution instead
      z <- y/ scale
      gradient[ 1 ] <- -length( x.in )/ scale + sum( exp( -z ) )/ scale
      gradient[ 2 ] <- length( x.in )/ scale - sum( y )/ scale^2 +
        sum( y* exp( -z )/ scale^2 )
      gradient[ 3 ] <- 0
    } else {
      z <- 1 + y* gamma
      gradient[ 1 ] <- sum( z^{-alpha}/ scale ) - sum( alpha* gamma/ z )
      gradient[ 2 ] <- length( x.in )/ scale -
        sum( alpha* shape* y/ ( scale^2 * z ) ) +
        sum( z^{ - alpha }*y/ scale^2 )
      gradient[ 3 ] <- sum( alpha* y/ ( scale* z ) ) -
        sum( log( z )/ shape^ 2 ) +
        sum( z^{ -1/ shape }* log( z )/ shape^ 2 -
             y/ ( scale* shape* z^alpha ) )
    }
  } else {
    gradient <- numeric( 2 )
    if ( shape == 0 ){
      ## Only if the shape parameter is exactly zero use the
      ## gradient of the exponential function
      gradient[ 1 ] <- length( x.in )/ scale - sum( y/ scale^ 2 )
      gradient[ 2 ] <- 0
    } else {
      z <- 1 + y* gamma
      gradient[ 1 ] <- length( x.in )/scale -
        alpha* shape* sum( x.in/ ( z* scale^2) )
      gradient[ 2 ] <- -1/ shape^2* sum( log( z ) ) +
        alpha/ scale* sum( x.in/ z )
    }
  }
  return( gradient )
}

##' @title Calculated the gradient of the augmented negative log
##' likelihood of the GEV or GPD function.
##'
##' @description This function uses the \code{\link{likelihood.gradient}}
##' function and adds the linear constraints used in
##' \code{\link{fit.gev}} and \code{\link{fit.gpd}} to produce the
##' augmented Lagrangian version of the GEV or GP negative
##' log-likelihood function. 
##'
##' @details A convenience function not used by the fitting routines.
##'
##' It is only meant to work with constant parameters
##' and no covariates.
##'
##' @param parameters Vector containing the location, scale and shape
##' parameter for the GEV or the scale and shape parameter for the GPD.
##' If NULL the \code{\link{likelihood.initials}} is used. Default = NULL
##' @param x.in Time series.
##' @param model Determining whether to calculate the initial parameters
##' of the GEV or GPD function. Default = "gev"
##' @param lagrangian.multiplier Lagrangian multipliers used to weight
##' the linear contribution of the constraints. In most cases all of them
##' are zero, since optimization of the GEV/GP likelihood usually doesn't
##' take place inside a region of constraint violations. When supplying
##' this parameter it has to have the same length as present number of
##' constraints: number of points in x.in + 2, with the last two
##' constraints handling the lower bound of the scale and the shape
##' parameter. Default = 0 for all constraints.
##' @param penalty.parameter Penalty parameter used to weight the
##' quadratic contribution of the constraints. In the end of a typical
##' constrained GEV or GP optimization this parameter is 1000.
##' Default = 1000.
##' 
##' @family optimization
##'
##' @export
##' @return Numerical value of the gradient of the augmented negative
##' log likelihood.
##' @author Philipp Mueller
likelihood.gradient.augmented <- function( parameters, x.in,
                                          model = c( "gev", "gpd" ),
                                          lagrangian.multiplier = 
                                            rep( 0, length( x.in ) + 2 ),
                                          penalty.parameter = 1000 ){
  ## The augmented gradient consists of the GEV/GP likelihood
  ## gradient and some additive term for the constraint violations.
  ## Defining the constraints
  constraints <- function( parameters, x.in, model ){
    if ( model == "gev" ){
      return( as.numeric(
          c( parameters[ 2 ] - .03,
            .95 + parameters[ 3 ]*
            ( x.in - parameters[ 1 ])/ parameters[ 2 ],
            parameters[ 3 ] + .95 ) ) )
    } else {
      return( as.numeric(
          c( parameters[ 1 ] - .03,
            .95 + parameters[ 2 ]*
            ( x.in )/ parameters[ 1 ],
            parameters[ 2 ] + .95 ) ) )
    }
  }
  gradient <- climex:::likelihood.gradient( parameters = parameters,
                                           x.in = x.in, model = model )
  ## Check whether a constraint is violated and at its contribution to
  ## the gradient
  if ( model == "gev" ){
    for ( ii in 1 : length( x.in ) ){
      constraint <- parameters[ 3 ]*
        ( x.in[ ii ] - parameters[ 1 ])/ parameters[ 2 ]
      if ( constraint <= -.95 ){
        gradient[ 1 ] <- gradient[ 1 ] -
          lagrangian.multiplier[ ii ]* parameters[ 3 ]/ parameters[ 2 ] -
          penalty.parameter* constraint* parameters[ 3 ]/ parameters[ 2 ]
        gradient[ 2 ] <- gradient[ 2 ] -
          lagrangian.multiplier[ ii ]* parameters[ 3 ]*
          ( x.in[ ii ] - parameters[ 1 ] )/ parameters[ 2 ]^ 2 -
          penalty.parameter* constraint* parameters[ 3 ]*
          ( x.in[ ii ] - parameters[ 1 ] )/ parameters[ 2 ]^ 2
        gradient[ 3 ] <- gradient[ 3 ] +
          lagrangian.multiplier[ ii ]*
          ( x.in[ ii ] - parameters[ 1 ] )/ parameters[ 2 ] +
          penalty.parameter* constraint*
          ( x.in[ ii ] - parameters[ 1 ] )/ parameters[ 2 ]
      }
    }
    if ( parameters[ 2 ] <= .05 ){
      gradient[ 2 ] <- gradient[ 2 ] +
        lagrangian.multiplier[ length( x.in ) + 1 ] +
        penalty.parameter* parameters[ 2 ]
    }
    if ( parameters[ 3 ] <= -.95 ){
      gradient[ 3 ] <- gradient[ 3 ] +
        lagrangian.multiplier[ length( x.in ) + 2 ] +
        penalty.parameter* parameters[ 3 ]
    }
  } else {
    ## Generalized Pareto version
    for ( ii in 1 : length( x.in ) ){
      constraint <- parameters[ 2 ]* x.in[ ii ]/ parameters[ 1 ]
      if ( constraint <= -.95 ){
        gradient[ 1 ] <- gradient[ 1 ] -
          lagrangian.multiplier[ ii ]* constraint/ parameters[ 1 ] -
          penalty.parameter* constraint^ 2/ parameters[ 1 ]
        gradient[ 2 ] <- gradient[ 2 ] +
          lagrangian.multiplier[ ii ]* x.in[ ii ]/ parameters[ 1 ] +
          penalty.parameter* constraint* x.in[ ii ]/ parameters[ 1 ]
      }
    }
    if ( parameters[ 1 ] <= .05 ){
      gradient[ 1 ] <- gradient[ 1 ] +
        lagrangian.multiplier[ length( x.in ) + 1 ] +
        penalty.parameter* parameters[ 1 ]
    }
    if ( parameters[ 2 ] <= -.95 ){
      gradient[ 2 ] <- gradient[ 2 ] +
        lagrangian.multiplier[ length( x.in ) + 2 ] +
        penalty.parameter* parameters[ 2 ]
    }
  }   
  return( gradient )
}

##' @title Estimates the initial GEV or GPD parameters of a time series.
##'
##' @details Two main methods are used for the estimation: the L-moments
##' method of Hosking & Wallis  and an estimation using
##' the first two moments of the Gumbel distribution. For the later one
##' a modification was added: By looking at skewness of the time series x
##' and with respect to some heuristic thresholds a shape parameter
##' between -.4 and .2 is assigned for the GEV distribution. In case of
##' the GP one, the sign of the skewness matches the sign of the series'
##' shape parameter.
##'
##' Warning: both methods do not work for samples with diverging (or
##' pretty big) mean or variance.
##'
##' If no working initial parameter combination could be found using
##' those methods, the function will perform a constrained random
##' walk on the parameters until a working pair is found.
##'
##' @param x Time series/numeric.
##' @param model Determining whether to calculate the initial parameters
##' of the GEV or GPD function. Default = "gev"
##' @param use.skewness Determines if the skewness is getting used to
##' determine the initial shape parameter. Default = TRUE.
##'
##' @family optimization
##'
##' @export
##' @return Numerical vector containing the c( location, scale, shape )
##' estimates for method = "gev" or the c( scale, shape ) estimates for
##' method = "gpd".
##' @author Philipp Mueller
likelihood.initials <- function( x, model = c( "gev", "gpd" ),
                                use.skewness = TRUE ){
  if ( missing( model ) )
    model <- "gev"
  model <- match.arg( model )
  if ( model == "gev" ){
    ## Method of moments
    sc.init <- sqrt( 6* stats::var( x ) )/ pi
    loc.init <- mean( x ) - 0.57722* sc.init
    if ( use.skewness ){
      x.skewness <- moments::skewness( x )
      ## When, for some reason, the time series consists of just a
      ## sequence of one unique number the calculation of the skewness
      ## returns NaN and the function throws an error
      if ( is.nan( x.skewness ) ){
        ## If you can not calculate the skewness, set the shape parameter
        ## to .001
        x.skewness <- .8
      }
      if ( x.skewness > 4 ){
        sh.init <- .75
      } else if ( x.skewness > 2.5 ){
        sh.init <- .6
      } else if ( x.skewness > 1.6 ){
        sh.init <- .35
      } else if ( x.skewness > .7 ){
        sh.init <- .001
      } else if( x.skewness > .1 ){
        sh.init <- -.1
      } else if ( x.skewness > -.2 ){
        sh.init <- -.2775
      } else  if ( x.skewness > -1 ){
        sh.init <- -.4
      } else {
        sh.init <- -.75
      }
    } else {
      ## No modification with respect to the ismev package
      sh.init <- .1
    }
    initial.mom <- c( loc.init, sc.init, sh.init )
    ## Approximationg using the Lmoments method of Hosking, Wallis
    ## and Wood (1985). Therefore I will use the interior of the
    ## extRemes:::initializer.lmoments function
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
  } else {
    ## model == "gpd"
    ## Approximation using the Lmoments method.
    ## Since I decided to not calculate the threshold inside the
    ## fitting (or even inside this function - for the sake of the
    ## Linux principle), I will use the interior of the
    ## extRemes:::initializer.lmoments function
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
    ## Approximation using the method of moments
    sc.init <- sqrt( var( x ) )
    if ( use.skewness ){
      ## For positive shape parameters the skewness to the time series is
      ## positive as well. For negative it's negative. This way at least
      ## the sign of the shape (but unfortunately not the magnitude) can
      ## be estimated
      x.skewness <- moments::skewness( x )
      ## For series of absurdly high values (e.g. big shape and scale
      ## parameters) the skewness function can return NaN
      if ( !is.nan( x.skewness ) && x.skewness < 0 ){
        x.skewness <- 1.5
      }
      if ( x.skewness > 4.5 ){
        sh.init <- .75
      } else if ( x.skewness > 2.5 ){
        sh.init <- .5
      } else if ( x.skewness > 1.8 ){
        sh.init <- .25
      } else if ( x.skewness > 1.5 ){
        sh.init <- .05
      } else if ( x.skewness > 1.2 ){
        sh.init <- -.05
      } else if ( x.skewness > .8 ){
        sh.init <- -.25
      } else if ( x.skewness > .2 ){
        sh.init <- -.5
      } else {
        sh.init <- -.75
      }
    } else {
      sh.init <- .1
    }
    initial.mom <- c( sc.init, sh.init )
  }
  ## Calculate the likelihood of the initial parameter obtained by
  ## the two methods and pick the one yielding the lowest value.
  initials <- list( initial.lmom, initial.mom )
  initials.likelihood <- Reduce( c, lapply( initials, function( ii )
    suppressWarnings( likelihood( as.numeric( ii ), x.in = x,
                                 model = model ) ) ) )
  if ( !all( is.nan( as.numeric( initials.likelihood ) ) ) ){
    return( as.numeric( initials[[ which.min(
                                     initials.likelihood ) ]] ) )
  } else {
    ## None of the methods above yielded reasonable initial parameters.
    ## In order to perform the optimization after all, let's slightly
    ## change the values until they can be evaluated.
    x.initial <- initial.mom
    ## In order to ensure that this functions provides the same values
    ## in a deterministic sense, a seed is set. This pseudo-random
    ## exploration of the negative log-likelihood space is necessary
    ## since we would had to tune three parameters at a time (which is
    ## a quite tedious thing to do).
    set.seed( 33221 )
    suppressWarnings({
      while ( is.na( climex::likelihood( x.initial, x.in = x,
                                        model = model ) ) ){
        if ( model == "gev" ){
          x.initial[ 1 ] <- x.initial[ 1 ] +
            rnorm( 1, sd = .2* x.initial[ 2 ] )
          x.initial[ 2 ] <- max( x.initial[ 2 ] +
                                 rnorm( 1, sd = .2* x.initial[ 2 ] ),
                                .05 ) 
          x.initial[ 3 ] <-
            min( -.95, max( .95, x.initial[ 3 ] +
                                 rnorm( 1, sd = .2* x.initial[ 2 ] ) ) )
        } else {
          x.initial[ 1 ] <- max( x.initial[ 1 ] +
                                 rnorm( 1, sd = .2* x.initial[ 1 ] ),
                                .05 ) 
          x.initial[ 2 ] <-
            min( -.95, max( .95, x.initial[ 2 ] +
                                 rnorm( 1, sd = .2* x.initial[ 1 ] ) )
                                ) } }
    } )
    return( as.numeric( x.initial ) )
  }
}
