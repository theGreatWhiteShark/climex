##' @title Separates the input into blocks of equal size and returns the maximum or minimum of the block as result.
##'
##' @details If 'separation.mode' is set to "years" the data is separated according to it's time stamps. If not the size of a block is determined by the 'block.length' parameter or calculated via the 'block.number' parameter. For calculating the mean of the blocks have a look at the \code{\link{stats::ave}} function.
##'
##' @param input.bulk Provided data of class 'xts'.
##' @param block.number Specifies the number of blocks the input data is going to be separated in.
##' @param block.length Length of the blocks. For the sake of simplicity the last block is not forced to match the length of the other plots.
##' @param block.mode This parameter determines if the maximum "max" or the minimum "min" of a block shall be returned. Default: "max".
##' @param separation.mode "years" is used to split the data according to its date values instead. If 'block.length' or 'block.number' is specified this argument will not be considered and set to "none". Default = "years".
##' @return Of class 'xts'.
##' @family extremes
##' @author Philipp Mueller
##' @export
##' @import xts
##'
##' @examples
##' block( temp.potsdam )
block <- function( input.bulk,
                  block.number = round( length( input.bulk )/ 50 ),
                  block.length = NULL, block.mode = c( "max", "min" ),
                  separation.mode = c( "years", "none" ) ){
  if ( !all( class( input.bulk ) == c( "xts", "zoo" ) ) )
    stop( "The block function works to input of class 'xts' only!" )
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
      block.length <- length( input.bulk )/ ( block.number ) +
        1/ ( block.number )
    }
  } else
    separation.mode <- "years"
  if ( missing( block.mode ) )
    block.mode <- "max"
  block.mode <- match.arg( block.mode )
  ## according to the desired block.length the data is now separated
  ## into snippets and those are saved inside a list
  if ( separation.mode == "years" ){
    input.index <- data.frame( value = input.bulk,
                              index = year( input.bulk ),
                              row.names = index( input.bulk ) )
  } else {
    ## All data belonging to the same block share the same index value
    input.index <- data.frame(
        value = input.bulk,
        index =  floor( ( seq( 1 : length( input.bulk ) ) - 1 )/
                        block.length ) + 1,
        row.names = index( input.bulk ) )
  }
  input.blocked <- split( input.index,input.index$index )
  ## Extract the maxima or minima from the blocked data
  if ( block.mode == "max" ){
    input.extremes <- Reduce(
        rbind, lapply( input.blocked, function( x ){
          data.frame( date = row.names( x )[ which.max( x[[ 1 ]] ) ],
                     value = x[ which.max( x[[ 1 ]] ), 1 ] ) } ) )
  } else
    input.extremes <- Reduce(
        rbind, lapply( input.blocked, function( x ){
          data.frame( date = row.names( x )[ which.min( x[[ 1 ]] ) ],
                     value = x[ which.min( x[[ 1 ]] ), 1 ] ) } ) )
  extremes.xts <- xts( input.extremes[[ 2 ]] ,
                      order.by = as.Date( input.extremes[[ 1 ]] ) )
  return( extremes.xts )
}

##' @title Decluster point over threshold data used for GP fitting.
##'
##' @details Inspired by the decluster algorithm used in the extRemes package
##'
##' @param x Time series
##' @param threshold Has to be set sufficient high to fulfill the asymptotic condition for the GP distribution.
##'
##' @return Declustered values above the provided threshold (xts).
##' @export
##' @import xts
##' @author Philipp Mueller
decluster <- function( x, threshold ){
  ## Caution: x is the full time series and not the blocked one!
  x.extremal.index <- extRemes::extremalindex( x, threshold,
                                              na.action = stats::na.omit )
  ifelse( x.extremal.index[ 1 ] >= 1,
         cluster.size <- 0,
         cluster.size <- x.extremal.index[ 3 ] )
  ## cluster.size is the number of indices a two points have to be away
  ## from each other to belong to different indices
  n <- length( x )
  which.x.over.threshold <- x > threshold
  which.x.over.threshold <- stats::na.omit( which.x.over.threshold )
  x.over.threshold <- x[ which.x.over.threshold ]
  ## amount of points over threshold
  n.over.threshold <- sum( which.x.over.threshold )
  ## index of those points in the ts 'x'
  index.x.over.threshold <- ( 1 : n )[
      stats::na.omit( which.x.over.threshold ) ] 
  which.cluster <- rep( 1, n.over.threshold )
  x.result <- x
  ## number of indices the threshold exceedences are apart from eachother
  x.distances <- diff( index.x.over.threshold )
  ## which point belongs to which cluster
  which.cluster[ 2 : n.over.threshold ] <- 1 +
    cumsum( x.distances > cluster.size )
  x.cluster <- split( x.over.threshold, which.cluster )
  x.cluster.max <- as.numeric( lapply( x.cluster, max ) )
  ## Only the maxima of the clusters survive
  x.over.threshold[ -which( x.over.threshold %in% x.cluster.max ) ] <- NA
  x.result[ which.x.over.threshold ] <- x.over.threshold
}

##' @title Extracts all data above a certain threshold
##' @details Due to the UNIX principle (make each program do one thing well) I decided to provide this extra function instead of incorporating it into the fitting function. After extracting all data above the threshold it is going to be subtracted from the data. In addition all exceedance can be declustered. This step is highly recommended since the extreme value theory is only valid for data without correlations and short-range correlations (which are present in most measured data) can be filtered out using this procedure. 
##'
##' @param x Time series or numerical vector.
##' @param threshold Value which has to be exceeded.
##' @param decluster Flag indicating whether of not to decluster the obtained exceedance. Default = TRUE.
##' @param na.rm Flag indicating whether to remove all NA values from the time series (removed points in clusters). For important steps like calculating the Lmoments of the time series there must not be any NA left. Default = TRUE.
##'
##' @export
##' @return Declustered time series of the same format as the input.
##' @author Philipp Mueller 
threshold <- function( x, threshold, decluster = TRUE, na.rm = TRUE ){
  if ( missing( x ) )
    stop( "Please provide a time series to apply the threshold to!" )
  if ( missing( threshold ) )
    stop( "Please provide a threshold to be applied to the time series!" )
  ## declustering of the data
  if ( decluster ){
    x.threshold <- climex::decluster( x, threshold ) - threshold
  } else
    x.threshold <- x[ x > threshold ] - threshold
  ## removing the NA
  if ( na.rm )
    x.threshold <- na.omit( x.threshold )        
  return( x.threshold )
}


##' @title Calculation of the return levels.
##'
##' @details Uses the climex:::rlevd function at its core (a port from the extRemes package) but also can handle the outputs of the \code{\link{fit.gev}} and \code{\link{fit.gpd}} function, is capable of calculating numerous return levels at once and also calculates the errors of the return levels. For the errors the ML fit is using the option hessian=TRUE (if not done already) or uses a Monte Carlo based approach. If no fitting object is provided, no errors will be calculated. 
##'
##' @param x Parameter input. Class numeric, climex.fit.gev or climex.fit.gpd.
##' @param return.period Numeric vector of the return periods in years. Default = 100.
##' @param error.estimation Method of calculating the standard errors of the return levels. Using option "MLE" it is calculated using the Delta method and the MLE of the GEV/GPD parameters. For the GPD type the total length of the original time series has to be provided as well or the MLE error estimation will not be able to work. Alternative one can use Monte Carlo simulations with "MC" for which monte.carlo.sample.size samples of the same size as x will be drawn from a GEV/GP distribution constituted by the obtained MLE of the GEV/GPD parameters of x. The standard error is then calculated via the square of the variance of the calculated return levels. Sometimes the inversion of the hessian fails (since the are some NaN in the hessian) (which is also the reason why the ismev package occasionally does not work). Option "none" just skips the calculation of the error and return just a numeric value of the estimate. To avoid broken dependencies  with existing code this option will be default. Default = "none".
##' @param model Determining whether to calculate the initial parameters of the GEV or GPD function. Default = "gev".
##' @param monte.carlo.sample.size Number of samples used to obtain the Monte Carlo estimate of the standard error of the fitting. Default = 1000.
##' @param threshold Optional threshold for the GPD model. If present it will be added to the return level to produce a value which fits to underlying time series. Default = NULL.
##' @param total.length Total number of observations in the time series the exceedance were obtained from (before! applying the threshold). This argument is needed to calculate the standard error of the return level via the delta method of the MLE in the GPD model. Default = NULL.
##' @param original.time.series Time series used with \code{link{fit.gpd}} on which already a threshold (the one supplied here as well) was applied. Necessary to transform the return level for numerical input and the GPD model from m-th observation return level to annual return level. If omitted the return level will be per observation. Default = NULL.
##' @param silent Throws an warning whenever the "gpd" model is used and the original.time.series is not supplied. Since this can be annoying one can also disable it. Default = FALSE.
##'
##' @return If error.estimation == "none" a numerical vector containing the estimates of the return levels will be returned. Else a list containing the estimates and their standard errors will be returned.
##' @export
##' @examples
##' fit.results <- fit.gev( block( anomalies( temp.potsdam ) ) )
##' return.level( fit.results, return.period = c( 10, 50, 100 ), error.estimation = "MLE" )
return.level <- function( x, return.period = 100,
                         error.estimation = c( "none", "MC", "MLE" ),
                         model = c( "gev", "gpd" ),
                         monte.carlo.sample.size = 1000,
                         threshold = NULL, total.length = NULL,
                         original.time.series = NULL, silent = FALSE ){
  if ( any( class( x ) == "climex.fit.gev" ) ){
    model <- "gev"
    return.levels <- Reduce( c, lapply( return.period, function( y )
      as.numeric( climex:::rlevd( y, x$par[ 1 ], x$par[ 2 ], x$par[ 3 ],
                                 model = "gev", silent = TRUE ) ) ) )
  } else if ( any( class( x ) == "climex.fit.gpd" ) ){
    model <- "gpd"
    ## m-observation return level = return.period* the mean number of
    ## exceedance per year. This way the unit of the provided return
    ## level and its error are  not 'per observation' but 'per year'.
    ## In this step we harness the power of the 'xts' package
    m <- return.period* mean( apply.yearly( x$x,
                                           function( y ) length( y ) ) )
    ## When a threshold is supplied, the one in the fitted object will be
    ## overwritten
    if ( !is.null( threshold ) )
      x$threshold <- threshold
    return.levels <- Reduce( c, lapply( m, function( y )
      as.numeric( climex:::rlevd( y, scale = x$par[ 1 ],
                                 shape = x$par[ 2 ],
                                 model = "gpd", threshold = x$threshold,
                                 silent = TRUE ) ) ) )
  } else if ( any( class( x ) == "numeric" ) ){
    ## Neither a object from fit.gev nor from fit.gpd but a numerical
    ## vector containing the GEV/GPD parameters was supplied
    ## Which of those two distribution should ti be?
    if ( length( x ) == 3 ){
      model <- "gev"
    } else if ( length( x ) == 2 ){
      model <- "gpd"
    } else
      stop( "return.level: the provided parameters and model argument do not belong to each other!" )
    if ( model == "gev" ){
      return.levels <- Reduce( c, lapply( return.period, function( y )
        as.numeric( climex:::rlevd( y, x[ 1 ], x[ 2 ], x[ 3 ],
                                   model = "gev" ) ) ) )
    } else {
      ## m-observation return level = return.period* the mean number of
      ## exceedance per year. This way the unit of the provided return
      ## level and its error are  not 'per observation' but 'per year'.
      ## In this step we harness the power of the 'xts' package
      if ( !is.null( original.time.series ) ){
        m <- return.period* mean( apply.yearly(
                                original.time.series,
                                function( y ) length( y ) ) )
      } else {
        if ( !silent )
          warning( "return.level: Since the original time series was not supplied the return level will be not per once every x year but once every x observation" )
        m <- return.period
      }
      return.levels <- Reduce( c, lapply( m, function( y )
        as.numeric( climex:::rlevd( y, scale = x[ 1 ], shape = x[ 2 ],
                                   model = "gpd", threshold = threshold,
                                   silent = TRUE ) ) ) )
    }
  } else
    stop( "return.level is not implemented for this class of input values!" )
  ##
  ## Error estimation of the return level
  ##
  if ( error.estimation == "none" || class( x ) == "numeric" ){
    return( return.levels )
  } else if ( error.estimation == "MLE" ){
    if ( is.null( total.length ) && model == "gpd" ){
      warning( "The error estimation of the return level of the GP distribution does need the total length 'total.length' of the time series the exceedance are extracted from! Please supply it or use the Monte Carlo approach!" )
      return( list( return.levels = return.levels,
                   errors = rep( NaN, length( return.period ) ) ) )
    }
    if ( !any( names( x ) == "hessian" ) ){
      ## fit again and let stats::optim calculate the hessian. It's way
      ## more save this way
      if ( model == "gev" ){
        x$hessian <- fit.gev( x$x, initial = x$par,
                             error.estimation = "MLE" )$hessian
      } else {
        if ( is.null( threshold ) )
          threshold <- x$threshold
        x$hessian <- fit.gpd( x$x, initial = x$par,
                             threshold = threshold,
                             error.estimation = "MLE",
                             total.length = total.length )$hessian
      }
    }
    if ( any( is.nan( x$hessian ) ) ){
      ## If there are NaN in the hessian, the return levels
      ## can not be calculated. So try the numDeriv instead.
      x$hessian <- numDeriv::hessian( likelihood, x = x$par, x.in = x$x,
                                     model = model )
    }
    if ( any( is.nan( x$hessian ) ) ){
      ## If there are still NaN, let it be.
      warning( "return level: NaN in the hessian. Error estimates can not be calculated via the maximum likelihood estimates" )
      return( c( NaN, NaN, NaN ) )
    }
    ## Calculating the errors using the MLE
    error.covariance <- solve( x$hessian )
    ## Delta method for the return level
    parameter.estimate <- x$par
    errors <- data.frame( a = 0 )
    if ( model == "gev" ){
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
    } else {
      ## Formula according to Stuart Coles p. 82
      for ( rr in 1 : length( return.period ) ){
        zeta <- length( x )/ total.length # probability of an exceedance
        ## In addition the uncertainty of zeta has to be part of the
        ## error covariance matrix
        error.covariance.2 <- matrix( 0, 3, 3 )
        error.covariance.2[ 1 , 1 ] <- zeta*( 1 - zeta )/ total.length
        error.covariance.2[ 2 : 3, 2 : 3 ] <- error.covariance
        scale <- parameter.estimate[ 1 ]
        shape <- parameter.estimate[ 2 ]
        dz <- c( scale* m[ rr ]^shape* zeta^{ shape - 1 },
                shape^{ -1 }* ( ( m[ rr ]* zeta )^shape - 1 ),
                -scale* shape^{ -2 }* ( ( m[ rr ]* zeta )^ shape - 1 ) +
                scale* shape^{ -1 }* (
                  m[ rr ]* zeta )^shape* log( m[ rr ]* zeta ) )
        errors <- cbind( errors, dz %*% error.covariance.2 %*% dz )
      }
    }
    errors <- errors[ , -1 ]
    names( errors ) <- paste0( return.period, ".rlevel" )
  } else {
    ## Use the Monte Carlo method to determine the standard errors.
    parameter.estimate <- x$par
    ## Draw a number of samples and fit the GEV/GP parameters for all
    ## of them
    if ( model == "gev" ){
      samples.list <- lapply( 1 : monte.carlo.sample.size, function( y )
        climex:::revd( length( x$x ), parameter.estimate[ 1 ],
                      parameter.estimate[ 2 ],
                      parameter.estimate[ 3 ], model = "gev" ) )
    } else 
      samples.list <- lapply( 1 : monte.carlo.sample.size, function( y )
        climex:::revd( length( x$x ), scale = parameter.estimate[ 1 ],
                      shape = parameter.estimate[ 2 ], silent = TRUE,
                      threshold = threshold, model = "gpd" ) )
    samples.fit <- lapply( samples.list, function( y )
      stats::optim( likelihood.initials( y, model = model ),
                   likelihood, x = y, method = "Nelder-Mead",
                   model = model )$par )
    errors <- data.frame( a = 0 )
    if ( model == "gev" ){
      r.period <- return.period
    } else {
      r.period <- m
    }
    for ( rr in 1 : length( return.period ) )
      errors <- cbind(
          errors, 
          sqrt( stats::var( Reduce( c, lapply( samples.fit,
                                              function( z )
               climex::return.level( z, return.period = r.period[ rr ],
                                    error.estimation = "none",
                                    silent = TRUE ) ) ) ) ) )
    errors <- errors[ , -1 ]   
    names( errors ) <- paste0( return.period, ".rlevel" )
  }
  return( list( return.levels = return.levels, errors = errors ) )
}

##' @title Internal function to calculate the return level of GEV or GP distribution.
##' @details Port from the extRemes package to ensure compatibility and to make the threshold argument obligatory. This is just for internal usage. Please use the \link{\code{return.level}} function instead!
##'
##' @param period Return period in years.
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is optional but should be provided in order to create a representation of the fitted data exceedance. Default = NULL.
##' @param model Determines if to use the GEV or GP distribution. Default = "gev".
##' @param silent Whether to display warnings or not. Default = FALSE.  
##'
##' @return Numerical vector of the same length as 'period'.
##' @author Philipp Mueller 
rlevd <- function ( period, location = NULL, scale = NULL, shape = NULL, threshold = NULL, 
                   model = c( "gev", "gpd" ), silent = FALSE ){
  if ( missing( model ) )
    model <- "gev"
  model <- match.arg( model )
  if ( model == "gev" ){
    if ( is.null( location ) ||
         is.null( scale ) || is.null( shape ) )
      stop( "Please supply 'location', 'scale' and 'shape'!" )
  } else {
    if ( is.null( scale ) || is.null( shape ) )
      stop( "Please supply 'scale' and 'shape'!" )
    if ( is.null( threshold ) ){
      if ( !silent )
        warning( "No 'threshold' supplied! This needs to be added to the generated time series in order to resemble the original data points!" )
      location <- 0
    } else
      location <- threshold
  }
  if ( any( period <= 1 ) ) 
    stop( "rlevd: invalid period argument.  Must be greater than 1." )
  
  if ( model == "gev" ) {
    p <- 1 - 1/ period
    res <- climex:::qevd( p = p, location = location, scale = scale,
                         shape = shape, model = "gev", lower.tail = TRUE,
                         silent = silent )
  }
  else {
    res <- location + ( scale/ shape ) * ( period^shape - 1 )
  }
  names( res ) <- as.character( period )
  return( res )
}

##' @title Calculates the quantile of either the GEV or the GPD distribution
##' @details Port from the extRemes package to (again) get rid of the 'threshold' argument to be able to have an separate 'threshold()' function outside of the fitting function. 
##'
##' @param p (Numeric) probability vector.
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is optional but should be provided in order to create a representation of the fitted data exceedance. Default = NULL.
##' @param model Determines if to use the GEV or GP distribution. Default = "gev".
##' @param lower.tail Whether to sample the probabilities P[X <= x] or P[X > x]. Default = TRUE (first case).
##' @param silent Whether to display warnings or not. Default = FALSE.    
##'
##' @return Numerical vector of the same length as input argument p.
##' @author Philipp Mueller 
qevd <- function ( p, location = NULL, scale = NULL, shape = NULL,
                  threshold = NULL, model = c( "gev", "gpd" ),
                  lower.tail = TRUE, silent = FALSE ){
  if ( missing( model ) )
    model <- "gev"
  model <- match.arg( model )
  if ( model == "gev" ){
    if ( is.null( location ) ||
         is.null( scale ) || is.null( shape ) )
      stop( "Please supply 'location', 'scale' and 'shape'!" )
  } else {
    if ( is.null( scale ) || is.null( shape ) )
      stop( "Please supply 'scale' and 'shape'!" )
    if ( is.null( threshold ) ){
      if ( !silent )
        warning( "No 'threshold' supplied! This needs to be added to the generated time series in order to resemble the original data points!" )
      location <- 0
    } else
      location <- threshold
  }
  if ( scale <= 0 ) 
    stop( "qevd: invalid scale argument.  Must be > 0." )
  if ( min( p, na.rm = TRUE ) <= 0 || max( p, na.rm = TRUE ) >= 1 ) 
    stop( "qevd: invalid p argument.  Must have 0 < p < 1." )
  if ( !lower.tail )
    p <- 1 - p
  if ( model == "gev" ) {
    q <- location + scale * ( ( -log( p ) )^( -shape ) - 1 )/ shape
  }
  else {
    q <- location + scale * ( p^( -shape ) - 1 )/ shape
  }
  return( q )
}

##' @title Drawing random numbers from the GEV or GP distribution
##' @details This function was originally part of the extRemes package. But since one had to provide the threshold I couldn't use it insight the fit.gpd function. In contrast to the original implementation this function only features constant location, scale and shape parameters. If you want to do time dependent analysis of extreme events please refer to the original package.
##'
##' @param n Number of samples to draw
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is optional but should be provided in order to create a representation of the fitted data exceedance. Default = NULL.
##' @param model Determines if to use the GEV or GP distribution. Default = "gev".
##' @param silent Whether to display warnings or not. Default = FALSE.    
##'
##' @return Numerical vector of length n drawn from the corresponding distribution. 
##' @author Philipp Mueller 
revd <- function ( n, location = NULL, scale = NULL, shape = NULL,
                  threshold = NULL, model = c( "gev", "gpd" ),
                  silent = FALSE ){
  if ( missing( model ) )
    model <- "gev"
  model <- match.arg( model )
  if ( model == "gev" ){
    if ( is.null( location ) ||
         is.null( scale ) || is.null( shape ) )
      stop( "Please supply 'location', 'scale' and 'shape'!" )
    z <- rexp( n )
  } else {
    if ( is.null( scale ) || is.null( shape ) )
      stop( "Please supply 'scale' and 'shape'!" )
    if ( is.null( threshold ) ){
      if ( !silent )
        warning( "No 'threshold' supplied! This needs to be added to the generated time series in order to resemble the original data points!" )
      location <- 0
    } else
      location <- threshold
    z <- runif( n )
  }
  ## allocating memory
  result <- numeric( n ) + NA
  result <- location + scale * ( z^( -shape ) - 1 )/ shape
  return( result )
}

##' @title Port of the ismev::gev.dens function
##' @details It might not to be called by the user, but I can
##' certainly write some lines about it. This function takes
##' the fitted parameters of a GEV distribution and calculates
##' its density at specified sites z.
##'
##' @param parameters Fitted GEV parameters c( location, scale,
##' shape )
##' @param z Numerical vector of sites where to evaluate the
##' density of the GEV distribution.
##'
##' @return Numerical vector of same length as z.
##' @author Philipp Mueller
gev.density <- function ( parameters, z ){
  if ( class( parameters ) != "numeric" ||
       length( parameters ) != 3 )
    stop( "gev.density: Please provide a numerical vector of the form c( location, scale, shape )")
  location <- parameters[ 1 ]
  scale <- parameters[ 2 ]
  shape <- parameters[ 3 ]
  ( exp( -( 1 + ( shape* ( z - location ) )/
            scale )^( -1/ shape ) )*
    ( 1 + ( shape* ( z - location ) ) /
      scale )^( -1/ shape - 1) )/scale
}

##' @title Port of the ismev::gpd.dens function
##' @details It might not to be called by the user, but I can
##' certainly write some lines about it. This function takes
##' the fitted parameters of a GP distribution and calculates
##' its density at specified sites z.
##'
##' @param parameters Fitted GP parameters c( scale, shape )
##' @param threshold Threshold used to fit the GP distribution.
##' @param z Numerical vector of sites where to evaluate the
##' density of the GP distribution.
##'
##' @return Numerical vector of same length as z.
##' @author Philipp Mueller 
gpd.density <- function( parameters, threshold, z ) {
  if ( class( parameters ) != "numeric" ||
       length( parameters ) != 2 )
    stop( "gpd.density: Please provide a numerical vector of the form c( scale, shape )")
  scale <- parameters[ 1 ]
  shape <- parameters[ 2 ]
  ( 1 + ( shape* ( z - threshold ) )
    / scale )^( -1/shape - 1 )/ scale
}
