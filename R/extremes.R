##' @title Blocking data
##' @description Separates the input into blocks of equal size and
##'   returns the maximum or minimum of the block as result.
##'
##' @details If 'separation.mode' is set to "years" the data is separated
##' according to it's time stamps. If not the size of a block is
##' determined by the 'block.length' parameter or calculated via the
##' 'block.number' parameter. For calculating the mean of the blocks have
##' a look at the \code{\link[stats]{ave}} function.
##'
##' @param input.bulk Provided data of class 'xts'.
##' @param block.number Specifies the number of blocks the input data is
##' going to be separated in.
##' @param block.length Length of the blocks. For the sake of simplicity
##' the last block is not forced to match the length of the other plots.
##' @param block.mode This parameter determines if the maximum "max" or
##' the minimum "min" of a block shall be returned. Default: "max".
##' @param separation.mode "years" is used to split the data according to
##' its date values instead. If 'block.length' or 'block.number' is
##' specified this argument will not be considered and set to "none".
##' Default = "years".
##' @return Of class 'xts'.
##' @family extremes
##' @author Philipp Mueller
##' @export
##' @importFrom xts xts
##' @importFrom zoo index
##'
##' @family extremes
##' 
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

##' @title Decluster data
##' @description Decluster point over threshold data used for GP
##'   fitting. 
##'
##' @details This function determines clusters in a time series and
##' extract just their maximal value in order to remove short-range
##' correlations. All exceedances.position will be considered belonging
##' to a cluster until at least cluster.distance consecutive points fall
##' below the threshold. The parameter cluster.distance will be
##' determined using the extremal index as suggested in Ferro & Segers
##' (2003) when set to NULL. It thus provides a non-parametric way of
##' declustering.
##' It also features a special treatment of missing values. All of them
##' will be kept and the returned time series will have the same length
##' as the input. Separate missing values and small sequences will be
##' omitted in the cluster determination. But if more than 15 missing
##' values appear in a row, they will be replaced with the minimal value
##' of the time series for the cluster detection. This way exceedances
##' separated over a big temporal distance will not be considered to
##' belong to the same cluster.
##'
##' @param x Time series. The full one, not just the
##' exceedances.position! Can be of class 'xts' or 'numeric'
##' @param threshold Has to be set sufficient high to fulfill the
##' asymptotic condition for the GP distribution.
##' @param cluster.distance Specifies how many points have to be below
##' the threshold for the next point to be considered the starting point
##' of a new cluster. Only supply a value when you really know what you
##' are doing! Default = NULL
##' @param silent Whether or not to display warnings.
##'
##' @return Returns the original time series x with all the elements
##' within one cluster having smaller values than the clusters maximum
##' being replaced by NA.
##'
##' @family extremes
##' 
##' @export
##' @importFrom xts xts
##' @author Philipp Mueller
decluster <- function( x, threshold, cluster.distance = NULL,
                      silent = FALSE ){
  ## Caution: x is the full time series and not the blocked one!
  if ( any( is.na( x ) ) ){
    ## Handling of missing values.
    ## I will introduce a heuristic here. When less minimal.na points are
    ## missing, they are just removed. So they do not interfer with the
    ## determination of the cluster end. But if more points are missing,
    ## they are replaced with an arbitrary value below the threshold.
    ## This one will be overwritten with NA later on again. But in the
    ## meanwhile it will separate the last cluster from the next one.
    ## The idea is the following: When whole year of data is missing, it
    ## makes no sense to assign data points separated by such a distance
    ## to one and the same cluster. As done in the extRemes::decluster
    ## function.
    minimal.na <- 15 # NA in a row will be replaced with a low value
    na.index <- which( is.na( x ) )
    na.distance <- diff( na.index )
    na.cluster <- rep( 1, length( na.index ) )
    ## Each sequence of NA will get a separate number. As soon as there
    ## is a single point between to missing values, those two will be
    ## considered belonging to different sequences.
    if ( length( na.index ) > 1 ){
      na.cluster[ 2 : length( na.index ) ] <- 1 + cumsum( na.distance > 1 )
    }
    na.list <- split( na.index, na.cluster )
    ## Logical vector whether or not to replace a NA by the minimum of
    ## the time series
    na.replace <- Reduce( c, lapply( na.list, function( y ){
      if ( length( y ) >= minimal.na ){
        return( rep( TRUE, length( y ) ) )
      } else {
        return( rep( FALSE, length( y ) ) ) } }
      ) )
    ## Replacing the long NA sequences by the minimal value
    x[ na.index ][ na.replace ] <- min( x, na.rm = TRUE )
    ## Now we are save to omit the NA for the cluster detection
    x.no.na.index <- which( !is.na( x ) )
    x.no.na <- stats::na.omit( x )
  } else {
    x.no.na <- x
  }
  if ( is.null( cluster.distance ) ){
    ## Estimating the number of points between two clusters using the
    ## extremal index.
    cluster.distance <- extremal.index( x.no.na, threshold,
                                       silent = TRUE )[ 3 ]
  } else if ( !silent ){
    warning( "You are choosing the cluster size by hand instead of relying on the non-parametric version. Proceed with care!" )
  }
  exceedances.position <- x.no.na > threshold
  ## Böse, böse, böse
  exceedances.position <- exceedances.position
  exceedances <- x.no.na[ exceedances.position ]
  ## Amount of points above the threshold
  exceedances.number <- sum( exceedances.position )
  ## Index of those points in the ts 'x'
  ## Böse, böse, böse
  exceedances.index <- ( 1 : length( x.no.na ) )[ exceedances.position ] 
  which.cluster <- rep( 1, exceedances.number )
  ## Number of indices the exceedances are apart from each other
  exceedances.distance <- diff( exceedances.index )
  ## Which point belongs to which cluster
  which.cluster[ 2 : exceedances.number ] <- 1 +
    cumsum( exceedances.distance > cluster.distance )
  ## Create a list in which element is a numerical vector containing the
  ## exceedances within a certain cluster
  cluster.list <- split( exceedances, which.cluster )
  ## Replacing all events in a cluster smaller than its maximum with NaN
  if ( is.xts( x.no.na ) ) {
    ## Extra care when input is of type xts
    index.not.max <- Reduce( c, lapply( cluster.list, function( y )
      index( y[ -which( y == max( y ) ) ] ) ) )
    exceedances[ index( exceedances ) %in% index.not.max ] <- NA
  } else {
    exceedances <- Reduce( c, lapply( cluster.list, function( y ){
      y[ -which( y == max( y ) ) ] <- NA
      return( y ) } ) )
  }
  ## Introducing the NaN to the original time series.
  x.no.na[ x.no.na > threshold ] <- exceedances
  if ( any( is.na( x ) ) ){
    ## Now I have to introduce the NA again or else my time series will
    ## shrink and I have both missing values marked by NA and those
    ## complete gone from the time series
    x[ x.no.na.index ] <- x.no.na
    ## Replaceing the long NA sequences with NA again
    x[ na.index ][ na.replace ] <- NA
  } else {
    x <- x.no.na
  }
  return( x )
}

##' @title Extremal index estimation
##' @description Estimates the extremal index of a time series.
##' @details The extremal index can be thought of as the inverse of the
##' mean cluster size. It can be calculated by the "blocks" method
##' of Ferro and Segers, 2003. I will use the bias-free estimator
##' provided in equation (4) in their paper. This one is supposed to be
##' the most robust one and is relying on a moment estimation.
##' Another way to estimate it, would be using the "runs" method as in
##' Stuart Coles (2001). But therefore one had to know the minimal
##' distance between the clusters first. Since the whole point of this
##' function to estimate exactly this quantity for its use in the
##' \code{\link{decluster}} function, I don't see the point of
##' implementing this method too.
##'
##' @param x Time series of class 'xts' or numerical vector.
##' @param threshold Only events exceeding a specific threshold will be
##' considered extreme events and thus will be subject of the
##' declustering.
##' @param silent Whether or not to display warnings.
##'
##' @family extremes
##'
##' @return Numerical vector containing c( extremal index, number of
##' clusters, minimal distance between clusters (minimal.distance) ).
##' @author Philipp Mueller 
extremal.index <- function( x, threshold, silent = FALSE ){
  ## Positions (logical) of all events exceeding the threshold
  exceedances.position <- x > threshold
  exceedances.number <- sum( exceedances.position )
  if ( exceedances.number == 0 ){
    warning( "No exceedances.position in extremal.index. The provided threshold parameter is most probably to high!" )
    return( NaN )
  }
  ## Distance between neighboring exceedances.position in number of
  ## observations
  exceedances.distance <- diff( ( 1 : length( x ) )[
                                    exceedances.position ] )
  if ( !any( exceedances.distance > 2 ) ){
    ## Sanity check
    if ( !any( exceedances.distance > 1 ) ){
      warning( "The maximal distance between exceedances.position in extremal.index is 1! The provided threshold is way to low!" )
    }
    ## If the largest interexceedance time (distance between exceedances)
    ## is smaller than 2, the (first order) unbiased estimator in the
    ## else counterpart is not defined. Therefore I will use a fallback
    ## one using the true moments of the exceedance distance
    ## distribution.
    theta <- 2* sum( exceedances.distance )^ 2/
      ( ( exceedances.number - 1 )* sum( exceedances.distance^2 ) )
  } else {
    theta <- 2* sum( exceedances.distance - 1 )^ 2/
      ( ( exceedances.number - 1 )*
        sum( ( exceedances.distance - 1 )*
             ( exceedances.distance - 2 ) ) )
  }
  ## Sanity check
  if ( theta > 1 ){
    theta <- 1
    cluster.distance <- 0
    if ( !silent ){
      warning( "The calculated extremal index in extremal.index was too big and has been reset to 1" )
    }
  }
  ## The (mean) number cluster need to be an integer
  cluster.number <- floor( theta* exceedances.number )
  ## Calculating the minimal distance between two clusters
  ## Ordering the exceedances.position
  exceedances.distance.ordered <- exceedances.distance[
      order( exceedances.distance, na.last = TRUE,
            decreasing = TRUE ) ]
  minimal.distance <- exceedances.distance.ordered[ cluster.number ]
  ## Ordered difference of distances between the clusters
  distance.difference <- c( diff( exceedances.distance.ordered ), 0 )
  if ( cluster.number > 1 &&
       distance.difference[ cluster.number - 1 ] == 0 ){
    ## We have a tie. There are several clusters having the same distance
    ## as the one with the cluster.number - 1 largest interexceedance
    ## distance. I don't really see the problem just now, but the paper
    ## of Ferro & Seger asked to reduce the cluster number until there
    ## is no tie anymore.
    cluster.index <- 1 : ( exceedances.number - 1 )
    ## Clusters sharing the exceedance distance with other ones. 
    cluster.ties <- cluster.index[ distance.difference == 0 ]
    cluster.step <- cluster.index[ distance.difference != 0 ]
    ## All indices between the last jump in the exceedance distance just
    ## before the current cluster.number largest cluster distance and
    ## the later one.
    cluster.before.step <- ( cluster.ties < cluster.number ) &
      ( cluster.ties > max( cluster.step[ cluster.step <
                                          cluster.number ] ) )
    ## The cluster.number - 1 exceedance distance is supposed to be a
    ## step in the exceedance distance.
    cluster.number <- min( cluster.ties[ cluster.before.step ] )
    minimal.distance <- exceedances.distance.ordered[ cluster.number ]
  }
  return( c( theta, cluster.number, minimal.distance ) )
}

##' @title Apply a threshold to data
##' @description Extracts all data above a certain threshold
##' @details Due to the UNIX principle (make each program do one thing
##' well) I decided to provide this extra function instead of
##' incorporating it into the fitting function. After extracting all data
##' above the threshold it is going to be subtracted from the data. In
##' addition all exceedance can be declustered. This step is highly
##' recommended since the extreme value theory is only valid for data
##' without correlations and short-range correlations (which are present
##' in most measured data) can be filtered out using this procedure. 
##'
##' @param x Time series or numerical vector.
##' @param threshold Value which has to be exceeded.
##' @param decluster Flag indicating whether of not to decluster the
##' obtained exceedance. Default = TRUE.
##' @param na.rm Flag indicating whether to remove all NA values from the
##' time series (removed points in clusters). For important steps like
##' calculating the Lmoments of the time series there must not be any NA
##' left. Default = TRUE.
##'
##' @family extremes
##' 
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
    x <- climex::decluster( x, threshold )
  }
  x.threshold <- x[ x > threshold ] - threshold
  ## removing the NA
  if ( na.rm )
    x.threshold <- stats::na.omit( x.threshold )        
  return( x.threshold )
}


##' @title Calculation of the return levels.
##' @description Calculate arbitrary return level and their error
##'   estimates for GEV and GP distributions.
##'
##' @details Uses the \code{\link{rlevd}} function at its core (a port
##'   from the \pkg{extRemes} package) but also can handle the outputs
##'   of the \code{\link{fit.gev}} and \code{\link{fit.gpd}} function,
##'   is capable of calculating numerous return levels at once and
##'   also calculates the errors of the return levels. For the errors
##'   the ML fit is using the option hessian=TRUE (if not done
##'   already) or uses a Monte Carlo based approach. If no fitting
##'   object is provided, no errors will be calculated. 
##'
##' @param x Parameter input. Class numeric, climex.fit.gev or
##' climex.fit.gpd.
##' @param return.period Numeric vector of the return periods in years.
##' Default = 100.
##' @param error.estimation Method for calculating the standard errors of
##' the fitted results. The errors of the GEV/GPD parameters will be
##' calculated as the square roots of the diagonal elements of the
##' inverse of the hessian matrix. The latter will be evaluated at the
##' maximum likelihood estimates (MLE) of the GEV/GPD parameters.
##' For all three methods of estimating the fitting errors of the
##'   return levels underlying series of threshold exceedances or
##'   block maxima is required. In case the user supplies numerical
##'   values to specify GEV/GPD parameters and not the output of
##'   either the  \code{\link{fit.gev}} or \code{\link{fit.gpd}}
##'   function no error estimation for the return level can be
##'   performed. 
##'
##' \strong{MLE}: The standard error of the return level is
##' calculated using the Delta method and the maximum likelihood
##' estimates of the GPD parameters. Note: For positive shape
##'   parameters bigger than 0.3 this approach tends to highly
##'   overestimates the errors of the return levels.
##' 
##' \strong{MC}: Alternative one can use a Monte Carlo method for which
##' \emph{monte.carlo.sample.size} samples of the same size as \emph{x} will be drawn
##' from a GEV/GPD distribution constituted by the obtained MLE of the GEV/GPD
##' parameters of \emph{x}. The standard error is then calculated via the square
##' of the variance of all fitted GEV/GPD parameters and calculated return
##' levels. Note: In its essence this approach is not an estimation of
##'   the error involved in fitting the time series to a GEV/GPD
##'   distribution. It is rather the mean error of fitting a
##'   GPD-distribution with the same length and parameters as
##'   estimated ones.
##'
##' \strong{bootstrap}: Using this option the provided time series
##'   \emph{x} will be sampled with replacement
##'   \emph{bootstrap.sample.size} times and with the same length as
##'   the original time series. The standard errors of the GEV/GPD
##'   parameters and return levels of all those sampled series is
##'   calculated and returned as an estimate of the fitting error.
##'   Note: Since the data is (hopefully) GEV/GPD-distributed, such a
##'   sampling has to be treated with a lot of care.
##'
##' Sometimes the inversion of the hessian fails (since the are some NaN
##' in the hessian) when calculating the error estimates using the
##'   maximum likelihood approach (MLE) (which is also the reason why
##'   the ismev package occasionally does not work). In such cases the
##'   Monte Carlo (MC) method is used as a fallback. Option
##'
##' \strong{none} skips the calculation of the error. 
##' Default = "MLE".
##' @param model Determining whether to calculate the initial parameters
##' of the GEV or GPD function. Default = "gev".
##' @param monte.carlo.sample.size Number of samples used to obtain the
##' Monte Carlo estimate of the standard error of the fitting.
##' Default = 1000.
##' @param bootstrap.sample.size Number of samples with replacements
##'   to drawn from the original series \emph{x} in order to determine
##'   the standard errors for the GPD parameters and return
##'   levels. Default = 100.
##' @param threshold Optional threshold for the GPD model. If present it
##' will be added to the return level to produce a value which fits to
##' underlying time series. Default = NULL.
##' @param total.length Total number of observations in the time series
##' the exceedance were obtained from (before! applying the threshold).
##' This argument is needed to calculate the standard error of the
##' return level via the delta method of the MLE in the GPD model.
##' Default = NULL.
##' @param thresholded.time.series Time series used with \code{\link{fit.gpd}}
##' on which already a threshold (the one supplied here as well) was
##' applied. Necessary to transform the return level for numerical input
##' and the GPD model from m-th observation return level to annual return
##' level. If omitted the return level will be per observation.
##' Default = NULL.
##' @param silent Throws an warning whenever the "gpd" model is used and
##' the \emph{thresholded.time.series} is not supplied. Since this can be annoying
##' one can also disable it. Default = FALSE.
##'
##' @return A list containing the estimates "return.level" and their
##'   standard errors "error".
##' @export
##'
##' @importFrom xts apply.yearly
##' @importFrom numDeriv hessian
##' @family extremes
##' 
##'
##' @examples
##' fit.results <- fit.gev( block( anomalies( temp.potsdam ) ) )
##' return.level( fit.results, return.period = c( 10, 50, 100 ),
##'               error.estimation = "MLE" )
return.level <- function( x, return.period = 100,
                         error.estimation = c( "none", "MC", "MLE",
                                              "bootstrap" ),
                         model = c( "gev", "gpd" ),
                         monte.carlo.sample.size = 1000,
                         bootstrap.sample.size = 100,
                         threshold = NULL, total.length = NULL,
                         thresholded.time.series = NULL,
                         silent = FALSE ){
  if ( any( class( x ) == "climex.fit.gev" ) ){
    model <- "gev"
    return.levels <- Reduce( c, lapply( return.period, function( y )
      as.numeric( rlevd( y, x$par[ 1 ], x$par[ 2 ], x$par[ 3 ],
                                 model = "gev", silent = silent ) ) ) )
  } else if ( any( class( x ) == "climex.fit.gpd" ) ){
    model <- "gpd"
    if ( !is.null( total.length ) ){
      if ( !silent && !is.null( x$control$total.length ) ){
        warning( "return.level: The total.length argument is already present in the supplied fitting object and will be overwritten." )
      }
      x$control$total.length <- total.length
    }
    if ( !is.xts( x$x ) ){
      if ( !silent )
        warning( "return.level: Since the original time series was not supplied the return level will be not per once every x year but once every x observation" )
      m <- return.period
      ## Since the exceedances aren't provided there is not way to
      ## estimate the exceedance probability. Therefore the errors
      ## will be m-th observation-based instead of m-th year-based.
      zeta <- FALSE
    } else if ( !is.null( x$control$total.length ) ){
      ## The maximum likelihood estimate of the probability of an
      ## exceedance to occur per year will be used.
      zeta <- length( x$x )/ x$control$total.length
      m <- return.period* 365.25* zeta
    } else {
      ## m-observation return level = return.period* the mean number of
      ## exceedance per year. This way the unit of the provided return
      ## level and its error are  not 'per observation' but 'per year'.
      ## In this step we harness the power of the 'xts' package
      m <- return.period* mean( apply.yearly(
                              x$x,
                              function( y ) length( y ) ) )
      zeta <- NULL
    }
    ## The determined 'm' always have to be bigger than one
    if ( any( m < 1 ) ){
      if ( !silent ){
        warning( "return.level: at least one of the m-observation return levels is smaller than one. Those will be omitted." )
      }
      return.period <- return.period[ -which( m < 1 ) ]
      m <- m[ -which( m < 1 ) ]
    }
    ## When a threshold is supplied, the one in the fitted object will
    ## be overwritten
    if ( !is.null( threshold ) )
      x$threshold <- threshold
    return.levels <- Reduce( c, lapply( m, function( y )
      as.numeric( rlevd( y, scale = x$par[ 1 ],
                                 shape = x$par[ 2 ],
                                 model = "gpd", threshold = x$threshold,
                                 silent = silent ) ) ) )
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
        as.numeric( rlevd( y, x[ 1 ], x[ 2 ], x[ 3 ],
                                   model = "gev" ) ) ) )
    } else {
      if ( !is.null( thresholded.time.series ) ){
        if ( !is.null( total.length ) ){
          ## The maximum likelihood estimate of the probability of an
          ## exceedance to occur per year will be used.
          zeta <- length( thresholded.time.series )/ total.length
          m <- return.period* 365.25* zeta
        } else {
          ## m-observation return level = return.period* the mean
          ## number of exceedance per year. This way the unit of the
          ## provided return level and its error are  not 'per
          ## observation' but 'per year'. 
          ## In this step we harness the power of the 'xts' package
          m <- return.period* mean( apply.yearly(
                                  thresholded.time.series,
                                  function( tt ) length( tt ) ) )
          zeta <- NULL
        }
      } else {
        if ( !silent )
          warning(
              "return.level: Since the original time series was not supplied the return level will be not per once every m year but once every m observation" )
        ## Neither the amount of threshold exceedances nor the length of
        ## the original series is supplied. Now there is no other
        ## chance than to calculate the observation-based return level.
        m <- return.period
        zeta <- FALSE
      }
      return.levels <- Reduce( c, lapply( m, function( mm )
        as.numeric( rlevd( mm, scale = x[ 1 ], shape = x[ 2 ],
                                   model = "gpd", threshold = threshold,
                                   silent = silent ) ) ) )
    }
  } else {
    stop(
        "return.level is not implemented for this class of input values!" )
  }
  ##
  ## Error estimation of the return level
  ##
  if ( error.estimation == "none" || class( x ) == "numeric" ){
    ## Dummy holding NaN instead of the return level errors.
    ## (Since there was not enough information supplied to calculate
    ## them).
    if ( model == "gev" ){
      errors <- rep( NA, length( return.period ) )
    } else {
      errors <- rep( NA, length( return.period ) )
    }
    return( list( return.level = return.levels, error = errors ) )
  } else if ( error.estimation == "bootstrap" ){
    ## As a simple alternative the threshold exceedances will be
    ## sampled with replacement and the parameters and return levels
    ## are calculated for all of the resampled series. The bootstrap
    ## error is than calculated as the standard error of all the
    ## GEV parameters and return levels.
    bootstrap.sample.list <-
      lapply( c( 1 : bootstrap.sample.size ), function( xx )
        sample( x$x, size = length( x$x ), replace = TRUE ) )
    ## Fitting the GEV parameters (recursively)
    if ( model == "gev" ){
      fitted.list <- lapply( bootstrap.sample.list, function( xx ){
        fit.gev( x = xx, initial = x$control$initial,
                likelihood.function = x$control$likelihood.function,
                gradient.function = x$control$gradient.function,
                error.estimation = "none",
                return.period = return.period,
                total.length = x$control$total.length,
                silent = TRUE ) } )
    } else {
      fitted.list <- lapply( bootstrap.sample.list, function( xx ){
        fit.gpd( x = xx, initial = x$control$initial,
                threshold = x$control$threshold,
                likelihood.function = x$control$likelihood.function,
                gradient.function = x$control$gradient.function,
                error.estimation = "none",
                return.period = return.period,
                total.length = x$control$total.length,
                silent = TRUE ) } )
    }
    ## Calculate the standard errors of all the fitted return
    ## levels.
    fitted.parameters <-
      Reduce( rbind, lapply( fitted.list, function( xx )
        xx$par ) )
    fitted.return.levels <-
      Reduce( rbind, lapply( fitted.list, function( xx )
        xx$return.level ) )
    ## Calculate the standard errors
    errors <- apply( cbind( fitted.parameters,
                           fitted.return.levels ), 2, stats::sd )
    ## Extracting the errors of the return levels.
    if ( model == "gev" ){
      errors <- errors[ 4 : ( 3 + length( return.period ) ) ]
    } else {
      errors <- errors[ 3 : ( 2 + length( return.period ) ) ]
    }
  } else if ( error.estimation == "MLE" ){
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
    ## Sometimes the obtained hessian is not invertible. If this is the
    ## case, recalculate it in order to access the fitting error
    ## estimates. Caution: this one will be without the constraints!
    ## Calculating the errors using the MLE
    ##
    ## If the shape parameter is exactly zero and the Gumbel
    ## distribution was fitted, the third row and column were just
    ## augmented by 0.
    if ( model == "gev" ){
      if ( x$par[ 3 ] != 0 ){
        error.covariance <- try( solve( x$control$hessian ),
                                silent = silent )
      } else {
        ## Omit the augmentation
        error.covariance <- try( solve(
            x$control$hessian[ 1 : 2, 1 : 2 ] ),
            silent = silent )
        ## Augment the result again to ensure compatibility
        if ( class( error.covariance ) != "try-error" ){
          dummy.matrix <- matrix( rep( 0, 9 ), nrow = 3, ncol = 3 )
          dummy.matrix[ 1 : 2, 1 : 2 ] <- error.covariance
          error.covariance <- dummy.matrix
        }
      }
    } else {
      if ( x$par[ 2 ] != 0 ){
        error.covariance <- try( solve( x$control$hessian ),
                                silent = silent )
      } else {
        ## Omit the augmentation
        error.covariance <- try( solve(
            x$control$hessian[ 1 ] ),
            silent = silent )
        ## Augment the result again to ensure compatibility
        if ( class( error.covariance ) != "try-error" ){
          dummy.matrix <- matrix( rep( 0, 4 ), nrow = 2, ncol = 2 )
          dummy.matrix[ 1 ] <- error.covariance
          error.covariance <- dummy.matrix
        }
      }
    }
    if ( class( error.covariance ) == "try-error" ){
      x.hessian <- numDeriv::hessian( likelihood, x = x$par, x.in = x$x,
                                     model = model )
      error.covariance <- solve( x.hessian )
      if ( any( is.nan( x.hessian ) ) ){
        ## If there are still NaN, let it be.
        warning( "return level: NaN in the hessian. Error estimates can not be calculated via the maximum likelihood estimates" )
        return( rep( NA, length( return.period ) ) )
      }
    }
    ## Delta method for the return level
    parameter.estimate <- x$par
    parameter.error.estimate <- as.numeric( x$se )
    errors <- data.frame( a = 0 )
    if ( model == "gev" ){
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
    } else {
      ## Formula according to Stuart Coles p. 82
      for ( rr in 1 : length( return.period ) ){
        ## I omit the error estimation of the zeta in here. I don't
        ## sew how the error of exceedance probability will help the
        ## user.
        scale <- parameter.estimate[ 1 ]
        shape <- parameter.estimate[ 2 ]
        if ( is.null( zeta ) ){
          ## Calculate the exceedance probability in case only the
          ## threshold exceedances and not the original time series
          ## was supplied.
          zeta <- m[ rr ]/ ( return.period[ rr ]* 365.25 )
        }
        if ( !is.numeric( zeta ) ){
          ## In case zeta was set to FALSE neither the original time
          ## series nor the threshold exceedances were
          ## supplied. Therefor there is no way to estimate the mean
          ## exceedance probability and the zeta term has to be
          ## excluded from the error calculation.
          if ( shape != 0 ){
            ## GP distribution
            dz <- c( ( m[ rr ]^ shape - 1 )/ shape,
                    -scale* shape^{ -2 }*( m[ rr ]^shape - 1 ) +
                    scale/shape*m[ rr ]^shape* log( m[ rr ] ) )
            errors <- cbind( errors, dz %*% error.covariance %*% dz )
          } else {
            ## Exponential distribution
            dz <- log( m[ rr ] )
            errors <- cbind( errors, dz %*%
                                     parameter.error.estimate[ 1 ] %*%
                                     dz )
          }
        } else {
          if ( shape != 0 ){
            ## GP distribution
            dz <- c( scale* m[ rr ]^shape* zeta^{ shape - 1 },
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
              error.matrix[ 1, 1 ] <- zeta^2*( 1 - zeta )/
                length( x$x )
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
              error.matrix[ 1, 1 ] <- zeta^2*( 1 - zeta )/
                length( x$x )
            }
            error.matrix[ 2, 2 ] <- parameter.error.estimate[ 1 ]
            errors <- cbind( errors, dz %*% error.matrix %*% dz )
          }
        }
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
        revd( length( x$x ), parameter.estimate[ 1 ],
                      parameter.estimate[ 2 ],
                      parameter.estimate[ 3 ], model = "gev" ) )
    } else { 
      samples.list <- lapply( 1 : monte.carlo.sample.size, function( y )
        revd( length( x$x ), scale = parameter.estimate[ 1 ],
                      shape = parameter.estimate[ 2 ], silent = TRUE,
                      threshold = threshold, model = "gpd" ) )
    }
    samples.fit <- lapply( samples.list, function( y )
      suppressWarnings(
          stats::optim( likelihood.initials( y, model = model ),
                       likelihood, x = y, method = "Nelder-Mead",
                       model = model )$par ) )
    errors <- data.frame( a = 0 )
    if ( model == "gev" ){
      r.period <- return.period
    } else {
      r.period <- m
    }
    for ( rr in 1 : length( return.period ) )
      errors <- cbind(
          errors, 
          sqrt( stats::var( Reduce(
                           c, lapply(
                                  samples.fit,
                                  function( z )
                                    climex::return.level(
                                                z,
                                                return.period =
                                                  r.period[ rr ],
                                                error.estimation =
                                                  "none",
                                                silent = TRUE
                                            )$return.level
                              ) ) ) ) )
    errors <- errors[ , -1 ]   
    names( errors ) <- paste0( return.period, ".rlevel" )
  }
  return( list( return.level = return.levels, error = errors ) )
}

##' @title Return level calculation
##' @description Internal function to calculate the return level of
##'   GEV or GP distribution.
##' @details Port from the extRemes package to ensure compatibility and
##' to make the threshold argument obligatory. This is just for internal
##' usage. Please use the \code{\link{return.level}} function instead!
##'
##' @param period Return period in years.
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is
##' optional but should be provided in order to create a representation
##' of the fitted data exceedance. Default = NULL.
##' @param model Determines if to use the GEV or GP distribution.
##' Default = "gev".
##' @param silent Whether to display warnings or not. Default = FALSE.
##'
##' @family extremes
##'   
##'
##' @export
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
    res <- qevd( p = ( 1 - 1/period ), location = location,
                scale = scale,
                shape = shape, model = "gev", lower.tail = TRUE,
                silent = silent )
  } else {
    res <- qevd( p = 1/period, location = NULL, scale = scale,
                shape = shape, threshold = threshold,
                model = "gpd", lower.tail = TRUE,
                silent = silent )
  }
  names( res ) <- as.character( period )
  return( res )
}

##' @title Quantile calculation
##' @description Calculates the quantile of either the GEV or the GPD
##' distribution
##' @details Port from the extRemes package to (again) get rid of the
##' 'threshold' argument to be able to have an separate 'threshold()'
##' function outside of the fitting function. 
##'
##' @param p (Numeric) probability vector.
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is
##' optional but should be provided in order to create a representation
##' of the fitted data exceedance. Default = NULL.
##' @param model Determines if to use the GEV or GP distribution.
##' Default = "gev".
##' @param lower.tail Whether to sample the probabilities P[X <= x] or
##' P[X > x]. Default = TRUE (first case).
##' @param silent Whether to display warnings or not. Default = FALSE. 
##'
##' @family extremes
##'    
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
    p.rescaled <- -log( p )
    if ( shape == 0 ){
      ## Only for a perfect 0
      q <- location - scale* log( p.rescaled )
    } else {
      q <- location + scale * (
        ( p.rescaled )^( -shape ) - 1 )/ shape
    }
  }
  else {
    if ( shape != 0 ){
      ## Only for a perfect 0
      q <- location + scale * ( ( 1/p )^( shape ) - 1 )/ shape
    } else {
      q <- location + scale* log( 1/p )
    }
  }
  return( q )
}

##' @title Random numbers for GEV and GP
##' @description Drawing random numbers from the GEV or GP distribution
##' @details This function was originally part of the extRemes package.
##' But since one had to provide the threshold I couldn't use it insight
##' the fit.gpd function. In contrast to the original implementation this
##' function only features constant location, scale and shape parameters.
##' If you want to do time dependent analysis of extreme events please
##' refer to the original package.
##'
##' @param n Number of samples to draw
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is
##' optional but should be provided in order to create a representation
##' of the fitted data exceedance. Default = NULL.
##' @param model Determines if to use the GEV or GP distribution.
##' Default = "gev".
##' @param silent Whether to display warnings or not. Default = FALSE.
##'
##' @family extremes
##'     
##'
##' @export
##' 
##' @return Numerical vector of length n drawn from the corresponding
##' distribution. 
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
    z <- stats::rexp( n )
  } else {
    if ( is.null( scale ) || is.null( shape ) )
      stop( "Please supply 'scale' and 'shape'!" )
    if ( is.null( threshold ) ){
      if ( !silent )
        warning( "No 'threshold' supplied! This needs to be added to the generated time series in order to resemble the original data points!" )
      location <- 0
    } else
      location <- threshold
    z <- stats::runif( n )
  }
  if ( as.numeric( shape ) != 0 ){
    result <- as.numeric( location ) + as.numeric( scale )*
      ( z^( -as.numeric( shape ) ) - 1 )/ as.numeric( shape )
  } else {
    if ( model == "gev" ){
      result <- as.numeric( location ) - as.numeric( scale )*
        log( z )
    } else {
      result <- as.numeric( location ) +
        stats::rexp( n, rate = 1/ as.numeric( scale ) )
    }
  }
  return( result )
}

##' @title Probability density function of GEV distribution
##' @description Calculate the probability density function for the
##'   generalized extreme value (GEV) distribution at a given point or
##'   at a number of points (provided as a numerical vector). 
##' 
##' @details If you want to calculate the density of the Gumbel
##'   distribution, please stick to the c( location, scale, shape )
##'   scheme of the first argument while setting the shape parameter
##'   to zero. 
##'
##' Port of the \code{\link[ismev]{gev.dens}} function.
##'
##' @param parameters Fitted GEV parameters c( location, scale,
##' shape ).
##' @param z Numerical vector of sites where to evaluate the
##' density of the GEV distribution.
##'
##' @family extremes
##' 
##' @return Numerical vector of same length as z.
##' @author Philipp Mueller
gev.density <- function ( parameters, z ){
  if ( class( parameters ) != "numeric" || length( parameters ) != 3 ){
    stop( "gev.density: Please provide a numerical vector of the form c( location, scale, shape )")
  }
  location <- parameters[ 1 ]
  scale <- parameters[ 2 ]
  ## Small sanity check
  if ( scale < 0 ){
    stop( "The supplied scale parameter in climex:::gev.density is less than zero!" )
  }
  shape <- parameters[ 3 ]
  if ( shape != 0 ){
    ## There are certain ranges the PDF is defined in. If (some of the) z
    ## happens to be outside of this range, replace it with NaN
    if ( shape < 0 ){
      z[ z > ( ( location ) - scale/ shape ) ] <- NaN
    } else {
      z[ z < ( ( location ) - scale/ shape ) ] <- NaN
    }
    density <- ( exp( -( 1 + ( shape* ( z - location ) )/
                         scale )^( -1/ shape ) )*
                 ( 1 + ( shape* ( z - location ) ) /
                   scale )^( -1/ shape - 1) )/scale
  } else {
    ## The Gumbel distribution is only considered if the shape parameter
    ## matches exactly zero.
    density <- exp( - exp( - ( z - location )/ scale ) )*
      exp( - ( z - location )/scale )/ scale
  }
  return( density )
}

##' @title Probability density function of GP distribution
##' @description Calculate the probability density function for the
##'   generalized Pareto (GP) distribution at a given point or at a
##'   number of points (provided as a numerical vector). 
##' 
##' @details If you want to calculate the density of the exponential
##'   distribution, please stick to the c( scale, shape ) scheme of
##'   the `parameters' argument while setting the shape parameter to
##'   zero. 
##'
##' Port of the \code{\link[ismev]{gpd.dens}} function.
##'
##' @param parameters Fitted GP parameters c( scale, shape )
##' @param threshold Threshold used to fit the GP distribution.
##' @param z Numerical vector of sites where to evaluate the
##' density of the GP distribution.
##' 
##' @family extremes
##' 
##' @return Numerical vector of same length as z.
##' @author Philipp Mueller 
gpd.density <- function( parameters, threshold, z ) {
  if ( class( parameters ) != "numeric" ||
       length( parameters ) != 2 )
    stop( "gpd.density: Please provide a numerical vector of the form c( scale, shape )")
  scale <- parameters[ 1 ]
  ## Small sanity check
  if ( scale < 0 ){
    stop( "The supplied scale parameter in climex:::gpd.density is less than zero!" )
  }
  shape <- parameters[ 2 ]
  if ( shape != 0 ){
    ## There are certain ranges the PDF is defined in. If (some of the) z
    ## happens to be outside of this range, replace it with NaN.
    if ( shape < 0 ){
      z[ z < threshold | z > ( threshold - scale/ shape ) ] <- NaN
    } else {
      z[ z < threshold ] <- NaN
    }
    density <- ( 1 + ( shape* ( z - threshold ) )
      / scale )^( -1/shape - 1 )/ scale
  } else {
    ## The exponential distribution is only considered if the shape
    ## parameter is exactly equal to zero.
    
    ## There are certain ranges the PDF is defined in. If (some of the) z
    ## happens to be outside of this range, replace it with NaN.
    z[ z < threshold ] <- NaN
    density <- exp( -( z - threshold/ scale ) )/ scale
  }
  return( density )
}
