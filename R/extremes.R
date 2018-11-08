### extremes.R - Various helper function for the extreme value
##    analysis 
##' @title Blocking data
##' @description Separates the input into blocks of equal size and
##'   returns the maximum or minimum of the block as result.
##'
##' @details  If both the \strong{block.number} and
##'   \strong{block.length} arguments are NULL, the data will be
##'   split into annual blocks. If not, the
##'   size of a block is determined by the \strong{block.length}
##'   argument or calculated via the \strong{block.number} argument if
##'   the previous one is set to NULL. For calculating the mean of
##'   the blocks, please have a look at the \code{\link[stats]{ave}}
##'   function.
##'
##'   This function can be applied to lists of \pkg{xts} class elements
##'   too. 
##'
##' @param x Either an object of class \pkg{xts} or a list of those.
##' @param block.number Specifies the number of blocks the input data
##'   is going to be separated in. Default = NULL.
##' @param block.length Length of the blocks. For the sake of
##'   simplicity the last block is not forced to match the length of
##'   the other plots. Default = NULL.
##' @param extreme.type String specifying whether the maxima ("max")
##'   or minima ("min") of each block should be extracted. Default:
##'   "max". 
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##' 
##' @family extremes
##'
##' @export
##'
##' @importFrom xts xts
##' @importFrom zoo index
##' @importFrom parallel mclapply
##'
##' @family extremes
##' 
##' @return Same class as the input.
##' @author Philipp Mueller
##' 
##' @examples
##'   block( temp.potsdam )
block <- function( x, block.number = NULL, block.length = NULL,
                  extreme.type = c( "max", "min" ),
                  mc.cores = NULL ){
  UseMethod( "block" )
}
##' @title Blocking data
##' @description Separates the input into blocks of equal size and
##'   returns the maximum or minimum of the block as result.
##'
##' @details  If both the \strong{block.number} and
##'   \strong{block.length} arguments are NULL, the data will be
##'   split into annual blocks. If not, the
##'   size of a block is determined by the \strong{block.length}
##'   argument or calculated via the \strong{block.number} argument if
##'   the previous one is set to NULL. For calculating the mean of
##'   the blocks, please have a look at the \code{\link[stats]{ave}}
##'   function.
##'
##'   This function can be applied to lists of \pkg{xts} class elements
##'   too. 
##'
##' @param x A list of objects of class \pkg{xts}.
##' @param block.number Specifies the number of blocks the input data
##'   is going to be separated in. Default = NULL.
##' @param block.length Length of the blocks. For the sake of
##'   simplicity the last block is not forced to match the length of
##'   the other plots. Default = NULL.
##' @param extreme.type String specifying whether the maxima ("max")
##'   or minima ("min") of each block should be extracted. Default:
##'   "max". 
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##' 
##' @family extremes
##'
##' @export
##'
##' @importFrom xts xts
##' @importFrom zoo index
##' @importFrom parallel mclapply
##'
##' @family extremes
##' 
##' @return Same class as the input.
##' @author Philipp Mueller
##' 
##' @examples
##'   block( temp.potsdam )
block.list <- function( x, block.number = NULL, block.length = NULL,
                       extreme.type = c( "max", "min" ),
                       mc.cores = NULL ){
  if ( !is.null( mc.cores ) ){
    return( mclapply( x, function( xx )
      block( xx, block.number = block.number,
            block.length = block.length,
            extreme.type = extreme.type,
            mc.cores = mc.cores ),
      mc.cores = mc.cores ) )
  } else {
    return( lapply( x, function( xx )
      block( xx, block.number = block.number,
            block.length = block.length,
            extreme.type = extreme.type,
            mc.cores = mc.cores ) ) )
  }
}
##' @title Blocking data
##' @description Separates the input into blocks of equal size and
##'   returns the maximum or minimum of the block as result.
##'
##' @details If both the \strong{block.number} and
##'   \strong{block.length} arguments are NULL, the data will be
##'   split into annual blocks. If not, the
##'   size of a block is determined by the \strong{block.length}
##'   argument or calculated via the \strong{block.number} argument if
##'   the previous one is set to NULL. For calculating the mean of
##'   the blocks, please have a look at the \code{\link[stats]{ave}}
##'   function.
##'
##' @param x An object of class \pkg{xts}.
##' @param block.number Specifies the number of blocks the input data
##'   is going to be separated in. Default = NULL.
##' @param block.length Length of the blocks. For the sake of
##'   simplicity the last block is not forced to match the length of
##'   the other plots. Default = NULL.
##' @param extreme.type String specifying whether the maxima ("max")
##'   or minima ("min") of each block should be extracted. Default:
##'   "max". 
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##' 
##' @family extremes
##'
##' @export
##'
##' @importFrom xts xts
##' @importFrom zoo index
##' @importFrom parallel mclapply
##'
##' @family extremes
##' 
##' @return Same class as the input.
##' @author Philipp Mueller
##' 
##' @examples
##'   block( temp.potsdam )
block.xts <- block.default <- function( x,
                                       block.number = NULL,
                                       block.length = NULL,
                                       extreme.type = c( "max", "min" ),
                                       mc.cores = NULL ){
  if ( !all( class( x ) == c( "xts", "zoo" ) ) )
    stop( "The block.default function works to input of class 'xts' only!" )
  ## Initializing. The 'block.length' is the most important parameter
  if ( !is.null( block.length ) || !is.null( block.number ) ){
    separation.mode <- "none"
    if ( !is.null( block.length ) ){
      block.number <- floor( length( x )/ block.length ) + 1
    } else {
      ## separation according to the number of blocks is used
      if ( block.number <= 1 )
        stop( "Provide a block number greater than 1!" )
      if ( block.number* 5 > length( x ) )
        warning( "There are less than five times data points than actual blocks. This are many of blocks." )
      block.length <- length( x )/ ( block.number ) +
        1/ ( block.number )
    }
  } else {
    separation.mode <- "years"
  }
  if ( missing( extreme.type ) )
    extreme.type <- "max"
  extreme.type <- match.arg( extreme.type )
  ## according to the desired block.length the data is now separated
  ## into snippets and those are saved inside a list
  if ( separation.mode == "years" ){
    x.index <- data.frame( value = x,
                          index = year( x ),
                          row.names = index( x ) )
  } else {
    ## All data belonging to the same block share the same index value
    x.index <- data.frame(
        value = x,
        index =  floor( ( seq( 1 : length( x ) ) - 1 )/
                        block.length ) + 1,
        row.names = index( x ) )
  }
  x.blocked <- split( x.index, x.index$index )
  ## Extract the maxima or minima from the blocked data
  if ( extreme.type == "max" ){
    x.extremes <- Reduce(
        rbind, lapply( x.blocked, function( xx ){
          data.frame(
              date = row.names( xx )[ which.max( xx[[ 1 ]] ) ],
              value = xx[ which.max( xx[[ 1 ]] ), 1 ] ) } ) )
  } else
    x.extremes <- Reduce(
        rbind, lapply( x.blocked, function( xx ){
          data.frame(
              date = row.names( xx )[ which.min( xx[[ 1 ]] ) ],
              value = xx[ which.min( xx[[ 1 ]] ), 1 ] ) } ) )
  extremes.xts <- xts( x.extremes[[ 2 ]] ,
                      order.by = as.Date( x.extremes[[ 1 ]] ) )
  return( extremes.xts )
}

##' @title Decluster a time series.
##' @description Decluster point over threshold data as a
##'   preprocessing step for the fitting of the generalized Pareto
##'   (GP) distribution.
##'
##' @details This function determines clusters in an object of class
##'   \pkg{xts} and extracts just their maximal values in order to
##'   remove short-range correlations. All position of exceedances
##'   will be considered belonging to a cluster until at least
##'   \strong{cluster.distance} consecutive points fall below the
##'   \strong{threshold}.
##'
##'   The argument \strong{cluster.distance} will be determined using
##'   the extremal index as suggested in \emph{Ferro & Segers (2003)}
##'   when set to NULL. It, thus, provides a non-parametric way of
##'   declustering.
##' 
##'   It also features a special treatment of missing values. All of
##'   them will be kept and the returned time series will have the
##'   same length as the input. Separate missing values and small
##'   sequences will be omitted in the cluster determination. But if
##'   more than 15 missing values appear in a row, they will be
##'   replaced with the minimal value of the time series for the
##'   cluster detection. This way exceedances separated over a big
##'   temporal distance will not be considered to belong to the same
##'   cluster.
##'
##'   This function can be applied to lists of \pkg{xts} or
##'   \emph{numeric} class elements too. 
##'
##' @param x Either an object of class \pkg{xts} or \emph{numeric} or
##'   a list of those. The full time series has to be provided. Not
##'   just the exceedances over the \strong{threshold}!
##' @param threshold Numerical value. Has to be set sufficient high to
##'   fulfill the asymptotic condition for the GP distribution.
##' @param cluster.distance Numerical value specifying how many points
##'   have to be below the \strong{threshold} for the next point to be
##'   considered the starting point of a new cluster. Only supply a
##'   value when you really know what you are doing! Default = NULL
##' @param silent Whether or not to display warnings.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return Same class as the input. The time series will be similar
##'   to their original versions with all the elements within a
##'   cluster having smaller values than the clusters maximum being
##'   replaced by NA.
##'
##' @family extremes
##' 
##' @export
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @author Philipp Mueller
##' 
decluster <- function( x, threshold, cluster.distance = NULL,
                      silent = FALSE, mc.cores = NULL ){ 
  UseMethod( "decluster" )
}

##' @title Decluster a time series.
##' @description Decluster point over threshold data as a
##'   preprocessing step for the fitting of the generalized Pareto
##'   (GP) distribution.
##'
##' @details This function determines clusters in an object of class
##'   \pkg{xts} and extracts just their maximal values in order to
##'   remove short-range correlations. All position of exceedances
##'   will be considered belonging to a cluster until at least
##'   \strong{cluster.distance} consecutive points fall below the
##'   \strong{threshold}.
##'
##'   The argument \strong{cluster.distance} will be determined using
##'   the extremal index as suggested in \emph{Ferro & Segers (2003)}
##'   when set to NULL. It, thus, provides a non-parametric way of
##'   declustering.
##' 
##'   It also features a special treatment of missing values. All of
##'   them will be kept and the returned time series will have the
##'   same length as the input. Separate missing values and small
##'   sequences will be omitted in the cluster determination. But if
##'   more than 15 missing values appear in a row, they will be
##'   replaced with the minimal value of the time series for the
##'   cluster detection. This way exceedances separated over a big
##'   temporal distance will not be considered to belong to the same
##'   cluster.
##'
##'   This function can be applied to lists of \pkg{xts} or
##'   \emph{numeric} class elements too. 
##'
##' @param x A list of object of class \pkg{xts} or class
##'   \emph{numeric}. The full time series has to be provided. Not
##'   just the exceedances over the \strong{threshold}!
##' @param threshold Numerical value. Has to be set sufficient high to
##'   fulfill the asymptotic condition for the GP distribution.
##' @param cluster.distance Numerical value specifying how many points
##'   have to be below the \strong{threshold} for the next point to be
##'   considered the starting point of a new cluster. Only supply a
##'   value when you really know what you are doing! Default = NULL
##' @param silent Whether or not to display warnings.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return Same class as the input. The time series will be similar
##'   to their original versions with all the elements within a
##'   cluster having smaller values than the clusters maximum being
##'   replaced by NA.
##'
##' @family extremes
##' 
##' @export
##' @importFrom xts xts
##' @author Philipp Mueller
##' 
decluster.list <- function( x, threshold, cluster.distance = NULL,
                           silent = FALSE, mc.cores = NULL ){
  if ( !is.null( mc.cores ) ){
    return( mclapply( x, decluster, threshold = threshold,
                   cluster.distance = cluster.distance,
                   silent = silent, mc.cores = mc.cores ) )
  } else {
    return( lapply( x, decluster, threshold = threshold,
                   cluster.distance = cluster.distance,
                   silent = silent, mc.cores = mc.cores ) )
  }
}

##' @title Decluster a time series.
##' @description Decluster point over threshold data as a
##'   preprocessing step for the fitting of the generalized Pareto
##'   (GP) distribution.
##'
##' @details This function determines clusters in an object of class
##'   \pkg{xts} and extracts just their maximal values in order to
##'   remove short-range correlations. All position of exceedances
##'   will be considered belonging to a cluster until at least
##'   \strong{cluster.distance} consecutive points fall below the
##'   \strong{threshold}.
##'
##'   The argument \strong{cluster.distance} will be determined using
##'   the extremal index as suggested in \emph{Ferro & Segers (2003)}
##'   when set to NULL. It, thus, provides a non-parametric way of
##'   declustering.
##' 
##'   It also features a special treatment of missing values. All of
##'   them will be kept and the returned time series will have the
##'   same length as the input. Separate missing values and small
##'   sequences will be omitted in the cluster determination. But if
##'   more than 15 missing values appear in a row, they will be
##'   replaced with the minimal value of the time series for the
##'   cluster detection. This way exceedances separated over a big
##'   temporal distance will not be considered to belong to the same
##'   cluster.
##'
##'   This function can be applied to lists of \pkg{xts} class elements
##'   too. 
##'
##' @param x An object of class \pkg{xts} or \emph{numeric}. The full
##'   time series has to be provided. Not just the exceedances over
##'   the \strong{threshold}!
##' @param threshold Numerical value. Has to be set sufficient high to
##'   fulfill the asymptotic condition for the GP distribution.
##' @param cluster.distance Numerical value specifying how many points
##'   have to be below the \strong{threshold} for the next point to be
##'   considered the starting point of a new cluster. Only supply a
##'   value when you really know what you are doing! Default = NULL
##' @param silent Whether or not to display warnings.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return Same class as the input. The time series will be similar
##'   to their original versions with all the elements within a
##'   cluster having smaller values than the clusters maximum being
##'   replaced by NA.
##'
##' @family extremes
##' 
##' @export
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @author Philipp Mueller
##' 
decluster.xts <- decluster.default <- function( x, threshold,
                                               cluster.distance =
                                                 NULL, silent = FALSE,
                                               mc.cores = NULL ){
  ## Caution: x is the full time series and not the blocked one!
  if ( any( is.na( x ) ) ){
    ## Handling of missing values.
    ## I will introduce a heuristic here. When less minimal.na points
    ## are missing, they are just removed. So they do not interfere
    ## with the determination of the cluster end. But if more points
    ## are missing, they are replaced with an arbitrary value below
    ## the threshold. This one will be overwritten with NA later on
    ## again. But in the meanwhile it will separate the last cluster
    ## from the next one. The idea is the following: When whole year
    ## of data is missing, it makes no sense to assign data points
    ## separated by such a distance to one and the same cluster. As
    ## done in the extRemes::decluster function.
    minimal.na <- 15 # NA in a row will be replaced with a low value
    na.index <- which( is.na( x ) )
    na.distance <- diff( na.index )
    na.cluster <- rep( 1, length( na.index ) )
    ## Each sequence of NA will get a separate number. As soon as
    ## there is a single point between to missing values, those two
    ## will be considered belonging to different sequences.
    if ( length( na.index ) > 1 ){
      na.cluster[ 2 : length( na.index ) ] <-
        1 + cumsum( na.distance > 1 )
    }
    na.list <- split( na.index, na.cluster )
    ## Logical vector whether or not to replace a NA by the minimum of
    ## the time series
    na.replace <- Reduce( c, lapply( na.list, function( yy ){
      if ( length( yy ) >= minimal.na ){
        return( rep( TRUE, length( yy ) ) )
      } else {
        return( rep( FALSE, length( yy ) ) ) } }
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
  exceedances.position <- exceedances.position
  exceedances <- x.no.na[ exceedances.position ]
  ## Amount of points above the threshold
  exceedances.number <- sum( exceedances.position )
  ## Index of those points in the ts 'x'
  exceedances.index <-
    ( 1 : length( x.no.na ) )[ exceedances.position ] 
  which.cluster <- rep( 1, exceedances.number )
  ## Number of indices the exceedances are apart from each other
  exceedances.distance <- diff( exceedances.index )
  ## Which point belongs to which cluster
  which.cluster[ 2 : exceedances.number ] <- 1 +
    cumsum( exceedances.distance > cluster.distance )
  ## Create a list in which element is a numerical vector containing
  ## the exceedances within a certain cluster.
  cluster.list <- split( exceedances, which.cluster )
  ## Replacing all events in a cluster smaller than its maximum with
  ## NaN.
  if ( is.xts( x.no.na ) ) {
    ## Extra care when input is of type xts
    index.not.max <- Reduce( c, lapply( cluster.list, function( yy )
      index( yy[ -which( yy == max( yy ) ) ] ) ) )
    exceedances[ index( exceedances ) %in% index.not.max ] <- NA
  } else {
    exceedances <- Reduce( c, lapply( cluster.list, function( yy ){
      yy[ -which( yy == max( yy ) ) ] <- NA
      return( yy ) } ) )
  }
  ## Introducing the NaN to the original time series.
  x.no.na[ x.no.na > threshold ] <- exceedances
  if ( any( is.na( x ) ) ){
    ## Now I have to introduce the NA again or else my time series
    ## will shrink and I have both missing values marked by NA and
    ## those complete gone from the time series
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
##' @details The extremal index can be thought of as the inverse of
##'   the mean cluster size. It can be calculated by the \emph{blocks}
##'   method of Ferro and Segers, 2003. I will use the bias-free
##'   estimator provided in equation (4) in their paper. This one is
##'   supposed to be the most robust one and is relying on a moment
##'   estimation.
##'
##'   Another way to estimate it, would be using the \emph{runs}
##'   method as in Stuart Coles (2001). But therefore one had to know
##'   the minimal distance between the clusters first. Since the whole
##'   point of this function to estimate exactly this quantity for its
##'   use in the \code{\link{decluster}} function, I don't see the
##'   point of implementing this method too.
##'
##' @param x Time series of class \pkg{xts} or \emph{numerical}.
##' @param threshold Only events exceeding a specific threshold will
##'   be considered extreme events and thus will be subject of the
##'   declustering.
##' @param silent Whether or not to display warnings.
##'
##' @family extremes
##'
##' @return Numerical vector containing c( extremal index, number of
##'   clusters, minimal distance between clusters (minimal.distance))
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
    ## If the largest interexceedance time (distance between
    ## exceedances) is smaller than 2, the (first order) unbiased
    ## estimator in the else counterpart is not defined. Therefore I
    ## will use a fallback one using the true moments of the
    ## exceedance distance distribution.
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
    ## We have a tie. There are several clusters having the same
    ## distance as the one with the cluster.number - 1 largest
    ## interexceedance distance. I don't really see the problem just
    ## now, but the paper of Ferro & Seger asked to reduce the cluster
    ## number until there is no tie anymore.
    cluster.index <- 1 : ( exceedances.number - 1 )
    ## Clusters sharing the exceedance distance with other ones. 
    cluster.ties <- cluster.index[ distance.difference == 0 ]
    cluster.step <- cluster.index[ distance.difference != 0 ]
    ## All indices between the last jump in the exceedance distance
    ## just before the current cluster.number largest cluster distance
    ## and the later one.
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

##' @title Apply a threshold to a time series
##' @description Extracts all data above a certain threshold of a
##'   \pkg{xts} class object or a list of those.
##' @details After extracting all data above the \strong{threshold},
##'   the \strong{threshold} argument itself is going to be subtracted
##'   from the data.
##'
##'   In addition, all exceedance can be declustered. This step is
##'   highly recommendes, since the extreme value theory is only valid
##'   for data without correlations (which are present in most
##'   measured data). Using this method, at least the short-range
##'   correlations in the data can be taken care of.
##'
##' @param x Either an object of class \pkg{xts} or a list of those.
##' @param threshold Numerical value, which has to be exceeded by the
##'   data.
##' @param decluster Logical flag indicating whether or not to
##'   decluster the obtained exceedances over the
##'   \strong{threshold}. Default = TRUE.
##' @param cluster.distance Numerical value specifying how many points
##'   have to be below the \strong{threshold} for the next point to be
##'   considered the starting point of a new cluster. Only supply a
##'   value when you really know what you are doing! Default = NULL.
##' @param extreme.type String specifying whether the events below a
##'   very low ("min") or above a very high ("max") \strong{threshold}
##'   should be extracted. The \strong{threshold} will be subtracted
##'   from the data in both cases. Default: "max". 
##' @param na.rm Logical flag indicating whether to remove all NA
##'   values from the time series (removed points in clusters). For
##'   important steps, like calculating the \emph{Lmoments} of a time
##'   series, there must not be any NA left. Default = TRUE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family extremes
##'
##' @importFrom parallel mclapply
##' 
##' @export
##' @return Same class as the input
##' @author Philipp Mueller
threshold <- function( x, threshold, decluster = TRUE,
                      cluster.distance = NULL,
                      extreme.type = c( "max", "min" ), na.rm = TRUE,
                      mc.cores = TRUE ){
  UseMethod( "threshold" )
}
##' @title Apply a threshold to a time series
##' @description Extracts all data above a certain threshold of a
##'   \pkg{xts} class object or a list of those.
##' @details After extracting all data above the \strong{threshold},
##'   the \strong{threshold} argument itself is going to be subtracted
##'   from the data.
##'
##'   In addition, all exceedance can be declustered. This step is
##'   highly recommendes, since the extreme value theory is only valid
##'   for data without correlations (which are present in most
##'   measured data). Using this method, at least the short-range
##'   correlations in the data can be taken care of.
##'
##' @param x A list of objects of class \pkg{xts}.
##' @param threshold Numerical value, which has to be exceeded by the
##'   data.
##' @param decluster Logical flag indicating whether or not to
##'   decluster the obtained exceedances over the
##'   \strong{threshold}. Default = TRUE.
##' @param cluster.distance Numerical value specifying how many points
##'   have to be below the \strong{threshold} for the next point to be
##'   considered the starting point of a new cluster. Only supply a
##'   value when you really know what you are doing! Default = NULL.
##' @param extreme.type String specifying whether the events below a
##'   very low ("min") or above a very high ("max") \strong{threshold}
##'   should be extracted. The \strong{threshold} will be subtracted
##'   from the data in both cases. Default: "max". 
##' @param na.rm Logical flag indicating whether to remove all NA
##'   values from the time series (removed points in clusters). For
##'   important steps, like calculating the \emph{Lmoments} of a time
##'   series, there must not be any NA left. Default = TRUE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family extremes
##' 
##' @importFrom parallel mclapply
##'
##' @export
##' @return Same class as the input
##' @author Philipp Mueller
threshold.list <- function( x, threshold, decluster = TRUE,
                           cluster.distance = NULL,
                           extreme.type = c( "max", "min" ),
                           na.rm = TRUE, mc.cores = NULL ){
  if ( !is.null( mc.cores ) ){
    return( lapply( x, threshold.xts, threshold = threshold,
                   decluster = decluster,
                   cluster.distance = cluster.distance,
                   extreme.type = extreme.type,
                   na.rm = na.rm, mc.cores = mc.cores ) )
  } else {
    return( lapply( x, threshold.xts, threshold = threshold,
                   decluster = decluster,
                   cluster.distance = cluster.distance,
                   extreme.type = extreme.type,
                   na.rm = na.rm, mc.cores = mc.cores ) )
  }
}
##' @title Apply a threshold to a time series
##' @description Extracts all data above a certain threshold of a
##'   \pkg{xts} class object or a list of those.
##' @details After extracting all data above the \strong{threshold},
##'   the \strong{threshold} argument itself is going to be subtracted
##'   from the data.
##'
##'   In addition, all exceedance can be declustered. This step is
##'   highly recommendes, since the extreme value theory is only valid
##'   for data without correlations (which are present in most
##'   measured data). Using this method, at least the short-range
##'   correlations in the data can be taken care of.
##'
##' @param x An object of class \pkg{xts}.
##' @param threshold Numerical value, which has to be exceeded by the
##'   data.
##' @param decluster Logical flag indicating whether or not to
##'   decluster the obtained exceedances over the
##'   \strong{threshold}. Default = TRUE.
##' @param cluster.distance Numerical value specifying how many points
##'   have to be below the \strong{threshold} for the next point to be
##'   considered the starting point of a new cluster. Only supply a
##'   value when you really know what you are doing! Default = NULL.
##' @param extreme.type String specifying whether the events below a
##'   very low ("min") or above a very high ("max") \strong{threshold}
##'   should be extracted. The \strong{threshold} will be subtracted
##'   from the data in both cases. Default: "max". 
##' @param na.rm Logical flag indicating whether to remove all NA
##'   values from the time series (removed points in clusters). For
##'   important steps, like calculating the \emph{Lmoments} of a time
##'   series, there must not be any NA left. Default = TRUE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family extremes
##'
##' @importFrom parallel mclapply
##'
##' @export
##' @return Same class as the input
##' @author Philipp Mueller
threshold.xts <- threshold.default <- function( x, threshold,
                                               decluster = TRUE,
                                               cluster.distance =
                                                 NULL,
                                               extreme.type =
                                                 c( "max", "min" ),
                                               na.rm = TRUE,
                                               mc.cores = NULL ){  
  if ( !all( class( x ) == c( "xts", "zoo" ) ) )
    stop( "The threshold.default works to input of class 'xts' only!" )
  if ( missing( x ) )
    stop( "Please provide a time series to apply the threshold to!" )
  if ( missing( threshold ) )
    stop( "Please provide a threshold to be applied to the time series!" )
  ## Whether to handle the exceedances above a very high threshold or
  ## those events below a very low one.
  if ( missing( extreme.type ) ){
    extreme.type <- "max"
  } else {
    extreme.type <- match.arg( extreme.type )
  }
  ## If one deals with the events below a low threshold, the series
  ## will be inverted.
  if ( extreme.type == "min" ){
    x <- x * -1
    threshold <- threshold * -1
  }
  ## declustering of the data
  if ( decluster ){
    x <- climex::decluster( x, threshold )
  }
  x.threshold <- x[ x > threshold ] - threshold
  ## Flipping back the time series
  if ( extreme.type == "min" ){
    x.threshold <- x.threshold * -1
  }
  ## removing the NA
  if ( na.rm )
    x.threshold <- stats::na.omit( x.threshold )        
  return( x.threshold )
}

##' @title Calculation of the return levels.
##' @description Calculate arbitrary return levels and their error
##'   estimates for generalized extreme value (GEV) and generalized
##'   Pareto (GP) distributions.
##'
##' @details Uses the \code{\link{rlevd}} function at its core (a port
##'   from the \pkg{extRemes} package) but also can handle the outputs
##'   of the \code{\link{fit.gev}} and \code{\link{fit.gpd}} functions,
##'   is capable of calculating numerous return levels at once, and
##'   also calculates the errors of the return levels. For the errors
##'   the ML fit is using the option \code{hessian=TRUE} (if not done
##'   already) or uses a Monte Carlo based approach. If no fitting
##'   object is provided, no errors will be calculated.
##'
##'   This function is also capable of working with lists of fit
##'   or numerical objects.
##'
##' @param x Either of class \emph{numeric}, \emph{climex.fit.gev},
##'   \emph{climex.fit.gpd} or a list of those objects.
##' @param return.period Numeric vector of the return periods in years.
##'   Default = 100.
##' @param error.estimation Method for calculating the standard errors
##'   of the fitted results. The errors of the GEV/GP parameters will
##'   be calculated as the square roots of the diagonal elements of
##'   the inverse of the hessian matrix. The latter will be evaluated
##'   at the maximum likelihood estimates (MLE) of the GEV/GP
##'   parameters.
##'
##'   For all three methods of estimating the fitting
##'   errors of the return levels underlying series of threshold
##'   exceedances or block maxima is required. In case the user
##'   supplies numerical values to specify GEV/GP parameters and not
##'   the output of either the \code{\link{fit.gev}} or
##'   \code{\link{fit.gpd}} function no error estimation for the
##'   return level can be performed. 
##'
##'   \emph{MLE}: The standard error of the return level is
##'     calculated using the Delta method and the maximum likelihood
##'     estimates of the GEV/GP parameters. Note: For positive shape
##'     parameters bigger than 0.3 this approach tends to highly
##'     overestimates the errors of the return levels.
##' 
##'   \emph{MC}: Alternative one can use a Monte Carlo method for
##'     which \strong{monte.carlo.sample.size} samples of the same
##'     size as \strong{x} will be drawn from a GEV/GP distribution
##'     constituted by the obtained MLE of the GEV/GP parameters of
##'     \strong{x}. The standard error is then calculated via the
##'     square of the variance of all fitted GEV/GP parameters and
##'     calculated return levels. Note: In its essence this approach
##'     is not an estimation of the error involved in fitting the time
##'     series to a GEV/GP distribution. It is rather the mean error
##'     of fitting a GEV/GP-distribution with the same length and
##'     parameters as estimated ones.
##'
##'   \emph{bootstrap}: Using this option the provided time series
##'     \strong{x} will be sampled with replacement
##'     \strong{bootstrap.sample.size} times and with the same length
##'     as the original time series. The standard errors of the
##'     GEV/GP parameters and return levels of all those sampled
##'     series is calculated and returned as an estimate of the
##'     fitting error. 
##'     Note: Since the data is (hopefully) GEV/GP-distributed, such
##'     a sampling has to be treated with a lot of care.
##'
##'     Sometimes the inversion of the hessian fails (since the are
##'     some NaN in the hessian) when calculating the error estimates
##'     using the maximum likelihood approach (MLE) (which is also the
##'     reason why the \pkg{ismev} package occasionally does not
##'     work). In such cases the Monte Carlo (MC) method is used as a
##'     fallback.
##' 
##'   \emph{none} skips the calculation of the error. Default = "MLE".
##' @param model Determining whether to calculate the initial
##'   parameters of the GEV or GP function. Default = "gev".
##' @param monte.carlo.sample.size Number of samples used to obtain
##'   the Monte Carlo estimate of the standard error of the fitting.
##'   Default = 100.
##' @param bootstrap.sample.size Number of samples with replacements
##'   to drawn from the original series \strong{x} in order to
##'   determine the standard errors for the GP parameters and return
##'   levels. Default = 100.
##' @param threshold Optional threshold for the GP model. If present
##'   it will be added to the return level to produce a value which
##'   fits to underlying time series. Default = NULL.
##' @param total.length Total number of observations in the time
##'   series the exceedance were obtained from (before! applying the
##'   threshold). This argument is needed to calculate the standard
##'   error of the return level via the delta method of the MLE in the
##'   GP model. Default = NULL.
##' @param thresholded.time.series Time series used with
##'   \code{\link{fit.gpd}} on which already a threshold (the one
##'   supplied here as well) was applied. Necessary to transform the
##'   return level for numerical input and the GP model from m-th
##'   observation return level to annual return level. If omitted the
##'   return level will be per observation. Default = NULL.
##' @param extreme.type String indicating whether to calculate the
##'   quantiles at the right ("max") or left ("min") side of the PDF
##'   of the series. Default = "max".
##' @param silent Throws an warning whenever the "gpd" model is used
##'   and the \strong{thresholded.time.series} is not supplied. Since
##'   this can be annoying one can also disable it. Default = FALSE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return A list containing the estimates "return.level" and their
##'   standard errors "error" if the input was a fitting object or a
##'   numerical input. If, on the other hand, the input was a list of
##'   such objects, the output is a list of the aforementioned list.
##' @export
##'
##' @importFrom xts apply.yearly
##' @importFrom numDeriv hessian
##' @importFrom parallel mclapply
##' 
##' @family extremes
##'
##' @examples
##' fit.results <- fit.gev( block( anomalies( temp.potsdam ) ) )
##' return.level( fit.results, return.period = c( 10, 50, 100 ),
##'               error.estimation = "MLE" )
return.level <- function( x, return.period = 100,
                         error.estimation = c( "none", "MC", "MLE",
                                              "bootstrap" ),
                         model = c( "gev", "gpd" ),
                         monte.carlo.sample.size = 100,
                         bootstrap.sample.size = 100,
                         threshold = NULL, total.length = NULL,
                         thresholded.time.series = NULL,
                         extreme.type = c( "max", "min" ),
                         silent = FALSE, mc.cores = NULL, ... ){
  UseMethod( "return.level", x )
}
##' @title Calculation of the return levels.
##' @description Calculate arbitrary return levels and their error
##'   estimates for generalized extreme value (GEV) and generalized
##'   Pareto (GP) distributions.
##'
##' @details Uses the \code{\link{rlevd}} function at its core (a port
##'   from the \pkg{extRemes} package) but also can handle the outputs
##'   of the \code{\link{fit.gev}} and \code{\link{fit.gpd}} functions,
##'   is capable of calculating numerous return levels at once, and
##'   also calculates the errors of the return levels. For the errors
##'   the ML fit is using the option \code{hessian=TRUE} (if not done
##'   already) or uses a Monte Carlo based approach. If no fitting
##'   object is provided, no errors will be calculated.
##'
##'   This function is also capable of working with lists of fit
##'   or numerical objects.
##'
##' @param x A list of class \emph{numeric}, \emph{climex.fit.gev},
##'   or \emph{climex.fit.gpd} objects.
##' @param return.period Numeric vector of the return periods in years.
##'   Default = 100.
##' @param error.estimation Method for calculating the standard errors
##'   of the fitted results. The errors of the GEV/GP parameters will
##'   be calculated as the square roots of the diagonal elements of
##'   the inverse of the hessian matrix. The latter will be evaluated
##'   at the maximum likelihood estimates (MLE) of the GEV/GP
##'   parameters.
##'
##'   For all three methods of estimating the fitting
##'   errors of the return levels underlying series of threshold
##'   exceedances or block maxima is required. In case the user
##'   supplies numerical values to specify GEV/GP parameters and not
##'   the output of either the \code{\link{fit.gev}} or
##'   \code{\link{fit.gpd}} function no error estimation for the
##'   return level can be performed. 
##'
##'   \emph{MLE}: The standard error of the return level is
##'     calculated using the Delta method and the maximum likelihood
##'     estimates of the GP parameters. Note: For positive shape
##'     parameters bigger than 0.3 this approach tends to highly
##'     overestimates the errors of the return levels.
##' 
##'   \emph{MC}: Alternative one can use a Monte Carlo method for
##'     which \strong{monte.carlo.sample.size} samples of the same
##'     size as \strong{x} will be drawn from a GEV/GP distribution
##'     constituted by the obtained MLE of the GEV/GP parameters of
##'     \strong{x}. The standard error is then calculated via the
##'     square of the variance of all fitted GEV/GP parameters and
##'     calculated return levels. Note: In its essence this approach
##'     is not an estimation of the error involved in fitting the time
##'     series to a GEV/GP distribution. It is rather the mean error
##'     of fitting a GEV/GP-distribution with the same length and
##'     parameters as estimated ones.
##'
##'   \emph{bootstrap}: Using this option the provided time series
##'     \strong{x} will be sampled with replacement
##'     \strong{bootstrap.sample.size} times and with the same length
##'     as the original time series. The standard errors of the
##'     GEV/GPD parameters and return levels of all those sampled
##'     series is calculated and returned as an estimate of the
##'     fitting error. 
##'     Note: Since the data is (hopefully) GEV/GP-distributed, such
##'     a sampling has to be treated with a lot of care.
##'
##'     Sometimes the inversion of the hessian fails (since the are
##'     some NaN in the hessian) when calculating the error estimates
##'     using the maximum likelihood approach (MLE) (which is also the
##'     reason why the \pkg{ismev} package occasionally does not
##'     work). In such cases the Monte Carlo (MC) method is used as a
##'     fallback.
##' 
##'   \emph{none} skips the calculation of the error. Default = "MLE".

##' @param model String determining whether to calculate the initial
##'   parameters of the GEV ("gev") or GP ("gpd") function. Default =
##'   "gev"
##' @param monte.carlo.sample.size Number of samples used to obtain
##'   the Monte Carlo estimate of the standard error of the fitting.
##'   Default = 100.
##' @param bootstrap.sample.size Number of samples with replacements
##'   to drawn from the original series \strong{x} in order to
##'   determine the standard errors for the GEV/GP parameters and return
##'   levels. Default = 100.
##' @param threshold Optional threshold for the GP model. If present
##'   it will be added to the return level to produce a value which
##'   fits to underlying time series. Default = NULL.
##' @param total.length Total number of observations in the time
##'   series the exceedance were obtained from (before! applying the
##'   threshold). This argument is needed to calculate the standard
##'   error of the return level via the delta method of the MLE in the
##'   GP model. Default = NULL.
##' @param thresholded.time.series Time series used with
##'   \code{\link{fit.gpd}} on which already a threshold (the one
##'   supplied here as well) was applied. Necessary to transform the
##'   return level for numerical input and the GP model from m-th
##'   observation return level to annual return level. If omitted the
##'   return level will be per observation. Default = NULL.
##' @param extreme.type String indicating whether to calculate the
##'   quantiles at the right ("max") or left ("min") side of the PDF
##'   of the series. Default = "max".
##' @param silent Throws an warning whenever the "gpd" model is used
##'   and the \strong{thresholded.time.series} is not supplied. Since
##'   this can be annoying one can also disable it. Default = FALSE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return A list containing the estimates "return.level" and their
##'   standard errors "error" if the input was a fitting object or a
##'   numerical input. If, on the other hand, the input was a list of
##'   such objects, the output is a list of the aforementioned list.
##' @export
##'
##' @importFrom xts apply.yearly
##' @importFrom numDeriv hessian
##' @importFrom parallel mclapply
##' 
##' @family extremes
##'
##' @examples
##' fit.results <- fit.gev( block( anomalies( temp.potsdam ) ) )
##' return.level( fit.results, return.period = c( 10, 50, 100 ),
##'               error.estimation = "MLE" )          
return.level.list <- function( x, return.period = 100,
                              error.estimation =
                                c( "none", "MC", "MLE", "bootstrap" ),
                              model = c( "gev", "gpd" ),
                              monte.carlo.sample.size = 100,
                              bootstrap.sample.size = 100,
                              threshold = NULL, total.length = NULL,
                              thresholded.time.series = NULL,
                              extreme.type = c( "max", "min" ),
                              silent = FALSE, mc.cores = NULL, ... ){
  if ( !is.null( mc.cores ) ){
    x.result <- mclapply(
        x, return.level, return.period = return.period,
        error.estimation = error.estimation, model = model,
        monte.carlo.sample.size = monte.carlo.sample.size,
        bootstrap.sample.size = bootstrap.sample.size,
        threshold = threshold, total.length = total.length,
        thresholded.time.series = thresholded.time.series,
        extreme.type = extreme.type,
        silent = silent, mc.cores = mc.cores, ... )
    } else {
    x.result <- lapply(
        x, return.level, return.period = return.period,
        error.estimation = error.estimation, model = model,
        monte.carlo.sample.size = monte.carlo.sample.size,
        bootstrap.sample.size = bootstrap.sample.size,
        threshold = threshold, total.length = total.length,
        thresholded.time.series = thresholded.time.series,
        extreme.type = extreme.type,
        silent = silent, mc.cores = mc.cores, ... )
  }
  return( x.result )
}
##' @title Calculation of the return levels.
##' @description Calculate arbitrary return levels and their error
##'   estimates for generalized extreme value (GEV) distributions.
##'
##' @details Uses the \code{\link{rlevd}} function at its core (a port
##'   from the \pkg{extRemes} package) but also can handle the outputs
##'   of the \code{\link{fit.gev}} function, is capable of
##'   calculating numerous return levels at once, and also calculates
##'   the errors of the return levels. For the errors the ML fit is
##'   using the option \code{hessian=TRUE} (if not done already) or
##'   uses a Monte Carlo based approach. If no fitting object is
##'   provided, no errors will be calculated.
##'
##' @param x Of class \emph{climex.fit.gev}.
##' @param return.period Numeric vector of the return periods in years.
##'   Default = 100.
##' @param error.estimation Method for calculating the standard errors
##'   of the fitted results. The errors of the GEV parameters will
##'   be calculated as the square roots of the diagonal elements of
##'   the inverse of the hessian matrix. The latter will be evaluated
##'   at the maximum likelihood estimates (MLE) of the GEV
##'   parameters.
##'
##'   For all three methods of estimating the fitting errors of the
##'   return levels underlying series of threshold exceedances or
##'   block maxima is required. In case the user supplies numerical
##'   values to specify GEV parameters and not the output of the
##'   \code{\link{fit.gev}} function no error estimation for the
##'   return level can be performed.
##'
##'   \emph{MLE}: The standard error of the return level is
##'     calculated using the Delta method and the maximum likelihood
##'     estimates of the GEV parameters. Note: For positive shape
##'     parameters bigger than 0.3 this approach tends to highly
##'     overestimates the errors of the return levels.
##' 
##'   \emph{MC}: Alternative one can use a Monte Carlo method for
##'     which \strong{monte.carlo.sample.size} samples of the same
##'     size as \strong{x} will be drawn from a GEV distribution
##'     constituted by the obtained MLE of the GEV parameters of
##'     \strong{x}. The standard error is then calculated via the
##'     square of the variance of all fitted GEV parameters and
##'     calculated return levels. Note: In its essence this approach
##'     is not an estimation of the error involved in fitting the time
##'     series to a GEV distribution. It is rather the mean error
##'     of fitting a GEV-distribution with the same length and
##'     parameters as estimated ones.
##'
##'   \emph{bootstrap}: Using this option the provided time series
##'     \strong{x} will be sampled with replacement
##'     \strong{bootstrap.sample.size} times and with the same length
##'     as the original time series. The standard errors of the
##'     GEV parameters and return levels of all those sampled
##'     series is calculated and returned as an estimate of the
##'     fitting error. 
##'     Note: Since the data is (hopefully) GEV-distributed, such
##'     a sampling has to be treated with a lot of care.
##'
##'     Sometimes the inversion of the hessian fails (since the are
##'     some NaN in the hessian) when calculating the error estimates
##'     using the maximum likelihood approach (MLE) (which is also the
##'     reason why the \pkg{ismev} package occasionally does not
##'     work). In such cases the Monte Carlo (MC) method is used as a
##'     fallback.
##' 
##'   \emph{none} skips the calculation of the error. Default = "MLE".
##' @param model String determining whether to calculate the initial
##'   parameters of the GEV ("gev") or GPD ("gpd") function. Default =
##'   "gev". Note that this input argument will have no effect when
##'   calling the function with an object of class
##'   \emph{climex.fit.gev}.
##' @param monte.carlo.sample.size Number of samples used to obtain
##'   the Monte Carlo estimate of the standard error of the fitting.
##'   Default = 100.
##' @param bootstrap.sample.size Number of samples with replacements
##'   to drawn from the original series \strong{x} in order to
##'   determine the standard errors for the GEV parameters and return
##'   levels. Default = 100.
##' @param threshold Optional threshold for the GPD model. If present
##'   it will be added to the return level to produce a value which
##'   fits to underlying time series. Default = NULL. This argument
##'   won't be used when the function is called with an object of
##'   class \emph{climex.gev.fit}.
##' @param total.length Total number of observations in the time
##'   series the exceedance were obtained from (before! applying the
##'   threshold). This argument is needed to calculate the standard
##'   error of the return level via the delta method of the MLE in the
##'   GPD model. Default = NULL. This argument won't be used when the
##'   function is called with an object of class
##'   \emph{climex.gev.fit}.
##' @param thresholded.time.series Time series used with
##'   \code{\link{fit.gpd}} on which already a threshold (the one
##'   supplied here as well) was applied. Necessary to transform the
##'   return level for numerical input and the GPD model from m-th
##'   observation return level to annual return level. If omitted the
##'   return level will be per observation. Default = NULL. This
##'   argument won't be used when the function is called with an
##'   object of class \emph{climex.gev.fit}.
##' @param extreme.type String indicating whether to calculate the
##'   quantiles at the right ("max") or left ("min") side of the PDF
##'   of the series. Default = "max".
##' @param silent Throws an warning whenever the "gpd" model is used
##'   and the \strong{thresholded.time.series} is not supplied. Since
##'   this can be annoying one can also disable it. Default = FALSE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return A list containing the estimates "return.level" and their
##'   standard errors "error" if the input was a fitting object or a
##'   numerical input.
##' @export
##'
##' @importFrom xts apply.yearly
##' @importFrom numDeriv hessian
##' @importFrom parallel mclapply
##' 
##' @family extremes
##'
##' @examples
##' fit.results <- fit.gev( block( anomalies( temp.potsdam ) ) )
##' return.level( fit.results, return.period = c( 10, 50, 100 ),
##'               error.estimation = "MLE" )
return.level.climex.fit.gev <- 
  function( x, return.period = 100,
           error.estimation = c( "none", "MC", "MLE",
                                "bootstrap" ),
           model = c( "gev", "gpd" ),
           monte.carlo.sample.size = 100,
           bootstrap.sample.size = 100,
           threshold = NULL, total.length = NULL,
           thresholded.time.series = NULL,
           extreme.type = c( "max", "min" ),
           silent = FALSE, mc.cores = NULL, ... ){
    ## Whether to handle the extreme events at the right or left
    ## side of the PDF of the GEV distribution.
    ## If no particular input argument was provided for
    ## e.g. `error-estimation`, R dispatches the whole vector of
    ## different choices. By testing the length of the corresponding
    ## variable one can detect this case and use a default value
    ## instead.
    if ( !missing( model ) && length( model ) == 1 &&
         model != "gev" ){
      warning( "Mismatching object class and model argument!" )
    }
    if ( !missing( extreme.type ) && length( extreme.type ) == 1 ){
      warning(
          "Whether to extract the block maxima or minima will is specified by the fit function the input argument was generated by. The 'extreme.type' argument does not have any effect with this kind of input!" )
    }
    extreme.type <- x$control$extreme.type
    if ( missing( error.estimation ) || length( error.estimation ) != 1 ){
      error.estimation <- "none"
    }
    error.estimation <- match.arg( error.estimation )
    ## The location parameter and the time series itself have to
    ## be inverted when dealing with the block minima
    if ( extreme.type == "min" ){
      return.levels <-
        Reduce( c, lapply( return.period, function( yy )
          as.numeric( rlevd( yy, x$par[ 1 ]* -1, x$par[ 2 ],
                            x$par[ 3 ], model = "gev",
                            silent = silent ) ) ) )* -1
    } else {
      return.levels <-
        Reduce( c, lapply( return.period, function( yy )
          as.numeric( rlevd( yy, x$par[ 1 ], x$par[ 2 ],
                            x$par[ 3 ], model = "gev",
                            silent = silent ) ) ) )
    }
    ##
    ## Error estimation of the return level
    ##
    if ( error.estimation == "none" ){
      ## Dummy holding NaN instead of the return level errors.
      errors <- rep( NA, length( return.period ) )
      return( list( return.level = return.levels, error = errors ) )
    } else if ( error.estimation == "bootstrap" ){
      ## As a simple alternative the threshold exceedances will be
      ## sampled with replacement and the parameters and return
      ## levels are calculated for all of the resampled series. The
      ## bootstrap error is than calculated as the standard error of
      ## all the GEV parameters and return levels.
      bootstrap.sample.list <-
        lapply( c( 1 : bootstrap.sample.size ), function( xx )
          sample( x$x, size = length( x$x ), replace = TRUE ) )
      ## Fitting the GEV parameters (recursively)
      if ( extreme.type == "max" ){
        fitted.list <-
          lapply( bootstrap.sample.list, function( xx ){
            fit.gev( x = xx, initial = x$control$initial,
                    likelihood.function =
                      x$control$likelihood.function,
                    gradient.function = x$control$gradient.function,
                    error.estimation = "none",
                    return.period = return.period,
                    total.length = x$control$total.length,
                    extreme.type = extreme.type,
                    silent = TRUE ) } )
      } else {
        fitted.list <-
          lapply( bootstrap.sample.list, function( xx ){
            fit.gev( x = xx,
                    initial = x$control$initial,
                    likelihood.function =
                      x$control$likelihood.function,
                    gradient.function = x$control$gradient.function,
                    error.estimation = "none",
                    return.period = return.period,
                    total.length = x$control$total.length,
                    extreme.type = extreme.type,
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
      errors <- errors[ 4 : ( 3 + length( return.period ) ) ]
    } else if ( error.estimation == "MLE" ){
      if ( !any( names( x$control ) == "hessian" ) ){
        ## fit again and let stats::optim calculate the
        ## hessian. It's way more save this way.
        if ( extreme.type == "min" ){
          x$control$hessian <-
            fit.gev( x$x, initial = c( x$par[ 1 ]* -1,
                                      x$par[ 2 ],
                                      x$par[ 3 ] ),
                    extreme.type = extreme.type,
                    error.estimation = "MLE", silent = TRUE,
                    )$control$hessian
        } else {
          x$control$hessian <-
            fit.gev( x$x, initial = x$par,
                    extreme.type = extreme.type,
                    error.estimation = "MLE", silent = TRUE,
                    )$control$hessian
        }
      }
      ## Sometimes the obtained hessian is not invertible. If this
      ## is the case, recalculate it in order to access the fitting
      ## error estimates. Caution: this one will be without the
      ## constraints! Calculating the errors using the MLE
      ##
      ## If the shape parameter is exactly zero and the Gumbel
      ## distribution was fitted, the third row and column were just
      ## augmented by 0.
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
          dummy.matrix <- matrix( rep( 0, 9 ), nrow = 3,
                                 ncol = 3 )
          dummy.matrix[ 1 : 2, 1 : 2 ] <- error.covariance
          error.covariance <- dummy.matrix
        }
      }
      if ( class( error.covariance ) == "try-error" ){
        x.hessian <-
          numDeriv::hessian( likelihood, x = x$par, x.in = x$x,
                            model = "gev" )
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
          errors <-
            cbind( errors,
                  dz %*% error.covariance[ 1 : 2, 1 : 2 ] %*% dz ) 
        }
      }
      errors <- errors[ , -1 ]
      names( errors ) <- paste0( return.period, ".rlevel" )
    } else {
      ## Use the Monte Carlo method to determine the standard errors.
      parameter.estimate <- x$par
      ## Draw a number of samples and fit the GEV parameters for
      ## all of them
      if ( extreme.type == "min" ){
        ## To fit a series of block minima we have to multiply the
        ## series by -1, perform an ordinary GEV fit, and multiply
        ## the location parameter and the return levels by minus
        ## run transform the intermediate results back to fit the
        ## original time series.
        ## Now, to generate data, we have to multiply the
        ## resulting location parameter and transform the sampled
        ## series back by multiplying it by -1. This way it
        ## represents the original data set.
        samples.list <-
          lapply( 1 : monte.carlo.sample.size, function( yy )
            revd( length( x$x ), parameter.estimate[ 1 ]* -1,
                 parameter.estimate[ 2 ],
                 parameter.estimate[ 3 ], model = "gev" )* -1 )
      } else { 
        samples.list <-
          lapply( 1 : monte.carlo.sample.size, function( yy )
            revd( length( x$x ), parameter.estimate[ 1 ],
                 parameter.estimate[ 2 ],
                 parameter.estimate[ 3 ], model = "gev" ) )
      }
      samples.fit <- lapply( samples.list, function( yy )
        fit.gev( yy, error.estimation = "none", silent = TRUE,
                blocking = FALSE, extreme.type = extreme.type
                )$par )
      errors <- data.frame( a = 0 )
      r.period <- return.period
      for ( rr in 1 : length( return.period ) )
        errors <- cbind(
            errors, 
            sqrt( stats::var( Reduce(
                             c, lapply( samples.fit, function( zz )
                               climex::return.level(
                                           zz,
                                           return.period =
                                             r.period[ rr ],
                                           error.estimation =
                                             "none",
                                           extreme.type =
                                             extreme.type,
                                           model = "gev",
                                           threshold = x$threshold,
                                           total.length =
                                             total.length,
                                           thresholded.time.series =
                                             x$x,
                                           silent = TRUE
                                       )$return.level
                               ) ) ) ) )
      errors <- errors[ , -1 ]   
      names( errors ) <- paste0( return.period, ".rlevel" )
    }
    return( list( return.level = return.levels, error = errors ) )
  }

##' @title Calculation of the return levels.
##' @description Calculate arbitrary return levels and their error
##'   estimates for generalized Pareto (GP) distributions.
##'
##' @details Uses the \code{\link{rlevd}} function at its core (a port
##'   from the \pkg{extRemes} package) but also can handle the outputs
##'   of the \code{\link{fit.gpd}} functions, is capable of
##'   calculating numerous return levels at once, and also calculates
##'   the errors of the return levels. For the errors the ML fit is
##'   using the option \code{hessian=TRUE} (if not done already) or
##'   uses a Monte Carlo based approach. If no fitting object is
##'   provided, no errors will be calculated.
##'
##' @param x Of class \emph{climex.fit.gpd}.
##' @param return.period Numeric vector of the return periods in
##'   years. Default = 100.
##' @param error.estimation Method for calculating the standard errors
##'   of the fitted results. The errors of the GPD parameters will be
##'   calculated as the square roots of the diagonal elements of the
##'   inverse of the hessian matrix. The latter will be evaluated at
##'   the maximum likelihood estimates (MLE) of the GPD parameters.
##'
##'   For all three methods of estimating the fitting errors of the
##'   return levels underlying series of threshold exceedances or
##'   block maxima is required. In case the user supplies numerical
##'   values to specify GPD parameters and not the output of the
##'   \code{\link{fit.gpd}} function no error estimation for the
##'   return level can be performed.
##'
##'   \emph{MLE}: The standard error of the return level is calculated
##'     using the Delta method and the maximum likelihood estimates of
##'     the GPD parameters. Note: For positive shape parameters bigger
##'     than 0.3 this approach tends to highly overestimates the
##'     errors of the return levels.
##' 
##'   \emph{MC}: Alternative one can use a Monte Carlo method for
##'     which \strong{monte.carlo.sample.size} samples of the same
##'     size as \strong{x} will be drawn from a GPD distribution
##'     constituted by the obtained MLE of the GPD parameters of
##'     \strong{x}. The standard error is then calculated via the
##'     square of the variance of all fitted GPD parameters and
##'     calculated return levels. Note: In its essence this approach
##'     is not an estimation of the error involved in fitting the time
##'     series to a GPD distribution. It is rather the mean error of
##'     fitting a GPD-distribution with the same length and parameters
##'     as estimated ones.
##'
##'   \emph{bootstrap}: Using this option the provided time series
##'     \strong{x} will be sampled with replacement
##'     \strong{bootstrap.sample.size} times and with the same length
##'     as the original time series. The standard errors of the
##'     GPD parameters and return levels of all those sampled
##'     series is calculated and returned as an estimate of the
##'     fitting error. 
##'     Note: Since the data is (hopefully) GPD-distributed, such
##'     a sampling has to be treated with a lot of care.
##'
##'     Sometimes the inversion of the hessian fails (since the are
##'     some NaN in the hessian) when calculating the error estimates
##'     using the maximum likelihood approach (MLE) (which is also the
##'     reason why the \pkg{ismev} package occasionally does not
##'     work). In such cases the Monte Carlo (MC) method is used as a
##'     fallback.
##' 
##'   \emph{none} skips the calculation of the error. Default = "MLE".
##' @param model String determining whether to calculate the initial
##'   parameters of the GEV ("gev") or GPD ("gpd") function. Default =
##'   "gev". Note that this input argument will have no effect when
##'   calling the function with an object of class
##'   \emph{climex.fit.gpd}.
##' @param monte.carlo.sample.size Number of samples used to obtain
##'   the Monte Carlo estimate of the standard error of the fitting.
##'   Default = 100.
##' @param bootstrap.sample.size Number of samples with replacements
##'   to drawn from the original series \strong{x} in order to
##'   determine the standard errors for the GPD parameters and return
##'   levels. Default = 100.
##' @param threshold Optional threshold for the GPD model. If present
##'   it will be added to the return level to produce a value which
##'   fits to underlying time series. Default = NULL.
##' @param total.length Total number of observations in the time
##'   series the exceedance were obtained from (before! applying the
##'   threshold). This argument is needed to calculate the standard
##'   error of the return level via the delta method of the MLE in the
##'   GPD model. Default = NULL.
##' @param thresholded.time.series Time series used with
##'   \code{\link{fit.gpd}} on which already a threshold (the one
##'   supplied here as well) was applied. Necessary to transform the
##'   return level for numerical input and the GPD model from m-th
##'   observation return level to annual return level. If omitted the
##'   return level will be per observation. Default = NULL.
##' @param extreme.type String indicating whether to calculate the
##'   quantiles at the right ("max") or left ("min") side of the PDF
##'   of the series. Default = "max".
##' @param silent Throws an warning whenever the "gpd" model is used
##'   and the \strong{thresholded.time.series} is not supplied. Since
##'   this can be annoying one can also disable it. Default = FALSE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return A list containing the estimates "return.level" and their
##'   standard errors "error" if the input was a fitting object or a
##'   numerical input.
##' @export
##'
##' @importFrom xts apply.yearly
##' @importFrom numDeriv hessian
##' @importFrom parallel mclapply
##' 
##' @family extremes
##'
##' @examples
##' fit.results <- fit.gpd( threshold( temp.potsdam, threshold = 29,
##'   decluster = TRUE ) )
##' return.level( fit.results, return.period = c( 10, 50, 100 ),
##'               error.estimation = "MLE" )
return.level.climex.fit.gpd <- 
  function( x, return.period = 100,
           error.estimation = c( "none", "MC", "MLE",
                                "bootstrap" ),
           model = c( "gev", "gpd" ),
           monte.carlo.sample.size = 100,
           bootstrap.sample.size = 100,
           threshold = NULL, total.length = NULL,
           thresholded.time.series = NULL,
           extreme.type = c( "max", "min" ),
           silent = FALSE, mc.cores = NULL, ... ){
    ## Whether to handle the extreme events at the right or left
    ## side of the PDF of the GEV or GP distribution.
    if ( !missing( model ) && length( model ) == 1 &&
         model != "gpd" ){
      warning( "Mismatching object class and model argument!" )
    }
    if ( !missing( extreme.type ) && length( extreme.type ) == 1 ){
      warning(
          "Whether to extract the block maxima or minima will is specified by the fit function the input argument was generated by. The 'extreme.type' argument does not have any effect with this kind of input!" )
    }
    extreme.type <- x$control$extreme.type
    if ( missing( error.estimation ) || length( error.estimation ) != 1 ){
      error.estimation <- "none"
    }
    error.estimation <- match.arg( error.estimation )
    if ( !is.null( total.length ) ){
      if ( !silent && !is.null( x$control$total.length ) ){
        warning( "return.level: The total.length argument is already present in the supplied fitting object and will be overwritten." )
      }
      x$control$total.length <- total.length
    }
    if ( !is.xts( x$x ) ){
      if ( !silent ) {
        warning(
            "return.level: Since the original time series was not supplied the return level will be not per once every x year but once every x observation" )
      }
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
      ## m-observation return level = return.period* the mean
      ## number of exceedance per year. This way the unit of the
      ## provided return level and its error are  not 'per
      ## observation' but 'per year'. In this step we harness the
      ## power of the 'xts' package.
      m <- return.period* mean( apply.yearly(
                              x$x,
                              function( yy ) length( yy ) ) )
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
    ## When a threshold is supplied, the one in the fitted object
    ## will be overwritten
    if ( !is.null( threshold ) ){
      x$threshold <- threshold
    }
    ## The time series and threshold itself have to be inverted
    ## when dealing with all events below a certain threshold.
    if ( extreme.type == "min" ){
      return.levels <- Reduce( c, lapply( m, function( yy )
        as.numeric( rlevd( yy, scale = x$par[ 1 ],
                          shape = x$par[ 2 ],
                          model = "gpd",
                          threshold = x$threshold* -1,
                          silent = silent ) ) ) )* -1
    } else {
      return.levels <- Reduce( c, lapply( m, function( yy )
        as.numeric( rlevd( yy, scale = x$par[ 1 ],
                          shape = x$par[ 2 ],
                          model = "gpd", threshold = x$threshold,
                          silent = silent ) ) ) )
    }
    ##
    ## Error estimation of the return level
    ##
    if ( error.estimation == "none" ){
      ## Dummy holding NaN instead of the return level errors.
      errors <- rep( NA, length( return.period ) )
      return( list( return.level = return.levels, error = errors ) )
    } else if ( error.estimation == "bootstrap" ){
      ## As a simple alternative the threshold exceedances will be
      ## sampled with replacement and the parameters and return
      ## levels are calculated for all of the resampled series. The
      ## bootstrap error is than calculated as the standard error of
      ## all the GP parameters and return levels.
      bootstrap.sample.list <-
        lapply( c( 1 : bootstrap.sample.size ), function( xx )
          sample( x$x, size = length( x$x ), replace = TRUE ) )
      ## Fitting the GP parameters (recursively)
      fitted.list <-
        lapply( bootstrap.sample.list, function( xx ){
          fit.gpd( x = xx, initial = x$control$initial,
                  threshold = x$threshold,
                  likelihood.function =
                    x$control$likelihood.function,
                  gradient.function = x$control$gradient.function,
                  error.estimation = "none",
                  return.period = return.period,
                  total.length = x$control$total.length,
                  extreme.type = extreme.type,
                  silent = TRUE ) } )
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
      errors <- errors[ 3 : ( 2 + length( return.period ) ) ]
    } else if ( error.estimation == "MLE" ){
      if ( !any( names( x$control ) == "hessian" ) ){
        ## fit again and let stats::optim calculate the
        ## hessian. It's way more save this way. else {
        if ( is.null( threshold ) ){
          threshold <- x$threshold
        }
        x$control$hessian <-
          fit.gpd( x$x, initial = x$par, threshold = threshold,
                  error.estimation = "MLE", silent = TRUE,
                  extreme.type = extreme.type,
                  total.length = total.length )$control$hessian
      }
      ## Sometimes the obtained hessian is not invertible. If this
      ## is the case, recalculate it in order to access the fitting
      ## error estimates. Caution: this one will be without the
      ## constraints! Calculating the errors using the MLE
      ##
      ## If the shape parameter is exactly zero and the exponential
      ## distribution was fitted, the second row and column were just
      ## augmented by 0.
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
          dummy.matrix <- matrix( rep( 0, 4 ), nrow = 2,
                                 ncol = 2 )
          dummy.matrix[ 1 ] <- error.covariance
          error.covariance <- dummy.matrix
        }
      }
      if ( class( error.covariance ) == "try-error" ){
        x.hessian <-
          numDeriv::hessian( likelihood, x = x$par, x.in = x$x,
                            model = "gpd" )
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
      for ( rr in 1 : length( return.period ) ){
        ## I omit the error estimation of the zeta in here. I
        ## don't see how the error of exceedance probability will
        ## help the user.
        scale <- parameter.estimate[ 1 ]
        shape <- parameter.estimate[ 2 ]
        if ( is.null( zeta ) ){
          ## Calculate the exceedance probability in case only the
          ## threshold exceedances and not the original time
          ## series was supplied.
          zeta <- m[ rr ]/ ( return.period[ rr ]* 365.25 )
        }
        if ( !is.numeric( zeta ) ){
          ## In case zeta was set to FALSE neither the original
          ## time series nor the threshold exceedances were
          ## supplied. Therefor there is no way to estimate the
          ## mean exceedance probability and the zeta term has to
          ## be excluded from the error calculation.
          if ( shape != 0 ){
            ## GP distribution
            dz <- c( ( m[ rr ]^ shape - 1 )/ shape,
                    -scale* shape^{ -2 }*( m[ rr ]^shape - 1 ) +
                    scale/shape*m[ rr ]^shape* log( m[ rr ] ) )
            errors <-
              cbind( errors, dz %*% error.covariance %*% dz )
          } else {
            ## Exponential distribution
            dz <- log( m[ rr ] )
            errors <-
              cbind( errors,
                    dz %*% parameter.error.estimate[ 1 ] %*% dz )
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
            error.matrix <- matrix( rep( 0, 9 ), nrow = 3,
                                   ncol = 3 )
            if ( !is.null( total.length ) ){
              ## If the total length of the underlying series
              ## BEFORE thresholding is provided, we are glad to
              ## use it. 
              error.matrix[ 1, 1 ] <- zeta*( 1 - zeta )/
                total.length
            } else {
              ## If not we have to estimate it using the MLE of
              ## zeta number of exceedances/ total length.
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
            error.matrix <- matrix( rep( 0, 4 ), nrow = 2,
                                   ncol = 2 )
            if ( !is.null( total.length ) ){
              ## If the total length of the underlying series
              ## BEFORE thresholding is provided, we are glad to
              ## use it. 
              error.matrix[ 1, 1 ] <- zeta*( 1 - zeta )/
                total.length
            } else {
              ## If not we have to estimate it using the MLE of
              ## zeta number of exceedances/ total length.
              error.matrix[ 1, 1 ] <- zeta^2*( 1 - zeta )/
                length( x$x )
            }
            error.matrix[ 2, 2 ] <- parameter.error.estimate[ 1 ]
            errors <- cbind( errors, dz %*% error.matrix %*% dz )
          }
        }
      }
      errors <- errors[ , -1 ]
      names( errors ) <- paste0( return.period, ".rlevel" )
    } else {
      ## Use the Monte Carlo method to determine the standard errors.
      parameter.estimate <- x$par
      ## Draw a number of samples and fit the GP parameters for
      ## all of them
      ## Since the fit.gpd function is expect to handle data with
      ## the threshold already removed, the threshold must not be
      ## handed to the revd function.
      samples.list <-
        lapply( 1 : monte.carlo.sample.size, function( yy )
          revd( length( x$x ), scale = parameter.estimate[ 1 ],
               shape = parameter.estimate[ 2 ], silent = TRUE,
               threshold = 0, model = "gpd" ) )
      ## All sampled data will be positive numbers. Thus,
      ## `extreme.type` will be fixed.
      samples.fit <- lapply( samples.list, function( yy )
        fit.gpd( yy, error.estimation = "none", threshold = threshold,
                total.length = total.length, silent = TRUE,
                extreme.type = "max" )$par )
      errors <- data.frame( a = 0 )
      r.period <- m
      for ( rr in 1 : length( return.period ) )
        errors <- cbind(
            errors, 
            sqrt( stats::var( Reduce(
                             c, lapply( samples.fit, function( zz )
                               climex::return.level(
                                           zz,
                                           return.period =
                                             r.period[ rr ],
                                           error.estimation =
                                             "none",
                                           extreme.type =
                                             extreme.type,
                                           model = "gpd",
                                           threshold = x$threshold,
                                           total.length =
                                             total.length,
                                           thresholded.time.series =
                                             x$x,
                                           silent = TRUE
                                       )$return.level
                               ) ) ) ) )
      errors <- errors[ , -1 ]   
      names( errors ) <- paste0( return.period, ".rlevel" )
    }
    return( list( return.level = return.levels, error = errors ) )
  }

##' @title Calculation of the return levels.
##' @description Calculate arbitrary return levels and their error
##'   estimates for generalized extreme value (GEV) and generalized
##'   Pareto (GP) distributions.
##'
##' @details Uses the \code{\link{rlevd}} function at its core (a port
##'   from the \pkg{extRemes} package) but is also capable of
##'   calculating numerous return levels at once, and also calculates
##'   the errors of the return levels. For the errors the ML fit is
##'   using the option \code{hessian=TRUE} (if not done already) or
##'   uses a Monte Carlo based approach. If no fitting object is
##'   provided, no errors will be calculated.
##'
##' @param x Of class \emph{numeric}.
##' @param return.period Numeric vector of the return periods in
##'   years. Default = 100.
##' @param error.estimation The estimation of the fitting error is
##'   only for input argument of the class \emph{climex.fit.gev} or
##'   \emph{climex.fit.gpd}. Else the correct scaling of the can not
##'   be assured.
##' @param model String determining whether to calculate the initial
##'   parameters of the GEV ("gev") or GP ("gpd") function. Default =
##'   "gev"
##' @param monte.carlo.sample.size Number of samples used to obtain
##'   the Monte Carlo estimate of the standard error of the fitting.
##'   Default = 100.
##' @param bootstrap.sample.size Number of samples with replacements
##'   to drawn from the original series \strong{x} in order to
##'   determine the standard errors for the GEV/GP parameters and
##'   return levels. Default = 100.
##' @param threshold Optional threshold for the GP model. If present
##'   it will be added to the return level to produce a value which
##'   fits to underlying time series. Default = NULL.
##' @param total.length Total number of observations in the time
##'   series the exceedance were obtained from (before! applying the
##'   threshold). This argument is needed to calculate the standard
##'   error of the return level via the delta method of the MLE in the
##'   GP model. Default = NULL.
##' @param thresholded.time.series Time series used with
##'   \code{\link{fit.gpd}} on which already a threshold (the one
##'   supplied here as well) was applied. Necessary to transform the
##'   return level for numerical input and the GP model from m-th
##'   observation return level to annual return level. If omitted the
##'   return level will be per observation. Default = NULL.
##' @param extreme.type String indicating whether to calculate the
##'   quantiles at the right ("max") or left ("min") side of the PDF
##'   of the series. Default = "max".
##' @param silent Throws an warning whenever the "gpd" model is used
##'   and the \strong{thresholded.time.series} is not supplied. Since
##'   this can be annoying one can also disable it. Default = FALSE.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @return A list containing the estimates "return.level" and their
##'   standard errors "error" if the input was a fitting object or a
##'   numerical input.
##' @export
##'
##' @importFrom xts apply.yearly
##' @importFrom numDeriv hessian
##' @importFrom parallel mclapply
##' 
##' @family extremes
##'
##' @examples
##' fit.results <- fit.gev( block( anomalies( temp.potsdam ) ) )
##' return.level( fit.results, return.period = c( 10, 50, 100 ),
##'               error.estimation = "MLE" )
return.level.numeric <-
  function( x, return.period = 100,
           error.estimation = c( "none", "MC", "MLE",
                                "bootstrap" ),
           model = c( "gev", "gpd" ),
           monte.carlo.sample.size = 100,
           bootstrap.sample.size = 100,
           threshold = NULL, total.length = NULL,
           thresholded.time.series = NULL,
           extreme.type = c( "max", "min" ),
           silent = FALSE, mc.cores = NULL, ... ){
    ## Whether to handle the extreme events at the right or left
    ## side of the PDF of the GEV or GP distribution.
    if ( missing( extreme.type ) || length( extreme.type ) != 1 ){
      extreme.type <- "max"
    }
    if ( missing( error.estimation ) || length( error.estimation ) != 1 ){
      error.estimation <- "none"
    }
    error.estimation <- match.arg( error.estimation )
    extreme.type <- match.arg( extreme.type )
    ## Neither a object from fit.gev nor from fit.gpd but a
    ## numerical vector containing the GEV/GP parameters was
    ## supplied. Which of those two distribution should it be?
    if ( missing( model ) || length( model ) != 1 ){
      model <- "gev"
    }
    model <- match.arg( model )
    if ( length( x ) != 3 && model == "gev" ){
      stop(
          "If the GEV distribution is specified, all its three parameters have to be provided via the first argument of the return.level function!" )
    }
    if ( length( x ) != 2 && model == "gpd" ){
      stop(
          "If the GP distribution is specified, all its two parameters have to be provided via the first argument of the return.level function!" )
    } 
    if ( model == "gev" ){
      ## The location parameter and the time series itself have to
      ## be inverted when dealing with the block minima
      if ( extreme.type == "min" ){
        return.levels <-
          Reduce( c, lapply( return.period, function( yy )
            as.numeric( rlevd( yy, x[ 1 ]* -1, x[ 2 ],
                              x[ 3 ], model = model,
                              silent = silent ) ) ) )* -1
      } else {
        return.levels <-
          Reduce( c, lapply( return.period, function( yy )
            as.numeric( rlevd( yy, x[ 1 ], x[ 2 ],
                              x[ 3 ], model = model,
                              silent = silent ) ) ) )
      }
    } else {
      if ( !is.null( thresholded.time.series ) ){
        if ( !is.null( total.length ) ){
          ## The maximum likelihood estimate of the probability of
          ## an exceedance to occur per year will be used.
          zeta <- length( thresholded.time.series )/ total.length
          m <- return.period* 365.25* zeta
        } else {
          ## m-observation return level = return.period* the mean 
          ## number of exceedance per year. This way the unit of
          ## the provided return level and its error are  not 'per
          ## observation' but 'per year'. 
          ## In this step we harness the power of the 'xts'
          ## package.
          m <- return.period* mean( apply.yearly(
                                  thresholded.time.series,
                                  function( tt ) length( tt ) ) )
          zeta <- NULL
        }
      } else {
        if ( !silent )
          warning(
              "return.level: Since the original time series was not supplied the return level will be not per once every m year but once every m observation" )
        ## Neither the amount of threshold exceedances nor the
        ## length of the original series is supplied. Now there is
        ## no other chance than to calculate the observation-based
        ## return level.
        m <- return.period
        zeta <- FALSE
      }
      if ( extreme.type == "min" ){
        return.levels <- Reduce( c, lapply( m, function( yy )
          as.numeric( rlevd( yy, scale = x[ 1 ], shape = x[ 2 ],
                            model = model,
                            threshold = threshold* -1,
                            silent = silent ) ) ) )* -1
      } else {
        return.levels <- Reduce( c, lapply( m, function( yy )
          as.numeric( rlevd( yy, scale = x[ 1 ], shape = x[ 2 ],
                            model = model, threshold = threshold,
                            silent = silent ) ) ) )
      }
    }
    ##
    ## Error estimation of the return level
    ##
    if ( error.estimation != "none" ){
      warning(
          "The error estimation can only be provided for input objects returned by the fit.gev or fit.gpd function" )
    }
    ## Dummy holding NaN instead of the return level errors.
    ## (Since there was not enough information supplied to
    ## calculate them).
    errors <- rep( NA, length( return.period ) )
    return( list( return.level = return.levels, error = errors ) )
  }

##' @title Return level calculation
##' @description Internal function to calculate the return level of
##'   GEV or GP distribution.
##' @details Port from the \pkg{extRemes} package to ensure
##'   compatibility and to make the threshold argument
##'   obligatory. This is just for internal usage. Please use the
##'   \code{\link{return.level}} function instead!
##'
##' @param period Return period in years.
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is
##'   optional but should be provided in order to create a
##'   representation of the fitted data exceedance. Default = NULL.
##' @param model String determining whether to calculate the initial
##'   parameters of the GEV ("gev") or GP ("gpd") function. Default =
##'   "gev"
##' @param silent Whether to display warnings or not. Default = FALSE.
##'
##' @family extremes
##'
##' @export
##' 
##' @return Numerical vector of the same length as 'period'.
##' @author Philipp Mueller 
rlevd <- function ( period, location = NULL, scale = NULL,
                   shape = NULL, threshold = NULL, 
                   model = c( "gev", "gpd" ), silent = FALSE ){
  if ( missing( model ) )
    model <- "gev"
  model <- match.arg( model )
  if ( model == "gev" ){
    if ( is.null( location ) ||
         is.null( scale ) || is.null( shape ) ){
      stop( "Please supply 'location', 'scale' and 'shape'!" )
    }
  } else {
    if ( is.null( scale ) || is.null( shape ) ){
      stop( "Please supply 'scale' and 'shape'!" )
    }
    if ( is.null( threshold ) || length( threshold ) == 0 ){
      if ( !silent ){
        warning(
            "No 'threshold' supplied! This needs to be added to the generated time series in order to resemble the original data points!" )
      }
      location <- threshold <- 0
    } else {
      location <- threshold
    }
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
##' @description Calculates the quantile of either the GEV or the GP
##'   distribution
##' @details Port from the \pkg{extRemes} package to (again) get rid
##'   of the \strong{threshold} argument to be able to have an
##'   separate \code{\link{threshold}} function outside of the fitting
##'   function.
##'
##' @param p (Numeric) probability vector.
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is
##'   optional but should be provided in order to create a
##'   representation of the fitted data exceedance. Default = NULL.
##' @param model String determining whether to calculate the initial
##'   parameters of the GEV ("gev") or GP ("gpd") function. Default =
##'   "gev"
##' @param lower.tail Whether to sample the probabilities P[X <= x] or
##'   P[X > x]. Default = TRUE (first case).
##' @param silent Whether to display warnings or not. Default = FALSE. 
##'
##' @family extremes
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
##' @description Drawing random numbers from the GEV or GP
##'   distribution
##' @details This function was originally part of the \pkg{extRemes}
##'   package. But since one had to provide the \strong{threshold}, I
##'   couldn't use it insight the \code{\link{fit.gpd}} function. In
##'   contrast to the original implementation this function only
##'   features constant location, scale, and shape parameters. 
##'   If you want to do time dependent analysis of extreme events,
##'   please refer to the original package.
##'
##' @param n Number of samples to draw
##' @param location Of the GEV distribution. Default = NULL.
##' @param scale Of the GEV/GP distribution. Default = NULL.
##' @param shape Of the GEV/GP distribution. Default = NULL.
##' @param threshold Used in the GP distribution. This parameter is
##'   optional but should be provided in order to create a
##'   representation of the fitted data exceedance. Default = NULL.
##' @param model String determining whether to calculate the initial
##'   parameters of the GEV ("gev") or GP ("gpd") function. Default =
##'   "gev"
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
##'   shape ).
##' @param z Numerical vector of sites where to evaluate the
##'   density of the GEV distribution.
##'
##' @family extremes
##' 
##' @return Numerical vector of same length as z.
##' @author Philipp Mueller
gev.density <- function ( parameters, z ){
  if ( class( parameters ) != "numeric" ||
       length( parameters ) != 3 ){
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
    ## There are certain ranges the PDF is defined in. If (some of
    ## the) z happens to be outside of this range, replace it with NaN
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
    ## The Gumbel distribution is only considered if the shape
    ## parameter matches exactly zero.
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
##'   the \strong{parameters} argument while setting the shape
##'   parameter to zero. 
##'
##'   Port of the \code{\link[ismev]{gpd.dens}} function.
##'
##' @param parameters Fitted GP parameters c( scale, shape )
##' @param threshold Threshold used to fit the GP distribution.
##' @param z Numerical vector of sites where to evaluate the
##'   density of the GP distribution.
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
    ## There are certain ranges the PDF is defined in. If (some of
    ## the) z happens to be outside of this range, replace it with
    ## NaN. 
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
    
    ## There are certain ranges the PDF is defined in. If (some of
    ## the) z happens to be outside of this range, replace it with
    ## NaN.
    z[ z < threshold ] <- NaN
    density <- exp( -( z - threshold/ scale ) )/ scale
  }
  return( density )
}
## End of extremes.R
