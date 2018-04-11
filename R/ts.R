### ts.R - Helper functions to handle and preprocess time series in
##    the analysis
##' @title Akaike information criterion
##' @description Calculates the Akaike information criterion of a
##'   \emph{climex.fit.gev} or \emph{climex.fit.gpd} class object or a
##'   list of those.
##' @details The \emph{climex.fit.gev} object is of identical
##'   structure as the output of the \code{\link[stats]{optim}}
##    function.
##'  
##' @param x Either an object returned by the \code{\link{fit.gev}} or
##'   \code{\link{fit.gpd}} function or a list of such elements.
##'
##' @seealso \code{\link{bic}}, \code{\link{fit.gev}},
##'   \code{\link{fit.gpd}}
##' @family ts
##'
##' @export
##' 
##' @return Numeric value, if the input is a single fitting object, or
##'   a numerical vector, if \strong{x} is a list of such objects.
##' @author Philipp Mueller
aic <- function( x ){
  UseMethod( "aic" )
}
##' @title Akaike information criterion
##' @description Calculates the Akaike information criterion of a
##'   list of \emph{climex.fit.gev} or \emph{climex.fit.gpd} class
##'   objects.
##' @details The \emph{climex.fit.gev} object is of identical
##'   structure as the output of the \code{\link[stats]{optim}}
##'   function.
##'  
##' @param x Either a list of objects returned by the
##'   \code{\link{fit.gev}} or \code{\link{fit.gpd}} function.
##'
##' @seealso \code{\link{bic}}, \code{\link{fit.gev}},
##'   \code{\link{fit.gpd}}
##' @family ts
##'
##' @export
##' 
##' @return Numerical vector
##' @author Philipp Mueller
aic.list <- function( x ){
  ## Since the objects returned by the fitting functions are of class
  ## c( "list", "climex.fit.gXX" ), an exception is needed to hand
  ## them to the correct function call
  if ( any( class( x ) == "climex.fit.gev" ) ){
    x.result <- aic.climex.fit.gev( x )
  } else if ( any( class( x ) == "climex.fit.gpd" ) ){
    x.result <- aic.climex.fit.gpd( x )
  } else {
    x.result <- Reduce( c, lapply( x, aic ) )
  }
  return( x.result )
}
##' @title Akaike information criterion
##' @description Calculates the Akaike information criterion of a
##'   \emph{climex.fit.gev} or \emph{climex.fit.gpd} class object.
##' @details The \emph{climex.fit.gev} object is of identical
##'   structure as the output of the \code{\link[stats]{optim}}
##'   function.
##'  
##' @param x An object returned by the \code{\link{fit.gev}} or
##'   \code{\link{fit.gpd}} function.
##'
##' @seealso \code{\link{bic}}, \code{\link{fit.gev}},
##'   \code{\link{fit.gpd}}
##' @family ts
##'
##' @export
##' 
##' @return Numeric value
##' @author Philipp Mueller
aic.climex.fit.gev <- aic.climex.fit.gpd <- aic.default <- function( x ){
  if ( !any( class( x ) == "climex.fit.gev" ) &&
       !any( class( x ) == "climex.fit.gpd" ) )
    stop( "Wrong format provided in aic. Please supply an object returned by either fit.gev or fit.gpd!" )
  return( 2* x$value + 2* length( x$par ) )
}

##' @title Bayesian information criterion
##' @description Calculates the Bayesian information criterion of a
##'   \emph{climex.fit.gev} or \emph{climex.fit.gpd} class object or a
##'   list of those.
##' @details The \emph{climex.fit.gev} object is of identical
##'   structure as the output of the \code{\link[stats]{optim}}
##'   function.
##'  
##' @param x Either an object returned by the \code{\link{fit.gev}} or
##'   \code{\link{fit.gpd}} function or a list of such elements.
##'
##' @seealso \code{\link{aic}}, \code{\link{fit.gev}},
##'   \code{\link{fit.gpd}}
##' @family ts
##'
##' @export
##' 
##' @return Numeric value, if the input is a single fitting object, or
##'   a numerical vector, if \strong{x} is a list of such objects.
##' @author Philipp Mueller
bic <- function( x ){
  UseMethod( "bic" )
}
##' @title Bayesian information criterion
##' @description Calculates the Bayesian information criterion of a
##'   list of \emph{climex.fit.gev} or \emph{climex.fit.gpd} class
##'   objects.
##' @details The \emph{climex.fit.gev} object is of identical
##'   structure as the output of the \code{\link[stats]{optim}}
##'   function.
##'  
##' @param x Either a list of objects returned by the
##'   \code{\link{fit.gev}} or \code{\link{fit.gpd}} function.
##'
##' @seealso \code{\link{aic}}, \code{\link{fit.gev}},
##'   \code{\link{fit.gpd}}
##' @family ts
##'
##' @export
##' 
##' @return Numerical vector
##' @author Philipp Mueller
bic.list <- function( x ){
  ## Since the objects returned by the fitting functions are of class
  ## c( "list", "climex.fit.gXX" ), an exception is needed to hand
  ## them to the correct function call
  if ( any( class( x ) == "climex.fit.gev" ) ){
    x.result <- bic.climex.fit.gev( x )
  } else if ( any( class( x ) == "climex.fit.gpd" ) ){
    x.result <- bic.climex.fit.gpd( x )
  } else {
    x.result <- Reduce( c, lapply( x, bic ) )
  }
  return( x.result )
}
##' @title Bayesian information criterion
##' @description Calculates the Bayesian information criterion of a
##'   \emph{climex.fit.gev} or \emph{climex.fit.gpd} class object.
##' @details The \emph{climex.fit.gev} object is of identical
##'   structure as the output of the \code{\link[stats]{optim}}
##'   function.
##'  
##' @param x An object returned by the \code{\link{fit.gev}} or
##'   \code{\link{fit.gpd}} function.
##'
##' @seealso \code{\link{aic}}, \code{\link{fit.gev}},
##'   \code{\link{fit.gpd}}
##' 
##' @family ts
##'
##' @export
##' 
##' @return Numeric value
##' @author Philipp Mueller
bic.climex.fit.gev <- bic.climex.fit.gpd <- bic.default <- function( x ){
  if ( !any( class( x ) == "climex.fit.gev" ) &&
       !any( class( x ) == "climex.fit.gpd" ) )
    stop( "Wrong format provided in bic. Please supply an object returned by either fit.gev or fit.gpd!" )
  return( 2* x$value + length( x$par )* log( length( x$x ) ) )
}

##' @title Remove incomplete years
##' @description Removes all years, which contain either a NA or are
##'   incomplete (derived from the amount of time stamps within a
##'   year). 
##'
##' @details Since incomplete years are detected via the
##'   difference in the time stamps of neighbouring points, the user
##'   has to provide the basic time unit. (Numerical value of the
##'   result of the \code{\link[base]{diff}} function applied to two
##'   consecutive points.) For daily data the value is 1.
##' 
##' @param x Either a \pkg{xts} class object or a list of those.
##' @param time.unit Minimal differences found when applying the
##'   \code{\link[base]{diff}} function to \strong{x}. Default = 1.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @seealso \code{\link{remove.seasonality}}
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @import lubridate
##' 
##' @return Object of the same class as the input \strong{x}.
##' @author Philipp Mueller
remove.incomplete.years <- function( x, time.unit = 1,
                                    mc.cores = NULL ){
  UseMethod( "remove.incomplete.years" )
}
##' @title Remove incomplete years
##' @description Removes all years, which contain either a NA or are
##'   incomplete (derived from the amount of time stamps within a
##'   year). 
##'
##' @details Since incomplete years are detected via the
##'   difference in the time stamps of neighbouring points, the user
##'   has to provide the basic time unit. (Numerical value of the
##'   result of the \code{\link[base]{diff}} function applied to two
##'   consecutive points.) For daily data the value is 1.
##' 
##' @param x A list of \pkg{xts} class objects.
##' @param time.unit Minimal differences found when applying the
##'   \code{\link[base]{diff}} function to \strong{x}. Default = 1.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @seealso \code{\link{remove.seasonality}}
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @import lubridate
##' 
##' @return Object of the same class as the input \strong{x}.
##' @author Philipp Mueller
remove.incomplete.years.list <- function( x, time.unit = 1,
                                         mc.cores = NULL ){
  if ( !is.null( mc.cores ) ){
    return( mclapply( x, remove.incomplete.years,
                     time.unit = time.unit, mc.cores = mc.cores ) )
  } else {
    return( lapply( x, remove.incomplete.years,
                   time.unit = time.unit, mc.cores = mc.cores ) )
  }
}
##' @title Remove incomplete years
##' @description Removes all years, which contain either a NA or are
##'   incomplete (derived from the amount of time stamps within a
##'   year). 
##'
##' @details Since incomplete years are detected via the
##'   difference in the time stamps of neighbouring points, the user
##'   has to provide the basic time unit. (Numerical value of the
##'   result of the \code{\link[base]{diff}} function applied to two
##'   consecutive points.) For daily data the value is 1.
##' 
##' @param x A \pkg{xts} class object.
##' @param time.unit Minimal differences found when applying the
##'   \code{\link[base]{diff}} function to \strong{x}. Default = 1.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @seealso \code{\link{remove.seasonality}}
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @import lubridate
##' 
##' @return Object of the same class as the input \strong{x}.
##' @author Philipp Mueller
remove.incomplete.years.xts <- remove.incomplete.years.default <-
  function( x, time.unit = 1, mc.cores = NULL ){
    if ( !any( class( x ) == "xts" ) )
      stop(
          "Provided element of the wrong class in remove.incomplete.years. Provide object of class xts instead!" )
    ## Removes all values containing a NA, R's general type for
    ## missing data. 
    x <- x[ !is.na( x ) ]

    ## There is data available.
    if ( length( x ) > 0 ){
      ## The missing points are detected by looking for differences in 
      ## the time stamps of neighbouring points. If their are more
      ## than time.unit apart, there are some missing values in
      ## between,
      x.index <- index( x )
      x.index.diff <- diff( x.index )
      ## If the day before the gap is the last day of a year, it's year
      ## value shouldn't be included in the list of years ready to
      ## remove, since the corresponding gap doesn't affect it. Those
      ## gaps have to be treated specially.
      x.last.day.of.year <-
        grep( "12-31", index( x [
                           which( x.index.diff > time.unit ) ] ) )
      if ( length( x.last.day.of.year ) > 0 ){
        ## Adding the next year plus the year of the index behind the
        ## gap. 
        additional.years <- c( year( x[ which( x.index.diff > 1 )[
            x.last.day.of.year ] ] ) + 1,
            year( x[ which( x.index.diff > 1 )[
                x.last.day.of.year ] + 1 ] ) )
        x.index.diff <- x.index.diff[ -(
          which( x.index.diff > 1 )[ x.last.day.of.year ] ) ]
      } else {
        additional.years <- NULL
      }
      ## Determining the year before and after the gap
      incomplete.years.1 <- year( x[ ( which( x.index.diff > 1 ) ) ] )
      incomplete.years.2 <- year( x[ ( which( x.index.diff > 1 ) +
                                       1 ) ] )
      ## Be sure to include all years, even if several years passed
      ## within one gaps. The seq() function is used on every pair of
      ## points before and after the gap.
      if ( length( incomplete.years.1 ) > 0 &
           length( incomplete.years.2 ) > 0 ){
        ## Since the last day of the year is removed above, it's
        ## possible to have no years left detectable via the
        ## differences. 
        all.incomplete.years <-
          Reduce( unique, apply( cbind( incomplete.years.1,
                                       incomplete.years.2 ), 1,
                                function( yy )
                                  seq( yy[ 1 ], yy[ 2 ], 1 ) ) )
      } else {
        ## Creating an object of length 0 as placeholder
        all.incomplete.years <- incomplete.years.1
      }
      ## Check whether the first year starts at January 1st and the
      ## last on Dec. 31st. Else the corresponding years aren't
      ## complete.
      if ( length( grep( "-01-01", index( x[ 1 ] ) ) ) == 0 ){
        all.incomplete.years <- c( all.incomplete.years,
                                  year( x[ 1 ] ) )
      }
      if ( length( grep( "-12-31",
                        index( x[ length( x ) ] ) ) ) == 0 ){
        all.incomplete.years <- c( all.incomplete.years,
                                  year( x[ length( x ) ] ) )
      }
      if ( !is.null( additional.years ) ){
        all.incomplete.years <- unique( c( all.incomplete.years,
                                          additional.years ) )
      }
      return( x[ !(year(x) %in% all.incomplete.years ) ] )
    } else {
      ## There is no data left
      return( x )
    }
  }

##' @title Checking for N complete years of data
##' 
##' @description This function tests whether the series do has at
##'   least \strong{number.of.years} complete years of data.
##' 
##' @details It uses the \code{\link{remove.incomplete.years}}
##'   to check for the completeness of the individual years.
##'
##' @param x Either a time series of class \pkg{xts} or a list of
##'   them.
##' @param number.of.years The minimum number of complete years in the
##'   series. Default = 30.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @export
##' 
##' @importFrom parallel mclapply
##'
##' @family ts
##'
##' @return If the input is of type \pkg{xts}, the function returns
##'   TRUE or FALSE. If the input, on the other hand, is a list of
##'   objects of type \pkg{xts}, it returns a trimmed list
##'   containing only those elements, which indeed have more than N
##'   years of complete data.
##' @author Philipp Mueller 
check.completeness <- function( x, number.of.years = 30,
                               mc.cores = NULL){
  UseMethod( "check.completeness" )
}
##' @title Checking for N complete years of data
##' @description Removes all class \pkg{xts} time series from a
##'   list, which are not of at least \strong{number.of.years}
##'   complete years of data.
##'
##' @param x A list of \pkg{xts} class objects.
##' @param number.of.years The minimum number of complete years in the
##'   series. Default = 30.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @export
##'
##' @importFrom parallel mclapply
##'
##' @return Trimmed list containing only those elements, which indeed
##'   have more than N years of complete data.
##' @author Philipp Mueller
check.completeness.list <- function( x, number.of.years = 30,
                                    mc.cores = NULL ){
  if ( !is.null( mc.cores ) ){
    ## Apply the checks to all elements of the list.
    x.list.check <-
      Reduce( c, mclapply( x, check.completeness.xts,
                          number.of.years = number.of.years,
                          mc.cores = mc.cores ) )
  } else {
    ## Apply the checks to all elements of the list.
    x.list.check <-
      Reduce( c, lapply( x, check.completeness.xts,
                        number.of.years = number.of.years,
                        mc.cores = mc.cores ) )
  }
  ## Return only those element matching the conditions
  return( x[ x.list.check ] )
}
##' @title Checking for N complete years of data
##' 
##' @description This function tests whether a \pkg{xts} class object
##'   has at least \strong{number.of.years} complete years of data.
##' 
##' @details It uses the \code{\link{remove.incomplete.years}}
##'   to check for the completeness of the individual years.
##'
##' @param x A time series of class \pkg{xts}.
##' @param number.of.years The minimum number of complete years in the
##'   series. Default = 30.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @importFrom xts is.xts
##' @importFrom parallel mclapply
##'
##' @export
##' 
##' @family data.cleaning
##'
##' @return Logical
##' @author Philipp Mueller 
check.completeness.xts <- check.completeness.default <-
  function( x, number.of.years = 30, mc.cores = NULL ){
    ## Only accept objects of class 'xts'
    if ( !is.xts( x ) ){
      stop( "Only objects of class 'xts' accepted!" )
    }
    if ( !is.numeric( number.of.years ) ){
      stop( "The number of years has to be a 'numerical' value!" )
    }

    ## Remove all incomplete years
    x.complete.years <- remove.incomplete.years( x )

    ## Check whether the remaining number of years are at least of the
    ## specified number.
    x.check <- length( unique(
        lubridate::year( x.complete.years ) ) ) >=
      number.of.years
    return( x.check )
  }

##' @title Remove seasonality
##' @description Calculates the seasonal component of a time series
##'   and subtracts it from the original. 
##'
##' @details The function \code{\link[stats]{stl}} with the argument
##'   \code{s.window = 12} and a 
##'   conversion of the input into a \emph{ts} class object of daily
##'   data is used to calculate the seasonal component. This should be
##'   replaced by a more sophisticated solution as soon I digged
##'   deeper into the field of deseasonalization.
##'   \code{\link{remove.incomplete.years}} is used
##'   to remove incomplete years from the data set. This ensures a
##'   better calculation of the seasonal component but also requires
##'   to forecast it to the length of the original data set and align
##'   it at the right place for subtraction.
##'
##'   This function can also be applied to a list of \pkg{xts} class
##'   objects.
##'
##' @param x Either an object of class \pkg{xts} or a list of those.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family ts
##'
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @import lubridate
##' @return Some class as input.
##' @author Philipp Mueller
remove.seasonality <- function( x, mc.cores = NULL ){
  UseMethod( "remove.seasonality" )
}
##' @title Remove seasonality
##' @description Calculates the seasonal component of a time series
##'   and subtracts it from the original. 
##'
##' @details The function \code{\link[stats]{stl}} with the argument
##'   \code{s.window = 12} and a 
##'   conversion of the input into a \emph{ts} class object of daily
##'   data is used to calculate the seasonal component. This should be
##'   replaced by a more sophisticated solution as soon I digged
##'   deeper into the field of deseasonalization.
##'   \code{\link{remove.incomplete.years}} is used
##'   to remove incomplete years from the data set. This ensures a
##'   better calculation of the seasonal component but also requires
##'   to forecast it to the length of the original data set and align
##'   it at the right place for subtraction.
##'
##'   This function can also be applied to a list of \pkg{xts} class
##'   objects.
##'
##' @param x A list of objects of class \pkg{xts}.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family ts
##'
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @import lubridate
##' @return Some class as input.
##' @author Philipp Mueller
remove.seasonality.list <- function( x, mc.cores = NULL ){
  if ( !is.null( mc.cores ) ){
    return( mclapply( x, remove.seasonality, mc.cores = mc.cores ) )
  } else {
    return( lapply( x, remove.seasonality, mc.cores = mc.cores ) )
  }
}
##' @title Remove seasonality
##' @description Calculates the seasonal component of a time series
##'   and subtracts it from the original. 
##'
##' @details The function \code{\link[stats]{stl}} with the argument
##'   \code{s.window = 12} and a 
##'   conversion of the input into a \emph{ts} class object of daily
##'   data is used to calculate the seasonal component. This should be
##'   replaced by a more sophisticated solution as soon I digged
##'   deeper into the field of deseasonalization.
##'   \code{\link{remove.incomplete.years}} is used
##'   to remove incomplete years from the data set. This ensures a
##'   better calculation of the seasonal component but also requires
##'   to forecast it to the length of the original data set and align
##'   it at the right place for subtraction.
##'
##'   This function can also be applied to a list of \pkg{xts} class
##'   objects.
##'
##' @param x An object of class \pkg{xts}.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family ts
##'
##' @importFrom xts xts
##' @importFrom parallel mclapply
##' @import lubridate
##' @return Some class as input.
##' @author Philipp Mueller
remove.seasonality.xts <- remove.seasonality.default <-
  function( x, mc.cores = NULL ){
    if ( !any( class( x ) == "xts" ) )
      stop( "Provided element of the wrong class in remove.seasonality. Provide object of class xts instead!" )  
    ## To ensure reasonal results 
    if ( min( x, na.rm = TRUE ) == -999 )
      x[ x == -999 ] <- NA
    x.clean <- remove.incomplete.years( x )
    
    ## When e.g. no maximal temperatures are provided for a station
    ## the function will exist
    if ( length( x.clean ) == 0 )
      return( list( x = NA, modified = FALSE ) )
    
    ## Was time series modified?
    ifelse( length( x ) == length( x.clean ),
           MODIFIED <- FALSE, MODIFIED <- TRUE )
    
    ## maybe to much data was remove
    if ( length( x ) < 365*5 ){
      warning( "After the removal of the incomplete years less than five years are remaining in the data set in remove.seasonality" )
      return( list( x = NA, modified = MODIFIED ) )
    }
    
    ## Seasonal component of the time series
    x.seas <- stats::stl( stats::ts( as.numeric( x.clean ),
                                    frequency = 365 ),
                         s.window = 12 )$time.series[ , 1 ]
    
    ## Usually the seasonal term is more than just a constant
    ## sinusoidal function but exhibits some kind of trend
    ## itself. That's why it's only logical to use the same or the
    ## nearest year in the seasonal component for subtracting of
    ## original series. Subtracting all years which are also present
    ## in the seasonal component
    x.clean.years <- unique( year( x.clean ) )
    x.years <- unique( year( x ) )
    for ( yy in x.clean.years )
      x[ year( x ) == yy ] <- x[ year( x ) == yy ] -
        x.seas[ which( year( x.clean ) == yy ) ]
    
    ## Looping over all years don't accounted yet and substracting the
    ## year nearst in x.seas to the original one. The alignment of the
    ## to data sets has to be handled with care!
    for ( yy in x.years[ !x.years %in% x.clean.years ] ){
      ## closest seasonal component; Caution, can be a vector!
      ## if the year in x is a leap year one has to search for the
      ## closest leap year.
      ## If the closest year has not 366 days its distance is set to
      ## 999
      if ( length( x[ year( x ) == yy ] ) == 366 ){
        closest.year.vec <-  abs( x.clean.years - yy )
        for ( cc in 1 : length( closest.year.vec ) ){
          if ( length( x.clean[
              year( x.clean ) == x.clean.years[
                                     which( closest.year.vec ==
                                            min( closest.year.vec )
                                           ) ] ] )
              == 366 ){
            closest.year.pos <- which( closest.year.vec ==
                                       min( closest.year.vec ) )
            break
          } else
            closest.year.vec[
                which( closest.year.vec ==
                       min( closest.year.vec ) ) ] <- 999
        }
      } else {
        closest.year.pos <- which( abs( x.clean.years - yy ) ==
                                   min( abs( x.clean.years - yy ) ) )
      }
      closest.year <- x.clean.years[ closest.year.pos[ 1 ] ] 
      
      ## subtracts only those days of the closest year in x.seas which
      ## are also present in the x time series. ydays give the
      ## calender date (1--366)
      days.clean <- yday( x.clean[ year( x.clean ) == closest.year ] )
      days.x <- yday( x[ year( x ) == yy ] )
      ## Even if the length of x is not 366 it can still be a leap
      ## year and the 31.12. will be of the value 366. This way the
      ## seasonal part might not has the some number of entries (the
      ## closest year is not a leap year)
      if ( min( days.x ) == 1 && max( days.x ) == 366 &&
           length( days.x ) < 366 ){
        ## Somewhere at least one day is missing. This "step" has to
        ## be detected and the all days after the step have to be
        ## reduced by one
        step <- which( days.x - c( 1 : length( days.x ) ) > 0 )[ 1 ]
        days.x[ step : length( days.x ) ] <- days.x[
            step : length( days.x ) ] - 1
      } else if ( max( days.x ) == 366 && max( days.clean ) != 366 )
        days.x <- days.x - 1
      
      if ( min( days.x ) == 0 )
        stop( "Wait. Something went wrong in remove.seasonality. Check for the correct alignment of the seasonal part and the original time series!" )
      x[ year( x ) == yy ] <- x[ year( x ) == yy ] - 
        as.numeric( x.seas[ year( x.clean ) == closest.year ][
            days.clean %in% days.x ] )
    }
    return( list( x = x, modified = MODIFIED ) )
  }

##' @title Calculates the mode of a PDF
##' @description Calculates the mode of a PDF
##'
##' @param x Numerical input.
##'
##' @return Numerical
##' @author Philipp Mueller 
mode <- function( x ){
  ## Approximation of the mode (most probable/frequent element) of
  ## a time series 
  ## Working with unique() does not yield good results for a sample of
  ## a continuous distribution, therefore using the Kernel density
  ## approximation and determining the peak of the PDF
  x.pdf <- stats::density( x )
  return( x.pdf$x[ which.max( x.pdf$y ) ] )
}

##' @title Anomalies of a time series
##' @description Calculates the anomalies of an object of class
##'   \pkg{xts} or a list of such objects.
##'
##' @details Construction via the subtraction of the mean
##'   value of the specific date.
##'
##'   Uses the \code{\link[lubridate]{yday}}.
##' 
##' @param x Either a time series of class \pkg{xts} or a list of
##'   them.
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @importFrom lubridate yday
##' @importFrom parallel mclapply
##' 
##' @return Same class as the input
##' @author Philipp Mueller
##' 
anomalies <- function( x, mc.cores = NULL ){
  UseMethod( "anomalies" )
}
##' @title Anomalies of a time series
##' @description Calculates the anomalies of an object of class
##'   \pkg{xts} or a list of such objects.
##'
##' @details Construction via the subtraction of the mean
##'   value of the specific date.
##'
##'   Uses the \code{\link[lubridate]{yday}}.
##' 
##' @param x List of time series of class \pkg{xts}
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @importFrom lubridate yday
##' @importFrom parallel mclapply
##' 
##' @return Same class as the input
##' @author Philipp Mueller
##' 
anomalies.list <- function( x, mc.cores = NULL ){
  if ( !is.null( mc.cores ) ){
    return( mclapply( x, anomalies, mc.cores = mc.cores ) )
  } else {
    return( lapply( x, anomalies, mc.cores = mc.cores ) )
  }
}
##' @title Anomalies of a time series
##' @description Calculates the anomalies of an object of class
##'   \pkg{xts} or a list of such objects.
##'
##' @details Construction via the subtraction of the mean
##'   value of the specific date.
##'
##'   Uses the \code{\link[lubridate]{yday}}.
##' 
##' @param x A time series of class \pkg{xts}
##' @param mc.cores A numerical input specifying the number of cores
##'   to use for the multi core application of the function (see
##'   \code{\link[parallel]{detectCores}}). This functionality is only
##'   available if the input is a list of different objects. If NULL,
##'   the function will be calculated classically with only one core.
##'   Default = NULL.
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @importFrom lubridate yday
##' @importFrom parallel mclapply
##' 
##' @return Same class as the input
##' @author Philipp Mueller
anomalies.xts <- anomalies.default <- function( x, mc.cores = NULL ){
  if ( !any( class( x ) == "xts" ) )
    stop( "Only for the class 'xts' the anomalies can be calculated" )
  ## base::ave seems to calculate takes a vector and a factor vector
  ## of the same length. Then it calculates the mean for all values
  ## sharing a factor and is placing the mean value in the element
  ## corresponding to the position of the original one.
  x.anomalies <- x - stats::ave( x, as.factor( yday( x ) ),
                                FUN = function( xx )
                                  mean( xx, na.rm = TRUE ) )
  return( x.anomalies )
}
## End of ts.R
