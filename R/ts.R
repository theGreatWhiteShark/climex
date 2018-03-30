##' @title Akaike information criterion
##' @description Calculates the Akaike information criterion of a climex.fit.gev or climex.fit.gpd object.
##' @details The climex.fit.gev object is of identical structure as the output of the optim() function.
##' 
##' @param x Optim() like fit of a time series.
##'
##' @seealso \code{\link{bic}}
##' @family ts
##' 
##' @return Numeric value
##' @author Philipp Mueller
aic <- function( x ){
  if ( !any( class( x ) == "climex.fit.gev" ) &&
       !any( class( x ) == "climex.fit.gpd" ) )
    stop( "Wrong format provided in aic. Please supply an object returned by either fit.gev or fit.gpd!" )
  2* x$value + 2* length( x$par )
}

##' @title Bayesian information criterion
##' @description Calculates the Bayesian information criterion of a climex.fit.gev or climex.fit.gpd object.
##' @details The climex.fit.gev object is of identical structure as the output of the optim() function.
##'
##' @param x Object of class climex.fit.gev or climex.fit.gpd. When ensuring the presence of all additional list elements an output of optim() can also be used.
##'
##' @seealso \code{\link{aic}}
##' @family ts
##' 
##' @return Numeric value
##' @author Philipp Mueller
bic <- function( x ){
  if ( !any( class( x ) == "climex.fit.gev" ) &&
       !any( class( x ) == "climex.fit.gpd" ) )
    stop( "Wrong format provided in bic. Please supply an object returned by either fit.gev or fit.gpd!" )
  2* x$value + length( x$par )* log( length( x$x ) )
}

##' @title Remove incomplete years
##' @description Removes all years, which contain either a NA or are
##'   incomplete. 
##'
##' @details Since incomplete years are detected via the
##'   difference in the time stamps of neighbouring points, the user
##'   has to provide the basic time unit. (Numerical value of the
##'   result of the `diff' function applied to two consecutive
##'   points.) For daily data the value is 1.
##' 
##' @param x Time series. Currently only works with the class xts
##'   since reference date in other objects is rather hard to get.
##' @param time.unit Minimal differences found when applying the
##'   `diff' function to `x'. Default = 1.
##'
##' @seealso \code{\link{remove.seasonality}}
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @import lubridate
##' 
##' @return Time series of class xts
##' @author Philipp Mueller
remove.incomplete.years <- function( x, time.unit = 1 ){
  if ( !any( class( x ) == "xts" ) )
    stop(
        "Provided element of the wrong class in remove.incomplete.years. Provide object of class xts instead!" )
  ## Removes all values containing a NA, R's general type for missing
  ## data. 
  x <- x[ !is.na( x ) ]

  ## There is data available.
  if ( length( x ) > 0 ){
    ## The missing points are detected by looking for differences in
    ## the time stamps of neighbouring points. If their are more than
    ## time.unit apart, there are some missing values in between.
    x.index <- index( x )
    x.index.diff <- diff( x.index )
    ## If the day before the gap is the last day of a year, it's year
    ## value shouldn't be included in the list of years ready to
    ## remove, since the corresponding gap doesn't affect it. Those
    ## gaps have to be treated specially.
    x.last.day.of.year <-
      grep( "12-31", index( x [ which( x.index.diff > time.unit ) ] ) )
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
    ## Be sure to include all years, even if several years passed within
    ## one gaps. The seq() function is used on every pair of points
    ## before and after the gap.
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
    ## Check whether the first year starts at January 1st and the last
    ## on Dec. 31st. Else the corresponding years aren't complete.
    if ( length( grep( "-01-01", index( x[ 1 ] ) ) ) == 0 ){
      all.incomplete.years <- c( all.incomplete.years,
                                year( x[ 1 ] ) )
    }
    if ( length( grep( "-12-31", index( x[ length( x ) ] ) ) ) == 0 ){
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

##' @title Remove seasonality
##' @description Calculates the seasonal component of a time series
##'   and subtracts it from the original. 
##'
##' @details Only time series of class "xts" are accepted (on purpose
##'   because I want to get rid of handling both objects of class "ts"
##'   and "xts"). For now \code{\link[stats]{stl}} with s.window = 12
##'   and a 
##'   conversion of the input into a ts object of daily data is used
##'   to calculate the seasonal component. This should be replaced by
##'   a more sophisticated solution as soon I digged deeper into the
##'   field of
##'   deseasonalization. \code{\link{remove.incomplete.years}} is used
##'   to remove incomplete years from the data set. This ensures a
##'   better calculation of the seasonal component but also requires
##'   to forecast it to the length of the original data set and align
##'   it at the right place for subtraction. 
##'
##' @param x Time series of class "xts"
##'
##' @family ts
##'
##' @importFrom xts xts
##' @import lubridate
##' @return Deseasonalized time series of class "xts".
##' @author Philipp Mueller
remove.seasonality <- function( x ){
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
  
  ## Usually the seasonal term is more than just a constant sinusoidal
  ## function but exhibits some kind of trend itself. That's why it's
  ## only logical to use the same or the nearest year in the seasonal
  ## component for subtracting of original series.
  ## Subtracting all years which are also present in the seasonal
  ## component
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
    ## If the closest year has not 366 days its distance is set to 999
    if ( length( x[ year( x ) == yy ] ) == 366 ){
      closest.year.vec <-  abs( x.clean.years - yy )
      for ( cc in 1 : length( closest.year.vec ) ){
        if ( length( x.clean[
            year( x.clean ) == x.clean.years[
                                   which( closest.year.vec ==
                                          min( closest.year.vec ) ) ] ] )
            == 366 ){
          closest.year.pos <- which( closest.year.vec ==
                                     min( closest.year.vec ) )
          break
        } else
          closest.year.vec[ which( closest.year.vec ==
                                   min( closest.year.vec ) ) ] <- 999
      }
    } else {
      closest.year.pos <- which( abs( x.clean.years - yy ) ==
                                 min( abs( x.clean.years - yy ) ) )
    }
    closest.year <- x.clean.years[ closest.year.pos[ 1 ] ] 
    
    ## subtracts only those days of the closest year in x.seas which
    ## are also present in the x time series. ydays give the calender
    ## date (1--366)
    days.clean <- yday( x.clean[ year( x.clean ) == closest.year ] )
    days.x <- yday( x[ year( x ) == yy ] )
    ## Even if the length of x is not 366 it can still be a leap year
    ## and the 31.12. will be of the value 366. This way the seasonal
    ## part might not has the some number of entries (the closest year
    ## is not a leap year)
    if ( min( days.x ) == 1 && max( days.x ) == 366 &&
         length( days.x ) < 366 ){
      ## Somewhere at least one day is missing. This "step" has to
      ## be detected and the all days after the step have to be reduced
      ## by one
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
##' @description Calculates the anomalies of a time series.
##'
##' @details Construction via the subtraction of the mean
##'   (temperature) value of the specific date. I don't really like
##'   this one. Maybe try to add a weighting function with sharp
##'   bandwidth instead. Uses the \code{\link[lubridate]{yday}}.
##' 
##' @param x Time series of the class 'xts'.
##'
##' @family ts
##' @export
##' @importFrom xts xts
##' @importFrom lubridate yday
##' 
##' @return Time series of the class 'xts' of the same length as x containing the anomalies of the series.
##' @author Philipp Mueller
anomalies <- function( x ){
  if ( !any( class( x ) == "xts" ) )
    stop( "Only for the class 'xts' the anomalies can be calculated" )
  xDays <- data.frame( x = x, days = as.factor( yday( x ) ) )
  ## base::ave seems to calculate takes a vector and a factor vector of
  ## the same length. Then it calculates the mean for all values
  ## sharing a factor and is placing the mean value in the element
  ## corresponding to the position of the original one.
  xAnomalies <- x - stats::ave( x, as.factor( yday( x ) ),
                               FUN = function( x )
                                 mean( x, na.rm = TRUE ) )
  return( xAnomalies )
}
