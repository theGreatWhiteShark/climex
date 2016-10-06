##' @title  Calculates the Akaike information criterion of a gev.fit or climex.gev.fit object.
##' @details The climex.gev.fit object is of identical structure as the output of the optim() function.
##' 
##' @param x Optim()like fit of a time series.
##'
##' @seealso \code{\link{bic}}
##' @family ts
##' 
##' @return Numeric value
##' @author Philipp Mueller
aic <- function( x ){
    if ( ( any( class( x ) != "gev.fit" ) && any( class( x ) != "gpd.fit" ) ) &&
         !any( class( x ) == "climex.gev.fit" ) )
        stop( "Wrong format provided in aic. An object of class 'gev.fit' is required!" )
    if ( any( class( x ) == "climex.gev.fit" ) ){
        2* x$value + 2* length( x$par )
    } else
        2* x$nllh + 2* length( x$mle )
}

##' @title Calculates the Bayesian information criterion of a gev.fit or climex.gev.fit object.
##' @details The climex.gev.fit object is of identical structure as the output of the optim() function. In contrast to the aic the time series itself has to provided too.
##'
##' @param x Optim()like fit of a time series.
##' @param time.series Original time series.
##'
##' @seealso \code{\link{aic}}
##' @family ts
##' 
##' @return Numeric value
##' @author Philipp Mueller
bic <- function( x, time.series = NULL ){
    if ( ( any( class( x ) != "gev.fit" ) && any( class( x ) != "gpd.fit" ) ) && !any( class( x ) == "climex.gev.fit" ) )
        stop( "Wrong format provided in bic. An object of class 'gev.fit' is required!" )
    if ( any( class( x ) == "climex.gev.fit" ) ){
        2* x$value + length( x$par )* log( length( time.series ) )
    } else
        2* x$nllh + length( x$mle )* log( length( x$data ) )
}

##' @title Removes all years which contain either a NA or -999 or are incomplete.
##'
##' @param x Time series. Currently only works with the class xts since reference date in other objects is rather hard to get.
##'
##' @seealso \code{\link{remove.seasonality}}
##'
##' @family ts
##' @export
##' @import xts
##' @import lubridate
##' 
##' @return Time series of class xts
##' @author Philipp Mueller
remove.incomplete.years <- function( x ){
    if ( !any( class( x ) == "xts" ) )
        stop( "Provided element of the wrong class in remove.incomplete.years. Provide object of class xts instead!" )    
    ## If the time series contains -999 for the artifact they are going to be replaced by
    ## NA to assure a broder set of time series to work with this function.
    if ( length( stats::na.omit( x ) ) > 0 && min( x, na.rm = TRUE ) == -999 )
        x[ x == -999 ] <- NA
    if ( any( is.na( x ) ) ){
        na.pos <- which( is.na( x ) ) # positions of the artifacts
        na.dates <- index( x[ na.pos ] )
        na.years <- unique( year( na.dates ) )
        x <- x[ !year( x ) %in% na.years ]
    } 
    
    ## Checking for the completeness of the first and the last year. In principle this
    ## could also be done by just checking the date of the first and last element of the ts.
    ## So obviously there are data sets where half of a year is missing without any
    ## artifacts indicating this fact. So a loop over all the years in the data is needed
    for ( yy in unique( year( x ) ) ){
        if ( max( yday( x[ year( x ) == yy ] ) ) == 366 && length( x[ year( x ) == yy ] ) < 366 ){
            x <- x[ year( x ) != yy ]
        } else if ( length( x[ year( x ) == yy ] ) < 365 ){
            x <- x[ year( x ) != yy ]
        }
    }
    return( x )
}

##' @title Calculates the seasonal component of a time series and subtracts it from the original.
##'
##' @details Only time series of class "xts" are accepted (on purpose because I want to get rid of handling both objects of class "ts" and "xts"). For now stats::stl with s.window = 12 and a conversion of the input into a ts object of daily data is used to calculate the seasonal component. This should be replaced by a more sophisticated solution as soon I digged deeper into the field of deseasonalization. \code{\link{remove.incomplete.years}} is used to remove incomplete years from the data set. This ensures a better calculation of the seasonal component but also requires to forecast it to the length of the original data set and align it at the right place for subtraction.
##'
##' @param x Time series of class "xts"
##'
##' @family ts
##'
##' @import xts
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
    
    ## When e.g. no maximal temperatures are provided for a station the function will exist
    if ( length( x.clean ) == 0 )
        return( list( x = NA, modified = FALSE ) )
    
    ## Was time series modified?
    ifelse( length( x ) == length( x.clean ), MODIFIED <- FALSE, MODIFIED <- TRUE )
    
    ## maybe to much data was remove
    if ( length( x ) < 365*5 ){
        warning( "After the removal of the incomplete years less than five years are remaining in the data set in remove.seasonality" )
        return( list( x = NA, modified = MODIFIED ) )
    }
    
    ## Seasonal component of the time series
    x.seas <- stats::stl( stats::ts( as.numeric( x.clean ), frequency = 365 ),
                         s.window = 12 )$time.series[ , 1 ]
    
    ## Usually the seasonal term is more than just a constant sinusoidal function but
    ## exhibits some kind of trend itself. Thats why its only logical to use the same
    ## or the nearest year in the seasonal component for subtracting of original series.
    ## subtracting all years which are also present in the seasonal component
    x.clean.years <- unique( year( x.clean ) )
    x.years <- unique( year( x ) )
    for ( yy in x.clean.years )
        x[ year( x ) == yy ] <- x[ year( x ) == yy ] - x.seas[ which( year( x.clean ) == yy ) ]
    
    ## Looping over all years don't accounted yet and substracting the year nearst in
    ## x.seas to the original one. The alignment of the to data sets has to be handled with care!
    for ( yy in x.years[ !x.years %in% x.clean.years ] ){
        ## closest seasonal component; Caution, can be a vector!
        
        ## if the year in x is a leap year one has to search for the closest leap year
        ## If the closest year has not 366 days its distance is set to 999
        if ( length( x[ year( x ) == yy ] ) == 366 ){
            closest.year.vec <-  abs( x.clean.years - yy )
            for ( cc in 1 : length( closest.year.vec ) ){
                if ( length( x.clean[ year( x.clean ) ==
                                      x.clean.years[ which( closest.year.vec ==
                                                            min( closest.year.vec ) ) ] ] ) == 366 ){
                    closest.year.pos <- which( closest.year.vec == min( closest.year.vec ) )
                    break
                } else
                    closest.year.vec[ which( closest.year.vec == min( closest.year.vec ) ) ] <- 999
            }
        } else {
            closest.year.pos <- which( abs( x.clean.years - yy ) ==
                                       min( abs( x.clean.years - yy ) ) )
        }
        closest.year <- x.clean.years[ closest.year.pos[ 1 ] ] 
        
        ## subtracts only those days of the closest year in x.seas which are also present in
        ## the x time series. ydays give the calender date (1--366)
        days.clean <- yday( x.clean[ year( x.clean ) == closest.year ] )
        days.x <- yday( x[ year( x ) == yy ] )
        ## Even if the length of x is not 366 it can still be a leap year and the 31.12.
        ## will be of the value 366. This way the seasonal part might not has the some
        ## number of entries (the closest year is not a leap year)
        if ( min( days.x ) == 1 && max( days.x ) == 366 && length( days.x ) < 366 ){
            ## Somewhere at least one day is missing. This "step" has to be detected and
            ## the all days after the step have to be reduced by one
            step <- which( days.x - c( 1 : length( days.x ) ) > 0 )[ 1 ]
            days.x[ step : length( days.x ) ] <- days.x[ step : length( days.x ) ] - 1
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
##'
##' @return Numerical
##' @author Philipp Mueller 
mode <- function( x ){
    ## Approximation of the mode (most probable/frequent element) of a time series
    ## Working with unique() does not yield good results for a sample of a continuous distribution
    ## Therefore using the Kernel density approximation and determining the peak of the PDF
    x.pdf <- stats::density( x )
    
    return( x.pdf$x[ which.max( x.pdf$y ) ] )
}

##' @title Calculates the anomalies of a time series.
##'
##' @details Construction via the subtraction of the mean (temperature) value of the specific date. I don't really like this one. Maybe try to add a weighting function with sharp bandwidth instead. Uses the lubridate::yday()
##'
##' @param x Time series of the class 'xts'.
##'
##' @family ts
##' @export
##' @import xts
##' @importFrom lubridate yday
##' 
##' @return Time series of the class 'xts' of the same length as x containing the anomalies of the series.
##' @author Philipp Mueller
anomalies <- function( x ){
    if ( !any( class( x ) == "xts" ) )
        stop( "Only for the class 'xts' the anomalies can be calculated" )
    xDays <- data.frame( x = x, days = as.factor( yday( x ) ) )
    ## base::ave seems to calculate takes a vector and a factor vector of the same length.
    ## Then it calculates the mean for all values sharing a factor and is placing the
    ## mean value in the element corresponding to the position of the original one.
    xAnomalies <- x - stats::ave( x, as.factor( yday( x ) ),
                                FUN = function( x ) mean( x, na.rm = TRUE ) )
    return( xAnomalies )
}
