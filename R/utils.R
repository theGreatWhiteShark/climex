##' @title Convert data into dates and back.
##' @description Converts class 'numeric' into 'Date' and back and also transforms the DWD input (class 'integer') into class 'Date'.
##' @details The date entry has to be provided in the first column. Generic function for the following input classes: stations, years, bulk, integer, numeric, character and Date. For an easy way to produce a specific date a string ala "20140101" can be provided.
##'
##' @param input.data Data containing a date entry in its first column.
##' @param origin Sets origin date for the transformation of numerical data into class 'Date'. Per default its set to "1970-01-01" like its the convention in many software including R.
##' @family conversion utils
##'
##' @return Returns input in same format with second column of class date
##' @import lubridate
##' @author Philipp Mueller
convert.date <- function( input.data, origin = "1970-01-01" ){
  UseMethod( "convert.date" )
}
convert.date.character <- function( input.data, origin = 1970-01-01 ){
  return( as.Date( input.data, format = "%Y%m%d" ) )
}
convert.date.integer <- function( input.data, origin = "1970-01-01" ){
  if ( !is.integer( input.data ) )
    stop( "data provided for convert.date.integer has the wrong format. Class 'integer' is required!" )
  output.data <- as.Date( as.character( input.data[ ] ),
                         format = "%Y%m%d" )
  return( output.data )
}
convert.date.numeric <- function( input.data, origin = "1970-01-01" ){
  if ( !is.numeric( input.data ) )
    stop( "data provided for convert.date.numeric has the wrong format. Class 'numeric' is required!" )
  output.data <- as.Date( input.data[ ], origin = origin )
  return( output.data )
}
convert.date.Date <- function( input.data, origin = "1970-01-01"  ){
  if ( !is.Date( input.data ) )
    stop( "data provided for convert.date.Date has the wrong format. Class 'Date' is required!" )
  output.data <- as.numeric( input.data[ ] )
  return( output.data )
}

##' @title Converts fits done by \code{\link{optim}} into an object which can be used by the functions of the extRemes package
##'
##' @details Caution: the fit performed by the fevd function also calculates the hessian matrix. The element 'initial.results' contains the initial values of the GEV parameters calculated via the heuristic formula of the moments and the lmoments method. But I don't see a point right now to implement this too. In addition 'call' is not a real call.
##'
##' @param x Time series on which the.
##' @param fit GEV fit performed on the data set.
##'
##' @return Of class "fevd"
##' @author Philipp Mueller 
as.fevd <- function( x, fit = stats::optim( x ), call = "fevd(x = x)",
                    data.name = c( "x", "" ), weights = 1,
                    in.data = FALSE, method = "MLE", type = "GEV",
                    period.basis = "year",
                    par.models = list(
                        threshold = ~1, location = ~1, scale = ~1,
                        log.scale = FALSE, shape = ~1,
                        term.names = list(
                            threshold = character( 0 ),
                            location = character( 0 ),
                            scale = character( 0 ),
                            shape = character( 0 ) ) ),
                    const.loc = TRUE, const.scale = TRUE,
                    const.shape = TRUE, n = length( x ),
                    na.action = "na.fail",
                    parnames = c( "location", "scale", "shape" ),
                    results = fit, initial.results = NULL,
                    threshold = NULL, npy = NULL ){
  output <- list(
      call = call, data.name = data.name, weights = weights,
      in.data = in.data, x = as.numeric( x ), method = method,
      type = type, period.basis = period.basis,
      par.models = par.models, const.loc = const.loc,
      const.scale = const.scale, const.shape = const.shape, n = n,
      na.action = na.action, parnames = parnames,
      results = results, initial.results = initial.results,
      threshold = threshold, npy = npy )
  class( output ) <- "fevd"
  return( output )
}


