##' @title Convert data into dates and back.
##' @description Converts class 'numeric' into 'Date' and back and also
##' transforms the DWD input (class 'integer') into class 'Date'.
##' @details The date entry has to be provided in the first column.
##' Generic function for the following input classes: stations, years,
##' bulk, integer, numeric, character and Date. For an easy way to
##' produce a specific date a string ala "20140101" can be provided.
##'
##' @param input.data Data containing a date entry in its first column.
##' @param origin Sets origin date for the transformation of numerical
##' data into class 'Date'. Per default its set to "1970-01-01" like its
##' the convention in many software including R.
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

##' @title Summary of the fit results
##' @export
##' @author Philipp Mueller
print.climex.fit.gev <- function( x ){
  summary( x )
  invisible()
}
##' @title Summary of the fit results
##' @export
##' @author Philipp Mueller
summary.climex.fit.gev <- function( x ){
  cat( "\n" )
  cat( paste( length( x$x ), "block maxima fitted using then " ) )
  if ( x$control$error.estimation != "none" ){
    cat( paste( " Errors using",
               x$control$error.estimation, "approach." ) )
  }
  cat( "\n\n" )
  cat( "\t\tFunction evaluations:\n" )
  print( data.frame( function.eval = as.numeric( x$counts[ 1 ] ),
                    gradient.eval = ifelse( is.na( x$counts[ 2 ] ), 0,
                                           as.numeric( x$counts[ 2 ] ) ),
                    penalty.updates = x$outer.iteration,
                    row.names = "eval" ) )
  cat( "\n" )
  cat( "\t\tFit statistics:\n" )
  print( data.frame( nllh = x$value,
                    AIC = climex:::aic( x ),
                    BIC = climex:::bic( x ),
                    row.names = "augmented fit" ) )
  cat( "\n" )
  cat( "\t\tEstimated parameters:\n" )
  print( data.frame( parameter = x$par,
                    fitting.error = as.numeric( x$se[ 1 : 3 ] ),
                    row.names = c( "location", "scale", "shape" ) ) )
  cat( "\n" )
  cat( "\t\tEstimated return levels:\n" )
  print( data.frame( return.level = x$return.level,
                    fitting.error = as.numeric(
                        x$se[ 4 : length( x$se ) ] ),
                    row.names = paste( as.character(
                        x$control$return.period ),
                        "block return level" ) ) )
  cat( "\n" )
  invisible()
}
##' @title Summary of the fit results
##' @export
##' @author Philipp Mueller
print.climex.fit.gpd <- function( x ){
  summary( x )
  invisible()
}
##' @title Summary of the fit results
##' @export
##' @author Philipp Mueller
summary.climex.fit.gpd <- function( x ){
  cat( "\n" )
  cat( paste( length( x$x ), "exceedances over the threshold",
             x$threshold ) )
  cat( "\noptimization routine." )
  if ( x$control$error.estimation != "none" ){
    cat( paste( " Errors using",
               x$control$error.estimation, "approach." ) )
  }
  cat( "\n\n" )
  cat( "\t\tFunction evaluations:\n" )
  print( data.frame( function.eval = as.numeric( x$counts[ 1 ] ),
                    gradient.eval = ifelse( is.na( x$counts[ 2 ] ), 0,
                                           as.numeric( x$counts[ 2 ] ) ),
                    penalty.updates = x$outer.iteration,
                    row.names = "eval" ) )
  cat( "\n" )
  cat( "\t\tFit statistics:\n" )
  print( data.frame( nllh = x$value,
                    AIC = climex:::aic( x ),
                    BIC = climex:::bic( x ),
                    row.names = "augmented fit" ) )
  cat( "\n" )
  cat( "\t\tEstimated parameters:\n" )
  print( data.frame( parameter = x$par,
                    fitting.error = as.numeric( x$se[ 1 : 2 ] ),
                    row.names = c( "scale", "shape" ) ) )
  cat( "\n" )
  cat( "\t\tEstimated return levels:\n" )
  print( data.frame( return.level = x$return.level,
                    fitting.error = as.numeric(
                        x$se[ 3 : length( x$se ) ] ),
                    row.names = paste( as.character(
                        x$control$return.period ),
                        "year return level" ) ) )
  cat( "\n" )
  invisible()
}
