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
  cat( paste( length( x$x ), "block maxima fitted using the\n" ) )
  cat( paste( x$control$optim.method, "optimization routine.\n" ) )
  cat( "\n" )
  cat( "Estimated parameters\n" )
  print( data.frame( parameter = x$par, fitting.error = x$se[ 1 : 3 ],
                    row.names = c( "location", "scale", "shape" ) ) )
  cat( "\n" )
  cat( "Estimated return levels\n" )
  print( data.frame( return.level = x$return.level,
                    fitting.error = x$se[ 4 : length( x$se ) ],
                    row.names = as.character( x$control$return.period ) ) )
  cat( "\n" )
  if ( x$control$error.estimation != "none" ){
    cat( paste( "The error estimation was done using the",
               x$control$error.estimation, "approach\n" ) )
  }
  cat( paste( "The optimizaion took", x$counts$function, "function and",
             x$counts$gradient, "gradient evaluation\n as well as",
             x$outer.iteration,
             "updates of the penalty and|or Lagrangian parameter to complete" ) )

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
             x$threshold, "using the\n" ) )
  cat( paste( x$control$optim.method, "optimization routine.\n" ) )
  cat( "\n" )
  cat( "Estimated parameters\n" )
  print( data.frame( parameter = x$par, fitting.error = x$se[ 1 : 2 ],
                    row.names = c( "scale", "shape" ) ) )
  cat( "\n" )
  cat( "Estimated return levels\n" )
  print( data.frame( return.level = x$return.level,
                    fitting.error = x$se[ 3 : length( x$se ) ],
                    row.names = as.character( x$control$return.period ) ) )
  cat( "\n" )
  if ( x$control$error.estimation != "none" ){
    cat( paste( "The error estimation was done using the",
               x$control$error.estimation, "approach\n" ) )
  }
  cat( paste( "The optimizaion took", x$counts$function, "function and",
             x$counts$gradient, "gradient evaluation\n as well as",
             x$outer.iteration,
             "updates of the penalty and|or Lagrangian parameter to complete" ) )

  invisible()
}
