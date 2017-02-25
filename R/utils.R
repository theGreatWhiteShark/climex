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



