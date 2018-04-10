##' @title Load a data file into R
##' @description Searches the \emph{~/R/climex/} directory or a
##'   specified folder for .RData files recursively and displays the
##'   user its findings for her to choose one of them.
##' @details In order to use the data with the \pkg{climex}, the data
##'   should be of class \pkg{xts} or of lists of class \pkg{xts}
##'   objects.
##'
##'   You can use the \pkg{dwd2r} package to download and use the
##'   daily station data provided by the German weather service.
##'
##' @param download.path Specifies the folder in which the function
##'   will look for .RData files recursively. Per default the
##'   \emph{R/climex/} directory in your home folder will be used. You
##'   can overwrite 
##'   this behavior by setting \code{options( climex.path = "PATH" )}
##'   in your \emph{.Rprofile} path in your home.
##' @param envir Environment the data will be attached to. If
##'   not specified, the data will be loaded to the environment the
##'   function is called from. Default = NULL.
##' @family import
##'  
##' @export
##' @return Returns invisible but attaches the chosen .RData file to
##'   the specified R environment.
##' @author Philipp Mueller
source.data <- function( download.path = NULL, envir = NULL ){
  ## The folder to put all the temporary files of the climex
  ## package in is set in the options(). To modify it,
  ## overwrite the options( climex.path ) in the .Rprofile
  ## file in your home directory
  if ( is.null( download.path ) ){
    download.path <- getOption( "climex.path" )
  }

  ## Extract all .RData objects contained in the Climex path.
  data.path <- list.files( download.path, pattern = ".RData",
                          recursive = TRUE )
  ## Obtain the size of the file in MB
  data.size <- rep( NA, length( data.path ) )
  for ( dd in 1 : length( data.path ) ){
    data.size[ dd ] <- file.size(
        paste0( download.path, data.path[ dd ] ) )/ 1024^2
  }

  ## Print the user a compilation of all found objects.
  cat( '\nImporting data into your R session.\n\n' )
  cat( paste0( '\tData files found the folder ', download.path,
              ':\n\n' ) )
  cat( '   size:\tpath:\n' )
  for ( dd in 1 : length( data.path ) ){
    if ( data.size[ dd ] < 100 ){
      cat( paste0( dd, '. ', round( data.size[ dd ], digits = 2 ),
                  '\t\t', data.path[ dd ], '\n' ) )
    } else {
      cat( paste0( dd, '. ', round( data.size[ dd ], digits = 2 ),
                  '\t', data.path[ dd ], '\n' ) )
    }
  }
  cat( '\n\n' )
  cat(
      'Please select one file by entering the corresponding number.\n' )
  data.selection <- readline( 'Selection: ' )

  print( paste( "Loading file",
               data.path[ as.numeric( data.selection ) ], "..." ) )

  if ( is.null( envir ) ) {
    load( file = paste0( download.path,
                        data.path[ as.numeric( data.selection ) ] ),
         envir = parent.frame() )
  } else {
    load( file = paste0( download.path,
                        data.path[ as.numeric( data.selection ) ] ),
         envir = envir )
  } 
  invisible( )
}
