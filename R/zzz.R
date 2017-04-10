.onLoad <- function( libname, pkgname ){
  present.options <- options()
  climex.options <- list(
      climex.path = "~/R/climex/"
  )
  ## Check which of the climex options are set by the user in the .Rprofile
  mismatching.options <- !( names( climex.options ) %in%
                         names( present.options ) )
  if ( any( mismatching.options ) ){
    options( climex.options[ mismatching.options ] )
  }

  invisible()
}
