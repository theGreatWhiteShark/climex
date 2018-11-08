### utils.R - Auxiliary functions
##' @title Print details
##' @description Summary of the GEV fit results
##'
##' @param x Object of class \emph{climex.fit.gev}.
##' @param ... Additional parameters. They won't be handled in the
##'   function. This argument is only present to ensure S3 generic
##'   consistency with respect to the \code{\link[base]{print}}
##'   function.
##' @export
##'
##' @return \code{invisible()}
##' @author Philipp Mueller
print.climex.fit.gev <- function( x, ... ){
  summary( x )
  invisible()
}
##' @title Summarize details
##' @description Summary of the GEV fit results
##'
##' @param object Object of class \emph{climex.fit.gev}.
##' @param ... Additional parameters. They won't be handled in the
##'   function. This argument is only present to ensure S3 generic
##'   consistency with respect to the \code{\link[base]{summary}}
##'   function.
##' @export
##'
##' @return \code{invisible()}
##' @author Philipp Mueller
summary.climex.fit.gev <- function( object, ... ){
  cat( "\n" )
  cat( paste( length( object$x ), "block maxima fitted using then " ) )
  if ( object$control$error.estimation != "none" ){
    cat( paste( " Errors using",
               object$control$error.estimation, "approach." ) )
  }
  cat( "\n\n" )
  cat( "\t\tFunction evaluations:\n" )
  print( data.frame( function.eval = as.numeric( object$counts[ 1 ] ),
                    gradient.eval =
                      ifelse( is.na( object$counts[ 2 ] ), 0,
                             as.numeric( object$counts[ 2 ] ) ),
                    penalty.updates = object$outer.iteration,
                    row.names = "eval" ) )
  cat( "\n" )
  cat( "\t\tFit statistics:\n" )
  print( data.frame( nllh = object$value,
                    AIC = aic( object ),
                    BIC = bic( object ),
                    row.names = "augmented fit" ) )
  cat( "\n" )
  cat( "\t\tEstimated parameters:\n" )
  print( data.frame( parameter = object$par,
                    fitting.error = as.numeric( object$se[ 1 : 3 ] ),
                    row.names = c( "location", "scale", "shape" ) ) )
  cat( "\n" )
  cat( "\t\tEstimated return levels:\n" )
  print( data.frame( return.level = object$return.level,
                    fitting.error = as.numeric(
                        object$se[ 4 : length( object$se ) ] ),
                    row.names = paste( as.character(
                        object$control$return.period ),
                        "block return level" ) ) )
  cat( "\n" )
  invisible()
}
##' @title Print results
##' @description Summary of the GP fit results
##' @param x Object of class \emph{climex.fit.gpd}.
##' @param ... Additional parameters. They won't be handled in the
##'   function. This argument is only present to ensure S3 generic
##'   consistency with respect to the \code{\link[base]{print}}
##'   function.
##' @export
##'
##' @return \code{invisible()}
##' @author Philipp Mueller
print.climex.fit.gpd <- function( x, ... ){
  summary( x )
  invisible()
}
##' @title Summarize results
##' @description Summary of the GP fit results
##' @param object Object of class \emph{climex.fit.gpd}.
##' @param ... Additional parameters. They won't be handled in the
##'   function. This argument is only present to ensure S3 generic
##'   consistency with respect to the \code{\link[base]{summary}}
##'   function.
##' @export
##'
##' @return \code{invisible()}
##' @author Philipp Mueller
summary.climex.fit.gpd <- function( object, ... ){
  cat( "\n" )
  cat( paste( length( object$x ), "exceedances over the threshold",
             object$threshold ) )
  cat( "\noptimization routine." )
  if ( object$control$error.estimation != "none" ){
    cat( paste( " Errors using",
               object$control$error.estimation, "approach." ) )
  }
  cat( "\n\n" )
  cat( "\t\tFunction evaluations:\n" )
  print( data.frame( function.eval = as.numeric( object$counts[ 1 ] ),
                    gradient.eval =
                      ifelse( is.na( object$counts[ 2 ] ), 0,
                             as.numeric( object$counts[ 2 ] ) ),
                    penalty.updates = object$outer.iteration,
                    row.names = "eval" ) )
  cat( "\n" )
  cat( "\t\tFit statistics:\n" )
  print( data.frame( nllh = object$value,
                    AIC = aic( object ),
                    BIC = bic( object ),
                    row.names = "augmented fit" ) )
  cat( "\n" )
  cat( "\t\tEstimated parameters:\n" )
  print( data.frame( parameter = object$par,
                    fitting.error = as.numeric( object$se[ 1 : 2 ] ),
                    row.names = c( "scale", "shape" ) ) )
  cat( "\n" )
  cat( "\t\tEstimated return levels:\n" )
  print( data.frame( return.level = object$return.level,
                    fitting.error = as.numeric(
                        object$se[ 3 : length( object$se ) ] ),
                    row.names = paste( as.character(
                        object$control$return.period ),
                        "year return level" ) ) )
  cat( "\n" )
  invisible()
}
## End of utils.R
