### zzz.R - Assuring proper linking and export of internal functions.
## Link and export the C++ version of the GEV negative
## log-likelihood. Note that this function will only be used in the
## plot.likelihood function.
##' @useDynLib climex
##' @importFrom Rcpp sourceCpp
NULL

## End of zzz.R
