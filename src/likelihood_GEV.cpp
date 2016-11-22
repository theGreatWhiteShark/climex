
// includes from the plugin
#include <RcppArmadillo.h>
#include <Rcpp.h>


#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;


// user includes


// declarations
extern "C" {
SEXP likelihood_GEV( SEXP parameters, SEXP xin) ;
}

// definition

SEXP likelihood_GEV( SEXP parameters, SEXP xin ){
BEGIN_RCPP

Rcpp::DataFrame parametersDataframe( parameters );
Rcpp::NumericVector xVector( xin );
Rcpp::NumericVector loc = parametersDataframe[ "location" ];
Rcpp::NumericVector sc = parametersDataframe[ "scale" ];
Rcpp::NumericVector sh = parametersDataframe[ "shape" ];
Rcpp::NumericVector L(loc.size());
Rcpp::NumericVector y( xVector.size() );
for ( int ii = 0; ii < sc.size(); ii++ ){
    y = 1 + ( ( xVector - loc[ii])/sc[ii] )*sh[ii];    
    L[ii] =  log( sc[ii] )* xVector.size() + sum( pow( y, -1/sh[ii] ) ) + sum( log( y ) )*(1/sh[ii] + 1 ) ;
}                                          
return Rcpp::wrap( L ); 
END_RCPP
}



