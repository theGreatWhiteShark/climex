### All functions and modules in charge of fitting a GEV or GP function to
### the provided time series in the Climex app.

##' @title Selection input to determine the fitting routine/method 
##'
##' @family climex-fitting
##'
##' @import shiny
##' 
##' @return selectInput
##' @author Philipp Mueller 
generalFittingRoutineInput <- function(){
  selectInput( "selectOptimization", "Fitting routine",
              choices = c( "Nelder-Mead", "CG", "BFGS", "SANN",
                          "dfoptim::nmk" ),
              selected = c( "Nelder-Mead" ) )
}
