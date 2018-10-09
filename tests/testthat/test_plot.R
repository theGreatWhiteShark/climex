### test_plot.R - Tests checking the plotting routines.
library( climex )
library( ggplot2 )
context( "Check if the custom plotting routines work" )

x.block.fit <-
  climex::fit.gev(
              climex::block( climex::anomalies( temp.potsdam ) ),
              error.estimation = "none", silent = TRUE )
x.thresh.fit <-
  climex::fit.gpd(
              climex::threshold( temp.potsdam,
                                threshold = 29,
                                decluster = TRUE ),
              threshold = 29, error.estimation = "none",
              total.length = length( temp.potsdam ), silent = TRUE )

## I don't wanna see the plots

test_that( "ttplot results in plot object and takes the right inputs", {
  expect_true( is.ggplot( ttplot( temp.potsdam ) ) )
  expect_error( ttplot( as.numeric( temp.potsdam ) ) )
})

test_that( "plot.climex.fit.gev works on the result of fit.gev", {
  expect_true( is.ggplot( plot( x.block.fit ) ) )
})

test_that( "plot.climex.fit.gpd works on the result of fit.gpd", {
  expect_true( is.ggplot( plot( x.thresh.fit ) ) )
})
## End of test_plot.R
