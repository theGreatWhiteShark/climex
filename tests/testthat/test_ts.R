library( climex )
context( "Check function is R/ts.R" )

x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )
x.block.fit <- climex::fit.gev( x.block )
x.thresh.fit <- climex::fit.gpd( x.thresh,
                                total.length = length( temp.potsdam ) )

test_that( "aic accepts the right input and produces the right output", {
  expect_equal( climex:::aic( x.block.fit ), 447.2131 - 3.67e-05 )
  expect_equal( climex:::aic( x.thresh.fit ), 1454.037 - 0.000463 )
  expect_error( climex:::aic( x.block ) )
  expect_error( climex:::aic( as.numeric( x.block ) ) )
})
test_that( "bic accepts the right input and produces the right output", {
  expect_equal( climex:::bic( x.block.fit ), 455.6739 + 7.95e-06 )
  expect_equal( climex:::bic( x.thresh.fit ), 1461.792 )
  expect_error( climex:::bic( x.block ) )
  expect_error( climex:::bic( as.numeric( x.block ) ) )
})

x.removed <- climex::remove.incomplete.years( temp.potsdam )
test_that( "remove.incomplete.years works as expected", {
  expect_equal( length( temp.potsdam ), 45145 )
  expect_equal( length( x.removed ), 44924 )
  expect_equal( length(
      climex::remove.incomplete.years( x.removed[ -1 ] ) ), 44559 )
  expect_error( climex::remove.incomplete.years(
                            as.numeric( temp.potsdam ) ) )
})
x.removed[ 1 ] <- NA
test_that( "remove.incomplete.years get's rid of NA in time series",{
  expect_equal( min( climex::remove.incomplete.years( x.removed ) ),
               -16 )
  expect_equal( length( climex::remove.incomplete.years( x.removed ) ),
               44559 )
})

test_that( "check.completeness is throwing errors on wrong input", {
  expect_error( check.completeness( temp.potsdam,
                                   number.of.years = "30" ) )
  expect_error( check.completeness( 30 ) )
  expect_error( check.completeness( list( 30, temp.potsdam ) ) ) } )

x.complete <- x.incomplete <- remove.incomplete.years( temp.potsdam )
x.incomplete[ 2 ] <- NA
test_that( "check.completeness is performing as expected", {
  expect_true( check.completeness(
      x.complete,
      number.of.years = length( unique(
          lubridate::year( x.complete ) ) ) ) )
  expect_false( check.completeness(
      x.incomplete,
      number.of.years = length( unique(
          lubridate::year( x.complete ) ) ) ) )
  expect_equal( length( check.completeness(
      list( x.complete, x.incomplete ),
      number.of.years = length( unique(
          lubridate::year( x.complete ) ) ) ) ),
      1 ) } )

test_that( "mode is working", {
  expect_equal( climex:::mode( temp.potsdam ), 19.7559 - 2.08e-06 )
})

test_that( "anomalies get its job done", {
  expect_true( is.xts( climex::anomalies( temp.potsdam ) ) )
  expect_error( climex::anomalies( as.numeric( temp.potsdam ) ) )
  expect_equal( max( climex::anomalies( temp.potsdam ) ),
               16.32177 + 4.19e-06 )
  expect_equal( min( climex::anomalies( temp.potsdam ) ),
               -19.12258 - 6.45e-07 )
})
