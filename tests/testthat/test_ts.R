library( climex )
context( "Check function is R/ts.R" )

x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )
x.block.fit <- climex::fit.gev( x.block )
x.thresh.fit <- climex::fit.gpd( x.thresh,
                                total.length = length( temp.potsdam ) )

test_that( "aic accepts the right input and produces the right output", {
  expect_equal( climex:::aic( x.block.fit ), 447.2131,
               tolerance = 1e-4 )
  expect_equal( climex:::aic( x.thresh.fit ), 1454.037,
               tolerance = 1e-4 )
  expect_error( climex:::aic( x.block ) )
  expect_error( climex:::aic( as.numeric( x.block ) ) )
})
test_that( "bic accepts the right input and produces the right output", {
  expect_equal( climex:::bic( x.block.fit ), 455.6739,
               tolerance = 1e-4 )
  expect_equal( climex:::bic( x.thresh.fit ), 1461.792,
               tolerance = 1e-4 )
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
x.removed[ 1 ] <- -999
test_that( "remove.incomplete.years get's rid of -999 in time series (like in the DWD for missing data",{
  expect_equal( min( climex::remove.incomplete.years( x.removed ) ),
               -16 )
  expect_equal( length( climex::remove.incomplete.years( x.removed ) ),
               44559 )
})

test_that( "mode is working", {
  expect_equal( climex:::mode( temp.potsdam ), 19.7559,
               tolerance = 1e-4 )
})

test_that( "anomalies get its job done", {
  expect_true( is.xts( climex::anomalies( temp.potsdam ) ) )
  expect_error( climex::anomalies( as.numeric( temp.potsdam ) ) )
  expect_equal( max( climex::anomalies( temp.potsdam ) ), 16.32177,
               tolerance = 1e-4 )
  expect_equal( min( climex::anomalies( temp.potsdam ) ), -19.12258,
               tolerance = 1e-4 )
})
