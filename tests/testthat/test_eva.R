library( climex )
context( "Tools used in the extreme value analysis" )
x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29 )
x.block.fit <- fit.gev( x.block )
x.thresh.fit <- fit.gpd( x.thresh, threshold = 29,
                        total.length = length( temp.potsdam ) )

test_that( "block's block length argument works", {
  expect_equal( length( climex::block( temp.potsdam ) ), 124 )
  expect_equal( length( climex::block( temp.potsdam,
                                      block.length = 365 ) ), 124 )
  expect_equal( length( climex::block( temp.potsdam,
                                      block.length = 128 ) ), 353 )
  expect_equal( length( climex::block( temp.potsdam,
                                      block.length = 700 ) ), 65 )
})
test_that( "block's block mode argument works", {
  expect_equal( max( climex::block( temp.potsdam ) ), 38.6 )
  expect_equal( min( climex::block( temp.potsdam ) ), 29.5 )
  expect_equal( max( climex::block( temp.potsdam,
                                   block.mode = "min" ) ), 0 )
  expect_equal( min( climex::block( temp.potsdam,
                                   block.mode = "min" ) ), -16 )
})
test_that( "block's block number argument works", {
  expect_equal( length( climex::block( temp.potsdam,
                                      block.number = 12 ) ), 12 )
  expect_equal( length( climex::block( temp.potsdam,
                                      block.number = 392 ) ), 392 )
})
test_that( "block's output has the right format", {
  expect_equal( class( climex::block( temp.potsdam ) ),
               c( "xts", "zoo" ) )
})
test_that( "block just accepts class 'xts' objects", {
  expect_error( climex::block( as.numeric( temp.potsdam ) ) )
})

test_that( "threshold's results regarding their length", {
  expect_equal( length( climex::threshold( temp.potsdam,
                                          threshold = 26 ) ), 426 )
  expect_equal( length( climex::threshold( temp.potsdam,
                                          threshold = 28 ) ), 389 )
  expect_equal( length( climex::threshold( temp.potsdam,
                                          threshold = 32 ) ), 187 )
  expect_equal( length( climex::threshold( temp.potsdam,
                                          threshold = 26,
                                          decluster = FALSE ) ), 3719 )
  expect_equal( length( climex::threshold( temp.potsdam, threshold = 28,
                                          decluster = FALSE ) ), 2099 )
  expect_equal( length( climex::threshold( temp.potsdam, threshold = 32,
                                          decluster = FALSE ) ), 417 )
})
test_that( "threshold does the thresholding right", {
  expect_equal( min( climex::threshold( temp.potsdam,
                                       threshold = 26 ) ), .1 )
  expect_equal( min( climex::threshold( temp.potsdam, threshold = 26,
                                       decluster = FALSE ) ), .1 )
  expect_equal( max( climex::threshold( temp.potsdam,
                                       threshold = 26 ) ), 12.6 )
  expect_equal( max( climex::threshold( temp.potsdam, threshold = 26,
                                       decluster = FALSE ) ), 12.6 )
})

test_that( "decluster works as expected", {
  expect_equal( as.numeric( climex::decluster( temp.potsdam, 29 ) ),
               climex::decluster( as.numeric( temp.potsdam ), 29 ) )
  expect_warning( climex::decluster( temp.potsdam, 29,
                                    cluster.distance = 3 ) )
})

test_that( "extremal.index calculates correct results and throws warnings", {
  expect_equal( climex:::extremal.index( temp.potsdam, 29 ),
               c( 0.2413838, 350, 12 ), tolerance = 1E-6 )
  expect_warning( climex:::extremal.index( temp.potsdam, 100 ) )
})

test_that( "return.level of fit results and GEV/GP parameters as input", {
  expect_error( climex::return.level( temp.potsdam ) )
  expect_error( climex::return.level( as.numeric( temp.potsdam ) ) )
  expect_equal( climex::return.level( x.block.fit ), 15.55642,
               tolerance = 1E-6 )
  expect_equal( climex::return.level( x.block.fit$par ), 15.55642,
               tolerance = 1E-6 )
  expect_equal( climex::return.level( x.thresh.fit$par, model = "gpd",
                                     threshold = 29,
                                     thresholded.time.series = x.thresh ),
               38.1902, tolerance = 1E-4 )
  expect_equal( climex::return.level( x.thresh.fit, model = "gpd" ),
               38.1902, tolerance = 1E-4 )
})
test_that( "return.level can take return periods of different length and value", {
  expect_equal( climex::return.level( x.block.fit, return.period = 100 ),
               15.55642, tolerance = 1E-6 )
  expect_equal( climex::return.level( x.block.fit,
                                     return.period = c( 10, 20, 500 ) ),
               c( 14.15995, 14.68160, 16.12371 ), tolerance = 1E-6 )
})
test_that( "return.level has the right output", {
  expect_match( class( climex::return.level( x.block.fit ) ), "numeric" )
  expect_match( class( climex::return.level( x.block.fit,
                                            error.estimation = "none" )
                      ), "numeric" )
  expect_match( class( climex::return.level( x.block.fit$par,
                                            error.estimation = "MLE" )
                      ), "numeric" )
  expect_match( class( climex::return.level( x.block.fit,
                                            error.estimation = "MLE" )
                      ), "list" )
  expect_equal( names( climex::return.level( x.block.fit,
                                            error.estimation = "MLE" )
                      ), c( "return.levels", "errors" ) )
})
test_that( "return.level get the error estimation right for MLE", { 
  expect_equal( as.numeric(
      climex::return.level( x.block.fit,
                           return.period = c( 10, 100, 332 ),
                           error.estimation = "MLE" )$errors ),
      c( 0.028197, 0.06443386,  0.1015562 ), tolerance = 1E-6 )
  ## These are absurdly big errors. I already wrote a Github issue
  ## regarding it.
  expect_equal( as.numeric(
      climex::return.level( x.thresh.fit, return.period = c( 42, 637 ),
                           error.estimation = "MLE", threshold = 29,
                           total.length = length( temp.potsdam )
                           )$errors ),
      c( 65.44814, 4.062033 ), tolerance = 1E-3 )
})
test_that( "return.level get the error estimation right for MC", { 
  expect_equal( as.numeric(
      climex::return.level( x.block.fit,
                           return.period = c( 10, 100, 332 ),
                           error.estimation = "MC",
                           monte.carlo.sample.size = 100 )$errors ),
      c( .2, .3, .4 ), tolerance = .15 )
  expect_equal( as.numeric(
      climex::return.level( x.thresh.fit, return.period = 42,
                           error.estimation = "MC", threshold = 29,
                           monte.carlo.sample.size = 100 )$errors ),
      0.3445054, tolerance = .1 )
  expect_warning(
      climex::return.level( x.thresh.fit$par, return.period = 42,
                           error.estimation = "MC", threshold = 29,
                           monte.carlo.sample.size = 10 ) )
})
