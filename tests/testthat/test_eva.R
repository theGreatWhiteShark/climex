library( climex )
context( "Tools used in the extreme value analysis" )
x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29 )
x.block.fit <- fit.gev( x.block )
x.thresh.fit <- fit.gpd( x.thresh, threshold = 29,
                        total.length = length( temp.potsdam ) )
## Deal with the special cases of shape = 0 separately.
initial.gumbel <- c( climex::likelihood.initials(
                                 x.block, model = "gev" )[ 1 : 2 ], 
                    0 )
x.gumbel.fit <- climex::fit.gev( x.block, initial = initial.gumbel )
initial.exp <- c( climex::likelihood.initials(
                              x.thresh, model = "gpd" )[ 1 ], 0 )
x.exp.fit <- climex::fit.gpd( x.thresh, initial = initial.exp,
                             threshold = 29,
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
               c( 0.2413838 - 2.68e-08, 350, 12 ) )
  expect_warning( climex:::extremal.index( temp.potsdam, 100 ) )
})

test_that( "return.level of fit results and GEV/GP parameters as input", {
  expect_error( climex::return.level( temp.potsdam ) )
  expect_error( climex::return.level( as.numeric( temp.potsdam ) ) )
  expect_equal( climex::return.level( x.block.fit )$return.level,
               15.556303455 )
  expect_equal( climex::return.level( x.block.fit$par )$return.level,
               15.556303455 )
  expect_equal( climex::return.level( x.gumbel.fit )$return.level,
               17.9901487 )
  expect_equal( climex::return.level( x.gumbel.fit$par )$return.level,
               17.9901487 )
  expect_equal( climex::return.level( x.thresh.fit )$return.level,
               38.1899770466, tolerance = 5E-4 )
  expect_equal( climex::return.level( x.thresh.fit$par, model = "gpd",
                                     threshold = 29,
                                     thresholded.time.series =
                                       x.thresh )$return.level,
               38.1899770466, tolerance = 5E-4 )
  expect_equal( climex::return.level( x.exp.fit )$return.level,
               46.2283425, tolerance = 5E-4 )
  expect_equal( climex::return.level( x.exp.fit$par, model = "gpd",
                                     threshold = 29,
                                     thresholded.time.series =
                                       x.thresh )$return.level,
               46.2283425, tolerance = 5E-4 )
  ## The next one uses a more accurate estimate for the probability of
  ## a threshold exceedance.
  expect_equal( climex::return.level(
                            x.thresh.fit$par, model = "gpd",
                            threshold = 29,
                            thresholded.time.series = x.thresh,
                            total.length = length( temp.potsdam)
                        )$return.level,
               38.1912316541, tolerance = 5E-4 )
  expect_equal( climex::return.level( x.thresh.fit, model = "gpd"
                                     )$return.level,
               38.1912316541, tolerance = 5E-4 )
})
test_that( "return.level can take return periods of different length and value", {
  expect_equal( climex::return.level(
                            x.block.fit,
                            return.period = 100 )$return.level,
               15.556303455 )
  expect_equal( climex::return.level( x.block.fit,
                                     return.period = c( 10, 20, 500 )
                                     )$return.level,
               c( 14.1600474831, 14.6816437202, 16.1234297804 ) )
})
test_that( "return.level has the right output", {
  expect_match( class( climex::return.level( x.block.fit ) ), "list" )
  expect_match( class( climex::return.level( x.block.fit,
                                            error.estimation = "MLE" )
                      ), "list" )
  expect_equal( names( climex::return.level( x.block.fit,
                                            error.estimation = "MLE" )
                      ), c( "return.level", "error" ) )
})
test_that( "return.level get GEV error estimation right for MLE", {
  expect_equal( as.numeric(
      climex::return.level( x.block.fit,
                           return.period = c( 10, 100, 332 ),
                           error.estimation = "MLE" )$error ),
      c( 0.0281913645577, 0.0643674174564, 0.1014328231061 ) )
  expect_equal( as.numeric(
      climex::return.level( x.gumbel.fit,
                           return.period = c( 10, 100, 332 ),
                           error.estimation = "MLE" )$error ),
      c( 0.0786997134, 0.2326657115, 0.3473161352 ) )
})
## A dummy object without the total.length argument
x.thresh.fit.no.total.length <- x.thresh.fit
x.thresh.fit.no.total.length$control$total.length <- NULL
test_that( "return.level get GP error estimation right for MLE", {
  ## Without the total.length supplied (has to be estimated)
  expect_equal( as.numeric(
      climex::return.level( x.thresh.fit.no.total.length,
                           return.period = c( 42, 637 ),
                           error.estimation = "MLE", threshold = 29,
                           )$error ),
      c( 0.09898939, 0.09264045 ),
      tolerance = 1E-6 )
  ## With total.length supplied
  expect_equal( as.numeric(
      climex::return.level( x.thresh.fit, return.period = c( 42, 637 ),
                           error.estimation = "MLE", threshold = 29,
                           )$error ),
      c( 0.09873307, 0.09266329 ),
      tolerance = 1E-6 )
  ## Exponential
  expect_equal( as.numeric(
      climex::return.level( x.exp.fit, return.period = c( 42, 637 ),
                           error.estimation = "MLE", threshold = 29,
                           )$error ),
      c( 3.731791, 9.122182 ),
      tolerance = 1E-6 )
})
test_that( "return.level get the error estimation right for MC", { 
  expect_equal( as.numeric(
      climex::return.level( x.block.fit,
                           return.period = c( 10, 100, 332 ),
                           error.estimation = "MC",
                           monte.carlo.sample.size = 100 )$error ),
      c( .2, .3, .4 ), tolerance = .15 )
  expect_equal( as.numeric(
      climex::return.level( x.thresh.fit, return.period = 42,
                           error.estimation = "MC", threshold = 29,
                           monte.carlo.sample.size = 10 )$error ),
      0.3445054, tolerance = .15 )
  expect_warning(
      climex::return.level( x.thresh.fit$par, return.period = 42,
                           error.estimation = "MC", threshold = 29,
                           monte.carlo.sample.size = 10 ) )
})
test_that( "return.level get the error estimation right for bootstrap", { 
  expect_equal( as.numeric(
      climex::return.level( x.block.fit,
                           return.period = c( 10, 100, 332 ),
                           error.estimation = "bootstrap",
                           bootstrap.sample.size = 100 )$error ),
      c( 0.186546387962, 0.421330036318, 0.543310951990 ), tolerance = .15 )
  expect_equal( as.numeric(
      climex::return.level( x.thresh.fit, return.period = 42,
                           error.estimation = "bootstrap", threshold = 29,
                           bootstrap.sample.size = 10 )$error ),
      0.276584961888, tolerance = .15 )
})
