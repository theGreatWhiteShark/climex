### test_eva.R - Tests of the functions dedicated to the extreme value
###   analysis 
library( climex )
context( "Tools used in the extreme value analysis" )
x.block <- block( anomalies( temp.potsdam ) )
x.block.list <- block( list( anomalies( temp.potsdam ),
                            anomalies( temp.potsdam ) ) )
x.thresh <- threshold( temp.potsdam, threshold = 29 )
x.thresh.list <- threshold( list( temp.potsdam, temp.potsdam ),
                           threshold = 29 )
set.seed( 123 )
x.block.fit <- fit.gev( x.block, silent = TRUE )
x.block.fit.list <- fit.gev( x.block.list, silent = TRUE )
set.seed( 123 )
x.thresh.fit <- fit.gpd( x.thresh, threshold = 29,
                        total.length = length( temp.potsdam ),
                        silent = TRUE )
## Deal with the special cases of shape = 0 separately.
set.seed( 123 )
initial.gumbel <- c( likelihood.initials(
    x.block, model = "gev" )[ 1 : 2 ], 0 )
x.gumbel.fit <- fit.gev( x.block, initial = initial.gumbel,
                        silent = TRUE )
set.seed( 123 )
initial.exp <- c( likelihood.initials(
    x.thresh, model = "gpd" )[ 1 ], 0 )
x.exp.fit <- fit.gpd( x.thresh, initial = initial.exp,
                     threshold = 29,
                     total.length = length( temp.potsdam ),
                     silent = TRUE )

test_that( "block works with lists as with single objects", {
  expect_equal( unique( Reduce( c, lapply( x.block.list, length ) ) ),
               126 )
  expect_equal( unique(
      Reduce( c, lapply( block( list( temp.potsdam, temp.potsdam ),
                               block.length = 700 ), length ) ) ), 66 )
  expect_equal( min(
      Reduce( c, block( list( temp.potsdam, temp.potsdam ),
                       extreme.type = "min" ) ) ), -16 )
})
test_that( "block's block length argument works", {
  expect_equal( length( block( temp.potsdam ) ), 126 )
  expect_equal( length( block( temp.potsdam,
                              block.length = 365 ) ), 126 )
  expect_equal( length( block( temp.potsdam,
                              block.length = 128 ) ), 360 )
  expect_equal( length( block( temp.potsdam,
                              block.length = 700 ) ), 66 )
  expect_true( all(
      Reduce( c, lapply( list( temp.potsdam, temp.potsdam ),
                        function( tt ) length( block( tt ) ) ) ) ),
      124 )
})
test_that( "block's block mode argument works", {
  expect_equal( max( block( temp.potsdam ) ), 38.6 )
  expect_equal( min( block( temp.potsdam ) ), 29.5 )
  expect_equal( max( block( temp.potsdam,
                           extreme.type = "min" ) ), 0 )
  expect_equal( min( block( temp.potsdam,
                           extreme.type = "min" ) ), -16 )
})
test_that( "block's block number argument works", {
  expect_equal( length( block( temp.potsdam,
                              block.number = 12 ) ), 12 )
  expect_equal( length( block( temp.potsdam,
                              block.number = 392 ) ), 392 )
})
test_that( "block's output has the right format", {
  expect_equal( class( block( temp.potsdam ) ),
               c( "xts", "zoo" ) )
  expect_equal( class(
      block( list( temp.potsdam, temp.potsdam ) ) ),
      "list" )
})
test_that( "block just accepts class 'xts' objects", {
  expect_error( block( as.numeric( temp.potsdam ) ) )
  expect_error( block(
      list( as.numeric( temp.potsdam ), temp.potsdam ) ) )
})

test_that( "threshold works with lists as with single objects", {
  expect_equal( unique(
      Reduce( c, lapply(
                     threshold( list( temp.potsdam, temp.potsdam ),
                               threshold = 26, extreme.type = "max"
                               ), length ) ) ), 429 )
  expect_equal( unique(
      Reduce( c, lapply(
                     threshold( list( temp.potsdam, temp.potsdam ),
                               threshold = -5.5, extreme.type = "min"
                               ), length ) ) ), 144 )
})
test_that( "threshold's results regarding their length", {
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 26 ) ), 429 )
  expect_equal( length(
      threshold( temp.potsdam, threshold = 26,
                extreme.type = "max" ) ), 429 )
  expect_equal( length(
      threshold( temp.potsdam, threshold = -5.5,
                extreme.type = "min" ) ), 144 )
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 28 ) ), 395 )
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 32 ) ), 186 )
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 26,
                                  decluster = FALSE ) ), 3851 )
  expect_equal( length( threshold( temp.potsdam, threshold = 28,
                                  decluster = FALSE ) ), 2180 )
  expect_equal( length( threshold( temp.potsdam, threshold = 32,
                                  decluster = FALSE ) ), 441 )
})
test_that( "threshold does the thresholding right", {
  expect_equal( min( threshold( temp.potsdam,
                               threshold = 26 ) ), .1 )
  expect_equal( min( threshold( temp.potsdam, threshold = 26,
                               decluster = FALSE ) ), .1 )
  expect_equal( min( threshold( temp.potsdam,
                               threshold = -7,
                               extreme.type = "min" ) ), -9 )
  expect_equal( min( threshold( temp.potsdam, threshold = -7,
                               decluster = FALSE,
                               extreme.type = "min" ) ), -9 )
  expect_equal( max( threshold( temp.potsdam,
                               threshold = 26 ) ), 12.6 )
  expect_equal( max( threshold( temp.potsdam, threshold = 26,
                               decluster = FALSE ) ), 12.6 )
  expect_equal( max( threshold( temp.potsdam,
                               threshold = -7,
                               extreme.type = "min") ), -.1 )
  expect_equal( max( threshold( temp.potsdam, threshold = -9,
                               decluster = FALSE,
                               extreme.type = "min" ) ), -.1 )
  expect_true( is.xts( threshold( temp.potsdam,
                                  threshold = 26 ) ) )
  expect_true( is.xts( threshold( temp.potsdam,
                                 threshold = 26,
                                 extreme.type = "min" ) ) )
  expect_true( is.list( threshold(
      list( temp.potsdam, temp.potsdam ), threshold = 26 ) ) )
  expect_true( is.list( threshold(
      list( temp.potsdam, temp.potsdam ), threshold = 26,
      extreme.type = "min" ) ) )
})

test_that( "decluster works as expected", {
  expect_equal( as.numeric( decluster( temp.potsdam, 29 ) ),
               decluster( as.numeric( temp.potsdam ), 29 ) )
  expect_warning( decluster( temp.potsdam, 29,
                            cluster.distance = 3 ) )
  expect_true( is.numeric( decluster( temp.potsdam, 29 ) ) )
})
test_that( "decluster works with both lists and single objects", {
  expect_true( is.list( decluster(
      list( temp.potsdam, temp.potsdam ), 29 ) ) )
})

test_that( "extremal.index calculates correct results and throws warnings", {
  expect_equal( climex:::extremal.index( temp.potsdam, 29 ),
               c( 0.235840685, 357.000000000, 12.000000000 ) )
  expect_warning( climex:::extremal.index( temp.potsdam, 100 ) )
})
test_that( "the errors and warnings of return.level do work", {
  expect_error( return.level( temp.potsdam ) )
  expect_error( return.level( as.numeric( temp.potsdam ) ) )
  expect_warning( return.level( x.block.fit, model = "gpd" ) )
  expect_warning( return.level( x.thresh.fit, model = "gev" ) )
  expect_warning( return.level( x.block.fit, extreme.type = "min" ) )
  expect_warning( return.level( x.thresh.fit, extreme.type = "min" ) )
  expect_warning( return.level( x.thresh.fit, silent = FALSE,
                               total.length = 100 ) )
  expect_warning( return.level( x.thresh.fit,
                               return.period = .0001 ) )
  expect_error( return.level( as.numeric( x.block.fit$par ),
                             model = "gpd" ) )
  expect_error( return.level( as.numeric( x.thresh.fit$par ),
                             model = "gev" ) )
  expect_warning( return.level( as.numeric( x.thresh.fit$par ),
                               model = "gpd",
                               threshold = x.thresh.fit$threshold ) )
  expect_warning( return.level( as.numeric( x.thresh.fit$par ),
                               model = "gpd",
                               thresholded.time.series = temp.potsdam ) )
  expect_warning( return.level( as.numeric( x.thresh.fit$par ),
                               model = "gpd",
                               threshold = x.thresh.fit$threshold,
                               thresholded.time.series = temp.potsdam,
                               error.estimation = "MC") )
})
  
test_that( "return.level of fit results and GEV/GP parameters as input", {
  expect_equal( as.numeric(
      return.level( fit.gpd( temp.potsdam, thresholding = TRUE,
                            threshold = 24, silent = TRUE ),
                   error.estimation = "none",
                   return.period = c( 10, 20, 30, 40, 50 ),
                   )$return.level ),
      c( 37.0827710, 37.7935560, 38.1018892, 38.2833029, 38.4059229 ) )
  expect_equal( return.level( x.block.fit$par )$return.level,
               15.540398 )
  expect_equal( return.level( x.gumbel.fit )$return.level,
               18.0413413 )
  expect_equal( return.level( x.gumbel.fit$par )$return.level,
               18.0413413 )
  expect_equal( return.level( x.thresh.fit )$return.level,
               38.2157282 )
  expect_equal( return.level( x.thresh.fit$par, model = "gpd",
                             threshold = 29,
                             thresholded.time.series =
                               x.thresh )$return.level,
               38.2153566 )
  expect_equal( return.level( x.exp.fit )$return.level,
               46.3207659 )
  expect_equal( return.level( x.exp.fit$par, model = "gpd",
                             threshold = 29,
                             thresholded.time.series =
                               x.thresh )$return.level,
               46.3178091 )
  ## The next one uses a more accurate estimate for the probability of
  ## a threshold exceedance.
  expect_equal( return.level( x.thresh.fit$par, model = "gpd",
                             threshold = 29,
                             thresholded.time.series = x.thresh,
                             total.length = length( temp.potsdam)
                             )$return.level, 38.2157282 )
  expect_equal( return.level( x.thresh.fit, model = "gpd"
                             )$return.level, 38.2157282 )
})
test_that( "return.level can take return periods of different length and value", {
  expect_equal( return.level(
      x.block.fit,
      return.period = 100 )$return.level,
      15.540398 )
  expect_equal( return.level( x.block.fit,
                             return.period = c( 10, 20, 500 )
                             )$return.level,
               c( 14.1492757, 14.6703242, 16.1011481 ) )
})
test_that( "return.level has the right output", {
  expect_match( class( return.level( x.block.fit ) ), "list" )
  expect_match( class( return.level( x.block.fit,
                                    error.estimation = "MLE" )
                      ), "list" )
  expect_equal( names( return.level( x.block.fit,
                                    error.estimation = "MLE" )
                      ), c( "return.level", "error" ) )
})
test_that( "return.level works with both lists and single objects", {
  expect_equal( unique(
      Reduce( c, lapply( return.level( x.block.fit.list,
                                      return.period = 100 ),
                        function( rr ) rr$return.level ) ) ),
      15.540398 )
  expect_true( is.list(
      return.level( list( x.block.fit, x.block.fit ) ) ) )
  expect_true( is.list(
      return.level( list( x.block.fit, x.block.fit ) )[[ 1 ]] ) )
  expect_equal(
      unique( Reduce( c,
                     lapply(
                         return.level( list( x.block.fit,
                                            x.block.fit ),
                                      error.estimation = "MLE" ),
                         function( rr ) names( rr ) ) ) ),
      c( "return.level", "error" ) )
})
  
test_that( "return.level get GEV error estimation right for MLE", {
  expect_equal( as.numeric(
      return.level( x.block.fit,
                   return.period = c( 10, 100, 332 ),
                   error.estimation = "MLE" )$error ),
      c( 0.0276696300, 0.0610141771, 0.0950770096 ) )
  expect_equal( as.numeric(
      return.level( x.gumbel.fit,
                   return.period = c( 10, 100, 332 ),
                   error.estimation = "MLE" )$error ),
      c( 0.0789504091, 0.2330315156, 0.3477442593 ) )
  expect_equal( as.numeric(
      return.level( fit.gev( temp.potsdam, blocking = TRUE,
                            extreme.type = "min", silent = TRUE ),
                   return.period = c( 10, 20, 30, 40, 50 ),
                   error.estimation = "MLE" )$error ),
      c( 0.185857567, 0.270873933, 0.354397394, 0.430232165,
        0.498688690 ) )
})
## A dummy object without the total.length argument
x.thresh.fit.no.total.length <- x.thresh.fit
x.thresh.fit.no.total.length$control$total.length <- NULL
test_that( "return.level get GP error estimation right for MLE", {
  ## Without the total.length supplied (has to be estimated)
  expect_equal( as.numeric(
      return.level( x.thresh.fit.no.total.length,
                   return.period = c( 42, 637 ),
                   error.estimation = "MLE", threshold = 29,
                   )$error ),
      c( 0.0985615149, 0.0939199375 ) )
  ## With total.length supplied
  expect_equal( as.numeric(
      return.level( x.thresh.fit, return.period = c( 42, 637 ),
                   error.estimation = "MLE", threshold = 29,
                   )$error ),
      c( 0.0984851419, 0.0939272406 ) )
  ## Exponential
  expect_equal( as.numeric(
      return.level( x.exp.fit, return.period = c( 42, 637 ),
                   error.estimation = "MLE", threshold = 29,
                   )$error ),
      c( 3.71517331, 9.08015755 ) )
})
test_that( "return.level get the error estimation right for MC", {
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( x.block.fit,
                   return.period = c( 10, 100, 332 ),
                   error.estimation = "MC",
                   monte.carlo.sample.size = 10 )$error ),
      c( 0.196011633, 0.391741815, 0.494852114 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gev( temp.potsdam, blocking = TRUE,
                            extreme.type = "min", silent = TRUE ),
                   return.period = c( 10, 50 ),
                   monte.carlo.sample.size = 10,
                   error.estimation = "MC" )$error ),
      c( 0.489863687, 0.824245690 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( x.thresh.fit, return.period = 42,
                   error.estimation = "MC", threshold = 29,
                   monte.carlo.sample.size = 10 )$error ),
      0.168506055 )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gpd( temp.potsdam, threshold = -7,
                            thresholding = TRUE, silent = TRUE,
                            extreme.type = "min" ),
                   error.estimation = "MC",
                   monte.carlo.sample.size = 10 )$error ),
      0.713333227 )
  expect_warning(
      return.level( x.thresh.fit$par, model = "gpd",
                   return.period = 42,
                   error.estimation = "MC", threshold = 29,
                   monte.carlo.sample.size = 10 ) )
})
test_that( "return.level get the error estimation right for bootstrap", { 
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( x.block.fit,
                   return.period = c( 10, 100, 332 ),
                   error.estimation = "bootstrap",
                   bootstrap.sample.size = 10 )$error ),
      c( 0.104627306, 0.273167480, 0.358395056 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gev( temp.potsdam, blocking = TRUE,
                            extreme.type = "min", silent = TRUE ),
                   return.period = c( 10, 50 ),
                   bootstrap.sample.size = 10,
                   error.estimation = "bootstrap" )$error ),
      c( 0.304337871, 0.390096183 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( x.thresh.fit, return.period = 42,
                   error.estimation = "bootstrap", threshold = 29,
                   bootstrap.sample.size = 10 )$error ),
      0.185924244 )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gpd( temp.potsdam, thresholding = TRUE,
                            threshold = -7, silent = TRUE,
                            extreme.type = "min" ),
                   return.period = c( 10, 50 ),
                   bootstrap.sample.size = 10,
                   error.estimation = "bootstrap" )$error ),
      c( 0.345277289, 0.573791290 ) )
})
test_that( "return.level.climex.fit.gev yield equivalent results for minima and maxima", {
  expect_equal(
      climex::return.level(
                  climex::fit.gev( x.block, error.estimation = "none",
                                  extreme.type = "max",
                                  silent = TRUE ),
                  return.period = c( 20, 50, 200 ) )$return.level,
      climex::return.level( 
                  climex::fit.gev( -1* x.block,
                                  error.estimation = "none",
                                  extreme.type = "min",
                                  silent = TRUE ),
                  return.period = c( 20, 50, 200 ) )$return.level *
                                                    c( -1, -1, -1 ) )
  expect_equal(
      climex::return.level(
                  climex::fit.gev(
                              x.block, error.estimation = "none",
                              extreme.type = "max", silent = TRUE ),
                  error.estimation = "MLE",
                  return.period = c( 50, 200 ) )$error,
      climex::return.level(
                  climex::fit.gev(
                              -1* x.block, error.estimation = "none",
                              extreme.type = "min", silent = TRUE ),
                  error.estimation = "MLE",
                  return.period = c( 50, 200 ) )$error )
  expect_equal(
  { set.seed( 123 )
    as.numeric(
        climex::return.level(
                    climex::fit.gev( x.block, error.estimation = "none",
                                    extreme.type = "max", silent = TRUE ),
                    return.period = c( 50, 200 ),
                    error.estimation = "MC",
                    monte.carlo.sample.size = 10 )$error ) },
  { set.seed( 123 )
    as.numeric(
        climex::return.level(
                    climex::fit.gev( -1* x.block, error.estimation = "none",
                                    extreme.type = "min", silent = TRUE ),
                    return.period = c( 50, 200 ),
                    error.estimation = "MC",
                    monte.carlo.sample.size = 10 )$error ) } )
  expect_equal(
  { set.seed( 123 )
    climex::return.level(
                climex::fit.gev(
                            x.block, error.estimation = "none",
                            extreme.type = "max", silent = TRUE ),
                error.estimation = "bootstrap",
                return.period = c( 50, 200 ),
                bootstrap.sample.size = 10 )$error },
  { set.seed( 123 )
    climex::return.level(
                climex::fit.gev(
                            -1* x.block, error.estimation = "none",
                            extreme.type = "min", silent = TRUE ),
                error.estimation = "bootstrap",
                return.period = c( 50, 200 ),
                bootstrap.sample.size = 10)$error } )
})

test_that( "return.level.climex.fit.gpd yield equivalent results for minima and maxima", {
  expect_equal(
      climex::return.level(
                  climex::fit.gpd( x.thresh, error.estimation = "none",
                                  extreme.type = "max", silent = TRUE ),
                  return.period = c( 20, 50, 200 ),
                  threshold = 29 )$return.level,
      climex::return.level( 
                  climex::fit.gpd( -1* x.thresh,
                                  error.estimation = "none",
                                  extreme.type = "min", silent = TRUE ),
                  return.period = c( 20, 50, 200 ),
                  threshold = -29 )$return.level *
                                   c( -1, -1, -1 ) )
  expect_equal(
      climex::return.level(
                  climex::fit.gpd(
                              x.thresh, error.estimation = "none",
                              extreme.type = "max", silent = TRUE ),
                  error.estimation = "MLE",
                  return.period = c( 50, 200 ),
                  threshold = 29 )$error,
      climex::return.level(
                  climex::fit.gpd(
                              -1* x.thresh, error.estimation = "none",
                              extreme.type = "min", silent = TRUE ),
                  error.estimation = "MLE",
                  return.period = c( 50, 200 ),
                  threshold = -29 )$error )
  expect_equal(
  { set.seed( 123 )
    as.numeric(
        climex::return.level(
                    climex::fit.gpd( x.thresh, error.estimation = "none",
                                    extreme.type = "max", silent = TRUE ),
                    return.period = c( 50, 200 ),
                    error.estimation = "MC",
                    monte.carlo.sample.size = 10,
                    threshold = 29 )$error ) },
  { set.seed( 123 )
    as.numeric(
        climex::return.level(
                    climex::fit.gpd( -1* x.thresh, error.estimation = "none",
                                    extreme.type = "min", silent = TRUE ),
                    return.period = c( 50, 200 ),
                    error.estimation = "MC",
                    monte.carlo.sample.size = 10,
                    threshold = -29 )$error ) } )
  expect_equal(
  { set.seed( 123 )
    climex::return.level(
                climex::fit.gpd(
                            x.thresh, error.estimation = "none",
                            extreme.type = "max", silent = TRUE ),
                error.estimation = "bootstrap",
                return.period = c( 50, 200 ),
                bootstrap.sample.size = 10,
                threshold = 29 )$error },
  { set.seed( 123 )
    climex::return.level(
                climex::fit.gpd(
                            -1* x.thresh, error.estimation = "none",
                            extreme.type = "min", silent = TRUE ),
                error.estimation = "bootstrap",
                return.period = c( 50, 200 ),
                bootstrap.sample.size = 10,
                threshold = -29 )$error } )
})


test_that( "return.level.numeric yield equivalent results for minima and maxima", {
  expect_equal(
      climex::return.level( as.numeric( x.block.fit$par ),
                           extreme.type = "max", model = "gev",
                           return.period = c( 20, 50, 200 )
                           )$return.level,
      climex::return.level( as.numeric( x.block.fit$par* c( -1, 1, 1 ) ),
                           extreme.type = "min", model = "gev",
                           return.period = c( 20, 50, 200 )
                           )$return.level * c( -1, -1, -1 ) )
  expect_equal(
      climex::return.level( as.numeric( x.thresh.fit$par ),
                           extreme.type = "max", model = "gpd",
                           return.period = c( 20, 50, 200 ),
                           threshold = 29,
                           thresholded.time.series = temp.potsdam
                           )$return.level,
      climex::return.level( as.numeric( x.thresh.fit$par ),
                           extreme.type = "min", model = "gpd",
                           return.period = c( 20, 50, 200 ),
                           threshold = -29,
                           thresholded.time.series = temp.potsdam
                           )$return.level * c( -1, -1, -1 ) )
})
## End of test_eva.R
