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
x.block.fit <- fit.gev( x.block )
x.block.fit.list <- fit.gev( x.block.list )
set.seed( 123 )
x.thresh.fit <- fit.gpd( x.thresh, threshold = 29,
                        total.length = length( temp.potsdam ) )
## Deal with the special cases of shape = 0 separately.
set.seed( 123 )
initial.gumbel <- c( likelihood.initials(
    x.block, model = "gev" )[ 1 : 2 ], 0 )
x.gumbel.fit <- fit.gev( x.block, initial = initial.gumbel )
set.seed( 123 )
initial.exp <- c( likelihood.initials(
    x.thresh, model = "gpd" )[ 1 ], 0 )
x.exp.fit <- fit.gpd( x.thresh, initial = initial.exp,
                     threshold = 29,
                     total.length = length( temp.potsdam ) )

test_that( "block works with lists as with single objects", {
  expect_equal( unique( Reduce( c, lapply( x.block.list, length ) ) ),
               124 )
  expect_equal( unique(
      Reduce( c, lapply( block( list( temp.potsdam, temp.potsdam ),
                               block.length = 700 ), length ) ) ), 65 )
  expect_equal( min(
      Reduce( c, block( list( temp.potsdam, temp.potsdam ),
                       extreme.type = "min" ) ) ), -16 )
})
test_that( "block's block length argument works", {
  expect_equal( length( block( temp.potsdam ) ), 124 )
  expect_equal( length( block( temp.potsdam,
                              block.length = 365 ) ), 124 )
  expect_equal( length( block( temp.potsdam,
                              block.length = 128 ) ), 353 )
  expect_equal( length( block( temp.potsdam,
                              block.length = 700 ) ), 65 )
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
                               ), length ) ) ), 426 )
  expect_equal( unique(
      Reduce( c, lapply(
                     threshold( list( temp.potsdam, temp.potsdam ),
                               threshold = -5.5, extreme.type = "min"
                               ), length ) ) ), 144 )
})
test_that( "threshold's results regarding their length", {
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 26 ) ), 426 )
  expect_equal( length(
      threshold( temp.potsdam, threshold = 26,
                extreme.type = "max" ) ), 426 )
  expect_equal( length(
      threshold( temp.potsdam, threshold = -5.5,
                extreme.type = "min" ) ), 144 )
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 28 ) ), 389 )
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 32 ) ), 187 )
  expect_equal( length( threshold( temp.potsdam,
                                  threshold = 26,
                                  decluster = FALSE ) ), 3719 )
  expect_equal( length( threshold( temp.potsdam, threshold = 28,
                                  decluster = FALSE ) ), 2099 )
  expect_equal( length( threshold( temp.potsdam, threshold = 32,
                                  decluster = FALSE ) ), 417 )
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
  expect_equal( extremal.index( temp.potsdam, 29 ),
               c( 0.240429635, 350.000000000, 12.000000000 ) )
  expect_warning( extremal.index( temp.potsdam, 100 ) )
})

test_that( "return.level of fit results and GEV/GP parameters as input", {
  expect_error( return.level( temp.potsdam ) )
  expect_error( return.level( as.numeric( temp.potsdam ) ) )
  expect_equal( as.numeric(
      return.level( fit.gpd( temp.potsdam, thresholding = TRUE,
                            threshold = 24 ),
                   error.estimation = "none",
                   return.period = c( 10, 20, 30, 40, 50 ),
                   )$return.level ),
      c( 37.07028710, 37.78214081, 38.09138386, 38.27349623,
        38.39666952 ) )
  expect_equal( return.level( x.block.fit$par )$return.level,
               15.556303455 )
  expect_equal( return.level( x.gumbel.fit )$return.level,
               17.9901487 )
  expect_equal( return.level( x.gumbel.fit$par )$return.level,
               17.9901487 )
  expect_equal( return.level( x.thresh.fit )$return.level,
               38.19163319 )
  expect_equal( return.level( x.thresh.fit$par, model = "gpd",
                             threshold = 29,
                             thresholded.time.series =
                               x.thresh )$return.level,
               38.19037868 )
  expect_equal( return.level( x.exp.fit )$return.level,
               46.22896898 )
  expect_equal( return.level( x.exp.fit$par, model = "gpd",
                             threshold = 29,
                             thresholded.time.series =
                               x.thresh )$return.level,
               46.21915069 )
  ## The next one uses a more accurate estimate for the probability of
  ## a threshold exceedance.
  expect_equal( return.level( x.thresh.fit$par, model = "gpd",
                             threshold = 29,
                             thresholded.time.series = x.thresh,
                             total.length = length( temp.potsdam)
                             )$return.level, 38.19163319 )
  expect_equal( return.level( x.thresh.fit, model = "gpd"
                             )$return.level, 38.19163319 )
})
test_that( "return.level can take return periods of different length and value", {
  expect_equal( return.level(
      x.block.fit,
      return.period = 100 )$return.level,
      15.556303455 )
  expect_equal( return.level( x.block.fit,
                             return.period = c( 10, 20, 500 )
                             )$return.level,
               c( 14.1600474831, 14.6816437202, 16.1234297804 ) )
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
      15.556303455 )
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
      c( 0.0281913645577, 0.0643674174564, 0.1014328231061 ) )
  expect_equal( as.numeric(
      return.level( x.gumbel.fit,
                   return.period = c( 10, 100, 332 ),
                   error.estimation = "MLE" )$error ),
      c( 0.0786997134, 0.2326657115, 0.3473161352 ) )
  expect_equal( as.numeric(
      return.level( fit.gev( temp.potsdam, blocking = TRUE,
                            extreme.type = "min" ),
                   return.period = c( 10, 20, 30, 40, 50 ),
                   error.estimation = "MLE" )$error ),
      c( 0.1821540699, 0.2597489756, 0.3365221564, 0.4062240396,
        0.4690689464 ) )
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
      c( 0.09898939265, 0.09264044639 ) )
  ## With total.length supplied
  expect_equal( as.numeric(
      return.level( x.thresh.fit, return.period = c( 42, 637 ),
                   error.estimation = "MLE", threshold = 29,
                   )$error ),
      c( 0.09873307104, 0.09266329316 ) )
  ## Exponential
  expect_equal( as.numeric(
      return.level( x.exp.fit, return.period = c( 42, 637 ),
                   error.estimation = "MLE", threshold = 29,
                   )$error ),
      c( 3.731791490, 9.122182124 ) )
})
test_that( "return.level get the error estimation right for MC", {
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( x.block.fit,
                   return.period = c( 10, 100, 332 ),
                   error.estimation = "MC",
                   monte.carlo.sample.size = 10 )$error ),
      c( 0.1947308747, 0.3800561908, 0.4902978754 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gev( temp.potsdam, blocking = TRUE,
                            extreme.type = "min" ),
                   return.period = c( 10, 50 ),
                   monte.carlo.sample.size = 10,
                   error.estimation = "MC" )$error ),
      c( 0.4788046268, 0.7639197874 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( x.thresh.fit, return.period = 42,
                   error.estimation = "MC", threshold = 29,
                   monte.carlo.sample.size = 10 )$error ),
      0.2192616612 )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gpd( temp.potsdam, threshold = -7,
                            thresholding = TRUE,
                            extreme.type = "min" ),
                            error.estimation = "MC",
                            monte.carlo.sample.size = 10 )$error ),
      0.7133332268 )
  expect_warning(
      return.level( x.thresh.fit$par, return.period = 42,
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
      c( 0.1845829480, 0.4417705322, 0.5665245378 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gev( temp.potsdam, blocking = TRUE,
                            extreme.type = "min" ),
                   return.period = c( 10, 50 ),
                   bootstrap.sample.size = 10,
                   error.estimation = "bootstrap" )$error ),
      c( 0.3673362950, 0.5214746534 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( x.thresh.fit, return.period = 42,
                   error.estimation = "bootstrap", threshold = 29,
                   bootstrap.sample.size = 10 )$error ),
      0.4109950145 )
  set.seed( 123 )
  expect_equal( as.numeric(
      return.level( fit.gpd( temp.potsdam, thresholding = TRUE,
                            threshold = -7,
                            extreme.type = "min" ),
                   return.period = c( 10, 50 ),
                   bootstrap.sample.size = 10,
                   error.estimation = "bootstrap" )$error ),
      c( 0.3452772890, 0.5737912895 ) )
})
## End of test_eva.R
