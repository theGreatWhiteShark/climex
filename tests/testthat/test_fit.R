### test_fit.R - Test checking the fitting procedure.
library( climex )
context( "Testing the fitting procedure" )
x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )
set.seed( 123 )
x.block.fit <- climex::fit.gev( x.block, silent = TRUE )
set.seed( 123 )
x.thresh.fit <- climex::fit.gpd( x.thresh, silent = TRUE,
                                total.length = length( temp.potsdam ))
## Deal with the special cases of shape = 0 separately.
set.seed( 123 )
initial.gumbel <-
  c( climex::likelihood.initials(
                 x.block, model = "gev" )[ 1 : 2 ], 0 )
x.gumbel.fit <- climex::fit.gev( x.block, initial = initial.gumbel,
                                silent = TRUE )
set.seed( 123 )
initial.exp <- c( climex::likelihood.initials(
                              x.thresh, model = "gpd" )[ 1 ], 0 )
x.exp.fit <- climex::fit.gpd( x.thresh, initial = initial.exp,
                             threshold = 29, silent = TRUE,
                             total.length = length( temp.potsdam ) )

test_that( "fit.gev has the correct output format", {
  expect_equal( class( x.block.fit ),
               c( "climex.fit.gev", "list" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gev(
                             list( x.block, x.block ),
                             error.estimation = "none",
                             silent = TRUE ),
                             class ) ) ),
      c( "climex.fit.gev", "list" ) )
  expect_equal( names( x.block.fit ),
               c( "par", "value", "gradient", "counts",
                 "outer.iterations", "control", "se", "x",
                 "return.level" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gev(
                             list( x.block, x.block ),
                             error.estimation = "none",
                             silent = TRUE ),
                             names ) ) ),
      c( "par", "value", "gradient", "counts",
        "outer.iterations", "control", "se", "x",
        "return.level" ) )
  expect_equal( class( x.block.fit$par ), "numeric" )
  expect_equal( length( x.block.fit$par ), 3 )
  expect_equal( names( x.block.fit$par ),
               c( "location", "scale", "shape" ) )
  expect_true( all( x.block == x.block.fit$x ) )
  expect_equal( class( x.block.fit$return.level ), "numeric" )
  expect_equal( length( x.block.fit$return.level ), 1 )
})

test_that( "fit.gev's actual fitting works", {
  expect_equal( as.numeric( x.block.fit$par ),
               c( 11.704448187, 1.441659336, -0.264520978 ) )
  expect_equal( as.numeric( x.gumbel.fit$par ),
               c( 11.50406034, 1.42110193, 0.00000000 ) )
})

test_that( "The numerical fallback of fit.gev works", {
  expect_equal( as.numeric( fit.gev( as.numeric( x.block ),
                                    error.estimation = "none",
                                    silent = TRUE )$par ),
               c( 11.704448187, 1.441659336, -0.264520978 ) )
})

test_that( "fit.gev can handle the blocking", {
  expect_equal( as.numeric(
      fit.gev( climex::anomalies( temp.potsdam ),
              blocking = TRUE, silent = TRUE )$par ),
      c( 11.704448187, 1.441659336, -0.264520978 ) )
})

test_that( "fit.gev display messages and warnings", {
  expect_output( fit.gev( x.block, error.estimation = "none" ) )
  expect_output( fit.gev( x.block, error.estimation = "none",
                         debug = TRUE, silent = TRUE ) )
  expect_output( fit.gev( x.block, error.estimation = "none",
                         silent = FALSE ) )
})

test_that( "fit.gev works for the block minima as well", {
  expect_equal( as.numeric(
      fit.gev( climex::anomalies( temp.potsdam ),
              blocking = TRUE, silent = TRUE,
              extreme.type = "min" )$par ),
      c( -10.67216365907, 1.86952045484, 0.00243935977 ) )
  expect_equal( as.numeric(
      fit.gev( climex::anomalies( temp.potsdam ),
              blocking = TRUE, silent = TRUE,
              return.period = c( 20, 50, 100, 250 ),
              extreme.type = "min" )$return.level ),
      c( -16.2451693, -18.0017448, -19.3206702, -21.0606808 ) )
})

test_that( "the handing over of initial parameters works", {
  expect_equal( as.numeric(
      climex::fit.gev( x.block, silent = TRUE,
                      initial = climex::likelihood.initials( x.block )
                      )$par ),
      c( 11.704448187, 1.441659336, -0.264520978 ) )
  expect_equal( as.numeric( climex::fit.gev( x.block, silent = TRUE,
                                            initial = c( 12, 1.8, -.2 )
                                            )$par ),
               c( 11.704470820, 1.441601297, -0.264506354 ) )
  expect_equal( as.numeric( climex::fit.gev( x.block, silent = TRUE,
                                            initial = c( 12, 1.8, 0 )
                                            )$par ),
               c( 11.50411720, 1.42129927, 0.00000000 ) )
})

test_that( "fit.gev's error estimation works", {
  expect_equal( names( x.block.fit$se ),
               c( "location", "scale", "shape", "100.rlevel" ) )
  expect_equal(
      as.numeric( climex::fit.gev( x.block, silent = TRUE,
                                  error.estimation = "MLE" )$se ),
      c( 0.1389250092, 0.0960013090, 0.0427509297, 0.0610141771 ) )
  expect_equal(
      as.numeric( climex::fit.gev( x.block, silent = TRUE,
                                  error.estimation = "MLE",
                                  return.period = c( 100, 200 ),
                                  initial = initial.gumbel )$se ),
      c( 0.1343408287, 0.0914944818, 0.0000000000, 0.2330315156,
        0.2963832166 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( x.block, silent = TRUE,
                                  error.estimation = "MC",
                                  return.period = c( 100, 200 ),
                                  monte.carlo.sample.size = 10 )$se ),
      c( 0.1078120133, 0.0734621561, 0.0637794403, 0.3917418149,
        0.4523392157 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( x.block, silent = TRUE,
                                  error.estimation = "bootstrap",
                                  return.period = c( 100, 200 ),
                                  bootstrap.sample.size = 10 )$se ),
      c( 0.13234313, 0.09495922, 0.09392357, 0.40032124, 0.47744557 ),
      tolerance = 1e-6 )
  expect_equal(
      as.numeric( climex::fit.gev( temp.potsdam, blocking = TRUE,
                                  error.estimation = "MLE",
                                  return.period = c( 100, 200 ),
                                  extreme.type = "min", silent = TRUE,
                                  monte.carlo.sample.size = 100 )$se ),
      c( 0.3605547421, 0.2596083986, 0.0680054616, 0.7638714620,
        1.1019840854 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( temp.potsdam, blocking = TRUE,
                                  error.estimation = "MC",
                                  return.period = c( 100, 200 ),
                                  extreme.type = "min", silent = TRUE,
                                  monte.carlo.sample.size = 10 )$se ),
      c( 0.2695013604, 0.1834332594, 0.0637574263, 0.8475974044,
        1.0019305295 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( temp.potsdam, blocking = TRUE,
                                  error.estimation = "bootstrap",
                                  extreme.type = "min", silent = TRUE,
                                  return.period = c( 100, 200 ),
                                  bootstrap.sample.size = 10 )$se ),
      c( 0.40302692, 0.15256461, 0.04739434, 0.41928855, 0.53768576 ),
      tolerance = 1e-6 )
})

test_that( "fit.gev yield equivalent results for minima and maxima", {
  expect_equal(
      climex::fit.gev( x.block, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "max", silent = TRUE )$par,
      climex::fit.gev( -1* x.block, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "min", silent = TRUE )$par *
                                            c( -1, 1, 1 ) )
  expect_equal(
      climex::fit.gev( x.block, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "max",
                      silent = TRUE )$return.level,
      climex::fit.gev( -1* x.block, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "min",
                      silent = TRUE )$return.level *
                                            c( -1, -1, -1 ) )
  expect_equal(
      climex::fit.gev( x.block, error.estimation = "MLE",
                      return.period = c( 50, 200 ), silent = TRUE,
                      extreme.type = "max" )$se,
      climex::fit.gev( -1* x.block, error.estimation = "MLE",
                      return.period = c( 50, 200 ), silent = TRUE,
                      extreme.type = "min" )$se )
  ## In case of the minima the Monte Carlo method draws sample points
  ## for a different set of parameters: -1* location, scale, and
  ## shape. Therefore, the results do not perfectly agree regardless
  ## of the seeding.
  expect_equal(
  { set.seed( 123 )
    as.numeric(
        climex::fit.gev( x.block, error.estimation = "MC",
                        return.period = c( 50, 200 ),
                        monte.carlo.sample.size = 10,
                        extreme.type = "max",
                        silent = TRUE )$se ) },
  c( 0.1078120133, 0.0734621561, 0.0637794403, 0.3297606383,
    0.4523392157 ) )
  expect_equal(
  { set.seed( 123 )
    as.numeric(
        climex::fit.gev( -1* x.block, error.estimation = "MC",
                        return.period = c( 50, 200 ),
                        monte.carlo.sample.size = 10,
                        extreme.type = "min",
                        silent = TRUE )$se ) },
  c( 0.1078818193, 0.0734211095, 0.0637600664, 0.2763406531,
    0.4008931047 ) )
  expect_equal(
  { set.seed( 123 )
    as.numeric(
        climex::fit.gev( x.block, error.estimation = "bootstrap",
                        return.period = c( 50, 200 ),
                        bootstrap.sample.size = 10,
                        extreme.type = "max",
                        silent = TRUE )$se ) },
  { set.seed( 123 )
    as.numeric(
        climex::fit.gev( -1* x.block, error.estimation = "bootstrap",
                        return.period = c( 50, 200 ),
                        bootstrap.sample.size = 10,
                        extreme.type = "min",
                        silent = TRUE )$se ) })
})

test_that( "fit.gpd has the correct output format", {
  expect_equal( class( x.thresh.fit ), c( "climex.fit.gpd", "list" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gpd(
                             list( x.thresh, x.thresh ),
                             error.estimation = "none",
                             silent = TRUE ),
                             class ) ) ),
      c( "climex.fit.gpd", "list" ) )
  expect_equal( names( x.thresh.fit ),
               c( "par", "value", "gradient", "counts",
                 "outer.iterations", "x", "threshold", "control", "se",
                 "return.level" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gpd(
                             list( x.thresh, x.thresh ),
                             error.estimation = "none",
                             silent = TRUE ),
                             names ) ) ),
      c( "par", "value", "gradient", "counts",
        "outer.iterations", "x", "threshold", "control", "se",
        "return.level" ) )
  expect_equal( class( x.thresh.fit$par ), "numeric" )
  expect_equal( length( x.thresh.fit$par ), 2 )
  expect_equal( names( x.thresh.fit$par ),
               c( "scale", "shape" ) )
  expect_true( all( x.thresh == x.thresh.fit$x ) )
  expect_equal( class( x.thresh.fit$return.level ), "numeric" )
  expect_equal( length( x.thresh.fit$return.level ), 1 )
})

test_that( "fit.gpd display messages and warnings", {
  expect_output( fit.gpd( x.thresh, error.estimation = "none",
                         threshold = 19 ) )
  expect_output( fit.gpd( x.thresh, error.estimation = "none",
                         debug = TRUE, silent = TRUE,
                         threshold = 19  ) )
  expect_output( fit.gpd( x.thresh, error.estimation = "none",
                         silent = FALSE, threshold = 19 ) )
})

test_that( "fit.gpd's actual fitting works", {
  expect_equal( as.numeric( x.thresh.fit$par ),
               c( 4.319941007, -0.427087671 ) )
  expect_equal( as.numeric( x.exp.fit$par ),
               c( 3.05642081, 0.00000000 ) )
})

test_that( "fit.gpd can handle values below a threshold", {
  expect_equal(
      as.numeric( fit.gpd( threshold( temp.potsdam,
                                     threshold = -7,
                                     extreme.type = "min" ),
                          threshold = -7, silent = TRUE,
                          extreme.type = "min" )$par ),
      c( 3.734557656, -0.308715427 ) )
  expect_equal(
      as.numeric( fit.gpd( temp.potsdam, thresholding = TRUE,
                          threshold = -7, silent = TRUE,
                          extreme.type = "min" )$par ),
      c( 3.734557656, -0.308715427 ) )
  expect_equal(
      as.numeric( fit.gpd( threshold( temp.potsdam,
                                     threshold = -7,
                                     extreme.type = "min" )* -1,
                          threshold = 7, silent = TRUE )$par ),
      c( 3.734557656, -0.308715427 ) )
  expect_equal( as.numeric(
      fit.gpd( temp.potsdam, thresholding = TRUE, threshold = -7,
              return.period = c( 20, 50, 100, 250 ),
              extreme.type = "min", silent = TRUE
              )$return.level ),
      c( -14.7727380, -15.8381911, -16.4659840, -17.1142474 ) )
})

set.seed( 123 )
test_that( "fit.gpd can apply the threshold", {
  expect_equal(
      as.numeric( fit.gpd( temp.potsdam,
                          threshold = 29, decluster = TRUE,
                          thresholding = TRUE,
                          silent = TRUE )$par ),
      c( 4.319941007, -0.427087671 ) )
})

test_that( "The numerical fallback of fit.gev works", {
  expect_equal( as.numeric( fit.gpd( as.numeric(
      threshold( temp.potsdam, threshold = 29 ) ),
      threshold = 29, silent = TRUE )$par ),
      c( 4.319941007, -0.427087671 ) )
})

set.seed( 123 )
test_that( "the handing over of initial parameters works", {
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, initial = c( 2, -.1 ),
                      total.length = length( temp.potsdam ),
                      silent = TRUE )$par ),
      c( 4.320001471, -0.427098061 ) )
})

test_that( "fit.gpd's error estimation works", {
  expect_equal( names(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      total.length = length( temp.potsdam ),
                      silent = TRUE )$se ),
      c( "scale", "shape", "100.rlevel" ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      total.length = length( temp.potsdam ),
                      silent = TRUE )$se ),
      c( 0.2590614603, 0.0367377667, 0.0828584828 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      return.period = c( 100, 200 ),
                      initial = initial.exp, silent = TRUE,
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.160176539, 0.000000000, 5.169529954, 6.504857110 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( temp.potsdam, thresholding = TRUE,
                      threshold = -7, error.estimation = "MLE",
                      extreme.type = "min", silent = TRUE,
                      return.period = c( 100, 200 ),
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.559554817, 0.117590243, 1.086647074, 1.377331254 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MC",
                      return.period = c( 100, 200 ), silent = TRUE,
                      total.length = length( temp.potsdam ),
                      monte.carlo.sample.size = 10 )$se ),
      c( 0.2464134930, 0.0426672316, 0.2856291656, 0.3267305676 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( temp.potsdam, thresholding = TRUE,
                      threshold = -7, error.estimation = "MC",
                      extreme.type = "min", silent = TRUE,
                      return.period = c( 100, 200 ),
                      monte.carlo.sample.size = 10,
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.3286595455, 0.0652047004, 0.5820080996, 0.6884540732 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.block, error.estimation = "bootstrap",
                      return.period = c( 100, 200 ), silent = TRUE,
                      total.length = length( temp.potsdam ),
                      bootstrap.sample.size = 10 )$se ),
      c( 6.091499e-01, 2.295979e-06, 6.331553e-01, 6.370458e-01 ),
      tolerance = 1e-6 )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( temp.potsdam, thresholding = TRUE,
                      threshold = -7, error.estimation = "bootstrap",
                      extreme.type = "min", silent = TRUE,
                      return.period = c( 100, 200 ),
                      bootstrap.sample.size = 10,
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.43555351, 0.06157277, 0.45968648, 0.50308282 ),
      tolerance = 1e-6 )
})

test_that( "fit.gpd's threshold argument affect the result the way it is supposed to", {
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, threshold = 29, silent = TRUE,
                      total.length = length( temp.potsdam )
                      )$return.level ), 38.2157282 )
})

test_that( "fit.gpd yield equivalent results for minima and maxima", {
  expect_equal(
      climex::fit.gpd( x.thresh, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "max", silent = TRUE )$par,
      climex::fit.gpd( -1* x.thresh, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "min", silent = TRUE )$par )
  expect_equal(
      climex::fit.gpd( x.thresh, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "max",
                      silent = TRUE )$return.level,
      climex::fit.gpd( -1* x.thresh, error.estimation = "none",
                      return.period = c( 20, 50, 200 ),
                      extreme.type = "min", silent = TRUE
                      )$return.level * c( -1, -1, -1 ) )
  expect_equal(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      return.period = c( 50, 200 ),
                      extreme.type = "max", silent = TRUE )$se,
      climex::fit.gpd( -1* x.thresh, error.estimation = "MLE",
                      return.period = c( 50, 200 ),
                      extreme.type = "min", silent = TRUE )$se )
  expect_equal(
  { set.seed( 123 )
    as.numeric(
        climex::fit.gpd( x.thresh, error.estimation = "MC",
                        return.period = c( 50, 200 ),
                        monte.carlo.sample.size = 10,
                        extreme.type = "max", silent = TRUE )$se ) },
  { set.seed( 123 )
    as.numeric(
        climex::fit.gpd( -1* x.thresh, error.estimation = "MC",
                        return.period = c( 50, 200 ),
                        monte.carlo.sample.size = 10,
                        extreme.type = "min", silent = TRUE )$se ) } )
  expect_equal(
  { set.seed( 123 )
    as.numeric(
        climex::fit.gpd( x.thresh, error.estimation = "bootstrap",
                        return.period = c( 50, 200 ),
                        bootstrap.sample.size = 10,
                        extreme.type = "max", silent = TRUE )$se ) },
  { set.seed( 123 )
    as.numeric(
        climex::fit.gpd( -1* x.thresh, error.estimation = "bootstrap",
                        return.period = c( 50, 200 ),
                        bootstrap.sample.size = 10,
                        extreme.type = "min", silent = TRUE )$se ) })
})
## End of test_fit.R
