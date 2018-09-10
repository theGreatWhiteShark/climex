### test_fit.R - Test checking the fitting procedure.
library( climex )
context( "Testing the fitting procedure" )
x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )
set.seed( 123 )
x.block.fit <- climex::fit.gev( x.block )
set.seed( 123 )
x.thresh.fit <- climex::fit.gpd( x.thresh,
                                total.length = length( temp.potsdam )
                                )
## Deal with the special cases of shape = 0 separately.
set.seed( 123 )
initial.gumbel <-
  c( climex::likelihood.initials(
                 x.block, model = "gev" )[ 1 : 2 ], 0 )
x.gumbel.fit <- climex::fit.gev( x.block, initial = initial.gumbel )
set.seed( 123 )
initial.exp <- c( climex::likelihood.initials(
                              x.thresh, model = "gpd" )[ 1 ], 0 )
x.exp.fit <- climex::fit.gpd( x.thresh, initial = initial.exp,
                             threshold = 29,
                             total.length = length( temp.potsdam ) )

test_that( "fit.gev has the correct output format", {
  expect_equal( class( x.block.fit ),
               c( "climex.fit.gev", "list" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gev(
                             list( x.block, x.block ),
                             error.estimation = "none" ),
                             class ) ) ),
               c( "climex.fit.gev", "list" ) )
  expect_equal( names( x.block.fit ),
               c( "par", "value", "gradient", "counts",
                 "outer.iterations", "control", "se", "return.level",
                 "x" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gev(
                             list( x.block, x.block ),
                             error.estimation = "none" ),
                             names ) ) ),
               c( "par", "value", "gradient", "counts",
                 "outer.iterations", "control", "se", "return.level",
                 "x" ) )
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
               c( 11.727058700107, 1.429260028751, -0.260793037231 ) )
  expect_equal( as.numeric(  x.gumbel.fit$par ),
               c( 11.53103449, 1.40410971, 0.00000000 ) )
})
test_that( "fit.gev can handle the blocking", {
  expect_equal( as.numeric(
      fit.gev( climex::anomalies( temp.potsdam ),
              blocking = TRUE )$par ),
      c( 11.727058700107, 1.429260028751, -0.260793037231 ) )
})
test_that( "fit.gev works for the block minima as well", {
  expect_equal( as.numeric(
      fit.gev( climex::anomalies( temp.potsdam ),
              blocking = TRUE,
              extreme.type = "min" )$par ),
      c( -10.668575179281, 1.890623617371, -0.004473297533 ) )
  expect_equal( as.numeric(
      fit.gev( climex::anomalies( temp.potsdam ),
              blocking = TRUE,
              return.period = c( 20, 50, 100, 250 ),
              extreme.type = "min" )$return.level ),
      c( -16.24695564, -17.98166362, -19.27685233, -20.97602207 ) )
})
test_that( "the handing over of initial parameters works", {
  expect_equal( as.numeric(
      climex::fit.gev( x.block,
                      initial = climex::likelihood.initials( x.block )
                      )$par ),
      c( 11.727058700107, 1.429260028751, -0.260793037231 ) )
  expect_equal( as.numeric( climex::fit.gev( x.block,
                                            initial = c( 12, 1.8, -.2 )
                                            )$par ),
               c( 11.726995377463, 1.429153949695, -0.260734631118 ) )
  expect_equal( as.numeric( climex::fit.gev( x.block,
                                            initial = c( 12, 1.8, 0 )
                                            )$par ),
               c( 11.53116092, 1.40413665, 0.00000000 ) )
})
test_that( "fit.gev's error estimation works", {
  expect_equal( names( x.block.fit$se ),
               c( "location", "scale", "shape", "100.rlevel" ) )
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "MLE" )$se ),
      c( 0.1389855950341, 0.0961566040865, 0.0438037802173,
        0.0643674174564 ) )
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "MLE",
                                  return.period = c( 100, 200 ),
                                  initial = initial.gumbel )$se ),
      c( 0.1337865978, 0.0914901808, 0.0000000000, 0.2326657115,
        0.2959816668173 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "MC",
                                  return.period = c( 100, 200 ),
                                  monte.carlo.sample.size = 10 )$se ),
      c( 0.12972799791, 0.07040932957, 0.06981877900, 0.38005619085,
        0.44439722518 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "bootstrap",
                                  return.period = c( 100, 200 ),
                                  bootstrap.sample.size = 10 )$se ),
      c( 0.11515439343, 0.10455841206, 0.08419720735, 0.44177053223,
        0.51569600702 ) )
  expect_equal(
      as.numeric( climex::fit.gev( temp.potsdam, blocking = TRUE,
                                  error.estimation = "MLE",
                                  return.period = c( 100, 200 ),
                                  extreme.type = "min",
                                  monte.carlo.sample.size = 100 )$se ),
      c( 0.36612949122, 0.26371977572, 0.06783789252, 0.71156862124,
        1.01845512372 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( temp.potsdam, blocking = TRUE,
                                  error.estimation = "MC",
                                  return.period = c( 100, 200 ),
                                  extreme.type = "min",
                                  monte.carlo.sample.size = 10 )$se ),
      c( 0.33011545541, 0.17762892040, 0.06945887934, 0.99544519863,
        1.16183273180 ) )
  set.seed( 123 )
  expect_equal(
      as.numeric( climex::fit.gev( temp.potsdam, blocking = TRUE,
                                  error.estimation = "bootstrap",
                                  extreme.type = "min",
                                  return.period = c( 100, 200 ),
                                  bootstrap.sample.size = 10 )$se ),
      c( 0.49039119100, 0.22749633936, 0.06518210237, 0.67571490272,
        0.84240265697 ) )
})

test_that( "fit.gpd has the correct output format", {
  expect_equal( class( x.thresh.fit ), c( "climex.fit.gpd", "list" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gpd(
                             list( x.thresh, x.thresh ),
                             error.estimation = "none" ),
                             class ) ) ),
      c( "climex.fit.gpd", "list" ) )
  expect_equal( names( x.thresh.fit ),
               c( "par", "value", "gradient", "counts",
                 "outer.iterations", "x", "threshold", "control", "se",
                 "return.level" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gpd(
                             list( x.thresh, x.thresh ),
                             error.estimation = "none" ),
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
test_that( "fit.gpd's actual fitting works", {
  expect_equal( as.numeric( x.thresh.fit$par ),
               c( 4.2828385375, -0.4237078379 ) )
  expect_equal( as.numeric( x.exp.fit$par ),
               c( 3.040842809, 0.00000000 ) )
})
test_that( "fit.gpd can handle values below a threshold", {
  expect_equal(
      as.numeric( fit.gpd( threshold( temp.potsdam,
                                     threshold = -7,
                                     extreme.type = "min" ),
                          threshold = -7,
                          extreme.type = "min" )$par ),
      c( 3.7345576557, -0.3087154265 ) )
  expect_equal(
      as.numeric( fit.gpd( temp.potsdam, thresholding = TRUE,
                          threshold = -7,
                          extreme.type = "min" )$par ),
      c( 3.7345576557, -0.3087154265 ) )
  expect_equal(
      as.numeric( fit.gpd( threshold( temp.potsdam,
                                     threshold = -7,
                                     extreme.type = "min" )* -1,
                          threshold = 7 )$par ),
      c( 3.7345576557, -0.3087154265 ) )
  expect_equal( as.numeric(
      fit.gpd( temp.potsdam, thresholding = TRUE, threshold = -7,
              return.period = c( 20, 50, 100, 250 ),
              extreme.type = "min" )$return.level ),
      c( -14.77273801, -15.83819111, -16.46598398, -17.11424740 ) )
})
set.seed( 123 )
test_that( "fit.gpd can apply the threshold", {
  expect_equal(
      as.numeric( fit.gpd( temp.potsdam,
                          threshold = 29, decluster = TRUE,
                          thresholding = TRUE )$par ),
               c( 4.2828385375, -0.4237078379 ) )
})
set.seed( 123 )
test_that( "the handing over of initial parameters works", {
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, initial = c( 2, -.1 ),
                      total.length = length( temp.potsdam ) )$par ),
      c( 4.282381644735, -0.423654396346 ) )
})
test_that( "fit.gpd's error estimation works", {
  expect_equal( names(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      total.length = length( temp.potsdam ) )$se ),
      c( "scale", "shape", "100.rlevel" ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.25765277826, 0.03634470813, 0.08260489523 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      return.period = c( 100, 200 ),
                      initial = initial.exp,
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.1609652906, 0.0000000000, 5.1929868352, 6.5346314649 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( temp.potsdam, thresholding = TRUE,
                      threshold = -7, error.estimation = "MLE",
                      extreme.type = "min",
                      return.period = c( 100, 200 ),
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.5595548168, 0.1175902431, 1.0879687245, 1.3844627886 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MC",
                      return.period = c( 100, 200 ),
                      total.length = length( temp.potsdam ),
                      monte.carlo.sample.size = 10 )$se ),
      c( 0.23519750223, 0.03976175254, 0.32465579300, 0.36367354803 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( temp.potsdam, thresholding = TRUE,
                      threshold = -7, error.estimation = "MC",
                      extreme.type = "min",
                      return.period = c( 100, 200 ),
                      monte.carlo.sample.size = 10,
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.32865954545, 0.06520470042, 0.58476857510, 0.69127613997 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.block, error.estimation = "bootstrap",
                      return.period = c( 100, 200 ),
                      total.length = length( temp.potsdam ),
                      bootstrap.sample.size = 10 )$se ),
      c( 7.538023560e-01, 5.357404019e-06, 7.835045525e-01,
        7.883079040e-01 ) )
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( temp.potsdam, thresholding = TRUE,
                      threshold = -7, error.estimation = "bootstrap",
                      extreme.type = "min",
                      return.period = c( 100, 200 ),
                      bootstrap.sample.size = 10,
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.5936214310, 0.1150000019, 0.4998612247, 0.7263367971 ) )
})
test_that( "fit.gpd's threshold argument affect the result the way it is supposed to", {
  set.seed( 123 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, threshold = 29,
                      total.length = length( temp.potsdam )
                      )$return.level ), 38.19163319 )
})
## End of test_fit.R
