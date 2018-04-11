library( climex )
context( "Testing the fitting procedure" )
x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )
x.block.fit <- climex::fit.gev( x.block )
x.thresh.fit <- climex::fit.gpd( x.thresh,
                                total.length = length( temp.potsdam )
                                )
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

test_that( "fit.gev has the correct output format", {
  expect_equal( class( x.block.fit ),
               c( "list", "climex.fit.gev" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gev(
                             list( x.block, x.block ),
                             error.estimation = "none" ),
                             class ) ) ),
               c( "list", "climex.fit.gev" ) )
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
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "MC",
                                  return.period = c( 100, 200 ),
                                  monte.carlo.sample.size = 100 )$se ),
      c( 0.1445030146928, 0.0968594982045, 0.0581169970019,
        0.3178739721512, 0.3673455192856 ), tolerance = 4E-2 )
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "bootstrap",
                                  return.period = c( 100, 200 ),
                                  bootstrap.sample.size = 100 )$se ),
      c( 0.1646659951656, 0.0868258622212, 0.0907993677156,
        0.4499639755108, 0.5248481922853 ), tolerance = 4E-2 )
})

test_that( "fit.gpd has the correct output format", {
  expect_equal( class( x.thresh.fit ), c( "list", "climex.fit.gpd" ) )
  expect_equal(
      unique( Reduce( c, lapply( fit.gpd(
                             list( x.thresh, x.thresh ),
                             error.estimation = "none" ),
                             class ) ) ),
      c( "list", "climex.fit.gpd" ) )
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
               c( 4.282747985860, -0.423731344794 ),
               tolerance = 5E-4 )
  expect_equal( as.numeric( x.exp.fit$par ),
               c( 3.04073223, 0.00000000 ),
               tolerance = 5E-4 )
})
test_that( "the handing over of initial parameters works", {
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, initial = c( 2, -.1 ),
                      total.length = length( temp.potsdam ) )$par ),
      c( 4.282381644735, -0.423654396346 ),
      tolerance = 5E-4 )
})
test_that( "fit.gpd's error estimation works", {
  expect_equal( names(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      total.length = length( temp.potsdam ) )$se ),
      c( "scale", "shape", "100.rlevel" ) )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.257639111153, 0.036340254059, 0.08259123 ),
      tolerance = 5E-4 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      return.period = c( 100, 200 ),
                      initial = initial.exp,
                      total.length = length( temp.potsdam ) )$se ),
      c(  0.160953584, 0.000000000, 5.192609146, 6.534631464912 ),
      tolerance = 5E-4 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MC",
                      return.period = c( 100, 200 ),
                      total.length = length( temp.potsdam ),
                      monte.carlo.sample.size = 100 )$se ),
      c( 0.25406002, 0.03777357, 0.26423247, 0.2920621314167 ),
      tolerance = 8E-2 )
  expect_equal( as.numeric(
      climex::fit.gpd( x.block, error.estimation = "bootstrap",
                      return.period = c( 100, 200 ),
                      total.length = length( temp.potsdam ),
                      bootstrap.sample.size = 100 )$se ),
      c( 7.27448170583e-01, 8.95632920876e-06, 7.56097936060e-01,
        7.60732930106e-01 ), tolerance = 4E-2 )
})
test_that( "fit.gpd's threshold argument affect the result the way it is supposed to", {
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, threshold = 29,
                      total.length = length( temp.potsdam )
                      )$return.level ), 38.1915142245,
      tolerance = 5E-4 )
})
