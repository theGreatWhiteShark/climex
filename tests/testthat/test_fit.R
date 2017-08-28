library( climex )
context( "Testing the fitting procedure" )
x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )
x.block.fit <- climex::fit.gev( x.block )
x.thresh.fit <- climex::fit.gpd( x.thresh,
                                total.length = length( temp.potsdam )  )

test_that( "fit.gev has the correct output format", {
  expect_equal( class( x.block.fit ),
               c( "list", "climex.fit.gev" ) )
  expect_equal( names( x.block.fit ),
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
test_that( "fit.gev's actual fitting works for all methods", {
  expect_equal( as.numeric( climex::fit.gev( x.block )$par ),
               c( 11.727058700107, 1.429260028751, -0.260793037231 ) )
  expect_equal( as.numeric(
      climex::fit.gev( x.block, optim.method = "Nelder-Mead" )$par ),
      c( 11.727058700107, 1.429260028751, -0.260793037231 ) )
  expect_equal( as.numeric( climex::fit.gev( x.block,
                                            optim.method = "BFGS" )$par ),
               c( 11.726997127023, 1.429208110952, -0.260763842774 ) )
  expect_equal( as.numeric( climex::fit.gev( x.block,
                                            optim.method = "CG" )$par ),
               c( 11.72699708626, 1.42920804573, -0.26076369704 ) )
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
})
test_that( "fit.gev's error estimation works", {
  expect_equal( names( climex::fit.gev( x.block,
                                       error.estimation = "MLE" )$se ),
               c( "location", "scale", "shape", "100.rlevel" ) )
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "MLE" )$se ),
      c( 0.1389855950341, 0.0961566040865, 0.0438037802173,
        0.0643674174564 ) )
  expect_equal(
      as.numeric( climex::fit.gev( x.block,
                                  error.estimation = "MC",
                                  monte.carlo.sample.size = 100 )$se ),
      c( 0.1445030146928, 0.0968594982045, 0.0581169970019,
        0.3178739721512 ), tolerance = 4E-2 )
})


test_that( "fit.gpd has the correct output format", {
  expect_equal( class( x.thresh.fit ), c( "list", "climex.fit.gpd" ) )
  expect_equal( names( x.thresh.fit ),
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
test_that( "fit.gpd's actual fitting works for all methods", {
  expect_equal( as.numeric( climex::fit.gpd( x.thresh,
                                            total.length = length(
                                                temp.potsdam )  )$par ),
               c( 4.282789734412, -0.423708700144 ) )
  expect_equal( as.numeric( climex::fit.gpd( x.thresh,
                                            optim.method = "Nelder-Mead",
                                            total.length = length(
                                                temp.potsdam )  )$par ),
               c( 4.282789734412, -0.423708700144 ) )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, optim.method = "BFGS",
                      total.length = length( temp.potsdam ) )$par ),
      c( 4.282352838928, -0.423670486691 ) )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, optim.method = "CG",
                      total.length = length( temp.potsdam ) )$par ),
      c( 4.282614490266, -0.423702047173 ) )
})
test_that( "the handing over of initial parameters works", {
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh,
                      initial = climex::likelihood.initials(
                                            x.thresh, model = "gpd" ),
                      total.length = length( temp.potsdam ) )$par ),
      c(  4.282789734412, -0.423708700144 ) )
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
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MLE",
                      total.length = length( temp.potsdam ) )$se ),
      c( 0.2576391111525, 0.0363402540590, 0.0454172172595 ) )
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, error.estimation = "MC",
                      total.length = length( temp.potsdam ),
                      monte.carlo.sample.size = 100 )$se ),
      c( 0.2680637375583, 0.0409491757803, 0.2666002006646 ),
      tolerance = 8E-2 )
})
test_that( "fit.gpd throws a warning if total.length is not supplied", {
  expect_warning( climex::fit.gpd( x.thresh, silent = FALSE ) )
})
test_that( "fit.gpd's threshold argument affect the result the way it is supposed to", {
  expect_equal( as.numeric(
      climex::fit.gpd( x.thresh, threshold = 29,
                      total.length = length( temp.potsdam )
                      )$return.level ), 38.1902597338 )
})
