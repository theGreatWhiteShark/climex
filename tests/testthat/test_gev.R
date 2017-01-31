library( climex )
context( "Tools used in the GEV branch of extreme value analysis" )
x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29 )
x.block.fit <- fit.gev( x.block )
x.thresh.fit <- fit.gpd( x.thresh, threshold = 29 )

test_that( "block's block length argument works", {
    expect_equal( length( climex::block( temp.potsdam ) ), 124 )
    expect_equal( length( climex::block( temp.potsdam, block.length = 365 ) ), 124 )
    expect_equal( length( climex::block( temp.potsdam, block.length = 128 ) ), 353 )
    expect_equal( length( climex::block( temp.potsdam, block.length = 700 ) ), 65 )
})
test_that( "block's block mode argument works", {
    expect_equal( max( climex::block( temp.potsdam ) ), 38.6 )
    expect_equal( min( climex::block( temp.potsdam ) ), 29.5 )
    expect_equal( max( climex::block( temp.potsdam, block.mode = "min" ) ), 0 )
    expect_equal( min( climex::block( temp.potsdam, block.mode = "min" ) ), -16 )
})
test_that( "block's block number argument works", {
    expect_equal( length( climex::block( temp.potsdam, block.number = 12 ) ), 12 )
    expect_equal( length( climex::block( temp.potsdam, block.number = 392 ) ), 392 )
})
test_that( "block's output has the right format", {
    expect_equal( class( climex::block( temp.potsdam ) ), c( "xts", "zoo" ) )
})
test_that( "block just accepts class 'xts' objects", {
    expect_error( climex::block( as.numeric( temp.potsdam ) ) )
})

test_that( "return.level fit results and GEV/GP parameters as input", {
    expect_error( climex::return.level( temp.potsdam ) )
    expect_error( climex::return.level( as.numeric( temp.potsdam ) ) )
    expect_equal( climex::return.level( x.block.fit ), 15.55642-8.95e-07 )
    expect_equal( climex::return.level( x.block.fit$par ), 15.55642-8.95e-07 )
    expect_equal( climex::return.level( x.thresh.fit$par, model = "gpd",
                                       threshold = 29,
                                       original.time.series = x.thresh ), 38.05395+3e-06 )
    expect_equal( climex::return.level( x.thresh.fit, model = "gpd" ), 38.05395+3e-06 )
})
test_that( "return.level can take return periods of different length and value", {
    expect_equal( climex::return.level( x.block.fit, return.period = 100 ), 15.55642-8.95e-07 )
    expect_equal( climex::return.level( x.block.fit, return.period = c( 10, 20, 500 ) ),
                 c( 14.15995-3.82e-06, 14.68160-2.05e-06, 16.12371+3.76e-06 ) )
})
test_that( "return.level has the right output", {
    expect_match( class( climex::return.level( x.block.fit ) ), "numeric" )
    expect_match( class( climex::return.level( x.block.fit,
                        error.estimation = "none" ) ), "numeric" )
    expect_match( class( climex::return.level( x.block.fit$par,
                        error.estimation = "MLE" ) ), "numeric" )
    expect_match( class( climex::return.level( x.block.fit,
                        error.estimation = "MLE" ) ), "list" )
    expect_equal( names( climex::return.level( x.block.fit,
                                              error.estimation = "MLE" ) ),
                 c( "return.levels", "errors" ) )
})
test_that( "return.level get the error estimation right", { 
    expect_equal( as.numeric(
        climex::return.level( x.block.fit, return.period = c( 10, 100, 332 ),
                             error.estimation = "MC" )$errors ),
                 c( .2, .3, .4 ), tolerance = .15 )
    expect_equal( as.numeric(
        climex::return.level( x.block.fit, return.period = c( 10, 100, 332 ),
                             error.estimation = "MLE" )$errors ),
                 c( 0.028197, 0.06443386,  0.1015562 ), tolerance = 1E-6 )
    expect_equal( as.numeric(
        climex::return.level( x.thresh.fit, return.period = 42,
                             error.estimation = "MC", threshold = 29 )$errors ),
                 .45, tolerance = .1 )
    expect_equal( as.numeric(
        climex::return.level( x.thresh.fit, return.period = c( 42, 637 ),
                             error.estimation = "MLE", threshold = 29,
                             total.length = length( temp.potsdam ) )$errors ),
                 c( 2.3851777, 0.5759769 ), tolerance = 1E-6 )
    expect_warning( climex::return.level( x.thresh.fit$par, return.period = 42,
                                         error.estimation = "MC", threshold = 29,
                                         monte.carlo.sample.size = 10 ) )
})
