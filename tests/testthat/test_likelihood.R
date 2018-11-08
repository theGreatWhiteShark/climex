### test_likelihood.R - Tests checking the various likelihood
###   functions 
library( climex )
context( "Testing the likelihood and in particular the likelihood.initial function" )

x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )

test_that( "Input and output of likelihood are alright", {
  expect_equal( length( climex::likelihood( c( 12, 1.4, -.3 ),
                                           x.block ) ), 1 )
  expect_equal( class( climex::likelihood( c( 12, 1.4, -.3 ),
                                          x.block ) ), "numeric" )
  expect_error( climex::likelihood( x.block ) )
  expect_error( climex::likelihood( x.block, x.block ) )
  expect_error( climex::likelihood( c( 12, 1.4 ), x.block ) )
  expect_error( climex::likelihood( c( 12, 1.4 ), x.block,
                                   model = "gev" ) )
  expect_error( climex::likelihood( c( 12, 1.4, -.3 ), x.block,
                                   model = "gpd" ) )
})
test_that( "Likelihood spits the right vales", {
  expect_equal( climex::likelihood( c( 12, 1.4, -.3 ), x.block ),
               222.9288 - 5.47e-06 )
  expect_equal( climex::likelihood( c( 1.4, -.1 ), x.thresh,
                                   model = "gpd" ), 976.5173 )
})

test_that( "Input and output of likelihood.gradient are alright", {
  expect_equal( length( climex:::likelihood.gradient(
                                     c( 12, 1.4, -.3 ), x.block ) ),
               3 )
  expect_equal( length( climex:::likelihood.gradient( c( 12, 1.4 ),
                                                     x.block,
                                                     model = "gpd" ) ),
               2 )
  expect_equal( class( climex:::likelihood.gradient( c( 12, 1.4, -.3 ),
                                                    x.block ) ),
               "numeric" )
  expect_error( climex:::likelihood.gradient( x.block ) )
  expect_error( climex:::likelihood.gradient( x.block, x.block ) )
  expect_error( climex:::likelihood.gradient( c( 12, 1.4 ), x.block ) )
  expect_error( climex:::likelihood.gradient( c( 12, 1.4 ), x.block,
                                             model = "gev" ) )
  expect_error( climex:::likelihood.gradient( c( 12, 1.4, -.3 ), x.block,
                                             model = "gpd" ) )
})
test_that( "Likelihood.gradient spits the right vales", {
  expect_equal( climex:::likelihood.gradient( c( 12, 1.4, -.3 ),
                                             x.block ),
               c( 11.44009 - 3.34e-06, -14.32531 - 8.14e-07,
                 -35.40255 + 4.50e-06 ) )
  expect_equal( climex:::likelihood.gradient( c( 12, 1.4, 0 ),
                                             x.block ),
               c( 35.41967 - 2.68e-06, -29.82722 + 3.99e-06, 0 ) )
  expect_equal( climex:::likelihood.gradient( c( 1.4, -.1 ), x.thresh,
                                             model = "gpd" ),
               c( -525.510 - 4.68e-05, -1411.622 + 2.08e-04 ) )
  expect_equal( climex:::likelihood.gradient( c( 1.4, 0 ), x.thresh,
                                             model = "gpd" ),
               c( -298.7755 - 1.02e-05, 0 ) )
})

test_that( "likelihood.initials' inputs and outputs are alright", {
  expect_equal( class( climex::likelihood.initials( x.block ) ),
               "numeric" )
  expect_equal( class( climex::likelihood.initials(
                                   as.numeric( x.block ) ) ),
               "numeric" )
  expect_equal( length( climex::likelihood.initials( x.block ) ), 3 )
  expect_equal( length( climex::likelihood.initials( x.block,
                                                    model = "gpd" ) ),
               2 )
})
test_that( "likelihood.initials' results are okay", {
  expect_equal( climex::likelihood.initials( x.block, model = "gev" ),
               c( 11.7474404, 1.4419331, -0.2903362 ) )
  expect_equal( climex::likelihood.initials( x.block, model = "gpd" ),
               c( 1.437634 + 2.9e-07, -.05 ) )
})
test_that( "the augmented version works properly", {
  expect_equal( climex::likelihood( c( 20, 1, -2 ), x.block,
                                   model = "gev" ),
                674.608411755 )
  expect_equal( climex::likelihood.augmented( c( 20, 1, -2 ),
                                             x.block,
                                             model = "gev" ),
                1225.85841175 )
  expect_equal( climex::likelihood( c( 20, -2 ), x.thresh,
                                   model = "gpd" ),
                992.40108997 )
  expect_equal( climex::likelihood.augmented( c( 20, -2 ),
                                             x.thresh,
                                             model = "gpd" ),
               1543.70108998 )
  expect_equal( climex:::likelihood.gradient(
                             c( 20, 1, -2 ), x.block,
                             model = "gev" ),
                c( 38.6558114721, -169.5567167671, 118.4902267151 ) )
  expect_equal( climex:::likelihood.gradient.augmented(
                             c( 20, 1, -2 ), x.block,
                             model = "gev" ),
                c( 38.6558114721, -169.5567167671, -1881.5097732849 ) )
  expect_equal( climex:::likelihood.gradient(
                             c( 20, -2 ), x.thresh, model = "gpd" ),
                c( 24.7981139685, 108.0188055262 ) )
  expect_equal( climex:::likelihood.gradient.augmented(
                             c( 20, -2 ), x.thresh, model = "gpd" ),
                c( -21.2818860315, -2352.7811944738 ) )
})
## End of test_likelihood.R
