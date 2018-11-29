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
               227.618602 )
  expect_equal( climex::likelihood( c( 1.4, -.1 ), x.thresh,
                                   model = "gpd" ), 1001.81668 )
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
               c( 13.3923718, -14.9734295, -29.9261296 ) )
  expect_equal( climex:::likelihood.gradient( c( 12, 1.4, 0 ),
                                             x.block ),
               c( 39.2335927, -36.6318949, 0.0000000 ) )
  expect_equal( climex:::likelihood.gradient( c( 1.4, -.1 ), x.thresh,
                                             model = "gpd" ),
               c( -543.401688, -1477.170428 ) )
  expect_equal( climex:::likelihood.gradient( c( 1.4, 0 ), x.thresh,
                                             model = "gpd" ),
               c( -307.704082, 0.000000 ) )
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
               c( 11.723567489, 1.450082577, -0.291204219 ) )
  expect_equal( climex::likelihood.initials( x.block, model = "gpd" ),
               c( 1.44600767, -0.05000000 ) )
})
test_that( "the augmented version works properly", {
  expect_equal( climex::likelihood( c( 20, 1, -2 ), x.block,
                                   model = "gev" ),
                686.315286 )
  expect_equal( climex::likelihood.augmented(
                            c( 20, 1, -2 ), x.block, model = "gev" ),
                1237.56529 )
  expect_equal( climex::likelihood( c( 20, -2 ), x.thresh,
                                   model = "gpd" ),
                1011.11953 )
  expect_equal( climex::likelihood.augmented(
                            c( 20, -2 ), x.thresh, model = "gpd" ),
               1562.41953 )
  expect_equal( climex:::likelihood.gradient(
                             c( 20, 1, -2 ), x.block, model = "gev" ),
                c( 39.2195645, -172.6534768, 120.9420817 ) )
  expect_equal( climex:::likelihood.gradient.augmented(
                             c( 20, 1, -2 ), x.block, model = "gev" ),
                c( 39.2195645, -172.6534768, -1879.0579183 ) )
  expect_equal( climex:::likelihood.gradient(
                             c( 20, -2 ), x.thresh, model = "gpd" ),
                c( 25.4114594, 111.7781041 ) )
  expect_equal( climex:::likelihood.gradient.augmented(
                             c( 20, -2 ), x.thresh, model = "gpd" ),
                c( -20.6685406, -2349.0218959 ) )
})
## End of test_likelihood.R
