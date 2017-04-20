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
  expect_equal( climex:::likelihood.gradient( c( 1.4, -.1 ), x.thresh,
                                             model = "gpd" ),
               c( 525.510 + 4.68e-05, 1411.622 - 2.08e-04 ) )
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
               c( 1.437634 + 2.9e-07, .25 ) )
})
test_that( "likelihood.initials' type and modified argument work", {
  expect_equal( climex::likelihood.initials( x.block, model = "gev",
                                            type = "lmom" ),
               c( 11.7474404, 1.4419331, -0.2903362 ) )
  expect_equal( climex::likelihood.initials( x.block, model = "gev",
                                            type = "mom" ),
               c( 11.602097 - 7.60e-08, 1.120919 - 1.42e-07,
                 -0.277500 ) )
  expect_equal( climex::likelihood.initials( x.block, model = "gpd",
                                            type = "lmom" ),
               c( 172.1049 + 1.46e-05, -13.0504 + 2.67e-06 ) )
  expect_equal( climex::likelihood.initials( x.block, model = "gpd",
                                            type = "mom" ),
               c( 1.437634 + 2.9e-07, -0.05 ) )
  expect_equal( climex::likelihood.initials( x.block, model = "gev",
                                            type = "mom",
                                            modified = FALSE ),
               c( 11.602097 - 7.60e-08, 1.120919 - 1.42e-07, 0.1 ) )
  expect_equal( climex::likelihood.initials( x.block, model = "gpd",
                                            type = "mom",
                                            modified = FALSE ),
               c( 1.437634 + 2.9e-07, 0.1 ) )
})

