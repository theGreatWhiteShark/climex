library( climex )
context( "Testing the likelihood and in particular the likelihood.initial function" )

x.block <- climex::block( climex::anomalies( temp.potsdam ) )
x.thresh <- climex::threshold( temp.potsdam, threshold = 29, decluster = TRUE )

test_that( "Input and output of likelihood are alright", {
    expect_equal( length( climex::likelihood( c( 12, 1.4, -.3 ), x.block ) ), 1 )
    expect_equal( class( climex::likelihood( c( 12, 1.4, -.3 ), x.block ) ), "numeric" )
    expect_error( climex::likelihood( x.block ) )
    expect_error( climex::likelihood( x.block, x.block ) )
    expect_error( climex::likelihood( c( 12, 1.4 ), x.block ) )
    expect_error( climex::likelihood( c( 12, 1.4 ), x.block, model = "gev" ) )
    expect_error( climex::likelihood( c( 12, 1.4, -.3 ), x.block, model = "gpd" ) )
})
test_that( "Likelihood spits the right vales", {
    expect_equal( climex::likelihood( c( 12, 1.4, -.3 ), x.block ), 222.9288,
                 tolerance = 1e-4 )
    expect_equal( climex::likelihood( c( 1.4, -.1 ), x.thresh, model = "gpd" ),
                 2907.355, tolerance = 1e-4 )
})

test_that( "Input and output of likelihood.gradient are alright", {
    expect_equal( length( climex:::likelihood.gradient( c( 12, 1.4, -.3 ),
                                                      x.block ) ), 3 )
    expect_equal( length( climex:::likelihood.gradient( c( 12, 1.4 ),
                                                       x.block,
                                                       model = "gpd" ) ), 2 )
    expect_equal( class( climex:::likelihood.gradient( c( 12, 1.4, -.3 ),
                                                     x.block ) ), "numeric" )
    expect_error( climex:::likelihood.gradient( x.block ) )
    expect_error( climex:::likelihood.gradient( x.block, x.block ) )
    expect_error( climex:::likelihood.gradient( c( 12, 1.4 ), x.block ) )
    expect_error( climex:::likelihood.gradient( c( 12, 1.4 ), x.block,
                                              model = "gev" ) )
    expect_error( climex:::likelihood.gradient( c( 12, 1.4, -.3 ), x.block,
                                              model = "gpd" ) )
})
test_that( "Likelihood.gradient spits the right vales", {
    expect_equal( climex:::likelihood.gradient( c( 12, 1.4, -.3 ), x.block ),
                 c( 11.44009, -14.32531, -35.40255 ), tolerance = 1e-4 )
    expect_equal( climex:::likelihood.gradient( c( 1.4, -.1 ), x.thresh, model = "gpd" ),
                 c( 1014.373, 2051.769 ), tolerance = 1e-4 )
})

test_that( "likelihood.initials' inputs and outputs are alright", {
    expect_equal( class( climex::likelihood.initials( x.block ) ), "numeric" )
    expect_equal( class( climex::likelihood.initials( as.numeric( x.block ) ) ),
                 "numeric" )
    expect_equal( length( climex::likelihood.initials( x.block ) ), 3 )
    expect_equal( length( climex::likelihood.initials( x.block, model = "gpd" ) ), 2 )
})
test_that( "likelihood.initials' results are okay", {
    expect_equal( climex::likelihood.initials( x.block, model = "gev" ),
                 c( 11.7474404, 1.4419331, -0.2903362 ), tolerance = 1e-2 )
    expect_equal( climex::likelihood.initials( x.block, model = "gpd" )[ 1 ],
                 1.437634, tolerance = 1e-5 )
})
test_that( "likelihood.initials' type and modified argument work", {
    expect_equal( climex::likelihood.initials( x.block, model = "gev",
                                              type = "lmom" ),
                 c( 11.7474404, 1.4419331, -0.2903362 ), tolerance = 1e-5 )
    expect_equal( climex::likelihood.initials( x.block, model = "gev",
                                              type = "mom" ),
                 c( 11.602097, 1.120919, -0.277500 ), tolerance = 1e-5 )
    expect_equal( climex::likelihood.initials( x.block, model = "gpd",
                                              type = "lmom" ),
                 c( 172.1049, -13.0504 ), tolerance = 1e-5 )
    expect_equal( climex::likelihood.initials( x.block, model = "gpd",
                                              type = "mom" ),
                 c( 1.437634, 0.1 ), tolerance = 1e-5 )
    expect_equal( climex::likelihood.initials( x.block, model = "gev",
                                              type = "mom",
                                              modified = FALSE ),
                 c( 11.602097, 1.120919, 0.000010 ), tolerance = 1e-5 )
    ## no modification yet for the initials of the GP distribution
    ## expect_equal( climex::likelihood.initials( x.block, model = "gpd",
    ##                                           type = "mom",
    ##                                           modified = FALSE ),
    ##              c( 1.437634, 1.307424 ), tolerance = 1e-5 )
})
    
