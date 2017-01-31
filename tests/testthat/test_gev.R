library( climex )
context( "Tools used in the GEV branch of extreme value analysis" )

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
