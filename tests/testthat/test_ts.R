### test_ts.R - Tests for the helper functions of the time series
###   analysis 
library( climex )
library( lubridate )
context( "Check function is R/ts.R" )

x.block <- block( anomalies( temp.potsdam ) )
x.thresh <- threshold( temp.potsdam, threshold = 29,
                              decluster = TRUE )
x.block.fit <- fit.gev( x.block, error.estimation = "none",
                       silent = TRUE )
x.thresh.fit <- fit.gpd( x.thresh, error.estimation = "none",
                        total.length = length( temp.potsdam ),
                        silent = TRUE )

test_that( "aic accepts the right input and produces the right output", {
  expect_equal( aic( x.block.fit ), 447.213063 )
  expect_equal( aic( x.thresh.fit ), 1454.036538 )
  expect_equal( aic( list( x.block.fit, x.thresh.fit ) ),
               c( 447.213063, 1454.036538 ) )
  expect_error( aic( x.block ) )
  expect_error( aic( as.numeric( x.block ) ) )
  expect_error( aic( list( x.block, as.numeric( x.block ) ) ) )
})
test_that( "bic accepts the right input and produces the right output", {
  expect_equal( bic( x.block.fit ), 455.673908 )
  expect_equal( bic( x.thresh.fit ), 1461.79201 )
  expect_equal( bic( list( x.block.fit, x.thresh.fit ) ),
               c( 455.673908, 1461.792010 ) )
  expect_error( bic( x.block ) )
  expect_error( bic( as.numeric( x.block ) ) )
  expect_error( bic( list( x.block, as.numeric( x.block ) ) ) )
})

x.removed <- remove.incomplete.years( temp.potsdam )
## Removing an entry at the beginning/end of the year
x.removed.year.beginning <-
  x.removed[ -which( yday( x.removed ) == 1 )[ 2 ] ]
x.removed.year.end <-
  x.removed[ -( which( yday( x.removed ) == 1 )[ 4 ] - 1 ) ]
x.removed.multiple <- x.removed.year.end[
    -c( 3402, 10292, 12000, 32410 ) ]
test_that( "remove.incomplete.years works as expected", {
  expect_equal( length( temp.potsdam ), 45145 )
  expect_equal( unique( year( temp.potsdam ) ),
               seq( 1893, 2016, 1 ) )
  expect_equal( length( x.removed ), 44924 )
  expect_equal( unique( year( x.removed ) ),
               seq( 1893, 2015, 1 ) )
  expect_equal(
      unique( year( remove.incomplete.years(
          x.removed.year.beginning ) ) ),
      c( 1893, seq( 1895, 2015, 1 ) ) )
  expect_equal(
      unique( year( remove.incomplete.years(
          x.removed.year.end ) ) ),
      c( 1893, 1894, seq( 1896, 2015, 1 ) ) )
  expect_equal( length( remove.incomplete.years(
      x.removed.multiple ) ), 43099 )
  expect_equal( length(
      remove.incomplete.years(
          remove.incomplete.years( 
              x.removed.multiple ) ) ), 43099 )
  expect_equal( length(
      remove.incomplete.years( x.removed[ -1 ] ) ), 44559 )
  expect_equal(
      Reduce( c,
             lapply( remove.incomplete.years(
                 list( x.removed, x.removed[ -1 ] ) ), length ) ),
      c( 44924, 44559 ) )
  expect_error( remove.incomplete.years(
      as.numeric( temp.potsdam ) ) )
  expect_error( remove.incomplete.years(
      list( as.numeric( temp.potsdam ), temp.potsdam ) ) )
})
x.removed[ 1 ] <- NA
test_that( "remove.incomplete.years get's rid of NA in time series",{
  expect_equal( min( remove.incomplete.years( x.removed ) ),
               -16 )
  expect_equal( length( remove.incomplete.years( x.removed ) ),
               44559 )
})

test_that( "check.completeness is throwing errors on wrong input", {
  expect_error( check.completeness( temp.potsdam,
                                   number.of.years = "30" ) )
  expect_error( check.completeness( 30 ) )
  expect_error( check.completeness( list( 30, temp.potsdam ) ) ) } )

x.complete <- x.incomplete <- remove.incomplete.years( temp.potsdam )
x.incomplete[ 2 ] <- NA
test_that( "check.completeness is performing as expected", {
  expect_true( check.completeness(
      x.complete,
      number.of.years = length( unique(
          lubridate::year( x.complete ) ) ) ) )
  expect_false( check.completeness(
      x.incomplete,
      number.of.years = length( unique(
          lubridate::year( x.complete ) ) ) ) )
  expect_equal( length( check.completeness(
      list( x.complete, x.incomplete ),
      number.of.years = length( unique(
          lubridate::year( x.complete ) ) ) ) ),
      1 ) } )

test_that( "mode is working", {
  expect_equal( mode( temp.potsdam ), 19.7559 - 2.08e-06 )
})

test_that( "anomalies get its job done", {
  expect_true( is.xts( anomalies( temp.potsdam ) ) )
  expect_true( is.list(
      anomalies( list( temp.potsdam, temp.potsdam ) ) ) )
  expect_error( anomalies( as.numeric( temp.potsdam ) ) )
  expect_error( anomalies( list( as.numeric( temp.potsdam ),
                                temp.potsdam ) ) )
  expect_equal( max( anomalies( temp.potsdam ) ),
               16.32177 + 4.19e-06 )
  expect_equal( min( anomalies( temp.potsdam ) ),
               -19.12258 - 6.45e-07 )
  expect_equal(
      max( Reduce( c, anomalies( list( temp.potsdam,
                                      temp.potsdam ) ) ) ),
      16.32177 + 4.19e-06 )
  expect_equal(
      min( Reduce( c, anomalies( list( temp.potsdam,
                                      temp.potsdam ) ) ) ),
               -19.12258 - 6.45e-07 )
} )
## End of test_ts.R
