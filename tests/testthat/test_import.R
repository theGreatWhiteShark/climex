library( climex )
context( "Checking the data import via the API of the German weather service" )

url.recent <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/"
url.historical <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"
## individual files to be downloaded
files.recent <- strsplit( RCurl::getURL( url.recent, followlocation = TRUE,
                                        dirlistonly = TRUE ), "\n" )
files.historical <- strsplit( RCurl::getURL( url.historical, followlocation = TRUE,
                                            dirlistonly = TRUE ), "\n" )
files.recent.length <- length( files.recent[[ 1 ]] )

test_that( "DWD FTP still provides the files I need and RCurl gets them right", {
  expect_equal( class( files.recent ), "list" )
  expect_equal( class( files.historical ), "list" )
  expect_equal( class( files.recent[[ 1 ]] ), "character" )
  expect_equal( class( files.historical[[ 1 ]] ), "character" )
})
test_that( "there is one description file provided by the DWD", {
  expect_equal( length( grep( "Beschreibung", files.recent[[ 1 ]] ) ), 1 )
  expect_equal( length( grep( "Beschreibung", files.historical[[ 1 ]] ) ), 1 )
})
test_that( "more than 100 stations are provided as zip files by the DWD", {
  expect_true( length( grep( ".zip", files.recent[[ 1 ]] ) ) > 100 )
  expect_true( length( grep( ".zip", files.historical[[ 1 ]] ) ) > 100 )
})
test_that( "the sample tried in the next step is actually a .zip file too", {
  expect_match( files.recent[[ 1 ]][ files.recent.length ], ".zip" )
})

## download a sample
dir.create( "./download-test-funny-name-nobody-would-choose" )
setwd( "./download-test-funny-name-nobody-would-choose" )
utils::download.file( paste0( url.recent,
                             files.recent[[ 1 ]][ files.recent.length ] ),
                     destfile = "test.zip", quiet = TRUE )
utils::unzip( "./test.zip" )
test_that( "DWD's zip files content has not changed", {
  expect_equal( length( grep( "produkt", list.files() ) ), 1 )
})
data.file <- list.files()[ grep( "produkt", list.files() ) ]
data <- utils::read.table( data.file,
                          header = TRUE, sep = ";",
                          nrows = as.numeric( system(
                              paste( "wc -l", data.file, "| awk '{print $1}'" ),
                              intern = TRUE ) ) - 2 )
test_that( "data format provided by the DWD is still the same", {
  expect_equal( ncol( data ), 19 )
  expect_equal( names( data ),
               c( "STATIONS_ID", "MESS_DATUM", "QN_3", "FX", "FM", "QN_4",
                 "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM", "PM", "TMK",
                 "UPM", "TXK", "TNK", "TGK", "eor" ) )
})

## download description and format the description file
## this contains the information about the station (name,
## latitude and longitude) and is connected to the zip files
## via its ID
utils::download.file( paste0( url.recent,
                             files.recent[[ 1 ]][
                                 grep( "Beschreibung", files.recent[[ 1 ]] ) ] ),
                     destfile = "description.txt", quiet = TRUE )
description <- utils::read.table( "description.txt", header = FALSE,
                                 sep = "\t", stringsAsFactors = FALSE,
                                 encoding = "UTF-8", skip = 2 )
description <- split( description, seq( nrow( description ) ) )
test_that( "description file is still provided the same way", {
  expect_equal( class( description ), "list" )
  expect_equal( class( description[[ 1 ]] ), "data.frame" )
  expect_equal( class( description[[ 1 ]][ 1, 1 ] ), "character" )
})
description.content.all <- Reduce( c,strsplit( description[[ 1 ]][ 1, 1 ], " " ) )
description.content <- description.content.all[ !description.content.all %in% "" ]
test_that( "description file still starts with an integer ID", {
  expect_equal( floor( as.numeric( description.content[ 1 ] ) ) -
                as.numeric( description.content[ 1 ] ), 0 )
})
test_that( "description files features two dates as second and third values", {
  expect_equal( floor( as.numeric( description.content[ 2 ] ) ) -
                as.numeric( description.content[ 2 ] ), 0 )
  expect_equal( floor( as.numeric( description.content[ 3 ] ) ) -
                as.numeric( description.content[ 3 ] ), 0 )
  expect_true( as.numeric( description.content[ 2 ] ) > 18000000 )
  expect_true( as.numeric( description.content[ 3 ] ) > 18000000 )
})
test_that( "the forth, fifth and sixth entry of the description file are numeric", {
  expect_true( all( !is.nan( as.numeric (
                         description.content[ 4 : 6 ] ) ) ) )
})
test_that( "the last entry is an actual character string", {
  expect_warning( as.numeric( description.content[ 7 ] ) )
})
## cleanup
setwd( ".." )
unlink( "./download-test-funny-name-nobody-would-choose",
       recursive = TRUE )
