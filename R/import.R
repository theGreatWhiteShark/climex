##' @title Downloads daily weather data from observation stations in Germany and extracts minimum and maximum temperature as well as precipitation data.
##'
##' @details The download will be done using 'wget'. Per default the CLIMEX.PATH variable will be used to set the download path. Since this function will check the files already present it's strongly recommended to use the save.downloads options. Whenever this function is invoked again only updated files will be downloaded which saves a lot of traffic and time. The csv.export option can be used to export the time series into a data type file making it available outside of R too. In addition the geographic positions of the individual stations will be extracted and saved as well. They are needed for the leaflet module of the climex shiny app. CAUTION: since this procedure takes a while its run in parallel on all cores of your machine!
##'
##' @param save.downloads If TRUE the downloaded .zip files are stored in download.path/downloads_dwd. Else they will be deleted after the extracting. Default = TRUE.
##' @param download.path Specifies the data will be stored and downloaded too. It is advised to store it using the global variable CLIMEX.PATH which is also used for importing the saved data.
##' @param csv.export If TRUE creates an additional folder containing .csv files with the individual station data. Using this the data can be used outside of R too. Default = FALSE.
##' @param data.type Specifies which kind of information from the downloaded files should be extracted. This input can be a character vector  The options are: temp.max, temp.min, prec, default (for both the daily maximum and minimum temperature and the precipitation), temp.mean, vapor.pressure, cloud.amount, air.pressure, relative.humidity, wind.speed, temp.min.at.ground, wind.speed.peak, prec.type (0 = no precipitation, 1 = only rain (before 1979), 2 = unknown, 4 = only rain (after 1979), 7 = only snow, 8 = snow or rain), sunshine.duration, snow.height. Default = default.
##'
##' @export
##' @import xts
##' @return invisible setwd()
##' @author Philipp Mueller 
download.data.dwd <- function( save.downloads = TRUE,
                              download.path = CLIMEX.PATH,
                              csv.export = FALSE,
                              data.type = c( "default", "temp.max",
                                            "temp.min", "prec",
                                            "temp.mean",
                                            "vapor.pressure",
                                            "cloud.amount",
                                            "air.pressure",
                                            "relative.humidity",
                                            "wind.speed",
                                            "temp.min.at.ground",
                                            "wind.speed.peak",
                                            "prec.type",
                                            "sunshine.duration",
                                            "snow.height" ) ){
  ## the CLIMEX.PATH is preferred but one can of course set a user
  ## specific path
  if ( missing( download.path ) && !exists( "CLIMEX.PATH" ) )
    stop( "The variable CLIMEX.PATH has to be set in the ~/.Rprofile beforehand!" )
  old.dir <- getwd()
  setwd( download.path ) 
  ## paths on the DWD servers
  url.recent <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/"
  url.historical <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"
  ## individual files to be downloaded
  files.recent <- strsplit( RCurl::getURL( url.recent,
                                          followlocation = TRUE,
                                          dirlistonly = TRUE ), "\n" )
  files.historical <- strsplit( RCurl::getURL( url.historical,
                                              followlocation = TRUE,
                                              dirlistonly = TRUE ),
                               "\n" )
  ## setting the column numbers for extraction based on the data.type
  ## input
  if ( missing( data.type ) )
    data.type <- "default"
  data.type <- match.arg( data.type )
  if ( data.type == "default" )
    data.type <- c( "temp.max", "temp.min", "prec" )
  data.columns <- numeric()
  if ( "temp.mean" %in% data.type )
    data.columns <- c( data.columns, 4 )
  if ( "vapor.pressure" %in% data.type )
    data.columns <- c( data.columns, 5 )
  if ( "cloud.amount" %in% data.type )
    data.columns <- c( data.columns, 6 )
  if ( "air.pressure" %in% data.type )
    data.columns <- c( data.columns, 7 )
  if ( "relative.humidity" %in% data.type )
    data.columns <- c( data.columns, 8 )
  if ( "wind.speed" %in% data.type )
    data.columns <- c( data.columns, 9 )
  if ( "temp.max" %in% data.type )
    data.columns <- c( data.columns, 10 )
  if ( "temp.min" %in% data.type )
    data.columns <- c( data.columns, 11 )
  if ( "temp.min.at.ground" %in% data.type )
    data.columns <- c( data.columns, 12 )
  if ( "wind.speed.peak" %in% data.type )
    data.columns <- c( data.columns, 13 )
  if ( "prec" %in% data.type )
    data.columns <- c( data.columns, 14 )
  if ( "prec.type" %in% data.type )
    data.columns <- c( data.columns, 15 )
  if ( "sunshine.duration" %in% data.type )
    data.columns <- c( data.columns, 16 )
  if ( "snow.height" %in% data.type )
    data.columns <- c( data.columns, 17 )
  ## it will be downloaded in separate folders
  if ( length( grep( "downloads_dwd", getwd() ) ) == 0 ){
    if ( !dir.exists( "./downloads_dwd" ) )
      dir.create( "./downloads_dwd" )
    setwd( "./downloads_dwd" )
  }
  if ( !dir.exists( "./recent" ) )
    dir.create( "./recent" )
  if ( !dir.exists( "./historical" ) )
    dir.create( "./historical" )
  if ( csv.export && !dir.exists( "./data_dwd" ) )
    dir.create( "./data_dwd" )

  download.content <- function( url.base, url.files ){
    for ( ff in url.files[[ 1 ]] ){
      ## Checks if stationsnumber in ff is already present in one name
      ## of the downloaded files
      split.string <- unlist( strsplit( ff, "_" ) )
      ## Depending on the recent or historical data the id of the
      ## station is the third or second entry. Therefore the first
      ## numerical entry will be used
      split.id.character <- paste0( "_",
                                   split.string[ grep( "[[:digit:]]",
                                                      split.string )[ 1 ]
                                                ], "_" )
      if ( length( grep( ff, list.files() ) ) != 0 ){
        ## file is already present
        next
      }
      if ( length( grep( split.id.character, list.files() ) ) != 0 ){
        ## the file is not present yet but on for the same id.
        ## So this must be an outdated version. Delete it.
        file.remove( grep( split.id.character, list.files(),
                          value = TRUE ) )
      }
      utils::download.file( url = paste0( url.base, ff ), destfile = ff,
                           method = "wget" )
    }
  }
  setwd( "./recent" )
  ## Always download the latest description file. Else the algorithm
  ## would fail importing recently added stations.
  file.description.recent <- grep( "Beschreibung",
                                  files.recent[[ 1 ]], value = TRUE )
  utils::download.file( url = paste0( url.recent,
                                     file.description.recent ),
                       destfile = file.description.recent,
                       method = "wget" )
  ## This will download quite a number of .zip files
  download.content( url.recent, files.recent )
  setwd( "../historical" )
  ## Always download the latest description file. Else the algorithm
  ## would fail importing recently added stations.
  file.description.historical <- grep( "Beschreibung",
                                      files.historical[[ 1 ]],
                                      value = TRUE )
  utils::download.file( url = paste0( url.historical,
                                     file.description.historical ),
                       destfile = file.description.historical,
                       method = "wget" )
  download.content( url.historical, files.historical )
  setwd( "../" )
  ## file containing the discription of the station data
  file.description.recent.raw <- utils::read.table( (
    paste0( "./recent/",
           list.files( "./recent/")[ grep( "Beschreibung",
                                          list.files( "./recent/" ) )
                                    ] ) ),
    header = FALSE, sep = "\t", stringsAsFactors = FALSE,
    encoding = "UTF-8", skip = 2 )
  ## split it into a list to make efficient use of the grep command
  file.description.recent <- split( file.description.recent.raw,
                                   seq( nrow(
                                       file.description.recent.raw ) ) )
  file.description.historical.raw <- utils::read.table( (
    paste0( "./historical/",
           list.files( "./historical/")[
               grep( "Beschreibung", list.files( "./historical/" ) ) ]
           ) ),
    header = FALSE, sep = "\t", stringsAsFactors = FALSE,
    encoding = "UTF-8", skip = 2 )
  ## split it into a list to make efficient use of the grep command
  file.description.historical <- split(
      file.description.historical.raw,
      seq( nrow( file.description.historical.raw ) ) )
  file.description.aux <- c( file.description.recent,
                            file.description.historical[
                                !( file.description.historical %in%
                                   file.description.recent ) ] )
  ## since its not possible to have ensure the correct encoding while
  ## importing the artifacts have to be replaced by hand
  file.d.1 <- lapply( file.description.aux, function( x )
    gsub( "\xfc", "ü", x ) )
  file.d.2 <- lapply( file.d.1, function( x )
    gsub( "\xf6", "ö", x ) ) 
  file.d.3 <- lapply( file.d.2, function( x )
    gsub( "\xe4", "ä", x ) )  
  file.d.4 <- lapply( file.d.3, function( x )
    gsub( "\xdf", "ß", x ) )  
  file.d.5 <- lapply( file.d.4, function( x )
    gsub( "\U3e63643c", "Ü", x ) )  
  file.description <- lapply( file.d.5, function( x )
    gsub( "\U3e36643c", "Ö", x ) )      

  ## extract a vector of all unique station IDs seen in the .zip files
  list.station.ids <- as.list( unique( c(
      Reduce( c, lapply( list.files( "./recent/" )[
                     grep( ".zip", list.files( "./recent/" ) ) ],
                     function( x ) strsplit( x, "_" )[[ 1 ]][ 3 ] ) ),
      Reduce( c, lapply( list.files( "./historical/" )[
                     grep( ".zip", list.files( "./historical/" ) ) ],
                     function( x ) strsplit( x, "_" )[[ 1 ]][ 2 ] ) )
  ) ) )
  ## this lists will contain all the final station data
  ## I will only extract the maximum, minimum temperature and the
  ## precipitation but it is easily extendable to all other columns
  stations.temp.max.xts <- stations.temp.min.xts <-
    stations.prec.xts <- list()
  extract.content <- function( station.id, data.column ){
    ## handle each station ID separately and extract all corresponding
    ## .zip files into auxiliary folders 
    flag.recent <- ifelse( length(
        grep( station.id, list.files( "./recent/" ) ) ) != 0,
        TRUE, FALSE )
    flag.historical <- ifelse( length(
        grep( station.id, list.files( "./historical/" ) ) ) != 0,
        TRUE, FALSE )
    ## If the zip file can not be extracted properly the unzip function
    ## will just raise a warning. It has to be converted to an error
    unlink( "./TMPrecent/", recursive = TRUE )
    unlink( "./TMPhistorical/", recursive = TRUE )
    options( warn = 2 )
    if ( flag.recent ){
      try.unzip <- try( utils::unzip( (
        paste0( "./recent/",
               grep( station.id, list.files( "./recent/" ),
                    value = TRUE ) ) ),
        exdir = "./TMPrecent" ), silent = TRUE )
      ## if something goes wrong here just return a placeholder of the
      ## same format
      if ( class( try.unzip ) == "try-error" && !flag.historical )
        return( xts( NA, order.by = lubridate::today() ) )
      if ( class( try.unzip ) == "try-error" )
        flag.recent <- FALSE
    }
    if ( flag.historical ){
      try.unzip <- try( utils::unzip( (
        paste0( "./historical/",
               grep( station.id, list.files( "./historical/" ),
                    value = TRUE ) ) ),
        exdir = "./TMPhistorical/" ), silent = TRUE )
      if ( class( try.unzip ) == "try-error" )
        return( xts( NA, order.by = lubridate::today() ) )
    }
    ## but we don't want potential warning in other packages break our
    ## code
    options( warn = 0 )
    ## get the path to the .txt files containing the information
    if ( flag.recent ){
      recent.file <- paste0( "./TMPrecent/",
                            grep( "produkt",
                                 list.files("TMPrecent/" ),
                                 value = TRUE ) )
      if ( length( grep( "produkt",
                        list.files("TMPrecent/" ),
                        value = TRUE ) ) == 0 )
        return( xts( NA, order.by = lubridate::today() ) )
      ## sometimes an older version is present due to an error
      ## occurring beforehand those have to be removed (at least from
      ## the recent.file variable)
      if ( length( recent.file ) > 1 )
        recent.file <- recent.file[ grep( station.id, recent.file ) ]
    }
    if ( flag.historical ){
      historical.file <- paste0( "./TMPhistorical/",
                                grep( "produkt",
                                     list.files("TMPhistorical/" ),
                                     value = TRUE ) )
      if ( length( grep( "produkt",
                        list.files("TMPhistorical/" ) ) ) == 0 )
        return( xts( NA, order.by = lubridate::today() ) )
      ## sometimes an older version is present due to an error
      ## occurring beforehand those have to be removed (at least from
      ## the historical.file variable)
      if ( length( historical.file ) > 1 )
        historical.file <- historical.file[ grep( station.id,
                                                 historical.file ) ]
    }
    ## data.ii will be a data.frame containing all the recent and
    ## historical data of one station
    if ( flag.recent ){
      ## sometimes there is a single delimiter symbol in the last line.
      ## This causes the read.table function to throw a warning and, if
      ## the file to read just consists of one line, to fail and has to
      ## be avoided check how many characters are present in the last
      ## line
      contains.delimiter.recent <- as.numeric(
          system( paste( "tail -1", recent.file, "| wc -w" ),
                 intern = TRUE ) ) == 0
      if ( contains.delimiter.recent ){
        ## calls "wc" via bash and counts the lines of the file to read
        ## in
        number.of.lines.recent <- as.numeric(
            system( paste( "wc -l", recent.file, "| awk '{print $1}'" ),
                   intern = TRUE ) )
        ## only the last line with the potential delimiter will be
        ## omitted minus two because of the omitted header
        data.ii <- utils::read.table( recent.file, header = TRUE,
                                     sep = ";",
                                     nrows = ( number.of.lines.recent -
                                               2 ) )
      } else
        data.ii <- utils::read.table( recent.file, header = TRUE,
                                     sep = ";" )
      if ( flag.historical ){
        contains.delimiter.historical <- as.numeric(
            system( paste( "tail -1", historical.file, "| wc -w" ),
                   intern = TRUE ) ) == 0
        if ( contains.delimiter.historical ){
          number.of.lines.hist <- as.numeric(
              system( paste( "wc -l", historical.file,
                            "| awk '{print $1}'" ),
                     intern = TRUE ) )
          data.hist <- utils::read.table( historical.file, header = TRUE,
                                         sep = ";",
                                         nrows = number.of.lines.hist -
                                           2 )
        } else
          data.hist <- utils::read.table( historical.file, header = TRUE,
                                         sep = ";" )
        ## in most cases some data of the recent observations are also
        ## included in the historical ones. But we of course don't want
        ## any duplicates
        suppressWarnings( data.ii <- rbind( data.hist[
                              -which( data.hist[ , 2 ] %in%
                                      data.ii[ , 2 ] ), ],
                              data.ii ) )
      }
    } else {
      contains.delimiter.historical <- as.numeric(
          system( paste( "tail -1", historical.file, "| wc -w" ),
                 intern = TRUE ) ) == 0
      if ( contains.delimiter.historical ){
        number.of.lines.hist <- as.numeric(
            system( paste( "wc -l", historical.file,
                          "| awk '{print $1}'" ),
                   intern = TRUE ) )
        data.ii <- utils::read.table( historical.file, header = TRUE,
                                     sep = ";",
                                     nrows = number.of.lines.hist - 2 )
      } else
        data.ii <- utils::read.table( historical.file, header = TRUE,
                                     sep = ";" )
    }
    ## delete the auxiliary folders
    unlink( "./TMPrecent/", recursive = TRUE )
    unlink( "./TMPhistorical/", recursive = TRUE )
    ## writing the data into the lists using the xts class
    results.tmp <- xts( data.ii[ , data.column ],
                       order.by = climex:::convert.date.integer(
                                               data.ii[ , 2 ] ) )
    ## artifacts in the DWD data base are stored as -999
    ## these are converted to NA
    results.tmp[ results.tmp == -999 ] <- NA
    return( results.tmp )
  }
  
  ## For each data type a separate list containing the corresponding
  ## data of all stations will be generated
  ## This takes some time. But unfortunately it can't be parallelized
  ## easily since all thread read and write to the same folders. So no
  ## "progress bar" either
  for ( dd in 1 : length( data.type ) ){
    print( paste( "parsing", data.type[ dd ], "data..." ) )
    assign( paste0( "stations.", data.type[ dd ] ),
           lapply( list.station.ids, function( x )
             extract.content( x, data.columns[ dd ] ) ) )
  }
  ## assigning the stations names
  extract.station.names <- function( station.id, file.description ){
    ## The station ID is placed in the first column and the date of the
    ## beginning of the observation period is placed in the second
    ## column. With just one line between the first and second column
    ## and the necessity of the second to start with either a 1 or a 2
    ## the station ID can be extracted uniquely.
    ## The conversion to numeric and back is necessary to delete zeros
    line.raw <- grep( paste0( " ", as.numeric( station.id ), " [1,2]" ),
                     file.description, value = TRUE )
    ## The extraction of the name fails, because the DWD has not yet
    ## added the station to the Description file holding of the
    ## stations meta data. This can happen (already happened to me).
    if ( length( line.raw ) == 0 ){
      ## Show a warning and recommend downloading the newest data.
      warning( "Some of the station data are not contained in the overall description file. Please make sure you have the most recent data!" )
      ## Worst case, just delete the corresponding file and download
      ## the new one.
      return( list( "none", c( 0, 0, 0 ) ) )
    }
    ## The name can consist of multiple words. In the previous column
    ## there is a digit and in the next the county consisting of one
    ## word (including a minus)
    line.full <- unlist( strsplit( line.raw, " " ) )
    line <- line.full[ line.full != "" ]
    line.last.digit <- utils::tail( grep( "[[:digit:]]", line ), 1 )
    line.last.word <- utils::tail( grep( "[[:alpha:]]", line ), 1 )
    station.name <- line[ line.last.digit + 1 ]
    if ( line.last.word - 1 > line.last.digit + 1 ){
      ## the stations name consists of more than one word
      for ( ww in ( line.last.digit + 2 ) : ( line.last.word - 1 ) )
        station.name <- paste( station.name, line[ ww ] )
    }                             
    ## in the 9th, 8th, 7th entries the stations coordinates are residing
    return( list( station.name,
                 c( as.numeric( line[ line.last.digit ] ),
                   as.numeric( line[ line.last.digit - 1 ] ),
                   as.numeric( line[ line.last.digit - 2 ] ) ) ) )
  }
  station.extracts <- parallel::mclapply( list.station.ids, function( x )
    extract.station.names( x, file.description ),
    mc.cores = parallel::detectCores( logical = FALSE ) )
  station.names <- Reduce( c, lapply( station.extracts,
                                     function( x ) x[[ 1 ]] ) )
  station.positions.aux <- Reduce( rbind, lapply( station.extracts,
                                                 function( x )
                                                   x[[ 2 ]] ) )
  station.positions <- data.frame(
      longitude = station.positions.aux[ , 1 ],
      latitude = station.positions.aux[ , 2 ],
      altitude = station.positions.aux[ , 3 ],
      name = station.names )
  ## assigning the names of the stations.
  ## this new assignment take in the order of 25ms in total
  for ( ss in paste0( "stations.", data.type ) ){
    tmp <- get( ss )
    names( tmp ) <- station.names
    assign( ss, tmp )
  }
  ## writing the data to dat files
  if ( csv.export ){
    ## creating a distinct folder for all the different data types
    for ( dd in data.type ){
      if ( !dir.exists( paste0( "data_dwd/", dd ) ) )
        dir.create( paste0( "data_dwd/", dd ) )
      data.temp <- get( paste0( "stations.", dd ) )
      for ( ll in 1 : length( data.temp ) )
        utils::write.table( data.frame(
                   date = index( data.temp[[ ll ]] ),
                   value = data.temp[[ ll ]], row.names = NULL ),
                   file = paste0( "data_dwd/", dd, "/",
                                 pracma::strRep(
                                             names( data.temp )[ ll ],
                                             "/", "-" ), ".csv" ),
                   sep = " ", row.names = FALSE)
    }
  }   
  ## delete all folders
  if ( !save.downloads ){
    unlink( "./recent/", recursive = TRUE )
    unlink( "./historical/", recursive = TRUE )
  }
  data.name <- data.type
  ## restore the input value of the selected data types
  if ( all( data.name == c( "temp.max", "temp.min", "prec" ) ) )
    data.name <- "default"
  ## save the extracted data
  save( list = c( paste0( "stations.", data.type ),
                  "station.positions" ),
       file = paste0( "./dwd_",
                     gsub( ".", "-", paste( data.name, 
                                           collapse = "_" ),
                          fixed = TRUE ),
                     ".RData" ) )
  setwd( old.dir )
  invisible()
}


##' @title Access to the manually imported weather data.
##' @details Load .RData file generated by \code{\link{download.data.dwd}}. This will be done interactively. First all .RData files present in the download folder will be printed. Than the user has to choose one of them by selecting its position in the presented character vector via a numeric value (e.g. 1 ). One can use the pick.default option to avoid the manual selecting.
##'
##' @param download.path Specifies the data is stored.
##' @param pick.default If TRUE it just search for the .RData file with "default" in it's name and skips the interactive picking
##' @param import Specifies if the data should be attached to the environment the function is called from or to the global one. The latter is necessary for deploying the app in the shiny server. Default = "local".
##'
##' @family import
##'  
##' @export
##' @return Has no specific output but attaches .RData file to search path.
##' @author Philipp Mueller
source.data <- function( pick.default = TRUE,
                        download.path = CLIMEX.PATH,
                        import = c( "local", "global" ) ){
  ## save the current path
  if ( missing( import ) )
    import <- "local"
  import <- match.arg( import )
  old.dir <- getwd()
  setwd( paste0( download.path, "downloads_dwd" ) )
  available.files <- grep( ".RData", list.files(), value = TRUE )
  if ( length( available.files ) == 0 )
    stop( "Folder does not contain a loadable .RData file!" )
  if ( pick.default ){
    ## just pick the result
    default.file <- grep( "default", available.files, value = TRUE )
    if ( length( default.file ) == 0 ){
      ## there is no default line. So back to the interactive picking
    } else {
      if ( import == "global" ){
        load( file = default.file[ 1 ], envir = .GlobalEnv )
      } else 
        load( file = default.file[ 1 ], envir = parent.frame() )
      setwd( old.dir )
      return( invisible() )
    }
  }
  print( available.files )
  chosen.file <- as.numeric( readline( prompt = "Choose file: " ) )
  print( paste( "loading", available.files[ chosen.file ] ) )
  if ( import == "global" ){
    load( file = available.files[ chosen.file ], envir = .GlobalEnv )
  } else 
    load( file = available.files[ chosen.file ], envir = parent.frame() )
  setwd( old.dir )
  invisible( )
}
