##' Data set of daily maximum temperatures of the Potsdam station provided by the DWD. If you intend to use this data please look at the \href{ftp://ftp-cdc.dwd.de/pub/CDC/Terms_of_use.pdf}{terms of use}. The data is stored in the \pkg{xts} format.
##'
##' The data is stored in a data.frame and assigned to the class 'bulk'. It contains 17 columns. In the first is the date of the measurement, in the 4. the daily mean temperature and in the 8. and 9. the lowest respective highest temperature of the individual day. All artifacts detected were replaced by -999. Originally the date was placed in the second column but the columns where switched to increase compatibility.
##'
##' @format Class \pkg{xts}
##' @source \url{http://www.dwd.de/}
##' @name temp.potsdam
NULL