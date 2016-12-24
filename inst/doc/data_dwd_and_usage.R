## ----prerequisites, cache = TRUE-----------------------------------------
## example path. Feel free to choose your own.
CLIMEX.PATH <<- "~/R/climex/"
if ( !dir.exists( CLIMEX.PATH ) )
    dir.create( CLIMEX.PATH, recursive = TRUE )

## ----downloading, eval = FALSE, cache = TRUE, dependson = "prerequisites"----
#  require( climex )
#  download.data.dwd() # this will take a while

## ----dwd-loading, eval = FALSE, cache = TRUE, dependson = "downloading"----
#  source.data( pick.default = TRUE )

## ----loading, cache = TRUE, dependson = "prerequisites"------------------
require( climex )
data( temp.potsdam )

## convenience function for plotting xts class time series using ggplot2
ttplot( temp.potsdam )

## ----incomplete-years, cache = TRUE, dependson = "loading"---------------

temp.potsdam.complete <- remove.incomplete.years( temp.potsdam )

ttplot( temp.potsdam.complete )


## ----anomalies, cache = TRUE, dependson = "incomplete-years"-------------

temp.potsdam.anomalies <- anomalies( temp.potsdam.complete )

ttplot( temp.potsdam.anomalies )


## ----blocking, cache = TRUE, dependson = "anomalies"---------------------

## Per default the block length of the block() function will be one year
temp.potsdam.blocked <- block( temp.potsdam.anomalies )

ttplot( temp.potsdam.blocked )


## ----fit, dependson = "blocking"-----------------------------------------
## The result will be a list of objects similar to the output of the
## optim function.
gev.potsdam <- climex::gev.fit( temp.potsdam.blocked )

return.level.potsdam <- climex::rlevd( gev.potsdam )
print( return.level.potsdam )

