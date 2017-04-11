#.First <- function(){
       ## Enabling the shiny server and the climex web app
       library( shiny )
       library( shinydashboard )
       library( leaflet )
       library( dygraphs )
       library( xts )
       library( ggplot2 )
       library( htmltools )
       library( climex )

       options( climex.path = "/srv/shiny-server/assets/" )
       climex::source.data()
       image.folder <<- "/srv/shiny-server/assets/images"
       options( shiny.usecairo = TRUE )
       options( encoding = "latin1" )

#}	