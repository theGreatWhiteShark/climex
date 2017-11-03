## This file seems to be some sort of relic but the namespacing did not work otherwise.
options( shiny.maxRequestSize = 1000* 1024^2 )

shinyServer( climex::climex.server )


 
