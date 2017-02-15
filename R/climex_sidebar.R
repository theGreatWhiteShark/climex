### Contains all modules associated with the sidebar of the climex app.

##' @title Selecting the database
##' @details For now there are three different data sources you can
##' choose:the input provided when calling climex::climex() on localhost
##' or a file loaded via the sidebar, the station data of the German
##' weather service (DWD) and artificial data sampled from a GEV
##' distribution. Beware: it's not a true module! Using the namespaces
##' does not really make sense because it has to be accessed from outside
##' of this context.
##'
##' @return Menu output containing a selection input.
##' @author Philipp Mueller 
sidebarDataBaseInput <- function(){
  menuItemOutput( "sidebarDataBase" )
}

##' @title Selecting the database
##' @details For now there are three different data sources you can
##' choose:the input provided when calling climex::climex() on localhost
##' or a file loaded via the sidebar, the station data of the German
##' weather service (DWD) and artificial data sampled from a GEV
##' distribution. Beware: it's not a true module! Using the namespaces
##' does not really make sense because it has to be accessed from outside
##' of this context.
##'
##' @return Menu output containing a selection input.
##' @author Philipp Mueller 
sidebarDataBase <- function(){
  renderMenu( {
    selectInput( "selectDataBase", "Data base",
                choices = c( "input", "DWD", "artificial data" ),
                selected = "DWD" )
    } )
}
