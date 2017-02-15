### Contains all modules associated with the sidebar of the climex app.

##' @title Selecting the database
##' @details Provides the shinydashboard::menuItemOutput for \code{\link{
##' sidebarDataBase}}. See the later one for details.
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##' 
##' @return menuItemOutput
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
##' @import shiny
##'
##' @family sidebar
##'
##' @return renderMenu
##' @author Philipp Mueller 
sidebarDataBase <- function(){
  renderMenu( {
    selectInput( "selectDataBase", "Data base",
                choices = c( "input", "DWD", "artificial data" ),
                selected = "DWD" )
    } )
}

##' @title Sidebar menu selection
##' @details Provides the shinydashboard::menuItemOutput for \code{\link{
##' sidebarDataSource}}. See the later one for details
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return menuItemOutput
##' @author Philipp Mueller 
sidebarDataSourceInput <- function(){
  menuItemOutput( "sidebarDataSource" )
}

##' @title Sidebar menu selection
##' @details Provides the second selection menu in the sidebar.
##' If input$selectDataBase, provided by \code{\link{sidebarDataBase}},
##' was set to "artificial data", the menu will be a numerical input
##' slider to provide the location parameter for the GEV or scale
##' parameter for the GP distribution. Else it will be a drop down menu
##' listing the names of all available stations (see \code{\link{
##' data.chosen}})
##'
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "input", "DWD", "artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param reactive.chosen Reactive value containing a list of the list
##' of all provided stations and a data.frame containing the meta data.
##' @param selected.station Reactive value provided by the \code{\link{
##' leafletClimex}} function. It contains the name of the station selected
##' by the user by clicking on the leaflet map. It's provided as a
##' character and NULL if no click occurred so far. 
##'
##' @import shiny
##'
##' @family sidebar
##'
##' @return renderMenu
##' @author Philipp Mueller 
sidebarDataSource <- function( selectDataBase, radioEvdStatistics,
                               reactive.chosen, selected.station ){
  renderMenu( {
    ## If artificial data was choosen as the input source, display
    ## a slider for the location parameter of the parent GEV
    ## distribution
    if ( !is.null( selectDataBase() ) &&
         selectDataBase() == "artificial data" ){
      if ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ){
        ## For GEV data
        sliderInput( "sliderArtificialDataLocation",
                    "location", -30, 30, 1, round = 15 )
      } else {
        ## For GP data just skip the slider for the location parameter
        sliderInput( "sliderArtificialDataScale",
                    "scale", 0, 4, .8, round = -2 )
      }
    } else {
      ## In case of the DWD data, just show the stations which amount of
      ## years is larger than the specified number (slider)
      ## list(stations.selected, position.selected). This already does
      ## the look-up in file.loading(), input$selectDataBase and
      ## input$selectDataType so no need to repeat these steps.
      data.selected <- reactive.chosen()
      ## Since this is not a crucial element and the other elements have
      ## a fallback to the Potsdam time series we can just wait until
      ## input$selectDataBase and input$sliderYears( used in data.chosen )
      ## are initialized 
      if ( is.null( selectDataBase() ) ||
           is.null( data.selected ) ){
        return( NULL )
      }
      ## Use the leaflet map to choose a individual station
      if ( !is.null( selected.station() ) ){
        station.name <- selected.station()
      } else {
        ## If not just use the Potsdam station for the DWD data
        ## or the first station in the list for every other source
        if ( selectDataBase() == "DWD" ){
          station.name <- "Potsdam"
        } else
          station.name <- as.character(
              data.selected[[ 2 ]]$name[ 1 ] )
      }
      ## export drop-down menu
      selectInput( "selectDataSource", "Station",
                  choices = as.character(  data.selected[[ 2 ]]$name ),
                  selected = as.character( station.name ) )
    }
  } )
}

##' @title Sidebar menu selection
##' @details Provides the shinydashboard::menuItemOutput for \code{\link{
##' sidebarDataType}}. See the later one for details
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return menuItemOutput
##' @author Philipp Mueller 
sidebarDataTypeInput <- function(){
  menuItemOutput( "sidebarDataType" )
}

##' @title Sidebar menu selection
##' @details Provides the third selection menu in the sidebar.
##' If input$selectDataBase, provided by \code{\link{sidebarDataBase}},
##' was set to "artificial data", the menu will be a numerical input
##' slider to provide the scale parameter for the GEV or shape
##' parameter for the GP distribution. Else it will be a drop down menu
##' listing the different types of data available at the chosen station.
##'
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "input", "DWD", "artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' 
##' @import shiny
##'
##' @family sidebar
##'
##' @return renderMenu
##' @author Philipp Mueller
sidebarDataType <- function( selectDataBase, radioEvdStatistics ){
  renderMenu( {
    if ( !is.null( selectDataBase() ) ){
      if ( selectDataBase() == "DWD" ){
        selectInput( "selectDataType", "Measured variable",
                    choices = c( "Daily max. temp.", "Daily min. temp.",
                                "Daily precipitation" ),
                    selected = "Daily max. temp" )
      } else if ( selectDataBase() == "artificial data" ){
        if ( is.null( radioEvdStatistics() ) ||
             radioEvdStatistics() == "GEV" ){
          sliderInput( "sliderArtificialDataScale", "scale", 0, 4,
                      0.8, round = -2 )
        } else {
          ## For GP distributed data
          sliderInput( "sliderArtificialDataShape", "shape", -1.5, 1.5,
                      -.25, round = -2 )
        }
      } else if ( selectDataBase() == "input" ){
        NULL
      }
    } else
      NULL } )
}

##' @title Sidebar menu selection
##' @details Provides the shinydashboard::menuItemOutput for \code{\link{
##' sidebarLoading}}. See the later one for details
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return menuItemOutput
##' @author Philipp Mueller 
sidebarLoadingInput <- function(){
  menuItemOutput( "sidebarLoading" )
}

##' @title Sidebar menu selection
##' @details Provides the fourth selection menu in the sidebar.
##' If input$selectDataBase, provided by \code{\link{sidebarDataBase}},
##' was set to "artificial data", the menu will be a numerical input
##' slider to provide the shape parameter for the GEV or NULL for the GP
##' distribution. Else it will be a file input for the user to load
##' additional station data.
##'
##' @param session Namespace session. For details check out
##' \link{ \url{ http://shiny.rstudio.com/articles/modules.html}} 
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "input", "DWD", "artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##' @param radioEvdStatistics Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' 
##' @import shiny
##'
##' @family sidebar
##'
##' @return renderMenu
##' @author Philipp Mueller
sidebarLoading <- function( session, selectDataBase,
                           radioEvdStatistics ){
  renderMenu( {
    if ( is.null( selectDataBase() ) )
      return( NULL )
    if ( selectDataBase() == "artificial data" &&
         ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ) ){
      sliderInput( "sliderArtificialDataShape", "shape", -1.5, 1.5,
                  -0.25, round = -2 )
    } else if ( selectDataBase() == "input" && (
      session$clientData$url_hostname == "localhost" ||
      session$clientData$url_hostname == "127.0.0.1"  ) ){
      ## due to security concerns only allow the file selection on
      ## localhost
      fileInput( "fileInputSelection", "Choose a .RData file" )
    } else
      return( NULL )
  } )
}
