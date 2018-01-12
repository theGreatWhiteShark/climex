##' @title The Climex app initialization
##' @description Shiny app combining most of the tools used in extreme
##'   value analysis using GEV fitting via maximum likelihood.
##'
##' @details It possible to provide a time series of type \pkg{xts} to
##' a list of elements of this type. This app need the its own css file.
##' This function will be exclusively called when the climex app is run
##' on localhost. In order to assure its correct behavior the necessary
##' file/folder structure in the climex.path (R option which has
##' to be set beforehand; "~/R/climex/" on default) will be check and
##' if necesary generated. The input time series is accepted in three
##' different formats
##' \enumerate{
##'  \item As a single time series in the formatted using the \pkg{xts}
##' package 
##'  \item As a named list of various xts time series 
##'  \item As a list containing two elements;  a list of xts time
##' series and a data.frame with the columns \emph{longitude},
##' \emph{latitude}, \emph{altitude}, \emph{name} corresponding to the
##' stations geo-coordinates, height and name 
##' }
##'
##' @family climex
##'
##' @return starts a shiny app.
##' @export
##' @import shiny
##' @import leaflet
##' @importFrom xts xts
##' @author Philipp Mueller 
climex <- function(){
  climex.path <- getOption( "climex.path" )
  ## this is unfortunately necessary since some JavaScript scripts
  ## are written out and some images are plotted which have to be
  ## accessible within the shiny app.
  ## If there are not the appropriate files present to run the shiny
  ## app, the apps as well as the folder structure has to be generated
  if ( !dir.exists( paste0( climex.path, "app" ) ) )
    dir.create( paste0( climex.path, "app" ) )
  if ( !dir.exists( paste0( climex.path, "app/www" ) ) )
    dir.create( paste0( climex.path, "app/www" ) )
  ## In order to display the animation the app needs the jquery
  ## scianimator. Since it is not able to access a system file on
  ## its own the wrapper climex() will copy it to the apps folder
  if ( !dir.exists( paste0( climex.path,
                           "app/www/jquery.scianimator.min.js" ) ) ){
    file.copy( paste0( system.file( "climex_app", package = "climex" ),
                      "/js/jquery.scianimator.min.js" ),
              to = paste0( climex.path,
                          "app/www/jquery.scianimator.min.js" ),
              overwrite = TRUE )
  }
  ## source of the gif: http://i.imgur.com/seuaOqf.gif
  ## since this is a non-commercial product there shouldn't be
  ## any problems http://imgur.com/tos
  file.copy( paste0( system.file( "climex_app", package = "climex" ),
                    "/js/loadingGif.js" ),
            to = paste0( climex.path, "app/www/loadingGif.js" ),
            overwrite = TRUE ) 
  file.copy( paste0( system.file( "climex_app", package = "climex" ),
                    "/res/loading.gif" ),
            to = paste0( climex.path, "app/www/loading.gif" ) )      
  writeLines( "shinyUI( climex.ui() )", con = paste0( climex.path,
                                                     "app/ui.R" ) )
  writeLines( "shinyServer( climex.server )",
             con = paste0( climex.path, "app/server.R" ) )
  ## If one or more time series without any map information are
  ## provided as input arguments, start the app in the "General" tab
  ## right away
  ## if ( !is.null( x.input ) && length( x.input ) != 2 ){ 
  ##   writeLines( "shinyUI( climex.ui( selected = 'General' ) )",
  ##              con = paste0( climex.path, "app/ui.R" ) )
  ## }
  runApp( paste0( climex.path, "app" ) )
}


##' @title The Climex app server
##' @description Server-side part of the \code{\link{climex}} function.
##'
##' @details Since it grew organically most of the features are defined
##' inside this function. Okay, why did I decided to define the
##' climex.server and the climex.ui function? Since in this way I can
##' provide a local and a server side variant of the code which is
##' maintained in the same place. I do not really like the idea of
##' installing the newest version of the code from Github and than
##' copy/pasting the code in some files somewhere in
##'   /srv/shiny-server.
##'
##' @param input Namespace input. For more details check out
##' \url{http://shiny.rstudio.com/articles/modules.html}
##' @param output Namespace output.
##' @param session Namespace session.
##'
##' @return Function acting as the shiny server.
##' @import shiny
##' @importFrom xts xts
##' @import dygraphs
##' @import ggplot2
##'
##' @family climex
##' 
##' @export
##' 
##' @author Philipp Mueller 
climex.server <- function( input, output, session ){
  ## Create a custom environment to host the variables `last.values`,
  ## `last.1`, `last.2`, and `last.3` in, as well as the station data
  ## in. By referring to this environment the corresponding variables
  ## do not have to be defined globally.
  climex.environment <- new.env( parent = emptyenv() )

  ## Load the station data and assign it to the custom environment.
  source.data( pick.default = TRUE, envir = climex.environment )
  
########################################################################
######### Customizing the sidebar and launching its reactives ##########
########################################################################
  ## Everything in my code is always of the style this.is.a.name. So
  ## why do I use the camel case thisIsAName for the shiny objects?
  ## Well, since CSS file do not support the point separator.
  ## Type of database (input, DWD, artificial data)
  output$sidebarDataBase <- climex:::sidebarDataBase( session )
  ## Individual station or location (GEV)/scale(GP) for artificial data
  output$sidebarDataSource <-
    climex:::sidebarDataSource( reactive( input$selectDataBase ),
                               reactive( input$radioEvdStatistics ),
                               reactive.chosen, selected.station )
  ## Measurement type or scale (GEV)/shape(GP) for artificial data
  output$sidebarDataType <-
    climex:::sidebarDataType( reactive( input$selectDataBase ),
                             reactive( input$radioEvdStatistics ) )
  ## File input prompt for "input" or shape for GEV artificial data
  output$sidebarLoading <-
    climex:::sidebarLoading( session, reactive( input$selectDataBase ),
                            reactive( input$radioEvdStatistics ) )
  ## Removing incomplete years (GEV) or clusters (GP)
  output$sidebarCleaning <-
    climex:::sidebarCleaning( reactive( input$radioEvdStatistics ),
                             reactive( input$selectDataBase ) )
  ## Slider for choosing the length of the artificial time series
  output$sidebarSeriesLength <-
    climex:::sidebarSeriesLength( reactive( input$selectDataBase ) )
  ## Introducing a dropdown menu for the deseasonalization in the Options
  ## box in the General tab.
  output$deseasonalizeSelection <-
    climex:::deseasonalizeSelection(
                 reactive( input$selectDataBase ) )
  ## A radioButton to determine whether to calculate the minimal or
  ## maximal extremes
  output$generalButtonMinMax <-
    climex:::generalButtonMinMax(
                 reactive( input$radioEvdStatistics ),
                 reactive( input$selectDataType ) )
  ## Displaying a loading gif whenever the app is busy
  callModule( climex:::sidebarLoadingGif, "busy" )
  ## Display a link to the app's imprint whenever it is run using
  ## shiny-server
  output$sidebarImprint <- climex:::sidebarImprint( session )
  ## Reactive value listening for file input
  reactive.loading <- climex:::file.loading(
                                   reactive( input$fileInputSelection ) )
  ## Reactive value which holds the selected stations chosen either via
  ## the leaflet map or the sidebar
  reactive.chosen <- climex:::data.chosen(
                                  reactive( input$selectDataBase ),
                                  reactive( input$sliderYears ),
                                  reactive( input$selectDataType ),
                                  reactive.loading,
                                  climex.environment )
  ## Reactive value selecting a specific time series according to the
  ## choices in the sidebar/leaflet map
  reactive.selection <-
    climex:::data.selection(
                 reactive.chosen, reactive( input$selectDataSource ),
                 reactive( input$selectDataBase ),
                 reactive( input$sliderThreshold ),
                 reactive( input$radioEvdStatistics ),
                 reactive( input$sliderArtificialDataLocation ),
                 reactive( input$sliderArtificialDataScale ),
                 reactive( input$sliderArtificialDataShape ),
                 reactive( input$buttonDrawTS ),
                 reactive( input$sliderSeriesLength ) )
########################################################################
  
########################################################################
#################### Data preprocessing and fitting ####################
########################################################################
  ## Slider input determining the block length (GEV) or threshold (GP)
  output$generalExtremeExtraction <-
    climex:::generalExtremeExtraction(
                 reactive( input$radioEvdStatistics ),
                 climex:::deseasonalize.interactive,
                 reactive( input$selectDeseasonalize ),
                 reactive( input$buttonMinMax ), reactive.selection,
                 reactive( input$selectDataBase ) )
  ## Reactive value containing a list of the extracted extreme events,
  ## the deseasonalized and pure time series. In addition, it's also
  ## performing both the extraction and the deseasonalization
  reactive.extreme <-
    climex:::data.extremes(
                 reactive.selection, reactive( input$radioEvdStatistics ),
                 reactive( input$sliderBlockLength ),
                 reactive( input$sliderThreshold ),
                 reactive( input$checkboxDecluster ),
                 climex:::deseasonalize.interactive,
                 reactive( input$selectDeseasonalize ),
                 reactive( input$selectDataBase ),
                 reactive( input$buttonMinMax ),
                 climex:::extremes.interactive,
                 climex:::cleaning.interactive,
                 reactive( input$checkboxIncompleteYears ) )
  ## Reactive value performing the GEV/GP fit to the selected time series
  ## and providing the fitted object of class 'climex.fit.gev' or
  ## 'climex.fit.gpd'.
  reactive.fitting <- climex:::data.fitting(
                                   reactive.extreme,
                                   reactive.rows,
                                   climex:::fit.interactive,
                                   reactive( input$radioEvdStatistics ),
                                   reactive( input$buttonMinMax ),
                                   reactive( input$sliderThreshold ),
                                   reactive( input$selectDataBase ) )
########################################################################
  
########################################################################
############### Plotting of the time series and fit ####################
########################################################################
  ## Displaying of the original, intermediate and final time series.
  ## Reactive value containing a logical vector indicating which element
  ## of reactive.extreme()[[ 1 ]] should still be used after clicking and
  ## brushing in the extreme's ggplot2 plot. In addition the plotting and
  ## rendering of the extremes, deseasonalized and pure time series is
  ## done in here.
  reactive.rows <- callModule( climex:::generalTimeSeriesPlot, "ts",
                              reactive.extreme,
                              reactive( input$selectDataBase ),
                              reactive( input$selectDataType ),
                              climex:::function.get.y.label,
                              reactive( input$radioEvdStatistics ),
                              reactive( input$sliderThreshold ),
                              reactive( input$buttonMinMax ) )
  ## Plotting of the fitted distribution and the corresponding PP, QQ
  ## and return level goodness-of-fit plots.
  callModule( climex:::generalFitPlot, "fit", reactive.extreme,
             reactive.rows, reactive.fitting,
             reactive( input$buttonMinMax ),
             reactive( input$radioEvdStatistics ),
             climex:::function.get.y.label,
             reactive( input$selectDataBase ), 
             reactive( input$selectDataType ),
             reactive( input$sliderThreshold ) )
  ## Table containing the fits results. Those four global variable will
  ## store the previous results of the GEV/GP fitting. This is
  ## unfortunately necessary in order to highlight the progress in the
  ## table red or green.

  ## Assign the place holder to the custom environment. This way they
  ## are accessible between function calls without handing them over,
  ## but are not defined globally either.
  climex.environment$last.values <-
    climex.environment$last.1 <-
      climex.environment$last.2 <-
        climex.environment$last.3 <- rep( 0,  )
  output$generalFitStatistics <-
    climex:::generalFitStatistics( reactive.fitting, reactive.extreme,
                                  reactive( input$sliderThreshold ),
                                  reactive( input$buttonMinMax ),
                                  reactive( input$radioEvdStatistics ),
                                  climex:::color.table,
                                  climex.environment )
########################################################################

  ## Module providing the leaflet map tab and returning a reactive value
  ## containing all stations with a length longer than the minimal length
  ## slider in the Leaflet tab.
  selected.station <- callModule(
      climex:::leafletClimex, "leaflet", reactive.chosen,
      reactive( input$buttonMinMax ),
      reactive( input$radioEvdStatistics ),
      reactive( input$sliderYears ), reactive.extreme,
      reactive.fitting,
      reactive( input$sliderThreshold ), climex:::fit.interactive,
      climex:::cleaning.interactive, climex:::deseasonalize.interactive,
      climex:::extremes.interactive, reactive( input$selectDataSource ),
      reactive( input$checkboxIncompleteYears ),
      reactive( input$checkboxDecluster ),
      reactive( input$selectDeseasonalize ),
      reactive( input$sliderBlockLength ),
      reactive( input$selectDataBase ), climex.environment )
}

##' @title The Climex app UI
##' @description The user interface for the \code{\link{climex}}
##'   function. 
##'
##' @param selected Choose which tab is supposed to be selected when
##' starting the app
##'
##' @details Contains all the HTML codes.
##'
##' @return HTML code of the interface
##' @import shiny
##' @importFrom shinydashboard box
##' @importFrom shinydashboard dashboardPage
##' @importFrom shinydashboard dashboardHeader
##' @importFrom shinydashboard dashboardSidebar
##' @importFrom shinydashboard dashboardBody
##' @importFrom shinydashboard menuItemOutput
##' @importFrom shinydashboard menuItem
##' @importFrom shinydashboard renderMenu
##' @importFrom shinydashboard sidebarMenu
##' @importFrom shinydashboard tabItem
##' @importFrom shinydashboard tabItems
##' @importFrom shinydashboard tabBox
##' @importFrom dygraphs dygraphOutput
##' @importFrom htmltools includeCSS
##' @importFrom htmltools h2
##' 
##' @export
##'
##' @family climex
##' 
##' @author Philipp Mueller 
climex.ui <- function( selected = c( "Map", "General" ) ){
  if ( missing( selected ) )
    selected <- "Map"
  selected <- match.arg( selected )
  dashboardPage(
    shinytoastr::useToastr(),
    header = dashboardHeader(
        title = a(
            "Climex",
            href = "https://github.com/theGreatWhiteShark/climex",
            id = "climexLink" )
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem( "Map", tabName = "tabMap",
                 icon = icon( "leaf", lib = "glyphicon" ),
                 selected = ifelse( selected == "Map", TRUE, FALSE ) ),
        menuItem( "General", tabName = "tabGeneral",
                 icon = icon( "stats", lib = "glyphicon" ),
                 selected = ifelse( selected == "General",
                                   TRUE, FALSE ) ),
        climex:::sidebarDataBaseInput(),
        climex:::sidebarDataSourceInput(),
        climex:::sidebarDataTypeInput(),
        climex:::sidebarLoadingInput(),
        climex:::sidebarSeriesLengthInput(),
        climex:::sidebarCleaningInput(),
        climex:::sidebarLoadingGifOutput( "busy" ),
        climex:::sidebarImprintInput() ) ),
    body = dashboardBody(
      ## shinyjs::useShinyjs(),
      includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                         "/css/climex.css" ) ),
      includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                         "/css/reset.css" ) ),
      includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                         "/css/styles.css" ) ),
      includeCSS( paste0( system.file( "climex_app", package = "climex" ),
                         "/css/scianimator.css" ) ),
      tabItems(
        tabItem(
          tabName = "tabMap",
          tags$style( type = "text/css",
                     "#leaflet-map {height: calc(100vh - 80px) !important;}" ),
         climex:::leafletClimexUI( "leaflet" ) ),
        tabItem(
          tabName = "tabGeneral",
          ## In order guarantee the correct behavior of the rendering
          ## of the boxes and tables for smaller screen sizes too I
          ## assign customized classes to the boxes and reconfigure
          ## their ordering using CSS3 if the max-width is below a
          ## certain threshold
          fluidRow(
            climex:::generalFitPlotOutput( "fit" ),
            box( title = h2( "Options" ), width = 4,
              height = 505, background = "orange",
              id = "boxGevStatistics",
              radioButtons( "radioEvdStatistics", 
                           label = "Limiting distribution", inline = TRUE,
                           choices = c( "GEV", "GP" ),
                           selected = "GEV" ),
              climex:::generalExtremeExtractionInput(),
              climex:::generalButtonMinMaxInput(),
              climex:::deseasonalizeSelectionInput() ) ),
          fluidRow(
              climex:::generalFitStatisticsTable(),
              climex:::generalTimeSeriesPlotOutput( "ts" ) ) ))))
}
