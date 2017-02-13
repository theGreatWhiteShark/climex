### Contains all modules associated with the leaflet map of the Climex
### app.

leafletClimexUI <- function( id ){
  # Create a namespace function using the provided id
  ns <- NS( id )
  tagList( 
      leafletOutput( ns( "map" ), width = "100%", height = 1000 ),
      ## 50px is the thickness of the top navigation bar
      absolutePanel( top = 50, right = 0, id = ns( "box" ),
                    sliderInput( ns( "sliderYears" ),
                                "Minimal length [years]",
                                0, 155, value = 65, step = 1 ),
                    tableOutput( ns( "table" ) ),
                    ## a plot of height 0? Well, its actually a
                    ## very nice trick since I need  a width value
                    ## for the generated pngs in the animation in
                    ## pixel. But I really want to make app to be
                    ## rendered nicely on different screen sizes.
                    ## Via the session$clientData I can access the
                    ## width and height of plots. Thus I can access
                    ## the width of this specific box via the
                    ## plotPlaceholder without seeing it at all.
                    plotOutput( ns( "placeholder" ),
                               height = 0, width = '100%' ) ),
      ## lift it a little but upwards so one can still see the
      ## card licensing
      absolutePanel( bottom = 32, right = 0,
                    id = ns( "markerBox" ),
                    sliderInput( ns( "sliderReturnLevel" ),
                                "Return level [years]",
                                30, 1000, value = 100 ),
                    actionButton( ns( "buttonDrawMarkers" ),
                                 "Calculate return levels" ) ) )
}
##' @title Leaflet interfaces to select stations or visualize spacial
##' information.
##' @details This module provides an interactive map to display the
##' locations of the individual stations. The user can choose
##' individual stations by clicking at them. In addition a dialog
##' will pop up telling the stations name, the length of the
##' time series and the 20, 50 and 100 year return level calculated
##' with the setting in the basic map without station positions        
##' select only those stations in Germany with a certain minimum
##' number of years.
##'
##' @param input Namespace input. For more details check out
##' \link{ \url{ http://shiny.rstudio.com/articles/modules.html } }
##' @param output Namespace output.
##' @param session Namespace session.
##' @param selectDataBase Character (select) input to determine the data
##' source. In the default installation there are three options:
##' c( "input", "DWD", "artificial data" ). The first one uses the data
##' provided as an argument to the call of the \code{\link{climex}}
##' function. The second one uses the database of the German weather
##' service (see \code{link{download.data.dwd}}). The third one allows
##' the user to produce random numbers distributed according to the GEV
##' or GP distribution. Determined by menuSelectDataBase.
##' Default = "DWD".
##' @param sliderMap Numerical (slider) input to determine the minimal
##' length (in years) of the time series to be displayed. Minimal value
##' is 0 and maximal is 155 (longest one in the DWD database), the
##' default value is 65 and the step width is 1.
##' @param selectDataType Character (select) input to determine which set
##' measurements should be used for the selected station. In case of the
##' default import of the DWD data, there are three options:
##' c( "Daily max. temp", "Daily min. temp", "Daily precipitation" ).
##' Determined by menuSelectDataSource2.
##' @param file.loading Reactive value allowing the user to load a time
##' series of class "xts" or "list" with "xts" as their elements into
##' the climex app.
##' @param x.input Input time series provided by the user while calling
##' the \code{\link{climex}} function. When supplying a different time
##' series using file.loading, this variable will be overwritten.
##' @param buttonMinMax Character (radio) input determining whether
##' the GEV/GP distribution shall be fitted to the smallest or biggest
##' vales. Choices: c( "Max", "Min ), default = "Max".
##' @param radioEvdStatistic Character (radio) input determining whether
##' the GEV or GP distribution shall be fitted to the data. Choices:
##' c( "GEV", "GP" ), default = "GEV".
##' @param data.blocking Reactive value returning a list containing three
##' elements: 1. the blocked time series, 2. the deseasonalized time
##' series, and 3. the pure time series.   
##' @param evd.fitting Reactive value containing the results of the fit
##' (\code{\link{fit.gev}} or \code{\link{fit.gpd}} depending on
##' radioEvdStatistic) to the blocked time series in
##' data.blocking()[[ 1 ]].
##' @param leafletMap_marker_click Reactive value containing the
##' coordinates of the last marker in the leaflet map, that was selected
##' by the user.
##' @param output_plotPlaceholderLeaflet_width Session client data
##' containing the width of a placeholder plot. This is used to determine
##' the actual width of the row in the leaflet map. Therefore all
##' output elements can have the same width and a consistent appearance.
##' Default = session$clientData$output_plotPlaceholderLeaflet_width
##' @param sliderThreshold Numerical (slider) input determining the
##' threshold used within the GP fit and the extraction of the extreme
##' events. Boundaries: minimal and maximal value of the deseasonalized
##' time series (rounded). Default: 0.8* the upper end point.
##' @param fit.interactive Function used to perform the actual GEV/GP
##' fit.
##' @param buttonDrawMarkers Logical (button) input determining whether
##' to calculate and render the return levels of all displayed stations.
##' This button is used for encapsulating all the reactive values in
##' this fitting procedure, since this one takes a lot of time.
##' 
##' @return 
##' @author Philipp Mueller 
leafletClimex <- function( input, output, session, selectDataBase,
                           sliderMap, selectDataType, file.loading,
                           x.input, buttonMinMax, radioEvdStatistic,
                           data.blocking, evd.fitting,
                           leafletMap_marker_click,
                           output_plotPlaceholderLeaflet_width,
                           sliderThreshold, fit.interactive,
                           buttonDrawMarkers
                           ){
  ## This functions extracts all stations containing more than a
  ## specified number of years of data
  data.chosen <- reactive( {
    if ( is.null( selectDataBase ) || is.null( sliderMap ) )
      return( NULL )
    ## the generation of the artificial data is handled in the
    ## data.selection reactive function
    if ( selectDataBase == "DWD" ){
      if ( is.null( selectDataType ) )
        return( NULL )
      selection.list <- switch( selectDataType,
                               "Daily max. temp." = stations.temp.max,
                               "Daily min. temp." = stations.temp.min,
                               "Daily precipitation" = stations.prec )
      ## to also cope the possibility of importing such position data
      positions.all <- station.positions
    } else if ( selectDataBase == "input" ){
      aux <- file.loading()
      if ( is.null( aux ) ){
        shinytoastr::toastr_error( "leafletClimex::data.chosen: no input file choosen in file.loading()!" )
        return( NULL )
      }
      if ( any( class( x.input ) == "xts" ) ){
        ## to assure compatibility
        aux <- list( x.input,
                    data.frame( longitude = NA, latitude = NA,
                               altitude = NA, name = "1" ) )
        names( aux[[ 1 ]] ) <- c( "1" )
        ## adding a dummy name which is going to be displayed in
        ## the sidebar
        return( aux )
      } else {
        ## two cases are accepted here: a list containing stations xts
        ## time series of contain such a list and a data.frame
        ## specifying the stations positions
        if ( class( x.input ) == "list" &&
             class( x.input[[ 1 ]] ) == "list" ){
          selection.list <- x.input[[ 1 ]]
          ## I will assume the second element of this list is a
          ## data.frame containing the coordinated, height and name of
          ## the individual stations
          positions.all <- x.input[[ 2 ]]
        } else {
          ## Just an ordinary list of xts elements
          selection.list <- x.input
          ##  dummy names
          if ( is.null( names( selection.list ) ) )
            names( selection.list ) <- as.character(
                seq( 1, length( selection.list ) ) ) 
          ## create a dummy
          positions.all <- data.frame(
              longitude = rep( NA, length( selection.list ) ),
              latitude = rep( NA, length( selection.list ) ),
              altitude = rep( NA, length( selection.list ) ),
              name = names( selection.list ) )
        }
      }
    }
    ## select time series with sufficient length 
    selection <- Reduce( c, lapply( selection.list, function( x )
      length( unique( lubridate::year( x ) ) ) ) ) >= sliderMap
    stations.selected <- selection.list[ selection ]
    positions.selected <- positions.all[ selection,  ]
    ## first element contains a list of all selected stations
    ## second element contains a data.frame with the longitude,
    ## latitude, altitude and name of each selected station
    return( list( stations.selected, positions.selected ) )
  } )
  
  ## create custom markers.
  ## This is essentially the same marker but with different colors.
  ## The selected one should be colored red and all the others blue. 
  blue.icon <-  makeIcon(
      iconUrl = paste0( system.file( "climex_app", package = "climex" ),
                       "/www/marker-icon.png" ),
      iconWidth = 25, iconHeight = 41, iconAnchorX = 12.5,
      iconAnchorY = 41,
      shadowUrl = paste0( system.file( "climex_app",
                                      package = "climex" ),
                         "/www/marker-shadow.png" ), shadowWidth = 41,
      shadowHeight = 41, shadowAnchorX = 12.5, shadowAnchorY = 41 )
  red.icon <-  makeIcon(
      iconUrl = paste0( system.file( "climex_app", package = "climex" ),
                       "/www/select-marker.png" ),
      iconWidth = 25, iconHeight = 41, iconAnchorX = 12.5,
      iconAnchorY = 41,
      shadowUrl = paste0( system.file( "climex_app",
                                      package = "climex" ),
                         "/www/marker-shadow.png" ), shadowWidth = 41,
      shadowHeight = 41, shadowAnchorX = 12.5, shadowAnchorY = 41 )
  
  ## Create the underlying map containing the Openstreetmap tile.
  ## This is the fundamental layer and all the markers will be
  ## added on top of it.
  output$map <- renderLeaflet( {
    leaflet() %>% fitBounds( 5, 46, 13, 55 ) %>%
      addTiles( "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
               attribution = '<code> Kartendaten: © <a href="https://openstreetmap.org/copyright">OpenStreetMap</a>-Mitwirkende, SRTM | Kartendarstellung: © <a href="http://opentopomap.org">OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a> </code>)' ) } )
  
  ## Depending on the number of minimal years and the selected data
  ## source markers will be placed at the geo-coordinates of the
  ## individual stations. 
  observe( {
    data.selected <- data.chosen()
    if ( !is.null( data.selected ) ){
      if ( any( is.na( c( data.selected[[ 2 ]]$longitude,
                         data.selected[[ 2 ]]$latitude ) ) ) ){
        ## I am dealing with either a placeholder or a compromised
        ## data.frame. Anyway, the leaflet map can not handle it
        return( NULL )
      }
      leafletProxy( "leaflet-map" ) %>%
        clearGroup( "stations" ) %>%
        addMarkers( data = data.selected[[ 2 ]], group = "stations",
                   lng = ~longitude,
                   icon = blue.icon, lat = ~latitude,
                   options = popupOptions( closeButton = FALSE ) )
    } } )
  
  ## The purpose of this function is to supply a data.frame
  ## containing the 50/100/500 year return level of all selected
  ## stations. Lets see how fast it will be. Maybe I will just
  ## calculate one return level per station.
  ## This will be calculated on demand (as soon as the user clicks
  ## the corresponding form)
  calculate.chosen.return.levels <- reactive( {
    data.selected <- data.chosen()
    ## selected return level
    return.level.year <- input$sliderReturnLevel 
    ## wait for initialization
    if ( is.null( input$sliderReturnLevel ) ||
         is.null( data.selected ) )
      return( NULL )
    ## if no geo-coordinates are provided for the time series,
    ## don't calculate the return levels
    if ( any( is.na( c( data.selected[[ 2 ]]$longitude,
                       data.selected[[ 2 ]]$latitude ) ) ) )
      return( NULL )
    ## clean the stations
    data.cleaned <- lapply( data.selected[[ 1 ]], cleaning.interactive )
    ## deseasonalize them
    data.deseasonalized <- lapply( data.cleaned,
                                  deseasonalize.interactive )
    ## block them
    data.blocked <- lapply( data.deseasonalized, blocking.interactive )
    ## choose whether to calculate the GEV or GP parameters
    if ( radioEvdStatistics == "GEV" ){
      model <- "gev"
      threshold <- NULL
    } else {
      model <- "gpd"
      threshold <- sliderThreshold
    }
    ## calculate the return level and append it to the data.selected[[ 2 ]] data.frame
    return.level.vector <- rep( NaN, length( data.blocked ) )
    for ( rr in 1 : length( data.blocked ) ){
      return.level.vector[ rr ] <-
        climex::return.level( fit.interactive( data.blocked[[ rr ]] ),
                             return.period = return.level.year,
                             model = model, error.estimation = "none",
                             total.length = length(
                                 data.selected[[ 1 ]][[ rr ]] ),
                             threshold = threshold )
      }
    data.selected[[ 2 ]]$return.level <- return.level.vector
    return( data.selected[[ 2 ]] )
  } ) 
  
  observe( {
    ## the calculation of all the return levels of the stations
    ## just takes too long. I put it in a different observe object
    ## and the only way to start the calculation will be using a
    ## button
    if ( is.null( buttonDrawMarkers ) ||
         buttonDrawMarkers < 1 )
      return( NULL )
      isolate( data.return.levels <- calculate.chosen.return.levels() )
      if ( !is.null( data.return.levels ) ){
        if ( any( is.na( c( data.return.levels$longitude,
                           data.return.levels$latitude ) ) ) ){
          ## I am dealing with either a placeholder or a compromised
          ## data.frame. Anyway, the leaflet map can not handle it
          return( NULL )
        }
        ## Same trick as in the animation tab: I use a plot of height
        ## 0 to obtain the current width of the element I want to
        ## place the legend next to. Unfortunately I do not know of
        ## any other trick right now to adjust an objects width
        ## according to the current screen width (CSS3 magic)
        isolate(
            map.width <-
              session$clientData&output_plotleaflet-placeholder_width )
        ## range of the return levels
        color.max <- max( data.return.levels$return.level )
        color.min <- min( data.return.levels$return.level )
        ## create a palette for the return levels of the individual
        ## circles
        palette <- colorNumeric( c( "navy", "skyblue", "limegreen",
                                   "yellow", "darkorange",
                                   "firebrick4" ),
                                c( color.min, color.max ) )
        map.leaflet <- leafletProxy( "leaflet-map" )
        map.leaflet <- clearGroup( map.leaflet, "returns" )
        map.leaflet <- addCircleMarkers(
            map.leaflet, data = data.return.levels,
            group = "returns", lng = ~longitude,
            color = ~palette( return.level ), lat = ~latitude,
            options = popupOptions( closeButton = FALSE ),
            fillOpacity = .8 )
        ## layer control to turn the return level layer on and off
        map.leaflet <- addLayersControl( map.leaflet,
                                        baseGroups = c( "stations",
                                                       "returns" ),
                                        position = "bottomright",
                                        options = layersControlOptions(
                                            collapsed = FALSE ) )
        map.leaflet <- addLegend( map.leaflet, pal = palette,
                                 values = c( color.min, color.max ),
                                 layerId = "leafletLegend",
                                 orientation = "horizontal",
                                 width = map.width )
        return( map.leaflet )
      } } )
  output$table <- renderTable( {
    data.selected <- data.chosen()
    if ( is.null( data.selected ) ||
         is.null( leafletMap_marker_click) )
      return( NULL )
    map.click <- leafletMap_marker_click
    station.name <- as.character(
        data.selected[[ 2 ]]$name[ which(
                                 data.selected[[ 2 ]]$latitude %in%
                                 map.click$lat &
                                 data.selected[[ 2 ]]$longitude %in%
                                 map.click$lng ) ] )
    leafletProxy( "leaflet-map" ) %>%
      clearGroup( group = "selected" )
    leafletProxy( "leaflet-map" ) %>%
      addMarkers( data = map.click, group = "selected", icon = red.icon,
                 lng = ~lng, lat = ~lat )
    ## calculate the GEV fit and various return levels
    x.fit.gev <- evd.fitting()
    x.data <- data.blocking()
    if ( is.null( x.fit.gev ) )
      return( NULL )
    if ( radioEvdStatistics == "GEV" ){
      model <- "gev"
    } else {
      model <- "gpd"
    }
    if ( buttonMinMax == "Max" || radioEvdStatistics == "GP" ){
      x.return.level <- climex::return.level(
                                    x.fit.gev,
                                    return.period = c( 100, 50, 20 ),
                                    model = model,
                                    error.estimation = "none",
                                    threshold = x.fit.gev$threshold,
                                    total.length = x.data[[ 1 ]] )
    } else
      x.return.level <- ( -1 )* climex:::return.level(
                                             x.fit.gev,
                                             return.period = c( 100,
                                                               50, 20 ),
                                             model = model,
                                             error.estimation = "none" )
    x.df <- data.frame( names = c( "100y return level",
                                  "50y return level",
                                  "20y return level" ),
                       x.return.level, row.names = NULL )
    colnames( x.df ) <- c( station.name, "" )
    x.df
  }, rownames = FALSE, digits = 3, width = 220 )
}
