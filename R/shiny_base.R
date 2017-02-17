##' @title Shiny app combining most of the tools used in extreme value
##' analysis using GEV fitting via maximum likelihood.
##'
##' @details It possible to provide a time series of type \pkg{xts} to
##' a list of elements of this type. This app need the its own css file.
##' This function will be exclusively called when the climex app is run
##' on localhost. In order to assure its correct behavior the necessary
##' file/folder structure in the CLIMEX.PATH (global variable which has
##' to be set beforehand; see vignette for details) will be check and
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
##' @family shiny
##'
##' @return starts a shiny app.
##' @export
##' @import shiny
##' @import leaflet
##' @import xts
##' @author Philipp Mueller 
climex <- function(){
  source.data( pick.default = TRUE, import = "global" )
  ## if ( missing( x.input ) ){
  ##   ## since we define this amigo global we also have to set it to
  ##   ## NULL by hand when not providing it to this function
  ##   x.input <<- NULL
  ## }
  ## if ( !is.null( x.input ) ){
  ##   ## checking for the right format of the input series
  ##   if ( any( class( x.input ) == "xts" ) ){
  ##     ## global because the other functions have to have access to it
  ##     x.input <<- x.input
  ##   } else if ( class( x.input ) == "list" ){
  ##     if ( all( Reduce( c, lapply( x.input, function ( y )
  ##       any( class( x ) == "xts" ) ) ) ) || (
  ##         length( x.input ) == 2 &&
  ##         "list" %in% Reduce( c, lapply( x.input, class ) ) &&
  ##         "data.frame" %in% Reduce( c, lapply( x.input, class ) ) ) ) {
  ##       x.input <<- x.input
  ##     } else {
  ##       x.input <<- NULL
  ##       stop( "the input time series has the wrong format!" )
  ##     }
  ##   } else {
  ##     x.input <<- NULL
  ##     stop( "the input time series has the wrong format!" )
  ##   }
  ## }
  if ( !"CLIMEX.PATH" %in% ls( envir = .GlobalEnv ) )
    stop( "Please define a global variable named CLIMEX.PATH (storing of the DWD data and the app interna) (see vignette for details)!" )
  ## will contain the folder in which the images of the animation
  ## can be found
  image.folder <<- paste0( CLIMEX.PATH, "app/www/images" )
  ## this is unfortunately necessary since some JavaScript scripts
  ## are written out and some images are plotted which have to be
  ## accessible within the shiny app.
  ## If there are not the appropriate files present to run the shiny
  ## app, the apps as well as the folder structure has to be generated
  if ( !dir.exists( paste0( CLIMEX.PATH, "app" ) ) )
    dir.create( paste0( CLIMEX.PATH, "app" ) )
  if ( !dir.exists( paste0( CLIMEX.PATH, "app/www" ) ) )
    dir.create( paste0( CLIMEX.PATH, "app/www" ) )
  ## In order to display the animation the app needs the jquery
  ## scianimator. Since it is not able to access a system file on
  ## its own the wrapper climex() will copy it to the apps folder
  if ( !dir.exists( paste0( CLIMEX.PATH,
                           "app/www/jquery.scianimator.min.js" ) ) ){
    file.copy( paste0( system.file( "climex_app", package = "climex" ),
                      "/js/jquery.scianimator.min.js" ),
              to = paste0( CLIMEX.PATH,
                          "app/www/jquery.scianimator.min.js" ),
              overwrite = TRUE )
  }
  ## source of the gif: http://i.imgur.com/seuaOqf.gif
  ## since this is a non-commercial product there shouldn't be
  ## any problems http://imgur.com/tos
  file.copy( paste0( system.file( "climex_app", package = "climex" ),
                    "/js/loadingGif.js" ),
            to = paste0( CLIMEX.PATH, "app/www/loadingGif.js" ),
            overwrite = TRUE ) 
  file.copy( paste0( system.file( "climex_app", package = "climex" ),
                    "/res/loading.gif" ),
            to = paste0( CLIMEX.PATH, "app/www/loading.gif" ) )      
  writeLines( "shinyUI( climex.ui() )", con = paste0( CLIMEX.PATH,
                                                     "app/ui.R" ) )
  writeLines( "shinyServer( climex.server )",
             con = paste0( CLIMEX.PATH, "app/server.R" ) )
  ## If one or more time series without any map information are
  ## provided as input arguments, start the app in the "General" tab
  ## right away
  ## if ( !is.null( x.input ) && length( x.input ) != 2 ){ 
  ##   writeLines( "shinyUI( climex.ui( selected = 'General' ) )",
  ##              con = paste0( CLIMEX.PATH, "app/ui.R" ) )
  ## }
  runApp( paste0( CLIMEX.PATH, "app" ) )
}


##' @title Server-side part of the \code{\link{climex}} function.
##'
##' @details Since it grew organically most of the features are defined
##' inside this function. Okay, why did I decided to define the
##' climex.server and the climex.ui function? Since in this way I can
##' provide a local and a server side variant of the code which is
##' maintained in the same place. I do not really like the idea of
##' installing the newest version of the code from Github and than
##' copy/pasting the code in some files somewhere in /srv/shiny-server.
##'
##' @return Function acting as the shiny server.
##' @import shiny
##' @import xts
##' @import dygraphs
##' @import ggplot2
##' @export
##' @author Philipp Mueller 
climex.server <- function( input, output, session ){
########################################################################
######### Customizing the sidebar and launching its reactives ##########
########################################################################
  ## Everything in my code is always of the style this.is.a.name. So
  ## why do I use the camel case thisIsAName for the shiny objects?
  ## Well, since CSS file do not support the point separator.
  ## Type of database (input, DWD, artificial data)
  output$sidebarDataBase <- climex:::sidebarDataBase()
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
    climex:::sidebarCleaning( reactive( input$radioEvdStatistics ) )
  ## Reactive value listening for file input
  reactive.loading <- climex:::file.loading(
                                   reactive( input$fileInputSelection ) )
  ## Reactive value which holds the selected stations chosen either via
  ## the leaflet map or the sidebar
  reactive.chosen <- climex:::data.chosen(
                                  reactive( input$selectDataBase ),
                                  reactive( input$sliderYears ),
                                  reactive( input$selectDataType ),
                                  reactive.loading )
  ## Reactive value selecting a specific time series according to the
  ## choices in the sidebar/leaflet map
  reactive.selection <-
    climex:::data.selection(
                 reactive.chosen, reactive( input$selectDataSource ),
                 reactive( input$selectDataBase ),
                 reactive( input$sliderYears ),

                 reactive( input$sliderThreshold ),
                 reactive( input$radioEvdStatistics ),
                 reactive( input$sliderArtificialDataLocation ),
                 reactive( input$sliderArtificialDataScale ),
                 reactive( input$sliderArtificialDataShape ) )
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
  ## Reactive value determining the initial parameters of the fitting
  ## procedure.
  reactive.initials <- climex:::data.initials(
                                    reactive( input$initialLocation ),
                                    reactive( input$initialScale ),
                                    reactive( input$initialShape ),
                                    reactive( input$radioEvdStatistics ),
                                    reactive.extreme )
  ## Reactive value performing the GEV/GP fit to the selected time series
  ## and providing the fitted object of class 'climex.fit.gev' or
  ## 'climex.fit.gpd'.
  reactive.fitting <- climex:::data.fitting(
                                   reactive.extreme, reactive.initials,
                                   reactive.rows,
                                   climex:::fit.interactive,
                                   reactive( input$radioEvdStatistics ),
                                   reactive( input$selectOptimization ),
                                   reactive( input$buttonMinMax ),
                                   reactive( input$checkboxRerun ),
                                   reactive( input$sliderThreshold ) )
########################################################################
  
########################################################################
############### Plotting of the time series and fit ####################
########################################################################
  ## Displaying of the original, intermediate and final time series.
  ## Reactive value containing a logical vector indicating which element
  ## of reactive.extreme()[[ 1 ]] should still be used after clicking and
  ## brushing in the extreme's gplot2 plot. In addition the plotting and
  ## rendering of the extremes, deseasonalized and pure time series is
  ## done in here.
  reactive.rows <- callModule( climex:::generalTimeSeriesPlot, "ts",
                              reactive.extreme,
                              reactive( input$selectDataBase ),
                              reactive( input$selectDataType ),
                              climex:::function.get.y.label,
                              reactive( input$radioEvdStatistics ),
                              reactive( input$sliderThreshold ) )
  ## Plotting of the fitted distribution and the corresponding PP, QQ
  ## and return level goodness-of-fit plots.
  callModule( climex:::generalFitPlot, "fit", reactive.extreme,
             reactive.rows, reactive.fitting,
             reactive( input$buttonMinMax ),
             reactive( input$radioEvdStatistics ),
             climex:::function.get.y.label,
             reactive( input$selectDataBase ), 
             reactive( input$selectDataType ),
             reactive( input$sliderThreshold ),
             reactive( input$selectOptimization ) )
  ## Table containing the fits results. Those four global variable will
  ## store the previous results of the GEV/GP fitting. This is
  ## unfortunately necessary in order to highlight the progress in the
  ## table red or green.
  last.values <<- last.1 <<- last.2 <<- last.3 <<- rep( 0,  )
  output$generalFitStatistics <-
    climex:::generalFitStatistics( reactive.fitting, reactive.extreme,
                                  reactive( input$sliderThreshold ),
                                  reactive( input$buttonMinMax ),
                                  reactive( input$radioEvdStatistics ),
                                  climex:::color.table )
########################################################################
  
########################################################################
################### Likelihood animation (tab2) ########################
########################################################################
  ## Fitting the MLE again with the algorithm of choice
  output$menuSliderLocationLim <- renderMenu( {
    x.fit.evd <- reactive.fitting()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    ## Hide input when fitting the GPD
    if ( input$radioEvdStatistics == "GP" )
      return( NULL )
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    sliderInput( "sliderLocationLim", "Location sampling limits",
                round( x.fit.evd$par[ 1 ], 1 ) - 10,
                round( x.fit.evd$par[ 1 ] + 10, 1 ),
                c( round( x.fit.evd$par[ 1 ], 1 ) - 5,
                  round( x.fit.evd$par[ 1 ], 1 ) + 5 ) ) } )
  output$menuSliderScaleLim <- renderMenu( {
    x.fit.evd <- reactive.fitting()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      x.fit.evd.scale <- x.fit.evd$par[ 2 ]
    } else {
      x.fit.evd.scale <- x.fit.evd$par[ 1 ]
    }
    sliderInput( "sliderScaleLim", "Scale sampling limits",
                round( max( 0, x.fit.evd.scale  - 10 ), 1 ),
                round( x.fit.evd.scale + 10, 1 ),
                c( round( max( 0, x.fit.evd.scale - 5 ), 1 ),
                  round( x.fit.evd.scale, 1 ) + 5 ) ) } )
  output$menuSliderShapeLim <- renderMenu( {
    x.fit.evd <- reactive.fitting()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      x.fit.evd.shape <- x.fit.evd$par[ 3 ]
    } else {
      x.fit.evd.shape <- x.fit.evd$par[ 2 ]
    }
    sliderInput( "sliderShapeLim", "Shape sampling limits",
                round( x.fit.evd.shape - 1, 1 ),
                round( x.fit.evd.shape + 1, 1 ),
                c( round( x.fit.evd.shape, 1 ) - .3,
                  round( x.fit.evd.shape, 1 ) + .3  ) ) } )
  ## To enable the user to input her/his own custom initialization
  ## points for the optimization it needs two things: three
  ## numerical inputs chosing the climex::likelihood.initials as
  ## default and a reactive vector gluing all together
  output$inputInitialLocation <- renderMenu( {
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      ## In order to fit the minimal extremes
      if ( ( !is.null( input$buttonMinMax ) ) &&
           ( input$buttonMinMax == "Min" ) )
        x.block <- x.block*( -1 )
    } else {
      model <- "gpd"
    }
    parameter.default <- climex::likelihood.initials( x.block,
                                                     model = model )
    numericInput( "initialLocation", "",
                 value = round( parameter.default[ 1 ], 4 ) ) } )
  output$inputInitialScale <- renderMenu( {
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      ## In order to fit the minimal extremes
      if ( ( !is.null( input$buttonMinMax ) ) &&
           ( input$buttonMinMax == "Min" ) )
        x.block <- x.block*( -1 )
    } else {
      model <- "gpd"
    }
    parameter.default <- climex::likelihood.initials( x.block,
                                                     model = model )
    numericInput( "initialScale", "",
                 value = round( parameter.default[ 2 ], 4 ),
                 min = 0 ) } )
  output$inputInitialShape <- renderMenu( {
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) || is.null( input$radioEvdStatistics ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      ## In order to fit the minimal extremes
      if ( ( !is.null( input$buttonMinMax ) ) &&
           ( input$buttonMinMax == "Min" ) )
        x.block <- x.block*( -1 )
    } else {
      model <- "gpd"
    }
    parameter.default <- climex::likelihood.initials( x.block,
                                                     model = model )
    numericInput( "initialShape", "",
                 value = round( parameter.default[ 3 ], 4 ) ) } )
  cached.table.init <- NULL
  initial.parameters.likelihood <- reactive( {
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.initial <- reactive.initials()
    input$tableDrawPoints
    x.fit.evd <- reactive.fitting()
    par.init <- x.fit.evd$par
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
      if ( is.null( input$sliderNumberInitialPoints ) ||
           is.null( input$sliderLocationLim ) ||
           is.null( input$sliderScaleLim ) ||
           is.null( input$sliderShapeLim ) ){
        return( c( NaN, NaN, NaN ) )
      }
    } else {
      model <- "gpd"
      if ( is.null( input$sliderNumberInitialPoints ) || 
           is.null( input$sliderScaleLim ) ||
           is.null( input$sliderShapeLim ) ){
        return( c( NaN, NaN ) )
      }
    }
    ## the first entry of the table should always be the actual
    ## point the optimization is starting from
    if ( input$radioEvdStatistics == "GEV" ){
      location <- c( x.initial[ 1 ],
                    round( stats::runif( (
                      input$sliderNumberInitialPoints - 1 ),
                      input$sliderLocationLim[ 1 ],
                      input$sliderLocationLim[ 2 ] ), 4 ) )
      scale <- c( x.initial[ 2 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderScaleLim[ 1 ],
                   input$sliderScaleLim[ 2 ] ), 4 ) )
      shape <- c( x.initial[ 3 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderShapeLim[ 1 ],
                   input$sliderShapeLim[ 2 ] ), 4 ) )
      table.init <- data.frame( location = location,
                               scale = scale, shape = shape )
    } else {
      scale <- c( x.initial[ 2 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderScaleLim[ 1 ],
                   input$sliderScaleLim[ 2 ] ), 4 ) )
      shape <- c( x.initial[ 3 ],
                 round( stats::runif( (
                   input$sliderNumberInitialPoints - 1 ),
                   input$sliderShapeLim[ 1 ],
                   input$sliderShapeLim[ 2 ] ), 4 ) )
      table.init <- data.frame( scale = scale, shape = shape )
    }
    ## but we only want to have starting points which do not
    ## result in a NA
    if ( input$radioEvdStatistics == "GEV" ){
      while ( any( is.nan( apply(
          table.init, 1, climex::likelihood,
          x.in = x.block, model = model ) ) ) ){
            for ( ii in 1 : nrow( table.init ) ){
              if ( is.nan( climex::likelihood(
                                       as.numeric( table.init[ ii, ] ),
                                       x.in = x.block,
                                       model = model ) ) )
                table.init[ ii, ] <- c(
                    round( stats::runif( 1, input$sliderLocationLim[ 1 ],
                                        input$sliderLocationLim[ 2 ] ),
                          4 ),
                    round( stats::runif( 1, input$sliderScaleLim[ 1 ],
                                        input$sliderScaleLim[ 2 ] ),
                          4 ),
                    round( stats::runif( 1, input$sliderShapeLim[ 1 ],
                                        input$sliderShapeLim[ 2 ] ),
                          4 ) ) } }
    } else {
      while ( any( is.nan( apply(
          table.init, 1, climex::likelihood,
          x.in = x.block, model = model ) ) ) ){
            for ( ii in 1 : nrow( table.init ) ){
              if ( is.nan( climex::likelihood(
                                       as.numeric( table.init[ ii, ] ),
                                       x.in = x.block,
                                       model = model ) ) )
                table.init[ ii, ] <- c(
                    round( stats::runif( 1, input$sliderScaleLim[ 1 ],
                                        input$sliderScaleLim[ 2 ] ),
                          4 ),
                    round( stats::runif( 1, input$sliderShapeLim[ 1 ],
                                        input$sliderShapeLim[ 2 ] ),
                          4 ) ) } }
    }
    return( table.init ) } )
  output$tableInitialPoints <- renderDataTable( {
    reactive.initials <- initial.parameters.likelihood()
    if ( all( is.na( reactive.initials ) ) )
      return( NULL )
    reactive.initials$ID <- seq( 1, nrow( reactive.initials ) )
    ## round the number in the table
    return( reactive.initials ) },
    ## the drawCallback ensures that the width of the parent table
    ## is not set to a specific pixel number but to 100% percent.
    ## This ensures its correct rendering on mobile devices
    options = list( dom = 't', pageLength = 5,
                   drawCallback = I( "function( settings )
            {document.getElementById( 'tableInitialPoints' ).style.width = '100%';}") ) )
  ## Displaying of the heuristic estimates for a wiser picking of
  ## the limits
  output$tableHeuristicEstimates <- renderDataTable( {
    x.block <- reactive.extreme()[[ 1 ]]
    if ( is.null( x.block ) ){
      ## if the initialization has not finished yet just wait a
      ## little longer
      return( NULL )
    }
    x.fit.evd <- reactive.fitting()
    if ( input$radioEvdStatistics == "GEV" ){
      model <- "gev"
    } else {
      model <- "gpd"
    }
    x.mle.par <- x.fit.evd$par
    x.initial <- reactive.initials()
    x.suggested <- climex::likelihood.initials( x.block, model = model )
    if ( input$radioEvdStatistics == "GEV" ){
      x.df <- data.frame( parameter = c( "fitting results",
                                        "suggested initials" ),
                         location = c( round( x.mle.par[ 1 ], 4 ),
                                      round( x.suggested[ 1 ], 4 ) ),
                         scale = c( round( x.mle.par[ 2 ], 4 ),
                                   round( x.suggested[ 2 ], 4 ) ),
                         shape = c( round( x.mle.par[ 3 ], 4 ),
                                   round( x.suggested[ 3 ], 4 ) ) )
    } else {
      x.df <- data.frame( parameter = c( "fitting results",
                                        "suggested initials" ),
                         scale = c( round( x.mle.par[ 1 ], 4 ),
                                   round( x.suggested[ 1 ], 4 ) ),
                         shape = c( round( x.mle.par[ 2 ], 4 ),
                                   round( x.suggested[ 2 ], 4 ) ) )
    }            
    return( x.df ) },
    options = list( dom = 't',
                   drawCallback = I( "function( settings )
            {document.getElementById( 'tableHeuristicEstimates' ).style.width = '100%';}") ) )
  ## the two dummies to get the current width of the screen
  output$plotPlaceholder <- renderPlot({
    ttplot( x.block ) } )
  output$plotPlaceholderLeaflet <- renderPlot({
    ttplot( x.block ) } )
  output$loadingImage <- renderUI({
    if ( session$clientData$url_hostname != "localhost" &&
         session$clientData$url_hostname != "127.0.0.1" ){
      folder <- "/assets/"
    } else
      folder <- ""
    return( img( src = paste0( folder, "loading.gif" ),
                id = "loadingGif" ) ) } )
  output$loadingScript <- renderUI({
    if ( session$clientData$url_hostname != "localhost" &&
         session$clientData$url_hostname != "127.0.0.1" ){
      folder <- "/assets/"
    } else
      folder <- ""
    return( div( tags$head( singleton(
                          tags$script( src = paste0( folder,
                                                    "/loadingGif.js"  )
                                      ) ) ) ) ) } )
  output$drawLikelihoodAnimation <- renderUI( {
    ## This reactive content only depends on the action button
    ## because of the use of the isolate() functions.        
    ## Don't make the plot the first time I look at the tab
    if ( input$buttonDrawAnimation < 1 )
      return( NULL )
      isolate( { x.block <- reactive.extreme()[[ 1 ]]
        if ( is.null( x.block ) ){
          ## if the initialization has not finished yet just wait a
          ## little longer
          return( NULL )
        } } )
      isolate( reactive.initials <- initial.parameters.likelihood() )
      isolate( {
        if ( input$selectOptimization == "dfoptim::nmk" ){
          optimization.method <- "nmk"
        } else 
          optimization.method <- input$selectOptimization
        ## I use the plot "plotPlaceholder" to determine the width
        ## of the current box and adjust the pixel width of the png
        ## pictures
        session.width <- session$clientData$output_plotPlaceholder_width
        session.plot.width <- floor( session.width/ 3 )
        print( "starting animation......." )
        ## a temporary folder will be generate to harvest the images
        ## of the animation whenever the animation will be redone
        ## the old folder should be removed to avoid wasting memory
        ## of the server. Therefore the folder name has to be a global
        ## variable
        if ( !is.null( image.folder ) )
          unlink( image.folder, recursive = TRUE )
        ## if the shiny server is running on localhost it is run in
        ## the CLIMEX.PATH folder and the folder containing the images
        ## is constantly overwritten to prevent the script from
        ## occupying to much space. Due to a setwd in the climex()
        ## wrapper we are already in this folder
        if ( session$clientData$url_hostname == "localhost" ||
             session$clientData$url_hostname == "127.0.0.1"  ){
          working.folder <- paste0( CLIMEX.PATH, "app/www" )
          ## in case of the local session the variable image.folder
          ## was already set in the wrapper climex::climex()
        } else {
          ## I decided to make the variable image.folder a global
          ## one because in this way the folder addressed with it
          ## can be deleted the next time this function is called
          working.folder <- "/srv/shiny-server/assets" 
          image.folder <<- paste0(
              "/srv/shiny-server/assets/tmp/images_",
              as.numeric ( as.POSIXct( lubridate::now() ) ) )
        }
        dir.create( image.folder, recursive = TRUE )
        if ( input$radioEvdStatistics == "GEV" ){
          location.lim <- isolate( input$sliderLocationLim )
          model <- "gev"
        } else {
          location.lim <- NULL
          model <- "gpd"
        }
        climex:::animation.wrapper(
                     time.series = x.block,
                     starting.points = reactive.initials,
                     location.lim = location.lim,
                     scale.lim = isolate( input$sliderScaleLim ),
                     shape.lim = isolate( input$sliderShapeLim ),
                     optimization.method = optimization.method,
                     optimization.steps = isolate(
                         input$sliderOptimizationSteps ),
                     optimization.rerun = input$checkboxRerun,
                     height = 300, width = session.plot.width,
                     model = model, delay = 300,
                     loopMode = "loop",
                     image.folder = image.folder,
                     working.folder = working.folder )
        ## if the code is not running on localhost the shiny server
        ## won't find the animation.js script using its absolute path
        if ( session$clientData$url_hostname != "localhost" &&
             session$clientData$url_hostname != "127.0.0.1" ){
          working.folder <- sub( "/srv/shiny-server", "",
                                working.folder )
          animation.script <- "/assets/"
        } else {
          working.folder <- ""
          animation.script <- ""
        }
        return( div(
            class = "scianimator",
            style = "display: inline-block;",
            tags$script( src = paste0( animation.script,
                                      "jquery.scianimator.min.js" ) ),
            tags$script( src = paste0( working.folder,
                                      "/animation.js" ) ),
            div( id = "animationLocSc", class = "animationClimex" ),
            div( id = "animationLocSh", class = "animationClimex" ),
            div( id = "animationScSh", class = "animationClimex" ) ) )
      } ) } )
########################################################################
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
      reactive( input$selectOptimization ),
      reactive( input$checkboxRerun ) )
}

##' @title The user interface for the \code{\link{climex}} function.
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
##' @export
##' @author Philipp Mueller 
climex.ui <- function( selected = c( "Map", "General", "Likelihood" ) ){
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
        menuItem( "Likelihood", tabName = "tabLikelihood",
                 icon = icon( "ok-circle", lib = "glyphicon" ),
                 selected = ifelse( selected == "Likelihood",
                                   TRUE, FALSE ) ),
        climex:::sidebarDataBaseInput(),
        climex:::sidebarDataSourceInput(),
        climex:::sidebarDataTypeInput(),
        climex:::sidebarLoadingInput(),
        climex:::sidebarCleaningInput(),
        ## while plotting the images for the animation a gif
        ## should be shown
        div( uiOutput( "loadingScript" ), uiOutput( "loadingImage" ),
            id = "loadingWrapper" ) ) ),
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
              height = 550, background = "orange",
              id = "boxGevStatistics",
              radioButtons( "radioEvdStatistics", label = NULL,
                           inline = TRUE,
                           choices = c( "GEV", "GP" ),
                           selected = "GEV" ),
              climex:::generalExtremeExtractionInput(),
              radioButtons( "buttonMinMax", "Type of extreme",
                           inline = TRUE,
                           choices = c( "Max", "Min" ),
                           selected = "Max" ),
              climex:::deseasonalizeInput(),
              climex:::generalFittingRoutineInput() ) ),
          fluidRow(
              climex:::generalFitStatisticsTable(),
              climex:::generalTimeSeriesPlotOutput( "ts" ) ) ),
        tabItem( tabName = "tabLikelihood",
          box( title =
              h2( "Starting points of the optimization routine" ),
            width = 8, status = "primary", id = "boxStartingPoints",
            dataTableOutput( "tableInitialPoints" ),
            ## a plot of height 0? Well, its actually a very nice
            ## trick since I need a width value for the generated
            ## pngs in the animation in pixel. But I really want to
            ## make app to be rendered nicely on different screen
            ## sizes. Via the session$clientData I can access the
            ## width and height of plots. Thus I can access the width
            ## of this specific box via the plotPlaceholder without
            ## seeing it at all.
            plotOutput( "plotPlaceholder", height = 0, width = '100%' ),
            htmlOutput( "drawLikelihoodAnimation" ) ),
          box( title = h2( "Options" ), width = 4,
            background = "orange", id = "boxHeuristic",
            dataTableOutput( "tableHeuristicEstimates" ),
            div( p( "actual initials", id = "tableInitialDescription" ),
                uiOutput( "inputInitialLocation" ),
                uiOutput( "inputInitialScale" ),
                uiOutput( "inputInitialShape" ),
                id = "initialTable" ),
            checkboxInput( "checkboxRerun",
                          "Rerun the optimization", value = TRUE ),
            sliderInput( "sliderNumberInitialPoints",
                        "Number of initial points", 1, 20, 5 ),
            uiOutput( "menuSliderLocationLim" ),
            uiOutput( "menuSliderScaleLim" ),
            uiOutput( "menuSliderShapeLim" ),
            sliderInput( "sliderOptimizationSteps",
                        "Which optimization steps", 0, 1, c( .1, .5 ) ),
            actionButton( "buttonDrawAnimation", "Start animation" ),
            actionButton( "tableDrawPoints", "Reset" ) ) ) ) ) )
}
