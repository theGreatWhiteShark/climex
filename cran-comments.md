## Test environments
* local Devuan 8.0, R-3.4.0
* Ubuntu 14.04 (on travis-ci), R-3.3.0
* win-builder (R-devel), (3.4.3), (3.3.3)


## `devtools::check()` results on the local machine

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Philipp Mueller <thetruephil@googlemail.com>’

New submission

Unknown, possibly mis-spelled, fields in DESCRIPTION:
  ‘Remotes’

* checking top-level files **NOTE**
   Non-standard file/directory found at top level:
     ‘res’
	 
   - I use this folder to provide e.g. the pictures included in the
     README.md file. Since the *win-builder* tries to build this file
     as well, I didn't included the folder in the *.Rbuildignore*
	 

* checking dependencies in R code ... **NOTE**
There are ::: calls to the package's namespace in its code. A package
  almost never needs to use ::: for its own objects:
  ‘cleaning.interactive’ ‘color.table’ ‘data.chosen’ ‘data.extremes’
  ‘data.fitting’ ‘data.selection’ ‘deseasonalize.interactive’
  ‘deseasonalizeSelection’ ‘deseasonalizeSelectionInput’
  ‘extremes.interactive’ ‘file.loading’ ‘fit.interactive’
  ‘function.get.y.label’ ‘generalButtonMinMax’
  ‘generalButtonMinMaxInput’ ‘generalExtremeExtraction’
  ‘generalExtremeExtractionInput’ ‘generalFitPlot’
  ‘generalFitPlotOutput’ ‘generalFitStatistics’
  ‘generalFitStatisticsTable’ ‘generalTimeSeriesPlot’
  ‘generalTimeSeriesPlotOutput’ ‘leafletClimex’ ‘leafletClimexUI’
  ‘sidebarCleaning’ ‘sidebarCleaningInput’ ‘sidebarDataBase’
  ‘sidebarDataBaseInput’ ‘sidebarDataSource’ ‘sidebarDataSourceInput’
  ‘sidebarDataType’ ‘sidebarDataTypeInput’ ‘sidebarImprint’
  ‘sidebarImprintInput’ ‘sidebarLoading’ ‘sidebarLoadingGif’
  ‘sidebarLoadingGifOutput’ ‘sidebarLoadingInput’ ‘sidebarSeriesLength’
  ‘sidebarSeriesLengthInput’

 - Along with the package I also supply a shiny app. The app is
   started using the function `climex()`, which writes the lines
   
	   shinyUI( climex.ui() )
	   
   in a file in `paste0( getOption( "climex.path" ), "app/ui.R" )` and
   
	   shinyServer( climex.server() )
	   
   in a file in `paste0( getOption( "climex.path" ), "app/server.R"
   )`. Since all the logic is hidden in the `climex.ui()` and
   `climex.server()` functions, the shiny app can easily run by users
   and maintained as a shiny server.
   
   Both functions, `climex.ui()` and `climex.server()`, call internal
   functions of the **climex** package. The once listed by this NOTE.
   
   I really do not want to export all of this internal stuff to the
   namespace of the user. Therefore I called the functions using `:::`.
   

* checking R code for possible problems ... NOTE
generalFitPlot: no visible binding for global variable ‘x’
generalFitPlot: no visible binding for global variable ‘y’
generalFitPlot: no visible binding for global variable ‘y.low’
generalFitPlot: no visible binding for global variable ‘y.high’
generalTimeSeriesPlot: no visible binding for global variable ‘value’
likelihood.plot : plot.plane: no visible binding for global variable
  ‘x’
likelihood.plot : plot.plane: no visible binding for global variable
  ‘y’
likelihood.plot : plot.plane: no visible binding for global variable
  ‘likelihood.low’
likelihood.plot: no visible binding for global variable ‘x’
likelihood.plot: no visible binding for global variable ‘z’
likelihood.plot: no visible binding for global variable ‘y’
plot.climex.fit.gev: no visible binding for global variable
  ‘..density..’
plot.climex.fit.gev: no visible binding for global variable ‘x.plot’
plot.climex.fit.gev: no visible binding for global variable ‘y.plot’
plot.climex.fit.gpd: no visible binding for global variable
  ‘..density..’
plot.climex.fit.gpd: no visible binding for global variable ‘x.plot’
plot.climex.fit.gpd: no visible binding for global variable ‘y.plot’
ttplot: no visible binding for global variable ‘value’
Undefined global functions or variables:
  ..density.. likelihood.low value x x.plot y y.high y.low y.plot z
  
  - All those variables are actually names columns and used within
    **ggplot2** routines.
	   
	   
## Downstream dependencies

Everything nice and clean

	> revdep_check()
	Reverse dependency checks for climex ==========================================
	Saving check results in `revdep/checks/`
	Saving install results in `revdep/install/`
	Computing reverse dependencies... 
	Installing dependencies for climex to /tmp/RtmptfyV3G/R-lib
	Installing climex 1.2.0 to /tmp/RtmptfyV3G/revdepfd15d10babe
	Setting env vars --------------------------------------------------------------
	NOT_CRAN    : false
	RGL_USE_NULL: true
	DISPLAY     : 
	Saving check results to `revdep/check.rds` ------------------------------------
	Cleaning up -------------------------------------------------------------------
	> revdep_check_save_summary()
	> revdep_check_print_problems()
	No ERRORs or WARNINGs found :)
