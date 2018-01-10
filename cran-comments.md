## `devtools::check()` results

* checking installed package size ... NOTE
  installed size is  7.4Mb
  sub-directories of 1Mb or more:
    doc   6.0Mb
	
 - There is quite some documentation in the package.

* checking dependencies in R code ... **NOTE**
There are ::: calls to the package's namespace in its code. A package
  almost never needs to use ::: for its own objects:
  ‘aic’ ‘bic’ ‘cleaning.interactive’ ‘color.table’
  ‘convert.date.integer’ ‘data.chosen’ ‘data.extremes’ ‘data.fitting’
  ‘data.selection’ ‘deseasonalize.interactive’ ‘deseasonalizeSelection’
  ‘deseasonalizeSelectionInput’ ‘extremes.interactive’ ‘file.loading’
  ‘fit.interactive’ ‘function.get.y.label’ ‘generalButtonMinMax’
  ‘generalButtonMinMaxInput’ ‘generalExtremeExtraction’
  ‘generalExtremeExtractionInput’ ‘generalFitPlot’
  ‘generalFitPlotOutput’ ‘generalFitStatistics’
  ‘generalFitStatisticsTable’ ‘generalTimeSeriesPlot’
  ‘generalTimeSeriesPlotOutput’ ‘gev.density’ ‘gpd.density’
  ‘leafletClimex’ ‘leafletClimexUI’ ‘likelihood.gradient’ ‘qevd’
  ‘return.level’ ‘revd’ ‘rlevd’ ‘sidebarCleaning’
  ‘sidebarCleaningInput’ ‘sidebarDataBase’ ‘sidebarDataBaseInput’
  ‘sidebarDataSource’ ‘sidebarDataSourceInput’ ‘sidebarDataType’
  ‘sidebarDataTypeInput’ ‘sidebarImprint’ ‘sidebarImprintInput’
  ‘sidebarLoading’ ‘sidebarLoadingGif’ ‘sidebarLoadingGifOutput’
  ‘sidebarLoadingInput’ ‘sidebarSeriesLength’
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
   
   
	   
