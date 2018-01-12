# About
This vignette provides an introduction to the climex package's web application run on localhost. It will guide you through both its setup and usage.


# Prerequisites

The climex web app needs several files to power it and all of them will be stored in *~/R/climex/* folder. If your are fine with this default behavior, feel free to skip the next three paragraphs.

A web app, like the Shiny-based one provided by this package, needs loads of different files: .css files to shape and beautify its appearance, .js files enabling the app to perform logic and calculations within your browser to provide further customization and a more natural interaction, all the different displayed images, gifs, and movies etc. etc.

Fortunately you don't have to care about all these files since the wrapper function `climex()` will take them from the package's installation path and group them all in a certain folder the app will be run in. The only thing you have to care about is *where* all these things happen.

By default all the app's resources will be stored in your home in a folder called *~/R/climex/*. To change its path, add the following lines to the R configuration file *~/.Rprofile* in your home.

```{r configuration, eval=FALSE}
## Replace PATH by the directory you want to store all files related to the climex app into
options( climex.path = "PATH" )
```

After setting the *climex.path* option, you need to download the daily station data [provided by the German weather service (DWD)](ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/) to power the leaflet map in the first tab. Just open a R shell in your terminal and enter the following commands

```{r DWD, eval=FALSE}
## Loading the climex package
library( climex )
## Downloading the data of the DWD. Be sure to don't specify the data.type argument or use
## data.type = "default" in here. The app is just tested of max/min temperature and precipitation yet.
download.data.dwd()
```

For a more thorough explanation of the web scrapping and downloading capabilities of this package please refer to the **Downloading and formatting of the station data of the DWD** section in the package's second vignette.

With all the DWD data at hand, you are free to lunch the climex app.

```{r wrapper, eval = FALSE, dependson = "prerequisites"}
library( climex )
## Lunching the climex app. Note: sometimes only the heading of the app
## is displayed when running it for the first time. In this case just
## close the app and start it again. Not quite sure why this is happening.
climex()
```

Now your R shell will open a server on your computer and the climex app will be started in a new tab of your default web browser (Firefox preferred).

To **stop the app** just go to your R shell and press Ctrl-c two times in a row.

# ![map-icon](glyphicons-2-leaf.png) Map tab

![map-screenshot](climex_map.jpeg)

This [leaflet](https://github.com/theGreatWhiteShark/leaflet)-based tab is the default entry point of the climex app and its main purpose is to provide a convenient way of navigation between the data of the different stations. 

While working with tens or hundreds of time series from different
geographical sites, it is always hard to keep their spatial relations,
like the distance between the stations or their individual altitude, in
mind. To overcome this problem, a map-based selection
interface was introduced in addition to the *Station* selector in the sidebar.

Apart from being a selection interface for the station data the **Map** tab features the following functions:

- Via the **Minimal length** slider only those time series containing more
  years than the chosen value will be shown on the map. This feature
  is extremely useful for finding long time series in the vastness of
  all your provided station data.
- When selecting a station via mouse click a summary statistic containing the
  station's name as well as its 20, 50 and 100 year return level will
  be displayed in the upper right corner of the map.
- In the control panel you can choose either the tiles of the [OpenTopoMaps](https://opentopomap.org/) or the once of the default [OpenStreetMaps](https://www.openstreetmap.org/) to provide your map. While the first one is more instructive in the context of climatic time series it also faced some down times recently. Therefore you always can use the default OSM map as a fallback solution.
- Using the **Return level** slider and the attached **Return level** slider 
  the chosen return level will be calculated for all shown stations. 
- After the calculation finished (the cat in the wheel disappeared), an additional select item appears where you can choose to display the stations as blue markers (*stations*) and/or their return level as colored cirlces (*returns*).
  But beware: this can take up to several minutes depending
  an the number of stations displayed! Since all stations selected via the Minimal length slider will be considered in the calculation and not just the ones displayed after zooming in, only a limited amount of stations is supported when running the climex app within a Shiny server.

![map-heatmap-screenshot](climex_heat-map.jpeg)

All the changes to the preprocessing options in the
**General** tab will be considered while calculating the summary
statistics and the return level of all displayed stations.
  
NOTE: The map is based on my fork of the leaflet package instead of the original one since the developer seem to dislike my [pull request](https://github.com/rstudio/leaflet/pull/342) for horizontal and sizable legends. Without this the legend of the colored return level markers would look just to weird and misplaced.

# The sidebar

All the different stations you select either via the **Map** tab or the **Station** drop-down menu come in three different flavors: as daily maximal and minimal temperature and as daily precipitation data. Which of them to use can be selected through the **Measured variable** drop-down menu.

Right below all these drop-down menus there is a very important radio button: the **Remove incomplete years** or **Declustering of the data** button (depending of the choice of the extreme value distribution in the **Options** box in the **General** tab). This button will ensure your time series is getting cleaned and short-range correlations as well as artifacts are getting removed. So disable it just if you really know what you are doing!

Apart from the time series provided by the German weather service (DWD) you can also perform the analysis on artificial data by selecting *Artificial data* in the **Data base** drop-down menu. Once selected, your time series will be sampled from either the generalized extreme value (GEV) or generalized Pareto (GP) distribution (as chosen within the **Options** box in the **General** tab). You can control the sampling process by adjusting the **Location**, **Scale**, and **Shape** slider for the distribution's parameters and the **Length** one for the number of points to sample from the distribution. Every time you update one of these sliders the time series is getting redrawn. Instead you can also use **Draw** button.

# ![general-icon](glyphicons-42-charts.png) General tab

![general-screenshot](climex_time-series.png)

This tab provides both an interface to change the preprocessing options used throughout the whole application as well as a
variety of tools to analyze an individual time series. Keep in mind: whenever you change an option within this tab it will affect all the optimization performed in the entire app. 

### *GEV fit* box

This box contain four different plots: 

- The overall fitting result consisting of a blueish histogram of the
  extreme events of the time series and the orange-colored GEV density
  function constructed using the fitted parameters.
- Three goodness-of-fit plots highlighting the quality of the
  individual fit. From top to bottom: a [p-p plot](https://en.wikipedia.org/wiki/P%E2%80%93P_plot), a [q-q plot](https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot), and a return level plot (see Stuart Coles, 2001, p. 58).

### *Options* box

Via the settings in this box all the preprocessing options are controlled throughout the app.

#### Limiting distribution

This radio buttons control whether to fit the generalized extreme value
(*GEV*) distribution or the generalized Pareto (*GP*) one.

#### Box length in days | Threshold

Via this slider the extreme events will be obtained from your time series. Depending on the chosen limiting distribution it will either determines the number of days of a time series constituting a block in the GEV approach or determines the height of the threshold above which all event will be considered to be extreme events. The default value of one year for the block method is recommended especially since the user can change the deseasonalization method and may end up with seasonal correlations in the data.

Remember: The approximation of the histogram using both the GEV and
the GP distribution is only valid for asymptotic block length and
threshold height. So don't pick to low values!

#### Type of extreme

If set to *Min*, the GEV distribution will be fitted to the minimal extremes. In this approach only the most lowest value in each block of the time series will be extracted.

#### Deseasonalization method

Using this selector you can choose if and how to get rid of the
short-range correlations in your time series introduced by the annual
cycle. Per default the [anomalies](https://en.wikipedia.org/wiki/Anomaly_(natural_sciences)#Atmospheric_sciences) will be calculated. But you can also
pick other R-based implementations like [stl](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/stl.html), [decompose](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/decompose.html), and the **ds** function from the [deseasonalize](https://cran.r-project.org/web/packages/deseasonalize/deseasonalize.pdf) package.

### *Results* box

This table displays several details of the GEV fitting procedure:

- The GEV/GP parameters *location*, *scale* and *shape*
- The augmented negative log-likelihood (*nllh*) of the fit as well as the [Akaike](https://en.wikipedia.org/wiki/Akaike_information_criterion) and [Bayesian Information criterion](https://en.wikipedia.org/wiki/Bayesian_information_criterion)
- The 100 year return level

For the distribution's parameter and the return level a small change while varying the preprocessing options does suggest the stability of the fitting procedure and thus a high quality of the time series. This will be indicated by green colors whereas large changes are indicated by the color red. For the nllh, AIC, and BIC the values should be as low as possible. That's why all decreases are marked green and all increases red.

To better review the influence of the individual parameter changes not just the current results and statistics but also the ones from the three last fits (in the *hist_1*, *hist_2* and *hist_3* column) are contained in the table.

### *Time series* box

In the *Pure* tab you can view the raw and unprocessed
time series. The *Deseasonalized* tab uses the former series and
applies the function specified in the *Deseasonalization method*
drop-down menu of the **Options** box to it. Both plots contain the
extracted extreme events as additional orange points and are generated
using the [dygraphs](https://cran.r-project.org/web/packages/dygraphs/index.html) library. That's why you are also able so zoom
into specific regions using your mouse.

The *Remaining* tab contains all the events in your time series which
are extracted by the blocking or thresholding. Those can
be considered as the extreme events of the series. Since it is
sometimes quite interesting to see what will happen to your fitted
parameters when discarding individual events (like the most extreme
ones), you can toggle all the points by clicking them or brush them
using your mouse. When deactivated they do not contribute to the
fitting anymore and the optimization is being redone instantly.

---
Icons: © [Glyphicon](http://glyphicons.com/) [CC-By-SA](https://creativecommons.org/licenses/by-sa/3.0/)
