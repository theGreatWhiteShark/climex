![leaflet map to handle a lot of station data](res/climex_map.png)
![control all the different steps involved in the extreme value analysis](res/climex_time-series.png)
![verify the results using an animation of the fitting procedure](res/climex_animation.png)

# Features
![map-icon](res/glyphicons-2-leaf.png)
- You can perform the extreme value analysis for a large number of
  different stations.
- The package contains a script for downloading all station data of the German
  weather service (DWD) to get you started with your climate analysis.
  
![general-icon](res/glyphicons-42-charts.png)
- You have full control of all the steps involved in GEV fitting via an intuitive
  GUI in a persistent way (changes will be applied to the
  analysis of all following time series).
- The fitting is done using a maximum likelihood procedure especially
  tuned to produce the best possible results.
  
![likelihood-icon](res/glyphicons-199-ok-circle.png)
- You can verify whether the fitting procedure worked or got stucked in a
  local minimum via an animation.
  
# Installation

To install the app from within R you can use the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package.

```
devtools::install_github( "theGreatWhiteShark/climex" )
```

To get started with the shiny-based [web application](vignettes/climex_app.Rmd) and the [underlying
R functions](vignettes/data_dwd_and_usage.Rmd) please refer to the package's vignettes.

---

### License

This package is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License, version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the GNU
General Public License for more details.

A copy of the GNU General Public License, version 3, is available at
<http://www.r-project.org/Licenses/GPL-3>


