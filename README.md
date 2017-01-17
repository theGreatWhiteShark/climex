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
- You have full control all the steps involved GEV fitting via an intuitive
  graphical interface in a persistent way (changes will be applied in the
  analysis of all following time series).
- The fitting is done using a maximum likelihood procedure especially
  tuned to produce the best possible results.
  
![likelihood-icon](res/glyphicons-199-ok-circle.png)
- You can verify if the fitting procedure worked or got stuck in a
  local minimum via an animation.
  
# Installation

Install the app within R using the *devtools* package via Github.

```
devtools::install_github( "theGreatWhiteShark/climex" )
```

To get started with the shiny-based web application and the underlying
R functions please refer to the package's vignettes.

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


