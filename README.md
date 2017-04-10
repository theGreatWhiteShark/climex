# Features
## An interactive web app
![leaflet map to handle a lot of station data](res/climex_map.png)
![control all the different steps involved in the extreme value analysis](res/climex_time-series.png)
![verify the results using an animation of the fitting procedure](res/climex_animation.png)

---

![map-icon](res/glyphicons-2-leaf.png)
- You can perform extreme value analysis on a large number of
  climate time series from different stations.
- You can calculate return.levels of arbitrary lengths for all 
  chosen stations and display the results comprehensively on a 
  map.
  
![general-icon](res/glyphicons-42-charts.png)
- You have full control of all the steps involved in extreme value fitting via an intuitive
  GUI in a persistent way (changes will be applied to the
  analysis of all following time series).
- Both the Generalized Extreme Value (GEV) and the Generalized
  Pareto (GP) approach are supported.
- You can exclude single points or whole parts of your time series 
  and the whole analysis will be updated immediately.
- The fitting is done using a maximum likelihood procedure especially
  tuned to produce the best possible results.
  
![likelihood-icon](res/glyphicons-199-ok-circle.png)
- For a better control over the fitting procedure, you can interactively
  set the starting points of the MLE optimization.
- Using a set of different starting points, you also check for local minima.
  If all of them result in the same parameter combination, everything is worked.

## Robust and sophisticated optimization

When searching for the best candidate to use among all the different extreme value packages in R, I noticed that almost all of them just called the stats::optim function with its default arguments. None of them thought about how to perform the actual fit as good as possible. The only exception (in summer 2016) was the **extRemes** package which uses a more sophisticated heuristics to determine the starting points of the optimization and the *BFGS* algorithm. But the later one does make the fitting numerically unstable.

In addition I found it quite hard to apply the **ismev** or **extRemes** to a large amount of time series in parallel. Both of them tend to throw errors quite frequently. The former one because it fails to invert the calculated hessian to estimate variance of the parameters and the later one due to the instability of the BFGS algorithm.

To overcome these two problems, I decided to write my own version of the stationary GEV/GP fitting with a more sophisticated best practice in the optimization (paper in submission) and an extended error handling.

## Convenient access to station data of the DWD

In order to obtain loads of data to perform my analysis on and to power the web application, I wrote some functions scrapping the web side of the German weather service (DWD). Even if you don't want to use this package, you can still use either the .Rdata objects containing all these time series (of class *xts*) or the exported .csv version of the individual stations.

# Installation

Since this shiny based web app uses rather new features of the R programming language, you have to have **at least R-3.3.0** or newer (what you can check by running `R --version` in the terminal). If you don't fulfill this condition yet, be sure to get a binary or the source of an appropriate R version via [CRAN](https://cran.r-project.org/).

This package is not part of the CRAN package collection yet. In order to install it you have to use the *install_github* function from the **devtools** package.

Just open a R shell on your computer and type the following commands

```
## Installing the devtools package. 
install.packages( "devtools" )

## Installing the climex package from Github.
devtools::install_github( "theGreatWhiteShark/climex" )
```

Even if you do not intend to use the full capabilities of the package but just parts like the download of the DWD data, be sure to nevertheless install the whole package via the above commands. It just uses 1.4 MB of space and you this way you ensure that all required packages will be installed and all environmental variables will be set.

# Usage

You are new to **R**? Then check out the [compiled list of resources](https://www.rstudio.com/online-learning/#R) from RStudio or the [official introduction](https://cran.r-project.org/doc/manuals/R-intro.pdf).

An in-depth introduction to the [general usage](vignettes/data_dwd_and_usage.Rmd) of the package and the shiny-based [web application](vignettes/climex_app.Rmd) can be found in the package's [vignettes](vignettes/).

When using this package in your own analysis, keep in mind that its functions expect your time series to be of class **xts** and not numeric!

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


