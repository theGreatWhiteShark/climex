[![Travis Build](https://travis-ci.org/theGreatWhiteShark/climex.svg?branch=master)](https://travis-ci.org/theGreatWhiteShark/climex.svg?branch=master)


## A robust and sophisticated optimization (or why to use this package over others)

The fitting of GEV and GP parameters using unconstrained optimization
(done by all other R packages on extreme value statistics) tends to
produce numerical artifacts. This is due to the presence of logarithms
in the negative log-likelihood and a limited range of shape parameters
for which the likelihood estimator is defined. While producing
absurdly large parameters or causing the optimization to fail when
using the **BFGS** algorithm (*extRemes*), the differences using the
**Nelder-Mead** algorithm (all other packages) might be small, barely
noticeable, and totally plausible.

In order to avoid those numerical artifacts, the **augmented
Lagrangian method** is used to incorporate both the logarithms and the
limited range of shape parameters as nonlinear constraints. With this
approach the optimization can be started at arbitrary initial
parameter combinations and will almost always converge to the global
optimum.

This solves two of the remaining problems of the extreme value analysis:
1. The user does not have to worry about the numerical optimization
   anymore. It will always produce the correct results 
2. The optimization itself becomes more robust and can now be used in
   massive parallel applications 

# Installation

In order to install this package, you have to use the *install_github*
function from the **devtools** package.

Just open a R shell on your computer and type in the following commands

``` r
## Installing the devtools package (in case your haven't done it yet).
install.packages( "devtools" )

## Installing the climex package from Github.
devtools::install_github( "theGreatWhiteShark/climex" )
```

This will install the climex package residing on the **master** branch
of this git repository. If you instead want to download and install a
different branch, use the *ref* argument to specify it. E.g.

``` r
devtools::install_github( "theGreatWhiteShark/climex", ref = "v1.2.0" )
```


## An interactive web application

A convenient interface to this core package can be found in the
[climexUI](https://github.com/theGreatWhiteShark/climexUI) package. It
comes with a full-fledged shiny application, which enables we user to
access and investigate a lot of different station data and, at the
same time, to tweak all the most important parameters involved in the
preprocessing and the fitting procedure of the GEV or GP
distribution. 

![leaflet map to handle a lot of station data](res/climex_map.jpeg)
![control all the different steps involved in the extreme value analysis](res/climex_time-series.png)
![explore the station data with your mobile device](res/climex_mobile.jpeg)

### Features

![map-icon](res/glyphicons-2-leaf.png)
- You can perform extreme value analysis on a large number of
  climatic time series from different stations
- You can calculate arbitrary return levels for all 
  stations and display the results comprehensively on a map
  
![general-icon](res/glyphicons-42-charts.png)
- You have full control over all steps involved in the extreme value 
  analysis of the station data via an intuitive
  GUI in a persistent way (changes will be applied to the
  analysis of all following time series)
- Both the Generalized Extreme Value (GEV) and the Generalized
  Pareto (GP) approach are supported
- You can exclude single points or whole parts of a time series 
  with the entire analysis updated immediately
- The fitting is done using a nonlinear constrained maximum likelihood 
  procedure based on the augmented Lagrangian method. Using this approach
  none of your fits will produce numerical artifacts

## Accessing station data

If you are at the very beginning of your analysis or still in search
of a vast data base to perform your analysis on, I recommend you
to check out the [dwd2r](https://github.com/theGreatWhiteShark/dwd2r)
package. It is capable to format and save the data in lists of *xts*
class objects and thus in the format most natural to work with in the
context of the **climex** package. 

# Usage

You are new to **R**? Then check out the [compiled list of
resources](https://www.rstudio.com/online-learning/#R) from RStudio or
the [official
introduction](https://CRAN.R-project.org/doc/manuals/R-intro.pdf).

A thorough introduction is provided for the [general
usage](res/README_data_dwd_and_usage.Rmd) of the package.

When using this package in your own analysis, keep in mind that its
functions expect your time series to be of class
[xts](https://CRAN.R-project.org/web/packages/xts/index.html) and not
numeric!

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
