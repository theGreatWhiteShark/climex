[![Travis Build](https://travis-ci.org/theGreatWhiteShark/climex.svg?branch=master)](https://travis-ci.org/theGreatWhiteShark/climex.svg?branch=master)

# Features

- Improved fitting routine (almost no numerical artifacts anymore)
- Different methods, including statistical ones, to access the error
  estimates of the return levels
- Better error handling allowing for massive parallel application
- Focuses on the handling of time series (**xts** instead of
  *data.frame* as its basic input class)
- A set of auxiliary functions frequently used in the extreme value
  analysis of climate time series

## Improved fitting routine

The fitting of the parameters of the stationary **GEV** and **GP**
distribution using unconstrained optimization 
(done by all other R packages involving extreme value statistics) tends to
produce numerical artifacts. This is due to the presence of logarithms
in the negative log-likelihood functions and a limited range of shape
values the maximum likelihood estimator is defined in. While yielding
absurdly large parameter values or causing the optimization to fail when
using the **BFGS** algorithm (in the *extRemes* package), the
differences using the **Nelder-Mead** algorithm (all other packages)
might be small, barely noticeable, and totally plausible. But they are
still present and spoil your calculation.

In order to avoid those numerical artifacts, the **augmented
Lagrangian method** is used to incorporate both the logarithms and the
limited range of shape values as nonlinear constraints. With this
approach, the optimization can be started at arbitrary initial
parameter combinations and will almost always converge to the global
optimum.

This solves two remaining problems of the extreme value analysis:
1. The user does not have to worry about the numerical optimization
   anymore. It will produce the correct results in almost all cases.
2. The optimization itself becomes more robust and can now be used in
   an massive parallel setting.
   
## Error estimates of the return level

An important part of the extreme value analysis is to access the
fitting errors introduced into the calculated return levels. The
default way of obtaining these is to 
use the so-called delta method assuming normality of the
log-likelihood at the fitting result. This assumption, however, is
not fulfilled in a lot of cases.

To nevertheless calculate a decent estimate of the fitting errors, the
**climex** package introduces two statistical
approaches, one based on bootstrap and the other one based on a Monte
Carlo approach. In comparison to the calculation of the confidence
intervals implemented in the *extRemes* package, the climex package
calculates the standard fitting error of the return levels. Since
there are a lot of different sources of errors in the extreme value
analysis, like too small block sizes, too low thresholds or
non-stationaries and/or correlations in the data, providing a
confidence interval (CI) might the misleading for some users. All
these additional errors are by no means included in the CI and 
have to be obtained in further studies to construct some appropriate
CI of the calculated return levels.

## Better error handling

Due to the use of either the **Nelder-Mead** or **BFGS** method, some
time series will throw errors when fitted using the other R packages
tailored for the extreme value analysis. When fitting 100 stations at
once you can expect at least one station to break your code.

In order to allow for a massive parallel application of the extreme
value analysis, the **climex** package features a more robust error
handling. In addition, the improved fitting routine allows the
optimization to handle initial parameter combinations far more distant
from the global optimum than feasible using any unconstrained
routine.

## Focused on handling time series

The fundamental object class handled in the **climex** package is the
time series class **xts** or lists of class xts objects. This allows
the user to harness all the additional functions tailored for the
analysis of time series, e.g. those of the **lubridate** package. It
also includes a couple of convenience functions often used within the
extreme value analysis, like blocking, application of a threshold,
declustering, deseasonalization etc.

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
comes with a full-fledged shiny application, which enables the user to
access and investigate a lot of different station data and, at the
same time, to tweak the most important parameters involved in the
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
package. It is capable of formatting and saving the station data
provided by the German weather service (DWD) in lists of *xts*
class objects and thus in the format most natural to work with in the
context of the **climex** package. 

# Usage

You are new to **R**? Then check out the [compiled list of
resources](https://www.rstudio.com/online-learning/#R) from RStudio or
the [official
introduction](https://CRAN.R-project.org/doc/manuals/R-intro.pdf).

A thorough introduction is provided for the [general
usage](vignettes/general-usage.Rmd) of the package.

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
