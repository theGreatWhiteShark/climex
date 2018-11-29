# v2.0.1*6000
- Updating the vignette. Removing the part describing the removed
  input function `source.data()` and the customization of the no
  longer available `climex.path` global option.
- Removing the `cran-comments.md` file.
- Removing GitHub part of the installation instruction in the
  README.md
- Updating the `temp.potsdam` data set. It now covers two more years.
- Updating all tests to cope with the new `temp.potsdam` data.
- Moving **res/** folder to **inst/res/** to match the **R**
  conventions and removing the obsolete glyphicons-199-ok-circle.png file
- Removing the deprecated `...` input argument of the `return.level` function.

# v2.0.0
The functionality of the 1. climex core package, 2. the shiny-based
GUI, and 3. import of the data of the German weather service (DWD)
will be split in three different entities. This way, it is more simple
regarding housekeeping. And I might finally succeed in getting a reply
from one of the CRAN fellows by just submitting the core package.
- Rewriting the **source.data()** function to search for .RData in a
  specified folder recursively, print all results well-formatted in
  the command line and prompt the user for a selection. The option for
  a default data set is now deprecated.
- Adding S3 function **checking.completeness**. For input of class
  *xts* it returns TRUE or FALSE whether or not the time series has at
  least the supplied number of complete years. If the input is of
  class *list*, the function trims the list to only those objects
  fulfilling the requirement.
- Rewriting the functions **fit.gev**, **fit.gpd**, **return.level**,
  **aic**, **bic**, **remove.seasonality**, **anomalies**, **block**,
  **remove.incomplete.years**, **decluster**, and **threshold** as S3
  objects. They now accept both the basic elements they already were
  able to accept beforehand and a list of those objects. In the latter
  case the output will be a list of the output of the individual
  calls.
- Streamlining the documentation and formatting
  Updating the formatting of the code and the documentation. All
  lambda functions now have running variables composed of two times
  the same character. Function arguments are marked with the \strong{}
  highlighting in the documentation and packages are referenced using
  \pkg{}.
- Allowing for parallel execution of S3 functions
  The functions **anomalies**, **block**, **decluster**,
  **threshold**, **check.completeness**, **fit.gev**, **fit.gpd**,
  **remove.incomplete.years**, **remove.seasonality**, and
  **return.level** can now harness the support of the *parallel*
  package using the *mclapply* function. Per default their additional
  input argument *mc.cores* is set to NULL. If set to a numerical
  value instead, this value is used as the number of cores and the
  parallel execution is triggered.
  The functions **aic** and **bic**, while being S3 generic, have been
  excluded since they are calculated in no time.
- The *separation.mode* argument of the **block** function was
  removed, since it was overwritten internally anyway.
- The default *block.number* argument of the **block** function was
  set to NULL. This way its internal handling using the **missing**
  function was changed to a check using **is.null** to ensure it will
  work when called by another function (the **fit.gev** one).
- The **threshold** function now hands the *cluster.distance* argument
  to the **decluster** function.
- **fit.gev** and **fit.gpd** are now capable for preprocessing the
  time series on their own. Using the arguments *blocking* and
  *thresholding* the functions can call **block** or **threshold**
  internally and hand over all the required arguments. This makes the
  use of those functions more convenient to the user.
- The *extreme.type* argument was introduced to the **fit.gev**,
  **fit.gpd**, **block**, **threshold**, and **return.level** function
  to be also able to fit the block minima or all extreme points below
  a certain low threshold. This argument replaced the *block.mode* one
  of the block function.
- Reducing the default value of the *monte.carlo.sample.size* argument
  of the **return.level** function form 1000 to 100.
- Bug fixes and additional test functions for the error estimation in
  the **return.level**, **fit.gev**, and **fit.gpd** function.
- Using seeds instead of tolerances in the test functions.
- Bug in **ttplot** prevented correct x axis for all base time units
  instead of days.
- The positions of the returned list elements *x* and *return.level*
  of the **fit.gev** function was switched.
- Both the **fit.gev** and **fit.gpd** function do now have a fallback
  method, which is able to handle numerical data. It contains a
  warning message suggesting the usage of **xts**-class objects
  instead. The resulting object will most probably be incompatible
  with a lot of functionalities.
- The **fit.gev** and **fit.gpd** functions do now feature a new
  input argument called *debug*. If set to TRUE, it displays both
  warning messages and intermediate results from the outer loop of the
  constrained optimization routine. In addition, the *silent* argument
  now properly mutes all output of the fitting functions.
- Fixing the correct linking and exporting of the C++-based function
  **likelihood_GEV** (only used within the **likelihood.plot**
  function since it yields a significant speedup in there.
- The **convert.date** function was some heritage from times the
  **climex** package was still performing the download and formatting
  of the data of the German weather service (DWD). Since this is no
  longer the case, the function was removed.
- The **source.data** function is now removed as well. It way more
  plausible to have it in the **dwd2r** package and a very bad idea to
  have two copies.
- The *climex.path* global variable is no longer part of the
  **climex** package. Since **dwd2r** uses its own directory for
  storing the downloads, *~/R/dwd_data* in the global option
  *dwd2r.download.path*, the **climex** package does not directly use
  or access the climex.path folder anymore. The variable will be
  instead defined by the **climexUI** package.
  
# v1.2.1
- Only perform the test of the API of the DWD on Unix-based
  systems. The *winbuilder* of CRAN just throws non-reproducible error
  messages. In some way the server is not able to properly download
  the .zip file. This doesn't seem to be an issue of this package at
  all. 
- The test of the API of the FTP server of the DWD is now only
  performed in case the computer running the tests has a working
  connection to the internet.
- Using Travis to run the tests when pushing the package to Github.
- Registered C++ routines as suggest in
  https://github.com/RcppCore/Rcpp/issues/636.
- Instead of toggling the attachment of the DWD data in the
  `source.data` function to either the global environment or the one
  used to call the function, the desired environment can now be
  provided as an input argument.
- Fixing the handling of global variables in the shiny app by using a
  custom environment `climex.environment` instead of true global
  variables.
- The vignettes in the *//vignette/* folder are no longer used as the
  online documentation. There were some problems with them loading
  images in R. Instead they are stripped of all figure links and a
  copy of them is introduced in the *//res/* folder. These ones still
  contain the figure links and are saved as proper .md files (and
  ignored during build). In the end the *//res/* folder was included
  again, since *win-builder* tries to build the README.md using
  **pandoc**. 
- Fixing a bug in the formula calculating the run.length in the
  `extremal.index` for a threshold distance then 2.
- Updating the documentation, namespacing, file layout, build ignore,
  DESCRIPTION file
- Using proper UTF-8 encoding
- With **bootstrap**a third option of calculating the error estimates
  for the GEV/GP parameters, as well as the return level is now
  available. 
- The `likelihood.initials` now uses a proper scaling when searching
  for an initial parameter combination using a Markov random walk.
- In the `fit.gev` function the `monte.carlo.sample.size` variable was
  not handed over properly.

# v1.2.0
The `return.level` function now returns a list containing both the
estimates and the approximated errors, regardless of whether an error
estimation was selected or not. The error estimate for the **GP return
level** can now be obtained without supplying the *total.length*
variable. Its value will be estimated. The `fit.gev` function is able
to directly fit a **Gumbel distribution** by supplying an initial
parameter combination with a shape parameter equal to zero. In the
same fashion the **exponential distribution** can be fitted via the
`fit.gpd` function.
# v1.1.0
All the optimization is now relying on the augmented Lagrangian method
implemented in the 'alabama' package. Therefore the 'dfoptim'
dependence, all traces of rerunning the algorithm and the third tab of
the Climex app were removed. Additional functions for the augmented
likelihood and its gradient were added. Changes of the API of the DWD
were incorporated.
# v1.0.3
The GenSA function is not that robust. Therefore I added the
stats::optim( method = "SANN" ) implementation as a fallback function
in case the former one does not work. In addition I updated the
documentation to the tile control panel in the leaflet tab of the
climex app and fixed a bug in the likelihood.gradient function.
# v1.0.2
The likelihood gradient now works for the shape parameter equals zero
too (for both GEV and GP). In addition the a bug in the internal
function likelihood.plot was fixed and the climex app now can choose
between the OpenStreetMaps and OpenTopoMaps tiles via a control
panel.
# v1.0.1
Improved heuristics for the estimation of the initial GEV and GP
parameters used during the optimization. The likelihood.initials
function now uses a more fine grained precision tree for deciding
about the initial value of the shape parameter for both the GEV and GP
distribution. Beforehand this function also featured a stochastic
component drawing random shape parameter around the estimate and
searching for the best possible. This was replaced by a sequence
function to make the fit deterministic and hence reproducible. In
addition a number of fixes and improvement in the documentation and
the vignettes were performed.
# v1.0.0
Major changes in the climex app. Among others modularization, numerous
bug fixes, and disabling of features not implemented yet (e.g. the
animation of GP fits). Removal of the global CLIMEX.PATH variable in
favor of a option called "climex.path". Fixing of the likelihood and
return level calculation of the shape parameter == 0. Introduction of
tests.
I will note down new features more properly in the future. Up to now I
(theGreatWhiteShark) was the only one using this app.
# v0.8.2
## With new version of the climex app
Now featuring a map to choose the individual stations and to show some
summary statistics.
# v0.8.1
## updating namespacing
Previously I wrote the NAMESPACE by hand. But since there was a
problem with the exporting of the box() function from the
*shinydashboard* package breaking my app I decided to redo
stuff. There seems to be a new version of Hadley's *R pkg* book. Or at
least I do not remember much of it. Now everything is clean up with
using the **::** operator in function and **@import** and **@export** 
in the ROxygen documentation
## .default fallback methods for functions.
In the very beginning of the package (when I still had the years,
stations and bulk S3 classes) it was more or less handy to have a
fallback method handling just plain numerical input. Since I just use
**xts** class input since quite a while its now time to drop the
.defaults and to clean up the package and it's exports.
But this might break some code which is directly referring to
e.g. block.xts()
## stripped the *ttplot.list* method
Since always use *multiplot* for it and its more a tool to inspect a
time series.
