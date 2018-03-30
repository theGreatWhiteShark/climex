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
