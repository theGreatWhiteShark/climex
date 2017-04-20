# v1.0.1
Improved heuristics for the estimation of the initial GEV and GP parameters used during the optimization. The likelihood.initials function now uses a more fine grained precision tree for deciding about the initial value of the shape parameter for both the GEV and GP distribution. Beforehand this function also featured a stochastic component drawing random shape parameter around the estimate and searching for the best possible. This was replaced by a sequence function to make the fit deterministic and hence reproducible. In addition a number of fixes and improvement in the documentation and the vignettes were performed.
# v1.0.0
Major changes in the climex app. Among others modularization, numerous bug fixes, and disabling of features not implemented yet (e.g. the animation of GP fits). Removal of the global CLIMEX.PATH variable in favor of a option called "climex.path". Fixing of the likelihood and return level calculation of the shape parameter == 0. Introduction of tests. 
I will note down new features more properly in the future. Up to now I (theGreatWhiteShark) was the only one using this app.
# v0.8.2
## With new version of the climex app
Now featuring a map to choose the individual stations and to show some summary statistics.
# v0.8.1
## updating namespacing
Previously I wrote the NAMESPACE by hand. But since there was a problem with the exporting of the
box() function from the *shinydashboard* package breaking my app I decided to redo stuff.
There seems to be a new version of Hadley's *R pkg* book. Or at least I do not remember much of it.
Now everything is clean up with using the **::** operator in function and **@import** and **@export**
in the ROxygen documentation
## .default fallback methods for functions.
In the very beginning of the package (when I still had the years, stations and bulk S3 classes) it
was more or less handy to have a fallback method handling just plain numerical input. Since I just
use **xts** class input since quite a while its now time to drop the .defaults and to clean up the
package and it's exports.
But this might break some code which is directly referring to e.g. block.xts()
## stripped the *ttplot.list* method
Since always use *multiplot* for it and its more a tool to inspect a time series. 
