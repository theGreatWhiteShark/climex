# climex 0.8.2
## With new version of the climex app
Now featuring a map to choose the individual stations and to show some summary statistics.
# climex 0.8.1
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
