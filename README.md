## climex
This app is intended to provide three things:
1. An easily accessible and robust framework to fit GEV distributions to data
2. A script allowing you to download a lot of data from the FTP servers of the German weather service DWD
3. A shiny based app containing almost all available functions of this package making the application of extreme value analysis quite straight forward.

For the first two points see the supplied vignettes. For the last point I will make some blog entries soon describing its usage and creation.

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

---

### TODO

- The labels of the leaflet legends do not really fit the positions.
- If there is not a big range in the leaflet's return levels the indices do not have the appropriate precision (e.g. 14 14 14 15 15 15 16).
- For some input combinations there are still errors thrown.
