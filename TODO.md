### TODO

- The labels of the leaflet legends do not really fit the positions.
- If there is not a big range in the leaflet's return levels the indices do not have the appropriate precision (e.g. 14 14 14 15 15 15 16).
- For some input combinations there are still errors thrown.
- The package still relies on the dfoptim version of the Nelder-Mead optimization which is not as reliable as the stats::optim one. In addition the visualization of the optimization routine is not available for BFGS, SANN and CG
- For enhancing the speed of the generation of the colored markers when calculating the return levels for all stations the blocking and anomalies function should be ported to C++
- Customizing loading gif
- Change return level in leaflet map to logarithmic scale
