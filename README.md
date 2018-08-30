# launchr

* **Version:** 0.1-0
* **License:** [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** George Ostrouchov
* **Project home**: https://github.com/RBigData/launchr
* **Bug reports**: https://github.com/RBigData/launchr/issues

Provides ease of use and configuration components for controlling a
distributed set of R instances on a cluster from a remote R session
(e.g. running on a laptop with RStudio). The intent is to provide an
easy-to-use interactive SPMD-style develpment platform for distributed
parallel programming in R. Other parallel approaches (multicore,
multithreading, and GPU) are compatible with a 
```{r}
launchr()
```
distributed server session, which provides a means of specifying the
number of R sessions per node, leaving cores available for these
shared-memory applications.

Leave BIG data on BIG platforms and
control its analysis using R codes on your laptop.

We started with one PBS-managed cluster and will continue to add more
configurations. At this point, adding a new cluster configuration is a
manual process that learns from existing examples. Once a few clusters
are configured, we plan to morph this into a more general and easier
configuration process.

## Installation

<!-- You can install the stable version from CRAN using the usual `install.packages()`:

```r
install.packages("launchr")
``` -->

The development version is maintained on GitHub:

```r
remotes::install_github("RBigData/launchr")
```
