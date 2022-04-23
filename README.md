# launchr

* **Version:** 0.2-0
* **License:** [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** George Ostrouchov
* **Project home**: https://github.com/RBigData/launchr
* **Bug reports**: https://github.com/RBigData/launchr/issues

Provides ease of use and configuration components for controlling a
distributed set of R instances on a cluster from a remote R session
(e.g. running on a laptop with RStudio). The intent is to provide an
easy-to-use interactive SPMD-style development platform for distributed
parallel programming in R. Other parallel approaches (multicore,
multithreading, and GPU) are compatible with a launchr() distributed
server session, which provides a means of specifying the number of R
sessions per node, potentially leaving cores available for further
parallel shared-memory applications.

Leave BIG data on BIG platforms and control its analysis using R codes
on your laptop.

A default script for launching on a PBS-managed cluster is constructed. A
means of editing the script by an R function allows for custom
modifications necessary for local configurations. An example
modification is provided in the inst/demo directory. Please let us
know about your configurations to help us provide an easier experience
for everyone.

For understanding how port forwarding works, see
[stackexchange](https://unix.stackexchange.com/questions/115897/whats-ssh-port-forwarding-and-whats-the-difference-between-ssh-local-and-remot).

When producing mass graphics in parallel on the remote server file
system, use \code{grDevices::dev.off()} to bypass a modified
\code{dev.off()} that intends to hijack graphics back to the client.

## Installation

<!-- You can install the stable version from CRAN using the usual `install.packages()`:

```r
install.packages("launchr")
``` -->

A stable development version is maintained on GitHub:

```r
remotes::install_github("RBigData/launchr")
```
