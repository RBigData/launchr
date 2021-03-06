% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launch.R
\name{launch}
\alias{launch}
\title{Launches a persistent pbdR server on a cluster}
\usage{
launch(server, user = Sys.getenv("USER"), FUN = function(script, ...)
  script, port = 55555, verbose = FALSE, fn = list(lnode_file =
  ".pbdR_server.sh", pbs_file = ".pbdR_server.pbs", head_node_file =
  ".pbdR_server_hnode", server_log = ".pbdR_server.o"), ...)
}
\arguments{
\item{server}{A string giving the hostname to run the pbdR server,
resolvable on your local platform.}

\item{user}{Username for server login.}

\item{FUN}{A function that modifies the default ssh script. It is
called with \code{FUN(script, ...)} so it has access to
unmatched arguments in \code{launch()}. The default is the
identity function.}

\item{port}{The port to be used for communication with the
server. Defaults to 55555.}

\item{verbose}{If TRUE, print generated scripts before
submission. A value > 1 will also prevent script submission.}

\item{fn}{A list of file names that will be created on the
server. Normally the defaults are fine.}

\item{...}{Arguments for script details. Typical parameters include
\describe{
  \item{nodes}{The number of nodes to allocate for the server.}
  \item{npernode}{The number of MPI ranks (R sessions) to start per node.}
  \item{walltime}{A string "hh:mm:ss" giving the maximum length of time in
    hours:minutes:seconds before server timeout shutdown.}
  \item{account}{Account used for your cluster allocation.}
  \item{modules}{A vector of strings giving the modules to load before
    starting the server.}
  \item{rwd}{A string giving a working directory on server.}
}}
}
\value{
A shell script as a vector of strings.
}
\description{
Launches a persistent pbdR server on a cluster
}
