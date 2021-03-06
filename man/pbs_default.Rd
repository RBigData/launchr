% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preload.r
\name{pbs_default}
\alias{pbs_default}
\title{Creates a default pbs script as a vector of strings. Modification
for platform-specific parameters should be done with a function via
the FUN parameter of the \code{launch()} function rather than
rewriting \code{pbs_default()}.}
\usage{
pbs_default(nodes = 1, npernode = 16, modules = "r",
  walltime = "01:00:00", account, warn_on_fork = TRUE, fn = NULL,
  rwd = "~/")
}
\arguments{
\item{nodes}{Number of nodes to use for the server}

\item{npernode}{Number of R instances to run per node}

\item{modules}{A vector of strings giving the modules to load
before starting the server.}

\item{walltime}{A string "hh:mm:ss" giving the maximum length of
time in hours:minutes:seconds before server timeout
shutdown. Note that while the server is running you may be
incurring time charges to your account.}

\item{account}{Account used for your cluster allocation}

\item{warn_on_fork}{Set to \code{FALSE} to suppress a warning when
forking a process (for example, when invoking
\code{parallel::mclapply()}).}

\item{fn}{A list providing names used for script files that are
created in the server rwd directory.}

\item{rwd}{Working directory on server. This is where script files
are created.}
}
\value{
A shell script as a vector of strings.
}
\description{
Creates a default pbs script as a vector of strings. Modification
for platform-specific parameters should be done with a function via
the FUN parameter of the \code{launch()} function rather than
rewriting \code{pbs_default()}.
}
