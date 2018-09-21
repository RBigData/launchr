#' Launches a persistent pbdR server on a cluster
#' @param server A string giving the hostname to run the pbdR server,
#'     resolvable on your local platform.
#' @param user Username for server login.
#' @param FUN A function that modifies the default ssh script. It is
#'     called with \code{FUN(script, ...)} so it has access to
#'     unmatched arguments in \code{launch()}. The default is the
#'     identity function.
#' @param port The port to be used for communication with the
#'     server. Defaults to 55555.
#' @param verbose If TRUE, print generated scripts before
#'     submission. A value > 1 will also prevent script submission.
#' @param fn A list of file names that will be created on the
#'     server. Normally the defaults are fine.
#' @param ... Arguments for script details. Typical parameters include
#' \describe{
#'   \item{nodes}{The number of nodes to allocate for the server.}
#'   \item{npernode}{The number of MPI ranks (R sessions) to start per node.}
#'   \item{walltime}{A string "hh:mm:ss" giving the maximum length of time in
#'     hours:minutes:seconds before server timeout shutdown.}
#'   \item{account}{Account used for your cluster allocation.}
#'   \item{modules}{A vector of strings giving the modules to load before
#'     starting the server.}
#'   \item{rwd}{A string giving a working directory on server.}
#' }
#' @return A shell script as a vector of strings.
#' @export
launch = function(server,
                  user = Sys.getenv("USER"),
                  FUN = function(script, ...) script,
                  port = 55555,
                  verbose = FALSE,
                  fn = list(
                      lnode_file = ".pbdR_server.sh",
                      pbs_file = ".pbdR_server.pbs",
                      head_node_file = ".pbdR_server_hnode",
                      server_log = ".pbdR_server.o"
                  ),
                  ...) {

    ## files created on server in rwd directory

    ## generate default script
    preload = lnode_script(fn, port, ...)
    args = ssh_args(port)
    ## TODO try to infer script form server name
    ## server = strsplit(SERVER, "[.]")[1]

    ## now process any modifications to the default by the FUN filter
    ## does the function exist in user's environment?
    FUN = match.fun(FUN)
    preload = FUN(script = preload, ...)

    ## TODO find a place to store user-contributed functions. inst/examples??
    ##        file = system.file(paste0(deparse(substitute(FUN)), ".R"),
    ##                           "examples", package = "launchr")

    ## echo scripts if requested
    if(verbose) {
        cat("\nScript for login node:\n")
        print(preload)
        cat("\nLocal ssh parameters:\n")
        print(args)

        if(verbose > 1) {
            cat("\nPrinted scripts only. No launch\n")
            return(invisible(FALSE))
        }
    }

    ## set the machine parameters
    rserver = pbdRPC::machine(hostname = server, user = user,
                              exec.type = "ssh", args = args)

    ## start server
    pbdRPC::start_cs(machine = rserver, cmd = "",
                     preload = paste0(preload, collapse = "\n"))

    return(invisible(TRUE))
}
