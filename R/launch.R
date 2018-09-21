#' Launches a persistent pbdR server on a cluster
#' @param SERVER A string giving the hostname to run the pbdR server, resolvable on your
#'   local platform.
#' @param user Username for server login.
#' @param FUN The function that returns a PBS script and a login node script
#' @param verbose If TRUE, print generated scripts before submission. A
#' value > 1 will also prevent script submission.
#' @param ... Arguments to be passed to FUN for script details. Typical
#' parameters include
#' \describe{
#'   \item{nodes} The number of nodes to allocate for the server.
#'   \item{npernode} The number of MPI ranks (R sessions) to start per node.
#'   \item{walltime} A string "hh:mm:ss" giving the maximum length of time
#'                 in hours:minutes:seconds before server timeout shutdown.
#'   \item{account} Account used for your cluster allocation
#'   \item{modules} A vector of strings giving the modules to load before
#'     starting the server.
#'   \item{rwd} Remote working directory as a string.
#' }
#' @return A shell script as a vector of strings.
#' @export
launch = function(SERVER, user = Sys.getenv("USER"), FUN = rhea, port = 55555,
                  verbose = FALSE, ...) {

    ## files created on server in rwd directory
    fn = list(
        lnode_file = ".pbdR_server.sh",
        pbs_file = ".pbdR_server.pbs",
        head_node_file = ".pbdR_server_hnode",
        server_log = ".pbdR_server.o"
    )

    ## generate default script
    preload = lnode_script(fn, port, ...)
    args = ssh_args(port)
    ## TODO try to infer script form server name
    ## server = strsplit(SERVER, "[.]")[1]

    ## now process any modifications to the default by the FUN filter

    ## does the function exist in user's environment?
    FUN = match.fun(FUN)
    preload = FUN(preload)

    ## TODO find a place to store user-contributed functions. inst/examples??
    ##        file = system.file(paste0(deparse(substitute(FUN)), ".R"),
    ##                           "examples", package = "launchr")

    ## print scripts if requested
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
    rserver = pbdRPC::machine(hostname = SERVER, user = user, exec.type = "ssh",
                              args = args)

    ## start server
    pbdRPC::start_cs(machine = rserver, cmd = "",
                     preload = paste0(preload, collapse = "\n"))

    return(invisible(TRUE))
}
