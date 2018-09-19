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
launch = function(SERVER, user = Sys.getenv("USER"), FUN = NULL,
                  verbose = FALSE, ...) {
    ## generate default script
    preload = lnode_script(...)
    args = ssh_args()
    ## TODO try to infer script form server name
    ## server = strsplit(SERVER, "[.]")[1]

    if(verbose) {
        print("Script for login node:")
        print(preload)
        print("Local ssh parameters:")
        print(args)

        if(verbose > 1) return("Printing script only. No launch")
    }

    ## now process any modifications to the default by the FUN filter
    if(!is.null(FUN)) {
        ## does the function exist in user's environment?
        FUN_mod = try(match.fun(FUN, silent=TRUE))
        if(class(FUN_mod) == "try-error") {
            ## next, try to find it in inst/templates of launchr
            template = system.file(server_fun, "templates", package = "launchr")
            if(length(template) > 0) {
                FUN_mod = template
            } else {
                return("FUN function not found")
            }
        }

    }
### I am here ... now reprocess default scripts with FUN or FUN_mod

    port = 55555

    scripts = FUN(...)

    rserver = pbdRPC::machine(hostname = SERVER, user = user, exec.type = "ssh",
                              args = scripts$args)

    pbdRPC::start_cs(machine = rserver, cmd = "",
                     preload = paste0(scripts$preload, collapse = "\n"))
    return(invisible(TRUE))

}
