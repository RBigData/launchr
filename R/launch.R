#' Launches a persistent pbdR server on a cluster
#'
#' @param nodes The number of nodes to allocate for the server.
#' @param npernode The number of MPI ranks (R sessions) to start per node.
#' @param walltime A string "hh:mm:ss" giving the maximum length of time
#'                 in hours:minutes:seconds before server timeout shutdown.
#' @param user Username for login.
#' @param server Hostname to run the pbdR server, resolvable on your
#'  platform.
#' @param account Account used for your cluster allocation
#' @param modules A vector of strings giving the modules to load before
#' starting the server.
#' @param rwd Remote working directory as a string.
#' @return A shell script as a vector of strings.
#' @export
launch = function(nodes, npernode = 16, server = "set_me",
                  modules = NULL, user = Sys.getenv("USER"),
                  account = NULL, walltime = "01:00:00", rwd = "~/") {
    ## parameters that will eventually migrate to control structures
    if(is.null(modules))
        return(print("Please specify at least one module"))
    if(is.null(account))
        return(print("Please specify account"))

    port = 55555

    if(server == "rhea.ccs.ornl.gov") {
        pscript = preload_rhea(nodes, npernode, account = account, rwd = rwd,
                               modules = modules, port = port)

        args = args_rhea(port)

        rserver = pbdRPC::machine(hostname = server,
                                  user = user,
                                  exec.type = "ssh",
                                  args = args)

    } else if(server == "or-condo-login.ornl.gov") {
        pscript = preload_or_condo(nodes, npernode, account = account,
                                   rwd = rwd,
                                   modules = modules, port = port)

        args = args_or_condo(port)

        rserver = pbdRPC::machine(hostname = server,
                                  user = user,
                                  exec.type = "ssh",
                                  args = args)

    } else {
        cat("Don't know how to launch on", server, "yet\n")
        return(invisible(FALSE))
    }

    pbdRPC::start_cs(machine = rserver, cmd = "",
                     preload = paste0(pscript, collapse = "\n"))
    return(invisible(TRUE))

}
