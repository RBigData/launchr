
#' @export
make_pbs_script = function(nodes, npernode, rwd, modules, walltime, server,
                           account, warn_on_fork, fn) {
    ## create qsub (head node) PBS script
    pbs_common= c(
        "#!/bin/bash",
        "#PBS -N pbdR_server",
        "#PBS -e .pbdR_server.e",
        "#PBS -o .pbdR_server.o",
        paste0("#PBS -A ", account),
        paste0("#PBS -l walltime=", walltime)
    )
    pbs_unique =
        switch(server,
               "rhea.ccs.ornl.gov" = paste0("#PBS -l nodes=", nodes),
               "or-condo-login.ornl.gov" = c(
                   paste0("#PBS -l nodes=", nodes, ":ppn=32"),
                   "#PBS -l qos=std",
                   "#PBS -q batch",
                   "#PBS -W group_list=cades-ccsd",
                   "#PBS -m abe"
               ),
               stop(paste("Don't know how to connect to", machine, " yet!"))
               )
    mod_vec = vector("character", length(modules))
    for(m in seq(along=modules))
        mod_vec[m] = paste0("module load ", modules[m])
    wof = ifelse(warn_on_fork, "", " --mca mpi_warn_on_fork 0 ")
    commands = c(
        "module list",
        paste0("cd ", rwd),
        paste0("hostname > ", fn$head_node_file),
        paste0("mpirun ", wof, " --map-by ppr:", npernode,
               ":node Rscript -e 'pbdCS::pbdserver()'")
    )
    script = c(pbs_common, pbs_unique, mod_vec, commands)

    script
}

## #' Launches a persistent pbdR server on a cluster
## #'
## #' @param nodes The number of nodes to allocate for the server.
## #' @param npernode The number of MPI ranks (R sessions) to start per node.
## #' @param walltime A string "hh:mm:ss" giving the maximum length of time
## #'                 in hours:minutes:seconds before server timeout shutdown.
## #' @param user Username for login.
## #' @param server Hostname to run the pbdR server, resolvable on your
## #'  platform.
## #' @param account Account used for your cluster allocation
## #' @param modules A vector of strings giving the modules to load before
## #' starting the server.
## #' @param rwd Remote working directory as a string.
## #' @return A shell script as a vector of strings.
## #' @export
## launch = function(nodes = 1, npernode = 16, server = "set_me",
##                   modules = NULL, user = Sys.getenv("USER"),
##                   account = NULL, walltime = "01:00:00", rwd = "~/") {
##     ## parameters that will eventually migrate to control structures
##     if(is.null(modules))
##         return(print("Please specify at least one module"))
##     if(is.null(account))
##         return(print("Please specify account"))

##     port = 55555

##     if(server == "rhea.ccs.ornl.gov") {
##         pscript = preload_rhea(nodes, npernode, account = account, rwd = rwd,
##                                modules = modules, port = port)

##         args = args_rhea(port)

##         rserver = pbdRPC::machine(hostname = server,
##                                   user = user,
##                                   exec.type = "ssh",
##                                   args = args)

##     } else if(server == "or-condo-login.ornl.gov") {
##         pscript = preload_or_condo(nodes, npernode, account = account,
##                                    rwd = rwd,
##                                    modules = modules, port = port)

##         args = args_or_condo(port)

##         rserver = pbdRPC::machine(hostname = server,
##                                   user = user,
##                                   exec.type = "ssh",
##                                   args = args)

##     } else {
##         cat("Don't know how to launch on", server, "yet\n")
##         return(invisible(FALSE))
##     }

##     pbdRPC::start_cs(machine = rserver, cmd = "",
##                      preload = paste0(pscript, collapse = "\n"))
##     return(invisible(TRUE))

## }

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
launch = function(nodes, npernode = 16, server = NULL,
                  modules = NULL, user = Sys.getenv("USER"),
                  account = NULL, walltime = "01:00:00", rwd = "~/",
                  port = 55555) {
    ## parameters that will eventually migrate to control structures
    if(is.null(modules))
        return(print("Please specify at least one module"))
    if(is.null(server))
        return(print("Server not specified"))
    if(is.null(account))
        return(print("Please specify account"))

    ## set file names to be created in rwd directory.
    fn = list(pbs_file = ".pbdR_server.pbs",
              head_node_file = ".pbdR_hostname.txt",
              lnode_file = ".pbdR_lnode.sh"
              )

    pscript = ssh_tunnel_qsub(nodes, npernode, account = account,
                              rwd = rwd, pbs_FUN = make_pbs_script,
                              modules = modules, port = port, fn = fn)
    args = args_tunnel(port)

    rserver = pbdRPC::machine(hostname = server,
                              user = user,
                              exec.type = "ssh",
                              args = args)

    pbdRPC::start_cs(machine = rserver, cmd = "",
                     preload = paste0(pscript, collapse = "\n"))

    return(invisible(TRUE))

}

