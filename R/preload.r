
#' Wraps a script with shell commands to write it to a file.
#' Sometimes referred to as a "here document"
#' @param script A vector of strings, usually each a shell command.
#' @param file_name A string giving the file name to use. Careful not to
#' conflict with an existing file name as it will be silently deleted and
#' overwritten.
#' @param eof A string to use for end-of-file. Must not occurr within the
#' script and must be unique among potential multiple uses of this function
#' for a script.
file_it = function(script, file_name, eof = NULL) {
    c(paste0("cat >> ", file_name, " << ", "'", eof, "'"), script, eof)
}

#' Constructs a shell script as a vector of strings. Its purpose is to run
#' on a login node of rhea.ccs.ornl.gov, submit a pbdR server batch job that
#' runs for a specified time or until closed. It also opens an ssh tunnel
#' from the login node to the running server head node.
#'
#' @param nodes The number of nodes to allocate for the server.
#' @param npernode The number of MPI ranks (R sessions) to start per node.
#' @param walltime A string "hh:mm:ss" giving the maximum length of time
#'                 in hours:minutes:seconds before server timeout shutdown.
#' @param user Username for login.
#' @param machine Hostname to run the server, resolvable on your platform.
#' @param port The port used by the server.
#' @param account Account used for your cluster allocation
#' @param modules A vector of strings giving the modules to load before
#' starting the server.
#' @param rwd Remote working directory as a string.
#' @param verbose Should the preload command be printed?
#' @param warn_on_fork Set to \code{FALSE} to suppress a warning when forking
#' a process (for example, when invoking \code{parallel::mclapply()}).
#' @return A shell script as a vector of strings.
ssh_tunnel_qsub = function(nodes = 1, npernode=16, walltime = "01:00:00",
                          user = NULL, machine = NULL, pbs_FUN = NULL,
                          account = NULL, modules=c("r"), rwd = "~",
                          show = FALSE,  port = 55555, warn_on_fork = TRUE,
                          fn) {
    ## Returns a script as a vector of strings to run on a login node so that it
    ##    starts a pbdR server and opens an ssh tunnel to its head node on
    ##    the specified port

    ## set working directory and file names to be created there
    cd = paste0("cd ", rwd)

    ## file cleanup command
    clean = paste("rm -f", fn$pbs_file, fn$head_node_file, fn$lnode_file)

    ## commands to write pbs script to its file
    pbs_script = pbs_FUN(nodes, npernode, rwd, modules, walltime, server,
                         account, warn_on_fork=TRUE, fn)
    make_pbs_file = file_it(pbs_script, fn$pbs_file, "..PBS-EOF..")

    ## command to qsub the script
    qsub = paste0("qsub ", fn$pbs_file)

    ## commands to wait for server start with progress dots
    wait_run = paste0("while [ ! -f ", fn$head_node_file,
                      " ]; do sleep 1; echo -n '.'; done; echo '.' ")

    ## command to tunnel from login to head node
    tunnel = paste0("ssh -f -L ", port, ":localhost:", port,
                    " -N \\$(cat ", fn$head_node_file, ")")

    ## commands to report login node and head node names
    tell = c("echo 'server login node: ' \\$(hostname)",
             paste0("echo 'server head node: ' \\$(cat ",
                    fn$head_node_file, ")"))

    lnode_script = c(cd, make_pbs_file, qsub, wait_run, tunnel, tell)

    ## commands to write login node script to its file
    make_lnode_file = file_it(lnode_script, fn$lnode_file, "..LNODE-EOF..")

    ## command to run the login node script
    run_file = paste0("source ", fn$lnode_file)

    ## put it all together
    lnode_command = c(cd, clean, make_lnode_file, run_file)
    if(show) print(lnode_command)

    lnode_command
}

#' Constructs a string of arguments for ssh exection on Rhea
#' @param port The port for server connection
#' @param verbose Should the args be printed?
#' @value A string of ssh arguments.
args_tunnel = function(port = 55555, show = FALSE) {
    args = paste0(" -f -L ", port, ":localhost:", port)
    if(show) print(args)
    args
}

#' Constructs a shell script as a vector of strings. Its purpose is to run
#' on a login node of rhea.ccs.ornl.gov, submit a pbdR server batch job that
#' runs for a specified time or until closed. It also opens an ssh tunnel
#' from the login node to the running server head node.
#'
#' @param nodes The number of nodes to allocate for the server.
#' @param npernode The number of MPI ranks (R sessions) to start per node.
#' @param walltime A string "hh:mm:ss" giving the maximum length of time
#'                 in hours:minutes:seconds before server timeout shutdown.
#' @param user Username for login.
#' @param machine Hostname to run the server, resolvable on your platform.
#' @param port The port used by the server.
#' @param account Account used for your cluster allocation
#' @param modules A vector of strings giving the modules to load before
#' starting the server.
#' @param rwd Remote working directory as a string.
#' @param verbose Should the preload command be printed?
#' @param warn_on_fork Set to \code{FALSE} to suppress a warning when forking
#' a process (for example, when invoking \code{parallel::mclapply()}).
#' @return A shell script as a vector of strings.
preload_rhea = function(nodes = 1, npernode=16, walltime = "01:00:00",
                        user = NULL, machine = "rhea.ccs.ornl.gov",
                        port = 55555,
                        account = NULL, modules=c("r"), rwd = "~",
                        show = FALSE, warn_on_fork = TRUE) {
    ## Returns a script as a vector of strings to run on a login node so that it
    ##    starts a pbdR server and opens an ssh tunnel to its head node on
    ##    the specified port

    ## set working directory and file names to be created there
    cd = paste0("cd ", rwd)
    pbs_file = ".pbdR_server.pbs" # script to run on head node
    head_node_file = ".pbdR_hostname"
    lnode_file = ".pbdR_lnode.sh"

    ## file cleanup command
    clean = paste("rm -f", pbs_file, head_node_file, lnode_file)

    ## create qsub (head node) PBS script
    wof = ifelse(warn_on_fork, "", " --mca mpi_warn_on_fork 0 ")
    pbs_parameters = c(
        "#!/bin/bash",
        "# File name: pbdR_server",
        paste0("#PBS -A ", account),
        paste0("#PBS -l walltime=", walltime),
        paste0("#PBS -l nodes=", nodes),
        "#PBS -e pbdR_server.e",
        "#PBS -o pbdR_server.o"
    )
    mod_vec = vector("character", length(modules))
    for(m in seq(along=modules))
        mod_vec[m] = paste0("module load ", modules[m])
    commands = c(
        "module list",
        cd,
        paste0("hostname > ", head_node_file),
        paste0("mpirun ", wof, " --map-by ppr:", npernode,
               ":node Rscript -e 'pbdCS::pbdserver()'")
    )
    pbs_script = c(pbs_parameters, mod_vec, commands)

    ## commands to write head script to its file
    make_pbs_file = file_it(pbs_script, pbs_file, "..PBS-EOF..")

    ## command to qsub the script
    qsub = paste0("qsub ", pbs_file)

    ## commands to wait for server start with progress dots
    wait_run = paste0("while [ ! -f ", head_node_file,
                      " ]; do sleep 1; echo -n '.'; done; echo '.' ")

    ## command to tunnel from login to head node
    tunnel = paste0("ssh -f -L ", port, ":localhost:", port,
                    " -N \\$(cat ", head_node_file, ")")

    ## commands to report login node and head node names
    tell = c("echo 'server login node: ' \\$(hostname)",
             paste0("echo 'server head node: ' \\$(cat ", head_node_file, ")"))

    lnode_script = c(cd, make_pbs_file, qsub, wait_run, tunnel, tell)

    ## commands to write login node script to its file
    make_lnode_file = file_it(lnode_script, lnode_file, "..LNODE-EOF..")

    ## command to run the login node script
    run_file = paste0("source ", lnode_file)

    ## put it all together
    lnode_command = c(cd, clean, make_lnode_file, run_file)
    if(show) print(lnode_command)

    lnode_command
}

#' Constructs a string of arguments for ssh exection on Rhea
#' @param port The port for server connection
#' @param verbose Should the args be printed?
#' @value A string of ssh arguments.
args_rhea = function(port = 55555, show = FALSE) {
    args = paste0(" -f -L ", port, ":localhost:", port)
    if(show) print(args)
    args
}

preload_or_condo = function(nodes = 1, npernode=16, walltime = "01:00:00",
                           user = NULL, machine = "rhea.ccs.ornl.gov",
                           port = 55555,
                           account = NULL, modules=c("R"), rwd = "~",
                           show = FALSE, warn_on_fork = TRUE) {
    ## Returns a script as a vector of strings to run on a login node so that it
    ##    starts a pbdR server and opens an ssh tunnel to its head node on
    ##    the specified port

    ## set working directory and file names to be created there
    cd = paste0("cd ", rwd)
    pbs_file = ".pbdR_server.pbs" # script to run on head node
    head_node_file = ".pbdR_hostname"
    lnode_file = ".pbdR_lnode.sh"

    ## file cleanup command
    clean = paste("rm -f", pbs_file, head_node_file, lnode_file)

    ## create qsub (head node) PBS script
    wof = ifelse(warn_on_fork, "", " --mca mpi_warn_on_fork 0 ")
    pbs_parameters = c(
        "#!/bin/bash",
        "#PBS -N pbdR_server",
        "#PBS -e pbdR_server.e",
        "#PBS -o pbdR_server.o",
        paste0("#PBS -A ", account),
        paste0("#PBS -l walltime=", walltime),
        paste0("#PBS -l nodes=", nodes, ":ppn=32"),
        "#PBS -l qos=std",
        "#PBS -q batch",
        "#PBS -W group_list=cades-ccsd",
        "#PBS -m abe"
    )
    mod_vec = vector("character", length(modules))
    for(m in seq(along=modules))
        mod_vec[m] = paste0("module load ", modules[m])
    commands = c(
        "module list",
        cd,
        paste0("hostname > ", head_node_file),
        paste0("mpirun ", wof, " --map-by ppr:", npernode,
               ":node Rscript -e 'pbdCS::pbdserver()'")
    )
    pbs_script = c(pbs_parameters, mod_vec, commands)

    ## commands to write head script to its file
    make_pbs_file = file_it(pbs_script, pbs_file, "..PBS-EOF..")

    ## command to qsub the script
    qsub = paste0("qsub ", pbs_file)

    ## commands to wait for server start with progress dots
    wait_run = paste0("while [ ! -f ", head_node_file,
                      " ]; do sleep 1; echo -n '.'; done; echo '.' ")

    ## command to tunnel from login to head node
    tunnel = paste0("ssh -f -L ", port, ":localhost:", port,
                    " -N \\$(cat ", head_node_file, ")")

    ## commands to report login node and head node names
    tell = c("echo 'server login node: ' \\$(hostname)",
             paste0("echo 'server head node: ' \\$(cat ", head_node_file, ")"))

    lnode_script = c(cd, make_pbs_file, qsub, wait_run, tunnel, tell)

    ## commands to write login node script to its file
    make_lnode_file = file_it(lnode_script, lnode_file, "..LNODE-EOF..")

    ## command to run the login node script
    run_file = paste0("source ", lnode_file)

    ## put it all together
    lnode_command = c(cd, clean, make_lnode_file, run_file)
    if(show) print(lnode_command)

    lnode_command
}

#' Constructs a string of arguments for ssh exection on Rhea
#' @param port The port for server connection
#' @param verbose Should the args be printed?
#' @value A string of ssh arguments.
args_or_condo = function(port = 55555, show=FALSE) {
    args = paste0(" -f -L ", port, ":localhost:", port)
    if(show) print(args)

    args
}
