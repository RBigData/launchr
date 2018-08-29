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
#' @return A shell script as a vector of strings.
preload_rhea = function(nodes = 1, npernode=16, walltime = "01:00:00",
                        user = "ost", machine = "rhea.ccs.ornl.gov",
                        port = 55555,
                        account = "gen011", modules=c("r"), rwd = "~") {
    ## Returns a script as a vector of strings to run on a login node so that it
    ##    starts a pbdR server and opens an ssh tunnel to its head node on
    ##    the specified port

    ## set working directory and file names to be created there
    cd = paste0("cd ", rwd)
    head_script_file_name = ".pbdR_server.pbs" # script to run on head node
    message_file_name = ".pbdR_hostname"
    preload_script_file_name = ".pbdR_lnode.sh"

    ## file cleanup command
    clean = paste("rm -f", head_script_file_name, message_file_name,
                  preload_script_file_name)

    ## create qsub (head node) PBS script
    pbs_parameters = c(
        "#!/bin/bash",
        "# File name: pbdR_server",
        paste0("#PBS -A ", account),
        paste0("#PBS -l walltime=", walltime),
        paste0("#PBS -l nodes=", nodes),
        "#PBS -e pbdRserver.e",
        "#PBS -o pbdRserver.o"
    )
    mod_vec = vector("character", length(modules))
    for(m in seq(along=modules))
        mod_vec[m] = paste0("module load ", modules[m])
    commands = c(
        "module list",
        cd,
        paste0("hostname > ", message_file_name),
        paste0("mpirun --map-by ppr:", npernode,
               ":node Rscript -e 'pbdCS::pbdserver()'")
    )
    head_script = c(pbs_parameters, mod_vec, commands)

    ## commands to write head script to its file
    make_head_script_file = c(
        paste0("cat >> ", head_script_file_name, " << ..PBS-EOF.."),
        head_script,
        "..PBS-EOF.."
    )
    ## command to qsub the script
    qsub = paste0("qsub ", head_script_file_name)

    ## commands to wait for server start with progress dots
    wait_run = paste0("while [ ! -f ", message_file_name,
                      " ]; do sleep 1; echo -n '.'; done; echo '.' ")

    ## command to tunnel from login to head node
    tunnel = paste0("ssh -f -L ", port, ":localhost:", port,
                    " -N \\$(cat ", message_file_name, ")")

    ## commands to report login node and head node names
    tell = c("echo 'server login: ' \\$(hostname)",
             paste0("echo 'server head node: ' \\$(cat ",
                    message_file_name, ")"))

    preload_script = c(cd, make_head_script_file, qsub, wait_run, tunnel, tell)

    ## commands to write login node script to its file
    make_preload_script_file = c(
        paste0("cat >> ", preload_script_file_name, " << '..LNODE-EOF..'"),
        preload_script,
        "..LNODE-EOF.."
    )

    ## command to run the login node script
    run_file = paste0("source ", preload_script_file_name)

    ## put it all together
    preload_command = c(cd, clean, make_preload_script_file, run_file)

    preload_command
}

#' Constructs a string of arguments for ssh exection on Rhea
#' @param port The port for server connection
#' @value A string of ssh arguments.
args_rhea = function(port = 55555) {
    paste0(" -f -L ", port, ":localhost:", port)
}
