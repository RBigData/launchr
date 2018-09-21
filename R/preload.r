
#' Wraps a script with shell commands to write it to a file.
#' Sometimes referred to as a "here document"
#' @param script A vector of strings, usually each a shell command.
#' @param file_name A string giving the file name to use. Careful not to
#' conflict with an existing file name as it will be silently deleted and
#' overwritten.
#' @param eof A string to use for end-of-file. Must not occurr within the
#' script and must be unique among potential nested uses of this function
#' for a script.
#' @return A vector of strings, each string a line of the script.
#' @export
here_doc = function(script, file_name, eof) {
    c(paste0("cat >> ", file_name, " << ", "'", eof, "'"), script, eof)
}




#' Creates a default pbs script as a vector of strings. Modification
#' for platform-specific parameters should be done with a function via
#' the FUN parameter of the \code{launch()} function rather than
#' rewriting \code{pbs_default()}.
#' @param nodes Number of nodes to use for the server
#' @param npernode Number of R instances to run per node
#' @param rwd Working directory on server. This is where script files
#'     are created.
#' @param modules A vector of strings giving the modules to load
#'     before starting the server.
#' @param walltime A string "hh:mm:ss" giving the maximum length of
#'     time in hours:minutes:seconds before server timeout
#'     shutdown. Note that while the server is running you may be
#'     incurring time charges to your account.
#' @param account Account used for your cluster allocation
#' @param warn_on_fork Set to \code{FALSE} to suppress a warning when
#'     forking a process (for example, when invoking
#'     \code{parallel::mclapply()}).
#' @param fn A list providing names used for script files that are
#'     created in the server rwd directory.
#' @return A shell script as a vector of strings.
#' @export
pbs_default = function(nodes = 1, npernode = 16, modules = "r",
                       walltime = "01:00:00", account,
                       warn_on_fork = TRUE, fn = NULL, rwd = "~/") {
  pbs = c(
    "#!/bin/bash",
    "#PBS -N pbdR_server",
    "#PBS -j oe",
    paste0("#PBS -o ", fn$server_log),
    paste0("#PBS -A ", account),
    paste0("#PBS -l walltime=", walltime),
    paste0("#PBS -l nodes=", nodes)
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

  c(pbs, mod_vec, commands)
}






#' Constructs a shell script as a vector of strings. Its purpose is to run
#' in a login node shell and submit a pbdR server batch job that
#' runs for a specified time or until closed. It also opens an ssh tunnel
#' from the login node to the running server head node.
#'
#' Modification for platform-specific parameters should be done with a
#' function via the FUN parameter of the \code{launch()} function
#' rather than rewriting \code{lnode_script()}.
#'
#' @param fn A list of file names that will be created on the
#'     server. Normally the defaults specified in \code{launch()}
#'     function are fine.
#' @param port The port used by the server.
#' @param rwd Remote working directory as a string.
#' @param ... Unmatched arguments are passed down to \code{pbs_default()}
#' @return A shell script as a vector of strings.
lnode_script = function(fn, port, rwd = "~/", ...) {
    ## Returns a script as a vector of strings to run on a login node so that it
    ##    starts a pbdR server and opens an ssh tunnel to its head node on
    ##    the specified port

    ## set working directory and file names to be created there
    cd = paste0("cd ", rwd) # TODO where will this be matched?

    ## file cleanup command
    clean = paste("rm -f", fn$pbs_file, fn$head_node_file, fn$lnode_file,
                  fn$server_log)

    ## commands to write pbs script to its file
    pbs_script = pbs_default(rwd = rwd, fn = fn, ...)
    make_pbs_file = here_doc(pbs_script, fn$pbs_file, "..PBS-EOF..")

    ## command to queue the script
    qsub = c(
        "echo 'Submitting pbdR server'",
        paste0("qsub ", fn$pbs_file)
        )

    ## commands to wait for server start with progress dots
    wait_run = paste0("while [ ! -f ", fn$head_node_file,
                      " ]; do sleep 1; echo -n '.'; done; echo '.' ")

    ## command to tunnel from login to head node
    tunnel = paste0("ssh -f -L ", port, ":localhost:", port,
                    " -N \\$(cat ", fn$head_node_file, ")")

    ## commands to report login node and head node names
    tell = c(
        "echo 'server login node: ' \\$(hostname)",
        paste0("echo 'server head node: ' \\$(cat ", fn$head_node_file, ")"),
        "echo 'Server running ... ready for client connections'"
    )

    lnode_script = c(cd, make_pbs_file, qsub, wait_run, tunnel, tell)

    ## commands to write login node script to its file
    make_lnode_file = here_doc(lnode_script, fn$lnode_file, "..LNODE-EOF..")

    ## command to run the login node script
    run_file = paste0("source ", fn$lnode_file)

    ## put it all together
    c(cd, clean, make_lnode_file, run_file)
}


#' Constructs a string of arguments for local ssh to forward a port
#' @param port The port for server connection
#' @return A string.
ssh_args = function(port) {
    args = paste0(" -f -L ", port, ":localhost:", port)
}
