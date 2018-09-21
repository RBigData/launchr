## This script is intended for learning how to interact with a distributed
## server running multiple R sessions.
##
## Install with the following:
## devtools::install_github("RBigData/launchr")

library(launchr)

## Custom function to replaces one line in default launch script by several lines
cades = function(script, nodes, ...) {
    new_lines = c(
        ## ppn=32 due to policy to concentrate cores on nodes when < 32
        paste0("#PBS -l nodes=", nodes, ":ppn=32"),
        "#PBS -l qos=std",
        "#PBS -q batch",
        "#PBS -W group_list=cades-ccsd",
        "#PBS -m abe"
    )
    ## new_lines replace one line in script
    line = which(startsWith(script, "#PBS -l nodes="))
    c(script[1:(line - 1)], new_lines, script[(line + 1):length(script)])
}

## example of a launch using default scripts
launch(server = "rhea.ccs.ornl.gov", verbose = 1,
       nodes = 2, npernode = 16, modules = c("hdf5", "r"), account = "gen011",
       walltime = "01:00:00", rwd = "~/eof/hosvd_code", warn_on_fork = FALSE)

## example of a launch using custom modification of a default script
launch(server = "or-condo-login.ornl.gov", FUN = cades, verbose = 2,
       nodes = 2, npernode = 16, modules = c("R"), account = "ccsd",
       walltime = "01:00:00", rwd = "~/test", warn_on_fork = FALSE)

## server submitted in queue. You MUST wait for node report
remoter::client() # submit ONLY when server head node reports
comm.size()
comm.rank()

## to see server session information and packages
if(comm.rank() == 0) sessionInfo()
if(comm.rank() == 0) loadedNamespaces()
if(comm.rank() == 0) .libPaths()

## reduction across ranks retains dimensions
x = c(1, 2)
x
allreduce(x)
allreduce(x, op="max")
x = matrix(1, nrow = 3, ncol = 4)
x
allreduce(x)
x = array(1, dim = c(4, 3, 2))
x
allreduce(x)

## remember that data on the server is different from the data on
## the client (your local R session)
exit()
x
remoter::client()
x

x = matrix(comm.rank()*10, nrow = 3, ncol = 4)
comm.print(x) # New concept: Who prints?
comm.print(x, rank.print=19) # Can inspect data on other ranks

exit(client.only=FALSE) # shuts down server and exits client

