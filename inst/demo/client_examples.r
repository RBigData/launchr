## This script is intended for learning how to interact with a distributed
## server running multiple R sessions.
##
## Install with the following:
## devtools::install_github("RBigData/launchr")

library(launchr)

cades = function(x, ...) {
    cades_unique = c(
        ## ppn=32 due to condo policy to concentrate cores on nodes when < 32
        paste0("#PBS -l nodes=", nodes, ":ppn=32"),
        "#PBS -l qos=std",
        "#PBS -q batch",
        "#PBS -W group_list=cades-ccsd",
        "#PBS -m abe"
    )
    ## add code to replace line in script !!!!
    script
}

rhea = function(x, ...) x

launch(SERVER = "rhea.ccs.ornl.gov", FUN = rhea, verbose = 1, nodes = 2, npernode = 16,
       account = "gen011", walltime = "01:00:00", rwd = "~/eof/hosvd_code", warn_on_fork = FALSE)

launch(nodes = 2, npernode = 16, server = "or-condo-login.ornl.gov", modules = c("R"), user = Sys.getenv("USER"), account = "ccsd", walltime = "01:00:00", rwd = "~/test")

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

exit(client.only=FALSE)

