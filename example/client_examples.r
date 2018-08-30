## This script is intended for learning how to interact with a distributed
## server running multiple R sessions.
##
## Install with the following:
## devtools::install_github("RBigData/launchr")

library(launchr)

launch(nodes = 2, npernode = 16, server = "rhea.ccs.ornl.gov",
       modules = c("r"), user = Sys.getenv("USER"), account = "your_account",
       walltime = "01:00:00", rwd = "~/demo")

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
