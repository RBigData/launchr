remoter::client()
comm.size()
comm.rank()

if(comm.rank() == 0) sessionInfo()
if(comm.rank() == 0) loadedNamespaces()
if(comm.rank() == 0) .libPaths()


x = c(1, 2)
x
allreduce(x)
allreduce(x, op="max")
x = matrix(1, nrow = 3, ncol = 4)
x
allreduce(x)



x = matrix(comm.rank() - 16, nrow = 3, ncol = 4)
comm.print(x) # New concept: Who prints?
comm.print(x, rank.print=19) # Can inspect data on other ranks
exit(client.only=FALSE)



library(pbdDMAT)
init.grid()
xd = new("ddmatrix", Data = x, bldim = dim(x), ICTXT = 2,
         dim = dim(x)*c(comm.size(), 1), ldim=dim(x))
class(xd)
comm.print(xd)
dim(xd)
dim(xd@Data)
comm.print(xd@Data)
comm.print(xd@Data, rank.print = 17)
xdbc = as.blockcyclic(xd)
cov(xd)
svd(xd)

