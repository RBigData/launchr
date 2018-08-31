library(launchr)

launch(nodes = 4, npernode = 16, server = "rhea.ccs.ornl.gov",
       modules = c("r", "hdf5"), user = Sys.getenv("USER"),
       account = "your_account", walltime = "01:00:00", rwd = "~")
## server submitted in queue. You MUST wait for it to start.
remoter::client() # submit ONLY when server head node reports
comm.size()

##
## Note: After an interactive development phase, the code below
## (without the client-server section above) would be run in batch.
## Plotting functions below will run in batch but are temporarily
## disabled for the client-server demo.
##
suppressMessages(library(rhdf5)) # can read hdf5 in parallel chunks
suppressMessages(library(pbdIO)) # some I/O helper functions
suppressMessages(library(kazaam)) # work with skinny matrices
suppressMessages(library(ggplot2)) # grammar of graphics plots
cwd = "/lustre/atlas/world-shared/gen011/pbdR_demo/"
window = 11 # number of time steps in window
w_center = 400 # ref time step size (first time in window)
maxplots = 24 # limit # plots
out_dir = paste0("/lustre/atlas/world-shared/gen011/pbdR_demo/",
                 "xgc-w", window, "-c", w_center, "/")

source("tensor_function_def.r")

myrank = comm.rank()
commsize = comm.size()

## note file names and variable names specfic to demo data
data_dir = "/lustre/atlas/world-shared/gen011/pbdR_demo/data/"
file_mesh= paste0(data_dir, "xgc.mesh.h5")
file_var = paste0(data_dir, "xgc.3d.#####.h5")
var = "dpot"

## Crate output directories
screedir = paste0(out_dir, "scree/")
ref = sprintf("%0.5d", w_center)
plotdir = paste0(out_dir, "plots", ref, "/")
if(myrank == 0) { # only one rank should be creating a directory
    dir.create(out_dir, showWarnings = FALSE)
    dir.create(screedir, showWarnings = FALSE)
    dir.create(plotdir, showWarnings = FALSE)
}
barrier() # all ranks must wait for directory creation

## parallel read the xgc mesh file and allgather to ranks
mesh = read_xgc_mesh(file_mesh)$rz

## Parallel read a window of steps and create tensor array as
## list with attributes.  Mesh dimension is split across ranks.
tens = read_xgc_window(file_var, var, w_center, window)$Data
tdim = dim(tens) # tensor dimensions (1, 2, 3d) = (toro, time, mesh)

## u1 slices: want dimensions (1, 3d*2) - need tshaq
u1tens = as.vector(tens) # (1, 2, 3d)
dim(u1tens) = c(tdim[1], tdim[2]*tdim[3]) # (1, 2*3d)
u1tens.s = tshaq(u1tens) # (1, 2*3d) tshaq
u1svd = svd(u1tens.s)
## scree_plots(u1svd$d, paste0(ref, "u1"), myrank, screedir)

## u3 mesh: want dimensions (3d, 1*2) - need shaq of transpose
u3tens = as.vector(tens) # (1, 2, 3d)
dim(u3tens) = c(tdim[1]*tdim[2], tdim[3]) # dim (1*2, 3d)
u3tens.s = shaq(t(u3tens)) # transposed so dim (3d, 1*2) shaq
u3svd = svd(u3tens.s)
## scree_plots(u3svd$d, paste0(ref, "u3"), myrank)
## space_plots(mesh, u3svd$d, DATA(u3svd$u), maxplots, myrank, tag="u3")

## u2 time: want dimensions (2, 1*3d) - need tshaq
## Note that tens has dims (toro, time, mesh) and we want an unfolding
##      with (time, toro*mesh). Respecting contiguous sections, we can
##      use u3tens with rows indexed by (toro, time) -> (time, toro)
rindex = rep(tdim[2]*(1:tdim[1] - 1), times=tdim[2]) +
    rep(1:tdim[2], each=tdim[1]) # reorders (1*2, 3d) to (2*1, 3d)
u2tens = u3tens[rindex, ] # reordered to (2, 1, 3d) dim (2*1, 3d)
dim(u2tens) = c(tdim[2], tdim[1]*tdim[3]) # dim (2, 1*3d)
u2tens.s = tshaq(u2tens) # (2, 1*3d)
u2svd = svd(u2tens.s)
## scree_plots(u2svd$d, paste0(ref, "u2"), myrank)

## Core tensor computation
u1core1 = crossprod(u1svd$u, u1tens) # dim (1, 2*3d), all local op

u3core1 = as.vector(u1core1) # (1, 2, 3d)
dim(u3core1) = c(tdim[1]*tdim[2], tdim[3]) # dim (1*2, 3d)
u2core1 = u3core1[rindex, ] # reordered to (2, 1, 3d) dim (2*1, 3d)
dim(u2core1) = c(tdim[2], tdim[1]*tdim[3]) # dim (2, 1*3d)
u2core21 = crossprod(u2svd$u, u2core1) # dim(2, 1*3d), all local

u3core21 = as.vector(u2core21) # (2, 1, 3d)
dim(u3core21) = c(tdim[2]*tdim[1], tdim[3]) # dim (2*1, 3d)
u3core321 = crossprod(u3svd$u, shaq(t(u3core21))) # dim (3td, 2*1)
## Note that 3td = 2*1 as 3 > 2*1 leads to 3 - 2*1 zero eigenvalues
##      so that leading dimension of u3core321 is 2*1 instead of 3.
##      u3core ends up a local matrix that is replicated. The shaq
##      crossprod collapses the long dimension.

## Now apply u2svd to whiten over time (it's a local operation!)
##     removes time covariance structure iid implications and systematic bias.
w2u2tens = crossprod(diag(1/u2svd$d) %*% u2svd$u, u2tens) # still (2*1, 3d)

## next reorder whitened to make u3 and redo u3 svd
w2u3tens = as.vector(w2u2tens) # (2, 1, 3d)
dim(w2u3tens) = c(tdim[2]*tdim[1], tdim[3]) # dim (2*1, 3d)
w2u3tens.s = shaq(t(w2u3tens)) # transposed so dim (3d, 2*1)
w2u3svd = svd(w2u3tens.s)
## scree_plots(w2u3svd$d, paste0(ref, "w2u3"), myrank)
## space_plots(mesh, w2u3svd$d, DATA(w2u3svd$u), maxplots, myrank, tag="w2u3")

## take u2-whitened and apply u1 whitening. Having systematic toro
##     distances may have iid implications and systematic bias.
w2u1tens = w2u3tens[order(rindex), ] # reverses (2*1, 3d) back to (1*2, 3d)
dim(w2u1tens) = c(tdim[1], tdim[2]*tdim[3]) # dim (1, 2*3d)
w1w2u1tens = crossprod(diag(1/u1svd$d) %*% u1svd$u, w2u1tens)

## next reoder w1w2u1 into u3 and redo u3 svd
w1w2u3tens = as.vector(w1w2u1tens) # (1, 2, 3d)
dim(w1w2u3tens) = c(tdim[1]*tdim[2], tdim[3]) # dim (1*2, 3d)
w1w2u3tens.s = shaq(t(w1w2u3tens)) # transposed so dim (3d, 1*2)
w1w2u3svd = svd(w1w2u3tens.s)
## scree_plots(w1w2u3svd$d, paste0(ref, "w1w2u3"), myrank)
## space_plots(mesh, w1w2u3svd$d, DATA(w1w2u3svd$u), maxplots, myrank, tag="w1w2u3")

## For batch, replace below by finalize()
exit(client.only = FALSE)
