read_xgc_mesh = function(file) {
  ## read mesh in parallel and allgather a full copy to everyone
  ##          this allows multiple plots in parallel
  if(endsWith(file, ".bp")) {
    mesh = bp.read(file, c("n_n", "n_t"))
    iopair_t = comm.chunk(mesh$n_t, form = "iopair")
    iopair_n = comm.chunk(mesh$n_n, form = "iopair")
    buffer =
      bp.read(file, c("nd_connect_list", "rz"),
              start=list(c(iopair_t[1], 0), c(iopair_n[1], 0)),
              count=list(c(iopair_t[2], 3), c(iopair_n[2], 2)))
    
    nd_connect_list = do.call(c, allgather(buffer[[1]]))
    dim(nd_connect_list) = c(3, mesh$n_n)
    rz = do.call(c, allgather(unlist(buffer[[2]])))
    dim(rz) = c(2, mesh$n_n)
  } else if(endsWith(file, ".h5")) {
    mesh = list(n_n=NULL, n_t=NULL)
    buffer = vector("list", 2)
    mesh$n_n = h5read(file, "n_n")
    
    iopair_n = comm.chunk(mesh$n_n, form = "iopair")
    ## rhdf5 needs to add 1 to C/Python written data!
    buffer = h5read(file, "coordinates/values",
                    start=c(0 + 1, iopair_n[1] + 1),
                    count=c(2, iopair_n[2]))
    rz = do.call(c, allgather(unlist(buffer)))
    dim(rz) = c(2, mesh$n_n)
    nd_connect_list = NULL
  } else {
    comm.print("Don't know file type")
    stop(11)
  }
  list(rz = t(rz), nd_connect_list = nd_connect_list)
}

read_xgc = function(file, var) {
  ## reads a 2-d array: a set of ploidal planes
  ## TODO make generic with a parameter on which dimension to split
  if(endsWith(file, ".bp")) {
    ## TODO not generic! needs to get dimensions from varname!
    par = bp.read(file, c("nnode", "nphi"))
    iopair = comm.chunk(par$nnode, form = "iopair")
    
    buffer = bp.read(file, var, start = c(iopair[1], 0),
                     count = c(iopair[2], par$nphi))[[1]]
  } else if(endsWith(file, ".h5")) {
    ## hdf5 file
    gd = h5ls(file) # TODO There has to be a better way to get dims!!
    par = as.numeric(strsplit(gd[gd$name == var, ]$dim, "x")[[1]])
    iopair = comm.chunk(par[2], form = "iopair")
    buffer = h5read(file, var,
                    start=c(0 + 1, iopair[1] + 1),
                    count=c(par[1], iopair[2]))
  } else {
    comm.print("Don't know file type")
    stop(11)
  }
  
  dim(buffer) = c(par[1], iopair[2])
  buffer
}

read_xgc_window = function(file, var, time, window) {
  ## read time window
  ## TODO needs to be generic with indicators of what to do with which
  ## dimensions.
  
  rex = "#+" # get length of rex in file name to replace with time
  rl = attr(regexpr(rex, file), "match.length")
  
  start = time - window %/% 2  # center the window on time
  
  ## construct file name for each window element and read the file
  wdat = vector("list", window)
  for(w in 1:window) {
    file_t = sub(rex, sprintf(paste0("%0.", rl, "d"), start + w - 1), file)
    wdat[[w]] = read_xgc(file_t, var)
  }
  ## wdat is a list with each element a time step in the window
  ## column dimension is the mesh
  ## row dimension is the poloidal slice
  
  ## TODO convert this to S4 (with a tensor package?)
  tdim = c(dim(wdat[[1]])[1], window, dim(wdat[[1]])[2]) # dims of tensor
  tdist = c(FALSE, FALSE, TRUE) # which dimensions are distributed
  tdimname = c("toro", "time", "mesh")
  umat = do.call(rbind, wdat) # final unfolded distributed matrix
  dim(umat) = tdim # this should make it a 3d array or a tensor
  class(umat) = "array"
  list(Data=umat, tdist=tdist, tdimname=tdimname)
}

byrow_to_bycol_gather = function(dat) {
  rcols = comm.chunk(ncol(dat), form="vector", all.rank=TRUE)
  for(r in 1:comm.size()) {
    if(comm.rank() == r - 1L) {
      Vc = do.call(rbind, gather(dat[, rcols[[r]]], rank.dest = r - 1L))
    } else {
      Vtemp = gather(dat[, rcols[[r]]], rank.dest = r - 1L)
    }
  }
  Vc
}

screePlot = function(d, file, which=1:length(d), cumvar=FALSE,
                     log10=FALSE, percent=FALSE, ylims=NULL,
                     width=5, height=4, sizeL=0.3, sizeP=0.4) {
  ## scree plot with options
  pdf(paste0(file, ".pdf"), width, height)
  df = data.frame(component = which, var = d[which],
                  cumvar = cumsum(d[which]))
  if(percent) {
    df$var = 100*df$var/sum(d)
    df$cumvar = 100*df$cumvar/sum(d)
  }
  myplot = ggplot(df, aes(component, var)) + geom_line(size = sizeL) +
    geom_point(size = sizeP, color = "blue")
  if(cumvar) myplot = myplot +
    geom_line(aes(component, cumvar), size = sizeL) +
    geom_point(aes(component, cumvar), size = sizeP,
               color = "red")
  if(log10) myplot = myplot + scale_y_log10()
  if(!is.null(ylims[1])) myplot = myplot + expand_limits(y = ylims)
  ggplot2:::print.ggplot(myplot + theme_bw())
  dev.off()
}

scree_plots = function(d, ttag, myrank, screedir) {
  ## need a ease of use concept for if-elses below? list with lapply?
  ##    current version only works with 5+ ranks!!
  if(myrank == 0) {
    screePlot(d^2, paste0(screedir, "scree", ttag))
  } else if(myrank == 1) {
    screePlot(d^2, paste0(screedir, "scree100Log", ttag), 1:100, log10=TRUE)
  } else if(myrank == 2) {
    screePlot(d^2, paste0(screedir, "screeLog", ttag), log10=TRUE)
  } else if(myrank == 3) {
    screePlot(d^2, paste0(screedir, "screeCumm", ttag), cumvar=TRUE)
  } else if(myrank == 4) {
    screePlot(d^2, paste0(screedir, "screeCummP", ttag), cumvar=TRUE,
              percent=TRUE)
  }
}

spacePlot = function(mesh, vals, file = "spacePlot", tagImage=FALSE) {
  ## plots vals on mesh to pdf file
  pdata = data.frame(x = mesh[, 1], y = mesh[, 2], val= vals)
  pdf(paste0(file, ".pdf"), 10, 8)
  myplot = ggplot(pdata, aes(x, y, color = val)) + geom_point(size=0.5) +
    scale_color_gradient2(low="blue", mid="green", high="red") +
    theme_void()
  if(tagImage) myplot = myplot + ggtitle(file)
  ggplot2:::print.ggplot(myplot)
  dev.off()
}

space_plots = function(mesh, d, Vr, maxplots, myrank, tag="") {
  ## Plot the poloidal slice coherent structures
  Vc = byrow_to_bycol_gather(Vr)
  nplots = min(maxplots, length(d))
  myPCs = comm.chunk(nplots, form="vector", type="balance")
  my.d = d[myPCs]
  PCdigits = floor(log10(length(d)) + 1)
  ftag = paste0(plotdir, tag, "pc%0.", PCdigits, "d")
  for(i in seq_along(myPCs)) {
    spacePlot(mesh, Vc[, i]*my.d[i], sprintf(ftag, myPCs[i]), tagImage=TRUE)
  }
}
## End function definition
