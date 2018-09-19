
### these need to be recast as calling and editing an existing default script!!!!!!!!!
### must also add correct FUN ideas in launch() function !!!!!!!!!
pbs_rhea = function(nodes = 1, npernode = 16, rwd = "~\ ", modules, walltime = "01:00:00",
                    account, warn_on_fork = TRUE, fn, ...) {
  pbs_common= c(
    "#!/bin/bash",
    "#PBS -N pbdR_server",
    "#PBS -j oe",
    "#PBS -o .pbdR_server.o",
    paste0("#PBS -A ", account),
    paste0("#PBS -l walltime=", walltime)
  )
  
  pbs_unique = paste0("#PBS -l nodes=", nodes)

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

  preload = c(pbs_common, pbs_unique, mod_vec, commands)
}
