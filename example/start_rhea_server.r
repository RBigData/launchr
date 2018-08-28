library(launchr)

pscript = preload_rhea(nodes = 2, npernode = 16, rwd = "~/demo",
                       modules = c("r/3.4.2", "hdf5"), port=port)

rhea_server = machine(hostname = "rhea.ccs.ornl.gov",
               user = "ost",
               exec.type = "ssh",
               args = args)

start_cs(machine = rhea_server, cmd = "",
         preload = paste0(pscript, collapse = "\n"))

