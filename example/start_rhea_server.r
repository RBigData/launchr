library(launchr)

pscript = preload_rhea(nodes = 2, npernode = 16, rwd = "~/demo",
                       modules = c("r/3.4.2", "hdf5"), port = 55555)

args = args_rhea(port = 55555)

rhea_server = pbdRPC::machine(hostname = "rhea.ccs.ornl.gov",
               user = "ost",
               exec.type = "ssh",
               args = args)

pbdRPC::start_cs(machine = rhea_server, cmd = "",
                 preload = paste0(pscript, collapse = "\n"))

