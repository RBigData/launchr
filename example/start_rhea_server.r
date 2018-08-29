library(launchr)

launch(nodes = 1, npernode = 16, server = "rhea.ccs.ornl.gov",
       modules = c("r"), user = "ost", account = "gen001",
       walltime = "01:00:00", rwd = "~/demo")
