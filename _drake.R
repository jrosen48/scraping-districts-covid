source("R/functions.R")
source("R/packages.R")
source("R/plan.R")

vis_drake_graph(plan)

drake_config(plan, verbose = 2)