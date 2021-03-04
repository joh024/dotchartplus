source("Rnoweb.R")

for(curf in c("layouthelper", "formulae", "dotchartplus", "dotchartplus-demos"))
  Rnoweb(paste0(curf, ".Rnw"))

source("layouthelper.R")
source("formulae.R")
source("dotchartplus.R")
source("dotchartplus-demos.R")
