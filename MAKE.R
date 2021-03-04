source("Rnoweb.R")
for(curf in c("layouthelper", "formulae", "dotchartplus", "dotchartplus-demos"))
  Rnoweb(paste0(curf, ".Rnw"))

## Build images for rendering the pdf
build_images = TRUE
if(build_images){
  source("layouthelper.R")
  source("formulae.R")
  source("dotchartplus.R")
  source("dotchartplus-demos.R")

  library(lattice)
  source("Examples_Sweave/ppyramid.R")

  Sweave("Examples_Sweave/layouthelper-examples.Rnw")
  Sweave("Examples_Sweave/dotchartplus-examples.Rnw")
  Sweave("Examples_Sweave/dotchartplus-demos-images.Rnw")
}
