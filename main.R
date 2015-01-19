# Name
# Date
library(raster)

source('R/ageCalculator.R')
source('R/HelloWorld.R')
source('R/minusRaster.R')


HelloWorld('john')
ageCalculator(2009)

# import dataset
r <- raster(system.file("external/rlogo.grd", package="raster")) 
r2 <- r 
# Filling the rasterLayer with new values.
r2[] <- (1:ncell(r2)) / 10
# Performs the calculation
r3 <- minusRaster(r, r2) 