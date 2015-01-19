# John Doe
# December 2014
# Import packages

library(raster)
# Function to substract 2 rasterLayers
minusRaster <- function(x, y, plot=FALSE) { 
  z <- x - y
  if (plot) {
    plot(z)
  }
  return(z)
}