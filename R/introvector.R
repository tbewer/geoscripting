# load sp package
library(sp)
library(rgdal)
# use rgeos for computing the length of lines 
library(rgeos)
# coordinates of two points identiefied in Google Earth, for example
pnt1_xy <- cbind(5.6725, 51.9883)   # enter your own coordinates
pnt2_xy <- cbind(5.6740, 51.9888) 
# combine coordinates in single matrix
coords <- rbind(pnt1_xy, pnt2_xy)
# make spatial points object
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
mypoints <- SpatialPoints(coords, proj4string=prj_string_WGS)
# inspect object
class(mypoints)
str(mypoints)

# create and display some attribute data and store in a data frame
mydata <- data.frame(cbind(id = c(1,2), 
                           Name = c("Tom", 
                                    "Efka")))
# make spatial points data frame
mypointsdf <- SpatialPointsDataFrame(
  coords, data = mydata, 
  proj4string=prj_string_WGS)
# plot pointsin a nice way
spplot(mypointsdf, zcol="Name", col.regions = c("red", "blue"), 
       xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01), 
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),
       scales= list(draw = TRUE))
spplot(mypointsdf[,2], col.regions = c(1,2))
# make line from coordinates
simple_line <- Line(coords)
lines_obj <- Lines(list(simple_line), "1")
spatlines <- SpatialLines(list(lines_obj), proj4string=prj_string_WGS)
line_data <- data.frame(Name = "straight line", row.names="1")
mylinesdf <- SpatialLinesDataFrame(spatlines, line_data)

# first grid covers entire extent 
p_lin <- spplot(mypointsdf[,2], col.regions=c("blue","red"),
                xlim = bbox(mypointsdf)[1, ]+c(-0.001,0.001), 
                ylim = bbox(mypointsdf)[2, ]+c(-0.001,0.001),
                scales= list(draw = TRUE)) # scales allows to plot coordinate axes

p <- p_lin + layer(sp.lines(mylinesdf, col.regions = "blue", scales= list(draw = TRUE)))
p

# write to kml ; below we assume a subdirectory data within the current 
# working directory.
dir.create("data", showWarnings = FALSE) 
writeOGR(mypointsdf, file.path("data","mypointsGE.kml"), 
         "mypointsGE", driver="KML", overwrite_layer=TRUE)
writeOGR(mylinesdf, file.path("data","mylinesGE.kml"), 
         "mylinesGE", driver="KML", overwrite_layer=TRUE)
# add addiotinal lines created in Gooogle Earth
dsn = file.path("data","route.kml")
ogrListLayers(dsn) ## to find out what the layers are
myroute <- readOGR(dsn, layer = ogrListLayers(dsn))
# assign correct projection
proj4string(myroute) <- prj_string_WGS

# add lines to existing SpatialLinesDataFrame
myroute$Description <- NULL # delete Description
mylinesdf <- rbind(mylinesdf, myroute)
## projection : see http://www.spatialreference.org/ for projection specific transformation
# define CRS object for RD projection
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
# perform the coordinate transformation from WGS84 to RD
mylinesRD <- spTransform(mylinesdf, prj_string_RD)

## calculate lenght of lines
mylinesdf$length <- gLength(mylinesRD, byid=T)

mylinesdf@data
#or
data.frame(mylinesdf)
## now tranform points into circles
# first assign RD_new CRS string
mypointsRD <- spTransform(mypointsdf, prj_string_RD)

pnt1_rd <- coordinates(mypointsRD)[1,]
pnt2_rd <- coordinates(mypointsRD)[2,]
# make circles around points, with radius equal to distance between points
# define a series of angles going from 0 to 2pi
ang <- pi*0:200/100
circle1x <- pnt1_rd[1] + cos(ang) * mylinesdf$length[1]
circle1y <- pnt1_rd[2] + sin(ang) * mylinesdf$length[1]
circle2x <- pnt2_rd[1] + cos(ang) * mylinesdf$length[1]
circle2y <- pnt2_rd[2] + sin(ang) * mylinesdf$length[1] 
c1 <- cbind(circle1x, circle1y)
c2 <- cbind(circle2x, circle2y)
# plot intermediate results
plot(c2, pch = 19, cex = 0.2, col = "red", xlim = range(circle1x, circle2x),ylim = range(circle1y, circle2y))
points(c1, pch = 19, cex = 0.2, col = "blue")
points(mypointsRD, pch = 3, col= "darkgreen")
lines(mylinesRD[2,])
