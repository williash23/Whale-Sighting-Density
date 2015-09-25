# Sara Williams
# 9/15/2015
# Denisty of ship tracks in grid over study area
################################################################################

#  Denisty of ship tracks in grid over study area - first step was to use ship track (Trk_0613.shp)
#   a shapefile received from Karen. It is all the GPS taken ship tracks from the start of the 
#   project through 2013. In ArcMap, I dissolved the ship tracks based on Event_ID (the date and 
#   name of the observer so that a track of a whole cruise was dissolved into a single line). Then,
#   I intersected this shapefile with a grid created  I created in ArcMap (0.011 lat x 0.011 lon) of
#   the study area using the pixellate function. I had already filtered the grid so that it only 
#   covered areas of water. After pixellating, I summed  the number the length of lines going 
#   through each grid cell and use this as a factor influencing whale detection. Intersection 
#   and summing was done in R using the packages maptools, and spatstats.

################################################################################	   
library(rgeos)
require(sp)
require(raster)
require(rgdal)
require(spatstat)
require(maptools)


# Load data (already in UTM)
ship <- shapefile("E:/AK_Shapefiles/Ship_track_diss.shp")

# Need to convert to a line segment pattern object with maptools
shipPSP <- as.psp(as(ship, 'SpatialLines'))

# Calculate lengths per cell
trackLength<- raster(pixellate.psp(ship, dimyx=1000), crs=projection(ship)))

#  Crop to smaller extent
track_sum_length <- crop(trackLength, extent(377769, 530000, 6300000, 6548068))

#  Write to raster (netCDF format)
 writeRaster(track_sum_length, filename='track_sum_length.nc', format="CDF")
		# class       : RasterLayer 
		# dimensions  : 607, 460, 279220  (nrow, ncol, ncell)
		# resolution  : 330.7813, 408.6424  (x, y)
		# extent      : 377768.9, 529928.3, 6300022, 6548068  (xmin, xmax, ymin, ymax)
		# coord. ref. : +proj=utm +zone=8 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
		# data source : C:\Users\sara.williams\Documents\GitHub\Whale-Sighting-Density\track_sum_length.nc 
		# names       : layer 
		# zvar        : layer 
 

# Plot
# spplot(trackLength, scales = list(draw=TRUE), xlab="x", ylab="y", 
       # col.regions= wes_palette("Zissou"))
################################################################################
# #  Alternate method:
# #  Denisty of ship tracks in grid over study area
# setwd("E:/AK_Shapefiles")

# #  Raster 100 x 100 cells   	   
# ship <- shapefile("E:/AK_Shapefiles/Ship_track_diss.shp")
# rs<- raster(extent(ship), crs=projection(ship), nrows=100, ncols=100)
# rs[] <- 1:ncell(rs)

# # Intersect lines with raster "polygons" and add length to new lines segments
# rsp <- rasterToPolygons(rs)
# rp <- intersect(ship, rsp)
# rp$length <- gLength(rp, byid=TRUE) / 1000
# x <- tapply(rp$length, rp$layer, sum)
# r <- raster(rs)
# r[as.integer(names(x))] <- x

		# #  Raster 1000 x 1000 cells   	  
		# ship <- shapefile("E:/AK_Shapefiles/Ship_track_diss.shp")
		# rs_lg<- raster(extent(ship), crs=projection(ship), nrows=1000, ncols=1000)
		# rs_lg[] <- 1:ncell(rs_lg)

		# # Intersect lines with raster "polygons" and add length to new lines segments
		# rsp_lg <- rasterToPolygons(rs_lg)
		# rp_lg <- intersect(ship, rsp_lg)
		# rp_lg$length <- gLength(rp_lg, byid=TRUE) / 1000
		# x_lg <- tapply(rp_lg$length, rp_lg$layer, sum)
		# r_lg <- raster(rs_lg)
		# r_lg[as.integer(names(x_lg))] <- x_lg

# # Plot results
# library(RColorBrewer)
# spplot(r, scales = list(draw=TRUE), xlab="x", ylab="y", 
       # col.regions=colorRampPalette(brewer.pal(9, "YlOrRd")), 
       # sp.layout=list("sp.lines", rp), 
       # par.settings=list(fontsize=list(text=15)), at=seq(0, 1800, 200))
# ################################################################################

# #  Generate grid from SST raster files
# sst <- readGDAL("E:/AK_Shapefiles/sst_201408221.tif")
# sst_grid <- as(sst, 'SpatialGridDataFrame')

# #  Convert grid to polygons of grid
# sst_poly <- Grid2Polygons(sst_grid)