# Sara Williams
# 9/15/2015; updated 12/1/2015
# Denisty of ship tracks in grid over study area
################################################################################

#  Denisty of ship tracks in grid over study area - first step was to use ship track (Trk_0613.shp)
#   a shapefile received from Karen. It is all the GPS taken ship tracks from the start of the 
#   project through 2013. In ArcMap, I merged the ship tracks based on Event_ID (the date and 
#   name of the observer so that a track of a whole cruise was dissolved into a single line). Then,
#   I intersected this shapefile with a grid that I created in ArcMap (0.011 lat x 0.011 lon) of
#   the study area using the pixellate function. I had already filtered the grid so that it only 
#   covered areas of water. After pixellating, I summed  the number the length of lines going 
#   through each grid cell and use this as a factor influencing whale detection. Intersection 
#   and summing was done in R using the packages maptools, and spatstats.

################################################################################	   

library(rgeos)
library(sp)
library(raster)
library(rgdal)
library(spatstat)
library(maptools)

# Load data (already in UTM)
trk_dat <- shapefile("C:/Users/sara.williams/Desktop/AK/grid_trk_join.shp")
names(trk_dat) <- c("num_trks", "TARGET_FID", "FID_SA_grid", "length_1", "Event_ID",
												   "length_2")
trk_dat_df <-as.data.frame(trk_dat)

#  Clean up dataset and generate desired values
trk_sum_grid_ID <- trk_dat_df %>%
										  dplyr::select(num_trks, FID_SA_grid, length_1, length_2) %>%
										  mutate(grid_ID = FID_SA_grid + 1 ) %>%
										  mutate(trk_length = length_1/1000) %>%
										  dplyr::select(grid_ID, trk_length) %>%
										  as.data.frame(.) %>%
										  distinct(grid_ID) %>%
										  arrange(grid_ID)
										  
#  Generate empty dataframe of all grid cell ID's
grid_df <- as.data.frame(seq(1, 11827, 1))
names(grid_df) <- c("grid_ID")

#  Join ship track lengths to empty grid cell ID dataframe so each grid cell
#  has a ship track length value.
whole_grid_trk_length <- right_join(trk_sum_grid_ID, grid_df, by ="grid_ID")
#   Change NA's to 0's.
whole_grid_trk_length[is.na(whole_grid_trk_length)] <- 0
# write.csv(whole_grid_trk_length, file="trk_length_by_gridID.csv")	

#  Attach adjusted density sum count to other covariates associated with grid ID.
grid_env_covs_sighting_density <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/env_covs_sighting_density_by_gridID.csv")
grid_env_ship_covs_sighting_density <- full_join(whole_grid_trk_length, grid_env_covs_sighting_density, by = "grid_ID")
# write.csv(grid_env_ship_covs_sighting_density, file="env_ship_covs_sighting_density_by_gridID.csv")
										  


