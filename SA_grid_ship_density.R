# Sara Williams
# 9/15/2015; updated 12/1/2015
# Denisty of ship tracks in grid over study area
################################################################################

#  Denisty of ship tracks in grid over study area - first step was to use ship track (Trk_0613.shp)
#   a shapefile received from Karen. It is all the GPS taken ship tracks from the start of the 
#   project through 2015. In ArcMap, I merged the ship tracks based on Event_ID (the date and 
#   name of the observer so that a track of a whole cruise was dissolved into a single line). Then,
#   I intersected this shapefile with a grid that I created in ArcMap (0.011 lat x 0.011 lon) of
#   the study area using the intersect geoprocessing tool. I had already filtered the grid so that it 
#   only  covered areas of water. Then, I used spatial join to attach the new intersected line shapefile
#   back to the original grid polygon shapefile (using match type "contains") and summed  the length of
#   lines going through each grid cell.

################################################################################     

library(rgeos)
library(sp)
library(raster)
library(rgdal)
library(spatstat)
library(maptools)

# Load data (already in UTM)
trk_dat <- shapefile("E:/AK_Shapefiles/trk_length_grd_join.shp")
names(trk_dat) <- c("num_trks", "grid_ID", "trk_length_sum_m")
trk_dat_df <-as.data.frame(trk_dat)

cent_dist_dat <- shapefile("E:/AK_Shapefiles/grid_centroids.shp")
cent_dist_dat_df <-as.data.frame(cent_dist_dat)
cent_dist_dat_df <- cent_dist_dat_df %>%
                    dplyr:: select(Id, NEAR_DIST)
names(cent_dist_dat_df) <- c("grid_ID", "dist_shore_m")

grd_dat <- shapefile("E:/AK_Shapefiles/SA_grid_no_land.shp")
names(grd_dat) <- c("grid_ID", "cell_length", "cell_area")
grd_dat_df_tmp <-as.data.frame(grd_dat)
grd_dat_df <- full_join(grd_dat_df_tmp, cent_dist_dat_df, by = "grid_ID")

#  Clean up dataset and generate desired values
trk_sum_grid_ID <- trk_dat_df %>%
                      mutate(trk_length_sum_km = trk_length_sum_m/1000) %>%
                      distinct(grid_ID) %>%
                      arrange(grid_ID)

trk_sum_standard_grid_ID_tmp <- full_join(grd_dat_df, trk_sum_grid_ID, by = "grid_ID")
trk_sum_standard_grid_ID <- trk_sum_standard_grid_ID_tmp %>%
                              mutate(stand_trk_length_m = trk_length_sum_m/cell_area) 
# write.csv(trk_sum_standard_grid_ID, file="trk_length_cell_area_by_gridID.csv")  

#  Attach adjusted density sum count to other covariates associated with grid ID.
grid_env_covs_sighting_density <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/env_covs_sighting_density_by_gridID.csv")
grid_env_ship_covs_sighting_density <- full_join(trk_sum_standard_grid_ID, grid_env_covs_sighting_density, by = "grid_ID")
# write.csv(grid_env_ship_covs_sighting_density, file="env_ship_covs_sighting_density_by_gridID.csv")

#  Subset to only grid cells within 3500m of ship track line (detection probability under excellent, group size 1 = .05)
red_grd_3500_tmp <- as.data.frame(shapefile("E:/AK_Shapefiles/trk_length_grid_join_3500.shp"))
red_grd_3500 <- red_grd_3500_tmp %>%
                  dplyr::select(Id) %>%
                  rename(grid_ID = Id)
red_grid_3500_adjusted_ship_density <- semi_join(grid_env_ship_covs_sighting_density, red_grd_3500, by = "grid_ID")
# write.csv(red_grid_3500_adjusted_ship_density, file="env_ship_covs_sighting_density_by_gridID_red_grid_05detect.csv")  

#  Subset to only grid cells within 2500m of ship track line (detection probability under excellent, group size 1 = .15)
red_grd_2500_tmp <- as.data.frame(shapefile("E:/AK_Shapefiles/trk_length_grid_join_2500.shp"))
red_grd_2500 <- red_grd_2500_tmp %>%
                  dplyr::select(Id) %>%
                  rename(grid_ID = Id)
red_grid_2500_adjusted_ship_density <- semi_join(grid_env_ship_covs_sighting_density, red_grd_2500, by = "grid_ID")
# write.csv(red_grid_2500_adjusted_ship_density, file="env_ship_covs_sighting_density_by_gridID_red_grid_15detect.csv")  
################################################################################
















# #  Generate empty dataframe of all grid cell ID's
# grid_df <- as.data.frame(seq(1, 11827, 1))
# names(grid_df) <- c("grid_ID")

# #  Join ship track lengths to empty grid cell ID dataframe so each grid cell
# #  has a ship track length value.
# whole_grid_trk_length <- right_join(trk_sum_grid_ID, grid_df, by ="grid_ID")
# #   Change NA's to 0's.
# whole_grid_trk_length[is.na(whole_grid_trk_length)] <- 0

