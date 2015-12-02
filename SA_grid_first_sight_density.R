# Sara Williams
# 9/21/2015; updated 12/1/2015
# Denisty of first sighting whale observations in grid over study area
#  Determine total number of whales (first sightings) seen in each grid cell in SA 
#   over entire study period.
################################################################################

# Created new variable.... ...first sighting density summed over grid cells and adjusted count based on detection probability given distance, count and visibility. 

# Also extracted all environmental covariates based on grid cell done using centroid X and Y of each grid cell.

# Also generated a variable of sum track length that passed through each grid cell --- made to adjust based on area of grid cell (not all are the same as land area was taken out).

#  Load packages
library(dplyr)
library(rgeos)
library(sp)
library(raster)
library(rgdal)
library(spatstat)
library(maptools)

################################################################################
#  Load data - grid and first sight intersect shapefile (already in UTM)
#  This is first sightings final data (n=3838)
first_sights_grd <- shapefile("E:/AK_Shapefiles/final_dat_first_sights_grid.shp")
#  Convert to regular dataframe
first_sights_grd_df <- as.data.frame(first_sights_grd)

# Adjust dataframe to match first sightings used in detection probability analysis
temp <- first_sights_grd_df %>%
			 dplyr::select(unique_eve, same_whale, ship,  count, count_fact, ship_speed, distance, 
										whale_beha, visibility, waves, X_whale_UT, Y_whale_UT,ship_spe_1, Id)
									
names(temp) <- c("unique_event_ID", "same_whale_ID", "ship", "count", "count_factor", 
									 "ship_speed", "ship_whale_distance", "whale_behavior", "visibility", 
									 "waves", "X_whale_UTM","Y_whale_UTM", "ship_speed_scaled", 
									 "grid_ID")

#  Create new variable for ship to whale distance rounded to nearest 10
temp2 <- temp %>%
			 mutate(ship_whale_dist_round = round(ship_whale_distance, -1))
			 
#  Make an index for each count-visibility-distance combination
dat <- temp2%>%
			   mutate(det_idx = paste(count,visibility,ship_whale_dist_round, sep="_"))	

#############################################################################			  

#  Generate response variable for sighting density analysis
#  Response = group size/ (detection probability at given distance and group size)			 

#  Load packages
library(utils)
library(Distance)

#  Establish detection probability for model including group size, visibility and distance
colnames(dat)[colnames(dat)=="ship_whale_distance"] <- "distance"
det_mod<-ds(data = dat, 
					  formula= ~1+visibility+count_factor,
					  truncation="15%",
					  transect="point",
					  key="hr",
					  adjustment=NULL)
summary(det_mod)

						# # # Summary for distance analysis 
						# # # Number of observations :  3262 
						# # # Distance range         :  0  -  4564.464 

						# # # Model : Hazard-rate key function 
						# # # AIC   : 54149.79 

						# # # Detection function parameters
						# # # Scale Coefficients:  
																	# # # estimate         se
						# # # (Intercept)         			 6.80005571 	0.04520739
						# # # visibilitygood      		 0.05762721 	0.05878325
						# # # visibilitypoor    			-0.60293981 	0.09824506
						# # # visibilitypoor-fog 		-1.29208358 	0.26440684
						# # # count2-3            			0.53660872	 	0.06558043
						# # # count4+             			0.80754628 	0.16804619

						# # # Shape parameters:  
																	# # # estimate         se
						# # # (Intercept) 					 0.8071008 		0.02417759

	#  Obtain detection probabilities for each scenario at each distance
	hr_excellent_1 <- function(x, scale = exp(6.800), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # baseline: vis excellent, group size 1
	hr_excellent_2 <- function(x, scale = exp(7.337), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis excellent, group size 2-3
	hr_excellent_lg <- function(x, scale = exp(7.608), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis excellent, group size 4+

	hr_good_1 <- function(x, scale = exp(6.800), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # baseline: vis good, group size 1
	hr_good_2 <- function(x, scale = exp(7.337), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis good, group size 2-3
	hr_good_lg <- function(x, scale = exp(7.608), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis good, group size 4+
			
	hr_poor_1 <- function(x, scale = exp(6.197), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis poor, group size 1
	hr_poor_2 <- function(x, scale = exp(6.734), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis poor, group size 2-3
	hr_poor_lg <- function(x, scale = exp(7.005), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis poor, group size 4+
			
	hr_poorfog_1 <- function(x, scale = exp(5.508), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis poor-fog, group size 1
	hr_poorfog_2 <- function(x, scale = exp(6.045), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis poor-fog, group size 2-3
	hr_poorfog_lg <- function(x, scale = exp(6.316), shape = exp(0.807)){
								1 - exp(-(x/scale)^(-shape))
				} # vis poor-fog, group size 4+

#  Generate distances at which to compute detection probability
x <- seq(min(dat$ship_whale_dist_round), 
				 max(dat$ship_whale_dist_round), 
				 by=10)

#  Create dataframes of detection probabilites for each distance under
#   each visibility and count scenario				 
det_exc_1 <-  as.data.frame(cbind(x, hr_excellent_1(x)))
names(det_exc_1) <- c("ship_whale_dist_round", "det_prob")
det_exc_2 <- as.data.frame(cbind(x, hr_excellent_2(x)))
names(det_exc_2) <- c("ship_whale_dist_round", "det_prob")
det_exc_lg <- as.data.frame(cbind(x, hr_excellent_lg(x)))
names(det_exc_lg) <- c("ship_whale_dist_round", "det_prob")

det_good_1 <-  as.data.frame(cbind(x, hr_good_1(x)))
names(det_good_1) <- c("ship_whale_dist_round", "det_prob")
det_good_2 <- as.data.frame(cbind(x, hr_good_2(x)))
names(det_good_2) <- c("ship_whale_dist_round", "det_prob")
det_good_lg <- as.data.frame(cbind(x, hr_good_lg(x)))
names(det_good_lg) <- c("ship_whale_dist_round", "det_prob")

det_poor_1 <- as.data.frame(cbind(x, hr_poor_1(x)))
names(det_poor_1) <- c("ship_whale_dist_round", "det_prob")
det_poor_2 <- as.data.frame(cbind(x, hr_poor_2(x)))
names(det_poor_2) <- c("ship_whale_dist_round", "det_prob")
det_poor_lg <- as.data.frame(cbind(x, hr_poor_lg(x)))
names(det_poor_lg) <- c("ship_whale_dist_round", "det_prob")

det_poorfog_1 <- as.data.frame(cbind(x, hr_poorfog_1(x)))
names(det_poorfog_1) <- c("ship_whale_dist_round", "det_prob")
det_poorfog_2 <- as.data.frame(cbind(x, hr_poorfog_2(x)))
names(det_poorfog_2) <- c("ship_whale_dist_round", "det_prob")
det_poorfog_lg <- as.data.frame(cbind(x, hr_poorfog_lg(x)))
names(det_poorfog_lg) <- c("ship_whale_dist_round", "det_prob")

#  Combine first sighting observations and their asscoiated detection 
#  probabilities by matching distances probabilities
exc_1 <- dat %>%
				  filter(visibility == "excellent" & count_factor == "1")
exc_1_probs <- left_join(exc_1, det_exc_1, by = "ship_whale_dist_round")
exc_2 <- dat %>%
				  filter(visibility == "excellent" & count_factor == "2-3")
exc_2_probs <- left_join(exc_2, det_exc_2, by = "ship_whale_dist_round")
exc_lg <- dat %>%
				  filter(visibility == "excellent" & count_factor == "4+")
exc_lg_probs <- left_join(exc_lg, det_exc_lg, by = "ship_whale_dist_round")

good_1 <- dat %>%
				  filter(visibility == "good" & count_factor == "1")
good_1_probs <- left_join(good_1, det_good_1, by = "ship_whale_dist_round")
good_2 <- dat %>%
				  filter(visibility == "good" & count_factor == "2-3")
good_2_probs <- left_join(good_2, det_good_2, by = "ship_whale_dist_round")
good_lg <- dat %>%
				  filter(visibility == "good" & count_factor == "4+")
good_lg_probs <- left_join(good_lg, det_good_lg, by = "ship_whale_dist_round")
				  
poor_1 <- dat %>%
				  filter(visibility == "poor" & count_factor == "1")
poor_1_probs <- left_join(poor_1, det_poor_1, by = "ship_whale_dist_round")
poor_2 <- dat %>%
				  filter(visibility == "poor" & count_factor == "2-3")
poor_2_probs <- left_join(poor_2, det_poor_2, by = "ship_whale_dist_round")
poor_lg <- dat %>%
				  filter(visibility == "poor" & count_factor == "4+")
poor_lg_probs <- left_join(poor_lg, det_poor_lg, by = "ship_whale_dist_round")
				  
poorfog_1 <- dat %>%
				  filter(visibility == "poor-fog" & count_factor == "1")
poorfog_1_probs <- left_join(poorfog_1, det_poorfog_1, by = "ship_whale_dist_round")
poorfog_2 <- dat %>%
				  filter(visibility == "poor-fog" & count_factor == "2-3")
poorfog_2_probs <- left_join(poorfog_2, det_poorfog_2, by = "ship_whale_dist_round")
poorfog_lg <- dat %>%
				  filter(visibility == "poor-fog" & count_factor == "4+")
poorfog_lg_probs <- left_join(poorfog_lg, det_poorfog_lg, by = "ship_whale_dist_round")

#  Row bind all of these new datasets 
final_detect_dat <- as.data.frame(bind_rows(exc_1_probs, exc_2_probs, exc_lg_probs, 
																	  good_1_probs, good_2_probs, good_lg_probs,
																	  poor_1_probs, poor_2_probs, poor_lg_probs, 
																	 poorfog_1_probs, poorfog_2_probs, poorfog_lg_probs))
															   
#  Generate new response variable from actual first sighting count and detection
#   probability.
#   Response = group size/ (detection probability at given distance and group size)		
adjusted_first_sight <- final_detect_dat %>%
											   mutate(adjusted_count= (count/det_prob)) 

#  Determine total adjusted first sigthings seen cumulatively in bay by grid cell.
adjusted_first_sight_gridID <- adjusted_first_sight%>%
															  group_by(grid_ID) %>%
									                          mutate(adjusted_count_sum= sum(adjusted_count)) %>%
									                          ungroup(.) %>%
									                          as.data.frame(.)	
# write.csv(adjusted_first_sight_gridID, file="adjusted_density_by_first_sighting.csv")			

#  Generate empty dataframe of all grid cell ID's
grid_df <- as.data.frame(seq(1, 11827, 1))
names(grid_df) <- c("grid_ID")

#  From adjusted density counts attached to first sights, get a sum adjusted density 
#   value for each grid cell that has observations in it.
grid_adjusted_density <- adjusted_first_sight_gridID %>%
													dplyr::select(grid_ID, adjusted_count_sum) %>%
													as.data.frame(.) %>%
													distinct(grid_ID) %>%
													arrange(grid_ID)

#  Join adjusted density sum count  to empty grid cell ID dataframe so each grid cell
#  has a density value.
whole_grid_adjusted_density <- right_join(grid_adjusted_density, grid_df, by ="grid_ID")
#   Change NA's to 0's.
whole_grid_adjusted_density[is.na(whole_grid_adjusted_density)] <- 0
# write.csv(whole_grid_adjusted_density, file="adjusted_density_by_gridID.csv")		

#  Attach adjusted density sum count to other covariates associated with grid ID.
grid_env_covs <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/env_covariates_gridID.csv")
grid_env_covs_sighting_density <- full_join(whole_grid_adjusted_density, grid_env_covs, by = "grid_ID")
# write.csv(grid_env_covs_sighting_density, file="env_covs_sighting_density_by_gridID.csv")

################################################################################














# #  Take a look at first sighting density plot.
# library(ggmap)
# library(RColorBrewer)
# mypalette<-brewer.pal(7,"Oranges")

# #  Convert UTM coordinates in data file to XY data for displaying points.
# plot_dat <- grid_env_covs_sighting_density
# coordinates(plot_dat) <- c("X_centroid_UTM_lon", "Y_centroid_UTM_lat")
# proj4string(plot_dat) <- CRS("+proj=utm +zone=8 + datum=WGS84 + ellps=WGS84") 
# plot_dat_xy <- as.data.frame(spTransform(plot_dat, CRS("+proj=longlat + datum=WGS84 + ellps=WGS84")))

# #  Create bounding box for study area map.
# bounds <- c(-137.5, 57.0, -134.5, 59.0)
# #  Create background map to hold whale location points.
# SA <- get_map(location=bounds, source="stamen", maptype="toner", crop=FALSE, zoom=9) 

# #  Display map and whale locations - locations color-coded by year.
# ggmap(SA) +
# geom_point(aes(x = X_centroid_UTM_lon, y = Y_centroid_UTM_lat, colour = adjusted_count_sum), 
						  # data =plot_dat_xy, size = 1) +
						  # scale_colour_gradient(limits=c(0, 620), low="grey", high="red")