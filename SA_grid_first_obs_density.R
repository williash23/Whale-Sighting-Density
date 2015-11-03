# Sara Williams
# 9/21/2015
# Denisty of first sighting whale observations in grid over study area
################################################################################

#  Load packages
library(dplyr)
################################################################################

#  Read in whale observation data. This data frame currently has whale locations in UTM (zone 8N) format only.
data <- read.csv("C:/Users/sara.williams/Documents/GitHub/Data-Cleaning/data/.....")
#   Sort out only first observations.
data <- filter(data, ObOrder_Time == 1)
# #   Write as csv for plotting in ArcMap as needed.
# write.csv(data, "whale_obs_clean_first_sight.csv")
#############################################################################

#  Variables that Determine  "how busy" the bay is (i.e. density of sightings)

		#  Determine total number of whales (first sightings) seen in each grid cell 

				#  ArcMap approach: 
				#   Intersect 2 shapefiles: 1. a point layer (whale_obs_clean_first_sight); 
				#   2. SA_grid_clip_no_land. This associated the cell ID from the SA grid with the points
				#   (the whale observations) that fell within the cell -  output is called 
				#   "SA_grid_all_first_sight_int". 
								
								#############################################################################
								#  All of the following steps in ArcMap became unncessary as I figured out the correct
								#   data manipulation in R using dplyr to get the values I wanted.
								
								#   Then, I used the "Summarize" function within the attribute table of this intersected 
								#   shapefile ("SA_grid_all_first_sight_int") to summarize by the grid cell ID ("FID_SA_gri") and sum 
								#   the "Count" variable. The result was "Sum_count_all" (a dBASE table). 
				
								#   Finally, I joined this output table with the original intersected first sighting points and grid. 
								#   This joined the two files based on "FID_SA_gri" from the "Sum_count_all" table
								#   and "FID_SA_gri" from the "SA_grid_all_first_sight_int" layer (clicking on this layer first). 
								#   I selected 'Keep all records'. The result is a shapefile (my study area grid) 
								#   with a variable that has the sum of all the Count values of all the points 
								#   (whale observations) within a cell (= total number of whales sighted within each 
								#   grid cell using first sightings only.)

								#  output was exported and saved as CSV file. 
								#   "C/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/first_sight_covs_grid_cell_counts.csv"
		
								#  Same approach as above but seperate by years.
								#   Determine total number of whales (first sightings) seen in each grid cell over all years
								#   First create new spatial data sets by filtering by year. Then use in ArcMap.
								# data_2008 <- filter(data, Year == 2008)
								# data_2009 <- filter(data, Year == 2009)
								# data_2010 <- filter(data, Year == 2010)
								# data_2011 <- filter(data, Year == 2011)
								# data_2012 <- filter(data, Year == 2012)
								# data_2013 <- filter(data, Year == 2013)
								# data_2014 <- filter(data, Year == 2014)
									# write.csv(data_2008, "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/2008_first_sight_clean.csv")
									# write.csv(data_2009, "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/2009_first_sight_clean.csv")
									# write.csv(data_2010, "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/2010_first_sight_clean.csv")
									# write.csv(data_2011, "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/2011_first_sight_clean.csv")
									# write.csv(data_2012, "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/2012_first_sight_clean.csv")
									# write.csv(data_2013, "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/2013_first_sight_clean.csv")
									# write.csv(data_2014, "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/2014_first_sight_clean.csv")
					 
						# #  R approach: Not finished - first step done in ArcMap was sufficient

						# library(rgeos)
						# require(sp)
						# require(raster)
						# require(rgdal)
						# require(spatstat)
						# require(maptools)

						# pts <- shapefile("E:/AK_Shapefiles/whale_obs_clean_first_sight.shp")
						# SA_grid <- shapefile("E:/AK_Shapefiles/SA_grid_clip_no_land.shp")
						# tst <- gIntersection(pts, SA_grid)
			
				#  Data manipulation
				library(dplyr)
				data <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/first_sight_covs_grid_cell_ID.csv")
		
				#  Determine total number of whales (first sigthings) seen in bay during each year.
				data2 <- data %>%
									group_by(Year) %>%
									mutate(first_sight_count_year = sum(Count)) 
				data2 <- as.data.frame(ungroup(data2))		
			 
				#  Determine total number of whales (first sigthings) seen in bay during each month.
				data3 <- data2 %>%
									group_by(Year, month) %>%
									mutate(first_sight_count_year_month = sum(Count)) 
				data3 <- as.data.frame(ungroup(data3))		
		
				#  Determine total number of whales (first sigthings) seen in bay during each date.
				data4 <- data3 %>%
									group_by(Event_Date) %>%
									mutate(first_sight_count_date = sum(Count)) 
				data4 <- as.data.frame(ungroup(data4))		
		
				#   Determine total number of whales (first sightings) seen in bay across all years by grid cell.
				data5 <- data4 %>%
							   group_by(grid_cell_ID) %>%
							   mutate(first_sight_count_cells = sum(Count)) 
				data5 <- as.data.frame(ungroup(data5))
			 
				#   Determine total number of whales (first sightings) seen in bay by grid cell AND by each year.
				data6 <- data5 %>%
							   group_by(grid_cell_ID, Year) %>%
							   mutate(first_sight_count_cell_year = sum(Count)) 
				data6 <- as.data.frame(ungroup(data6))
		
				#   Determine total number of whales (first sightings) seen in bay by grid cell AND by each month.
				data7 <- data6 %>%
							   group_by(grid_cell_ID, Year, month) %>%
							   mutate(first_sight_count_cell_year_month = sum(Count)) 
				data7 <- as.data.frame(ungroup(data7))
					
				#  Determine total number of whales (first sigthings) seen in bay by grid cell AND date.
				data8 <- data7 %>%
							 group_by(grid_cell_ID, Event_Date) %>%
							 mutate(first_sight_count_cell_day = sum(Count)) 
				data8 <- as.data.frame(ungroup(data8))		
		
		#  Determine total number of whales (first sightings) seen in a buffer around each sighting 

				#  ArcMap approach: 
				#   First I made a polygon shapefile for each 250, 500 and 1000m around each first sighting.
				#   I created a new field in this output called "FID_bXXX" for each file. 
				
				#   Then I used Spatial join on 2 shapefiles: 1. a point layer (whale_obs_clean_first_sight - 
				#   this was join feature );  2. each buffer polygon shapefile (this was the target feature) and
				#   used the "join_one_to_many" option and "intersect" option for match. This associated 
				#   the buffer ID  with the points (the whale observations) that fell within each buffer. 
				#   Thehe output is called "XXXbuff_all_first_sight_join". 
				
				#   Then, I used the "Summarize" function within the attribute table of this joined 
				#   shapefile ("XXXbuff_all_first_sight_join") to summarize by the buffer ID ("FID_bXXX") 
				#   and sum  the "Count" variable. The result was "Sum_count_XXXbuff" (a dBASE table). 
				
				#   Finally, I joined this output table with the original buffer polygon shapefile.
				#   This joined the two files based on "FID_bXXX" from the "Sum_count_XXXbuff" table
				#   and "FID_bXXX" from the buffer polygon shapefile layer (clicking on this layer first). 
				#   I selected 'Keep all records'. The result is a shapefile with a variable that has the sum 
				#   of all the Count values of all the points (whale observations) within each buffer 
				#   (= total number of whales sighted within each buffer using first sightings only.)

		
#############################################################################
			
