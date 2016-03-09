# Sara Williams
# 11/30/2015
# First sighting density analysis data set up
################################################################################
library(R2jags)
library(corrplot)
library(mcmcplots)    

dat <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/data/env_ship_covs_sighting_density_by_gridID_red_grid_15detect.csv")
dat$sst_clim_scaled <- as.numeric(scale(dat$sst_clim, T))
dat$chlor_clim_scaled <- as.numeric(scale(dat$chlor_clim, T))
dat$bath_scaled <- as.numeric(scale(dat$bath, T))
dat$trk_length_sum_km_scaled <- as.numeric(scale(dat$trk_length_sum_km, T))
dat$dist_shore_m_scaled <- as.numeric(scale(dat$dist_shore_m, T))
dat <- na.omit(dat)

plot(dat$adjusted_count_sum, dat$trk_length_sum_km_scaled)

cor_dat <- dat %>%
           dplyr::select(sst_clim_scaled, chlor_clim_scaled, bath_scaled,trk_length_sum_km_scaled,
           dist_shore_m_scaled)
m <- cor(cor_dat)
corrplot(m, method = "number", type = "upper", order = "FPC")

mod1 <- glm(adjusted_count_sum ~ trk_length_sum_km_scaled, data = dat)
mod2 <- glm(adjusted_count_sum ~ sst_clim_scaled, data = dat)
mod3 <- glm(adjusted_count_sum ~ chlor_clim_scaled, data = dat)
mod4 <- glm(adjusted_count_sum ~ dist_shore_m_scaled, data = dat)
mod5 <- glm(adjusted_count_sum ~ bath_scaled, data = dat)




# Zero inflated??
# Bayesian??
#  Interpolate NA values of climatologies - via bayesian model itself or in ArcMap?



# log prevents negatives
# conditional autoregressive model -- one form of a spatial model that allows neighboring cells to be more like each other.
# look at built in spatial models within R packages
# start using glm() (NA problem - drop them) to check out covariate relationships