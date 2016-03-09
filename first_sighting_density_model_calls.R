
#  Model call - ship trk length only
#   This should just be a normal model (not multivariate normal)
model <- "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/ship_dens_mod.txt"
jags.dat <- list(R= diag(1,2), 
                k = 2 ,
                n_cell = nrow(dat), 
                adjusted_count_sum = dat$adjusted_count_sum, 
                trk_length_sum_km_scaled = dat$trk_length_sum_km_scaled) 
params <- c("beta", "sigma")
inits <- function(){list(
            "beta" =  rep(0, 2),
            "omega" = diag(1, 2))
}

ship_dens_mod <- jags(data=jags.dat,
                         inits=inits, 
                         parameters.to.save=params, 
                         model.file=model,
                         n.chains =3, 
                         n.thin = 1, 
                                   n.iter = 100000, 
                         n.burnin = 50000)
ship_dens_mod
mcmcplot(ship_dens_mod)

#  Model call - ship trk length and distance to shore
model <- "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/ship_dens_shore_mod.txt"
jags.dat <- list(R= diag(1,3), 
                k = 3 ,
                n_cell = nrow(dat), 
                adjusted_count_sum = dat$adjusted_count_sum, 
                trk_length_sum_km_scaled = dat$trk_length_sum_km_scaled, 
                dist_shore_m_scaled = dat$dist_shore_m_scaled) 
params <- c("beta", "sigma")
inits <- function(){list(
            "beta" =  rep(0, 3),
            "omega" = diag(1, 3))
}

ship_dens_shore_mod <- jags(data=jags.dat,
                         inits=inits, 
                         parameters.to.save=params, 
                         model.file=model,
                         n.chains =3, 
                         n.thin = 1, 
                                   n.iter = 100000, 
                         n.burnin = 20000)
ship_dens_shore_mod
mcmcplot(ship_dens_shore_mod)


#  Model call - ship trk length and sst
model <- "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/ship_dens_sst_mod.txt"
jags.dat <- list(R= diag(1,3), 
                k = 3 ,
                n_cell = nrow(dat), 
                adjusted_count_sum = dat$adjusted_count_sum, 
                trk_length_sum_km_scaled = dat$trk_length_sum_km_scaled,
                sst_clim_scaled = dat$sst_clim_scaled)
params <- c("beta", "sigma")
inits <- function(){list(
            "beta" =  rep(0, 3),
            "omega" = diag(1, 3))
}
    
ship_dens_sst_mod <- jags(data=jags.dat,
                            inits=inits, 
                            parameters.to.save=params, 
                            model.file=model,
                            n.chains =3, 
                            n.thin = 1, 
                            n.iter = 50000, 
                            n.burnin = 10000)
ship_dens_sst_mod
mcmcplot(ship_dens_sst_mod)


#  Model call - ship trk length and chlor
model <- "C:/Users/sara.williams/Documents/GitHub/Whale-Sighting-Density/ship_dens_chlor_mod.txt"
jags.dat <- list(R= diag(1,3), 
                k =3 ,
                n_cell = nrow(dat), 
                adjusted_count_sum = dat$adjusted_count_sum, 
                trk_length_sum_km_scaled = dat$trk_length_sum_km_scaled, 
                chlor_clim_scaled = dat$chlor_clim_scaled)
                
params <- c("beta", "sigma")
inits <- function(){list(
            "beta" =  rep(0, 3),
            "omega" = diag(1, 3))
}
  
 ship_dens_chlor_mod <- jags(data=jags.dat,
                         inits=inits, 
                         parameters.to.save=params, 
                         model.file=model,
                         n.chains =3, 
                         n.thin = 1, 
                                   n.iter = 5000, 
                         n.burnin = 1000)
ship_dens_chlor_mod                   
mcmcplot(ship_dens_chlor_mod)