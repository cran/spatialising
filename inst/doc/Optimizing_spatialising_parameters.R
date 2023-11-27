## ----include = FALSE----------------------------------------------------------
set.seed(2023-05-10)
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(terra)
library(spatialising)
maine2013 = rast(system.file("raster/maine2013.tif", package = "spatialising"))
maine2016 = rast(system.file("raster/maine2016.tif", package = "spatialising"))

## -----------------------------------------------------------------------------
plot(c(maine2013, maine2016))

## -----------------------------------------------------------------------------
composition_index(c(maine2013, maine2016))

## -----------------------------------------------------------------------------
texture_index(c(maine2013, maine2016))

## -----------------------------------------------------------------------------
sim1 = kinetic_ising(maine2013, B = 0.3, J = 0.9, iter = ncell(maine2013) * 3, inertia = 150)
sim2 = kinetic_ising(maine2013, B = 0.7, J = 0.1, iter = ncell(maine2013) * 3, inertia = 150)

## -----------------------------------------------------------------------------
plot(c(sim1, sim2))

## -----------------------------------------------------------------------------
maine2016_metrics = c(composition_index(maine2016), texture_index(maine2016))
sim1_metrics = c(composition_index(sim1), texture_index(sim1))
sim2_metrics = c(composition_index(sim2), texture_index(sim2))

## -----------------------------------------------------------------------------
all_metrics = rbind(maine2016_metrics, sim1_metrics, sim2_metrics)
dist(all_metrics)

## -----------------------------------------------------------------------------
optimize_model = function(x){
  sim = spatialising::kinetic_ising(x = maine2013, B = x[1], J = x[2], 
                                    iter = ncell(maine2013) * 3, inertia = 150)  
  maine2016_metrics = c(composition_index(maine2016), texture_index(maine2016))
  sim_metrics = c(composition_index(sim), texture_index(sim))             
  dist(rbind(maine2016_metrics, sim_metrics))[[1]]
}

## -----------------------------------------------------------------------------
library(optimization)
optim_params = optim_sa(fun = optimize_model, 
                        start = c(0, 0), lower = c(-0.9, 0), upper = c(0.9, 0.9))

## -----------------------------------------------------------------------------
optim_params$par

## -----------------------------------------------------------------------------
sim_optim = kinetic_ising(maine2016, 
                          B = optim_params$par[1], J = optim_params$par[2], 
                          iter = ncell(maine2013) * 3, inertia = 150)
plot(sim_optim)

## -----------------------------------------------------------------------------
sim_optim_metrics = c(composition_index(sim_optim), texture_index(sim_optim))             
dist(rbind(maine2016_metrics, sim_optim_metrics))[[1]]

## -----------------------------------------------------------------------------
plot(c(maine2013, maine2016, sim_optim), nr = 1)

