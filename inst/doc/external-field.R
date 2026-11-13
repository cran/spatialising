## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(terra)
library(spatialising)

maine2013 = rast(system.file("raster/maine2013.tif", package = "spatialising"))
maine2016 = rast(system.file("raster/maine2016.tif", package = "spatialising"))

## -----------------------------------------------------------------------------
p = rast(system.file("raster/maine_probability.tif", package = "spatialising"))

## -----------------------------------------------------------------------------
names(maine2013) = "2013"
names(p) = "Probability of class 1"
names(maine2016) = "2016"
plot(c(maine2013, p, maine2016), nr = 1)

## -----------------------------------------------------------------------------
B = 0
external_field = 0.5 * log(p / (1 - p)) - B

## -----------------------------------------------------------------------------
set.seed(1)
without_field = kinetic_ising(maine2013, B = B, J = 0.3)

set.seed(1)
with_field = kinetic_ising(maine2013, B = B, J = 0.3, external_field = external_field)

## -----------------------------------------------------------------------------
names(without_field) = "Without external field"
names(with_field) = "With external field"
plot(c(without_field, with_field), nr = 1)

