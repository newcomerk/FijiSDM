#hatra3

library(dismo)
library(plotmo)
library(maxnet)
library(gam)
library(randomForest)
library(gbm)
library(maps)
library(fuzzySim)
library(modEvA)
library(sdmpredictors)
library(ecospat)

# define the target species:
myspecies <- "Holothuria atra"

# import models saved in previous practical:
load(paste0("../outputs/models_", myspecies, ".RData"))

# define colours and colour breaks for the maps:
clrs <- hcl.colors(10)
brks <- seq(0, 1, by = 0.1)

# PROJECT THE MODELS TO A DIFFERENT REGION ####

# import the occurrences data:
occurrences <- read.csv(paste0("../data/species_occurrence/occurrences_", myspecies, "_cleanedandcropped.csv"))

# convert to a spatial object:
occurrences_spatial <- occurrences
names(occurrences_spatial)
coordinates(occurrences_spatial) <- occurrences[ , c("decimalLongitude", "decimalLatitude")]
crs(occurrences_spatial) <- "+proj=longlat"
par(mfrow = c(1, 1))
plot(occurrences_spatial)


# import the complete (global coverage) layers:
layers <- stack(list.files("../data/sdmpredictors", pattern = "\\.tif$", full.names = TRUE))
plot(layers)

# now choose the projection extent, i.e. the region where you want to project your model predictions
# you can use e.g. the wider area that contains species occurrence points (including those that were left out of the modelling region):
proj_extent <- extent(occurrences_spatial)
# or you can choose a specific spatial window that interests you:
proj_extent <- extent(c(-15, 10, 30, 45))

# crop the global layers to the projection extent:
layers_proj <- crop(layers, proj_extent)
plot(layers_proj, col = clrs)


# ANALYSE ENVIRONMENTAL DISSIMILARITY ####

?mess  # multivariate environmental similarity surface
mess_proj <- mess(x = layers_proj, v = na.omit(getValues(layers_mod)))
mess_proj
plot(mess_proj, col = clrs, main = "MESS")  # negative values indicate environmental dissimilarity from the reference region
# in those places, our model projections are less reliable

# PROJECT TO DIFFERENT TIME PERIODS ####

# you can download future variables with 'sdmpredictors' package
# and from many online sources, see presentation "04_predictor_variables.pdf"
# let's take a peek at what's available in 'sdmpredictors' right now:

future_layers <- list_layers_future()
head(future_layers)
names(future_layers)
unique(future_layers$dataset_code)  # currently only 2 datasets have projected future values

future_worldclim <- subset(future_layers, dataset_code == "WorldClim")
future_biooracle <- subset(future_layers, dataset_code == "Bio-ORACLE")

# see which layers/scenarios are available for future WorldClim:
unique(future_worldclim[ , c("model", "scenario", "year", "version")])

# see which layers/scenarios are available for future Bio-ORACLE:
unique(future_biooracle[ , c("model", "scenario", "year", "version")])  # notice there are several versions of Bio-ORACLE; use the latest one:
future_biooracle <- subset(future_biooracle, version == max(future_biooracle$version))
unique(future_biooracle[ , c("model", "scenario", "year", "version")])


# select climate model(s), emissions scenario(s) and year(s) to project the models:
unique(future_worldclim[ , c("model", "scenario", "year")])

unique(subset(future_worldclim, model == "CCSM4" & scenario == "rcp26" & year == 2050))

options(sdmpredictors_datadir = "../dados/layers/future")

# example:
vars_sel_2050_rcp26 <- subset(future_worldclim, model == "CCSM4" & scenario == "rcp26" & year == 2050)[1:19, ]$layer_code
vars_sel_2100_rcp85 <- subset(future_worldclim, model == "CCSM4" & scenario == "rcp85" & year == 2070)$layer_code

layers_2050_rcp26 <- load_layers(layercodes = vars_sel_2050_rcp26, rasterstack = FALSE)
layers_2050_rcp26 <- stack(layers_2050_rcp26)
# then you can use these layers to project your models, as we did before when projecting to a different region

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictors_InvertsFut")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers_proj <- load_layers(layers_choice_inf, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers_proj  # a list of raster maps

# crop the global layers to the projection extent:
layers_proj <- crop(layers_proj, proj_extent)
plot(layers_proj, col = clrs)

# predict with each model to the projection layers:
#bioclim_proj <- predict(layers_proj, bioclim_mod)
#domain_proj <- predict(layers_proj[[names(domain_mod@presence)]], domain_mod)  # takes time!
maxent_proj <- predict(layers_proj, maxent_mod, type = "cloglog")
maxnet_proj <- predict(layers_proj, maxnet_mod, type = "cloglog")
#glm_proj <- predict(layers_proj, glm_mod, type = "response")
gam_proj <- predict(layers_proj, gam_mod, type = "response")
#rf_proj <- 1 - predict(layers_proj, rf_mod, type = "prob")
gbm_proj <- predict(layers_proj, gbm_mod, type = "response")

# convert probability predictions (from presence/absence models) to favourability:
#glm_proj_fav <- Fav(pred = glm_proj, sample.preval = prevalence(glm_mod$y))
gam_proj_fav <- Fav(pred = gam_proj, sample.preval = prevalence(gam_mod$y))
#rf_proj_fav <- Fav(pred = rf_proj, sample.preval = prevalence(rf_mod$y))
gbm_proj_fav <- Fav(pred = gbm_proj, sample.preval = prevalence(gbm_mod$data$y))

# map the extrapolated model predictions (model projections):
#plot(bioclim_proj, col = clrs, breaks = brks, main = "Bioclim")
#plot(domain_proj, col = clrs, breaks = brks, main = "Domain")
plot(maxent_proj, col = clrs, breaks = brks, main = "Maxent")
dev.copy(png, paste0("../outputs_KN/", myspecies, "MaxentProjection.png"))
dev.off()

plot(maxnet_proj, col = clrs, breaks = brks, main = "Maxnet")
dev.copy(png, paste0("../outputs_KN/", myspecies, "MaxnetProjection.png"))
dev.off()

#plot(glm_proj_fav, col = clrs, breaks = brks, main = "GLM")
plot(gam_proj_fav, col = clrs, breaks = brks, main = "GAM")
dev.copy(png, paste0("../outputs_KN/", myspecies, "GAMProjection.png"))
dev.off()

#plot(rf_proj_fav, col = clrs, breaks = brks, main = "RF")
plot(gbm_proj_fav, col = clrs, breaks = brks, main = "GBM")
dev.copy(png, paste0("../outputs_KN/", myspecies, "GBMProjection.png"))
dev.off()


names(layers_proj) <- gsub(pattern = "_cc26_2050", replacement = "_lonlat", x = names(layers_2050_rcp26))
