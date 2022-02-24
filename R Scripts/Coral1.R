#Coral1

#mangrove rasters

#mangrove <- raster(paste0("../../Mangrove GIS/A20001.tif"))
#plot(mangrove)

#Mangrove
library(rgbif)
library(maps)
library(rgdal)
library(scrubr)
library(sdmpredictors)
library(fuzzySim)
library(rgdal)
library(spatialEco)
library(sf)

myspecies <- c("Coral")
countries<-readOGR("data/countries/world_countries.shp")  # "../" means go up one level from the current folder or working directory

foo <- readOGR(paste0("../../Coral Map UNEP/CoralPts.shp"))
occurrences <- as(foo, "data.frame")


str(occurrences)
head(occurrences)

names(occurrences)[names(occurrences) == "coords.x1"] <- "Longitude"
names(occurrences)[names(occurrences) == "coords.x2"] <- "Latitude"

plot(countries, xlim = range(occurrences$Longitude), ylim = range(occurrences$Latitude))
map.axes()
points(occurrences[ , c("Longitude", "Latitude")], pch = 20, col = "red")  # compare e.g.with the range map of this species at https://www.iucnredlist.org to assess if the distribution is well represented


plot(mod_region, main = myspecies)
map.axes()
plot(countries, add=TRUE)
points(occurrences[ , c("Longitude", "Latitude")], pch = 20, col = "red", add=TRUE)  # compare e.g.with the range map of this species at https://www.iucnredlist.org to assess if the distribution is well represented
dev.copy(png, paste0("../outputs_KN/RawWidePoints_", myspecies, ".png"))
dev.off()




metadata <-data.frame()
a <- c("Species", myspecies)
f <- c("Number of Raw  Points in Window", nrow(occurrences))
metadata<-rbind(metadata, a, f)

#
#
#
# CLEAN SPECIES OCCURRENCE DATA ####

# mind that data may contain many errors; careful mapping, inspection and cleaning are necessary!
# here we'll first remove records of absence or zero-abundance (if any):
names(occurrences)
#sort(unique(occurrences$occurrenceStatus))  # check for different indications of "absent", which could be in different languages!
#absence_rows <- which(occurrences$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
#nrow(occurrences)
#if (length(absence_rows) > 0)  occurrences <- occurrences[-absence_rows, ]
nrow(occurrences)

# let's do some further data cleaning with functions of the 'scrubr' package (but note this cleaning is not exhaustive!)
occurrences <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(occurrences, lat = "Latitude", lon = "Longitude"))))
nrow(occurrences)

# add the cleaned occurrence data to the map:
points(occurrences[ , c("Longitude", "Latitude")], pch = 20, col = "blue")  # excluded points are not added in blue colour

# also eliminate presences with reported coordinate uncertainty (location error, spatial resolution) larger than 10x10 km2 (7071 m is the distance from the centroid to the corner of a 10x10 km2 square):
#occurrences <- coord_uncertain(occurrences, coorduncertainityLimit = 7071)
#nrow(occurrences)
# but note that this will only discard records where coordinate uncertainty is adequately reported in the dataset, which may not always be the case! Careful mapping and visual inspection are necessary

# add these less uncertain occurrence records with a different colour on top of the previous ones:
#points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "turquoise")


occurrences <- occurrences[grep("\\.[0-9][0-9][0-9]", occurrences$Latitude), ]
occurrences <- occurrences[grep("\\.[0-9][0-9][0-9]", occurrences$Longitude), ]
nrow(occurrences)


# add these less uncertain occurrence records with a different colour on top of the previous ones:
points(occurrences[ , c("Longitude", "Latitude")], pch = 20, col = "orange")

# save the cleaned data to disk as a .csv file:
write.csv(occurrences, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleanedandcropped.csv"), row.names = FALSE)

a <- c("Number of Editted Points in Final Window", nrow(occurrences))
metadata<-rbind(metadata, a)

# see the data you have on disk so far:
list.files("../data/species_occurrence_KN")

# from now on, you don't need to download and clean the data again - you can just import them from the .csv:
occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleanedandcropped.csv"))
head(occurrences)


# in addition to (or instead of) GBIF occurrence data, you can use presence (and absence) points for any species and source you may have, as long as they are in the same coordinate reference system (CRS) and they cover enough of the species' distribution to enable an informative model



#
#
#

layers_choice_crl <- c("BO2_salinitymean_bdmax", "BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                       "BO21_curvelmax_bdmax", "BO_bathymax", "BO_calcite", "BO_ph", "BO_cloudmean", "BO_damean", "BO2_lightbotrange_bdmax",
                       "BO2_lightbotmean_bdmax", "BO_parmean", 
                       "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_bathy_5m", "MS_biogeo05_dist_shore_5m",
                       "MS_biogeo02_aspect_NS_5m")

layers_choice_crlp <- c("BO2_salinitymean_bdmax", "BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                       "BO21_curvelmax_bdmax", "BO_bathymax", 
                       "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_bathy_5m", "MS_biogeo05_dist_shore_5m",
                       "MS_biogeo02_aspect_NS_5m")

layers_choice_crlf <- c("BO21_RCP85_2100_salinitymean_bdmax", "BO21_RCP85_2100_tempmin_bdmax", "BO21_RCP85_2100_tempmax_bdmax", 
                        "BO21_RCP85_2100_temprange_bdmax",
                       "BO21_RCP85_2100_curvelmax_bdmax", "BO_bathymax",  
                       "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_bathy_5m", "MS_biogeo05_dist_shore_5m",
                       "MS_biogeo02_aspect_NS_5m")

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictors_Coral")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers <- load_layers(layers_choice_crl, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers  # a list of raster maps

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictors_CoralPres")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers <- load_layers(layers_choice_crlp, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers  # a list of raster maps

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictors_CoralFut")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers <- load_layers(layers_choice_crlf, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers  # a list of raster maps

a <- c("Downloaded Env Layers", toString(layers_choice_crl))
metadata<-rbind(metadata, a)

# see how many elements in 'layers':
length(layers)

# plot a couple of layers to see how they look:
names(layers)
par(mar = c(2, 2, 1, 0))  # change plotting parameters to make margins slightly larger
plot(layers[[1]], main = names(layers)[1])
plot(layers[[5]], main = names(layers)[5])

# find out if your layers have different extents or resolutions:
#unique(pred_layers[pred_layers$dataset_code == "WorldClim", ]$cellsize_lonlat)  # in this case 0.08333333 - spatial resolution can then be coarsened as adequate for your species data and modelling region (see below)
unique(sapply(layers, extent))  # if you get more than one extent (which doesn't happen with WorldClim, but may happen with other datasets), you'll have to crop all layers to the minimum common extent before proceeding
# for example, if the first layer has the smallest extent:

#layers <- lapply(layers, crop, extent(layers[[1]]))

#note!! for MARSPEC these are not the same size, so I cropped layers to the mod_region, or my_region later on


# once all layers have the same extent and resolution, you can stack them:
layers <- stack(layers)
plot(layers)
dev.copy(png, paste0("../outputs_KN/CoralEnvLayers.png"))
dev.off()




#
#
#


write.csv(metadata, paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_Code1.csv"), row.names = FALSE)
#need to read in every time
metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_Code1.csv"))
