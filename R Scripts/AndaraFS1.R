#Hatra

#future projections

# (INSTALL AND) LOAD THE NECESSARY PACKAGES ####

source("00_packages.R")

library(rgbif)
library(maps)
library(rgdal)
library(scrubr)
library(sdmpredictors)
library(fuzzySim)


# DOWNLOAD SPECIES OCCURRENCE DATA ####

# here we'll download GBIF occurrence data for an example species; after running the script and understanding how everything works, you can replace this with another species of your choice (as long as it exists on GBIF) and run it again; but note things can be quite slow if there are many occurrence points!

#changed here
myspecies <- c("Anadara antiquata")
metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_Code1.csv"))

# mind that data download takes a long time when there are many occurrences!
#gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 50000)
#
## if your species is too widespread, you can download points only within a specified window of longitude and latitude coordinates:
##countries <- readOGR("../data/countries/world_countries.shp")  # "../" means go up one level from the current folder or working directory
#
#countries<-readOGR("../data/countries/world_countries.shp")  # "../" means go up one level from the current folder or working directory
#
#par(mar = c(2, 2, 2, 2))
#plot(countries)
#map.axes()  # add coordinates on the plot axes
## xmin, xmax, ymin, ymax coordinates of your region of interest:
##my_window <- c(176, 180, -21, -14)
##just Fiji
#
##all oceania-ish
##my_window <- c(144, 180, -21, -1)
#
#plot(as(extent(my_window), "SpatialPolygons"), border = "red", lwd = 2, add = TRUE)  # check if it's where you want it on the map
## plot country borders within 'my_window' only:
#plot(countries, xlim = my_window[1:2], ylim = my_window[3:4])
#map.axes()
## if global data is too much, download GBIF data from this window only:
#gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 50000, decimalLongitude = paste0(my_window[1:2], collapse = ", "), decimalLatitude = paste0(my_window[3:4], collapse = ", "))
#
#gbif_data  # if "Records found" is larger than "Records returned", you need to increase the 'limit' argument above (or decrease the coordinate window size) -- see help(occ_data) for options and limitations
#
## NOTE: If you plan to use these data in any report or publication, download them directly from www.gbif.org (then import the .csv to R) and note down the DOI and citation for that particular dataset. It is very important to properly cite the data sources! GBIF is not a source, just a repository for many people who put in very hard work to collect these data and make them available.
#
#
## check how the data are organized:
#names(gbif_data)
#names(gbif_data$meta)
#names(gbif_data$data)
#
#occurrences <- gbif_data$data
#
#unique(sapply(occurrences, class))
## some columns may be lists, which can not be saved in a .csv
#which(sapply(occurrences, class) == "list")
## remove these list columns:
#occurrences[which(sapply(occurrences, class) == "list")] <- NULL
#
## create a folder on disk and export the data as a .csv file:
#dir.create("../data/species_occurrence_KN")
#write.csv(occurrences, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_raw.csv"), row.names = FALSE)
#
## from here on, you don't need to download these data again from GBIF - you can just import them from the .csv:
#occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_raw.csv"))
#head(occurrences)
#
## map the occurrence records to see how they look:
#plot(countries, xlim = range(occurrences$decimalLongitude), ylim = range(occurrences$decimalLatitude), main = myspecies)
#map.axes()
#points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "red")  # compare e.g.with the range map of this species at https://www.iucnredlist.org to assess if the distribution is well represented
#dev.copy(png, paste0("../outputs_KN/RawWidePoints_", myspecies, ".png"))
#dev.off()
#
#
#metadata <-data.frame()
#a <- c("Species", myspecies)
#f <- c("Number of Raw GBIF Points in Window", nrow(occurrences))
#metadata<-rbind(metadata, a,f)
#
#
## CLEAN SPECIES OCCURRENCE DATA ####
#
## mind that data may contain many errors; careful mapping, inspection and cleaning are necessary!
## here we'll first remove records of absence or zero-abundance (if any):
#names(occurrences)
#sort(unique(occurrences$occurrenceStatus))  # check for different indications of "absent", which could be in different languages!
#absence_rows <- which(occurrences$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
#nrow(occurrences)
#if (length(absence_rows) > 0)  occurrences <- occurrences[-absence_rows, ]
#nrow(occurrences)
#
## let's do some further data cleaning with functions of the 'scrubr' package (but note this cleaning is not exhaustive!)
#occurrences <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(occurrences))))
#nrow(occurrences)
#
## add the cleaned occurrence data to the map:
#points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "blue")  # excluded points are not added in blue colour
#
## also eliminate presences with reported coordinate uncertainty (location error, spatial resolution) larger than 10x10 km2 (7071 m is the distance from the centroid to the corner of a 10x10 km2 square):
#occurrences <- coord_uncertain(occurrences, coorduncertainityLimit = 7071)
#nrow(occurrences)
## but note that this will only discard records where coordinate uncertainty is adequately reported in the dataset, which may not always be the case! Careful mapping and visual inspection are necessary
#
## add these less uncertain occurrence records with a different colour on top of the previous ones:
#points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "turquoise")
#
#
#occurrences <- occurrences[grep("\\.[0-9][0-9][0-9]", occurrences$decimalLatitude), ]
#occurrences <- occurrences[grep("\\.[0-9][0-9][0-9]", occurrences$decimalLongitude), ]
#nrow(occurrences)
#
#
## add these less uncertain occurrence records with a different colour on top of the previous ones:
#points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "orange")
#
## save the cleaned data to disk as a .csv file:
#write.csv(occurrences, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleaned.csv"), row.names = FALSE)
#
#a <- c("Number of Editted GBIF Points in Large Window", nrow(occurrences))
#metadata<-rbind(metadata, a)
#
## see the data you have on disk so far:
#list.files("../data/species_occurrence_KN")
#
## from now on, you don't need to download and clean the data again - you can just import them from the .csv:
#occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleaned.csv"))
#head(occurrences)
#
#
# in addition to (or instead of) GBIF occurrence data, you can use presence (and absence) points for any species and source you may have, as long as they are in the same coordinate reference system (CRS) and they cover enough of the species' distribution to enable an informative model


# DOWNLOAD ENVIRONMENTAL VARIABLES ####

# we'll use functions of the 'sdmpredictors' package to access different online datasets
#pred_datasets <- list_datasets(terrestrial = TRUE, marine = TRUE)
#pred_datasets
#names(pred_datasets)
#pred_datasets[ , 1:4]  # these are the datasets currently available for download using the 'sdmpredictors' package; you can search their names online for more info on their contents
#
#pred_layers <- list_layers(datasets = pred_datasets)
#unique(pred_layers$dataset_code)
#unique(pred_layers[pred_layers$dataset_code == "WorldClim", ]$name)  # example of terrestrial variables dataset
#unique(pred_layers[pred_layers$dataset_code == "MARSPEC", ]$name)  # example of marine variables dataset
#unique(pred_layers[pred_layers$dataset_code == "Bio-ORACLE", ]$name)  # example of marine variables dataset
#
#
## let's choose one dataset (e.g. WorldClim) and one particular set of variables (e.g. altitude and the bioclimatic ones, which are in rows 1 to 20):
#layers_choice <- unique(pred_layers[pred_layers$dataset_code == "WorldClim", c("name", "layer_code")])
#layers_choice
#

#layers_choice_in <- c("BO2_salinitymean_bdmax", "BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
#                      "BO21_curvelmax_bdmax", "BO_bathymax", "BO_calcite", "BO_chlomean", "BO_ph", "BO2_ppmean_bdmin",
#                      "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_bathy_5m", "MS_biogeo05_dist_shore_5m",
#                      "MS_biogeo02_aspect_NS_5m")
layers_choice_inp <- c("BO2_salinitymean_bdmax", "BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                       "BO21_curvelmax_bdmax",  
                       "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_biogeo05_dist_shore_5m",
                       "MS_biogeo02_aspect_NS_5m", "BO_bathymax", "MS_bathy_5m")
layers_choice_infs <- c("BO21_RCP85_2100_salinitymean_bdmax", "BO21_RCP85_2100_tempmin_bdmax", "BO21_RCP85_2100_tempmax_bdmax", 
                        "BO21_RCP85_2100_temprange_bdmax",
                        "BO21_RCP85_2100_curvelmax_bdmax", 
                        "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m",  "MS_biogeo05_dist_shore_5m",
                        "MS_biogeo02_aspect_NS_5m")
layers_choice_infs1 <- c("BO21_RCP26_2100_salinitymean_bdmax", "BO21_RCP26_2100_tempmin_bdmax", "BO21_RCP26_2100_tempmax_bdmax", 
                         "BO21_RCP26_2100_temprange_bdmax",
                         "BO21_RCP26_2100_curvelmax_bdmax", 
                         "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_biogeo05_dist_shore_5m",
                         "MS_biogeo02_aspect_NS_5m")
layers_choice_infs2 <- c("BO21_RCP45_2100_salinitymean_bdmax", "BO21_RCP45_2100_tempmin_bdmax", "BO21_RCP45_2100_tempmax_bdmax", 
                         "BO21_RCP45_2100_temprange_bdmax",
                         "BO21_RCP45_2100_curvelmax_bdmax", 
                         "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_biogeo05_dist_shore_5m",
                         "MS_biogeo02_aspect_NS_5m")

# let's choose one dataset (e.g. WorldClim) and one particular set of variables (e.g. altitude and the bioclimatic ones, which are in rows 1 to 20):
#layers_choice <- unique(pred_layers[pred_layers$dataset_code == "Bio-ORACLE", c("name", "layer_code")])
#layers_choice
#layers_choice <- layers_choice[1:18, ]
#layers_choice

#list.files("../data/sdmpredictors_Inverts")
## define folder for downloading / fetching the variables' map layers:
#options(sdmpredictors_datadir = "../data/sdmpredictorsInverts")
## load the layers to the current R session (downloading them if they aren't already in the folder defined above):
#layers <- load_layers(layers_choice_in, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
#layers  # a list of raster maps
#
# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictorsInvertsPres")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers <- load_layers(layers_choice_inp, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers  # a list of raster maps

## define folder for downloading / fetching the variables' map layers:
#options(sdmpredictors_datadir = "../data/sdmpredictorsInvertsFut")
## load the layers to the current R session (downloading them if they aren't already in the folder defined above):
#layers <- load_layers(layers_choice_inf, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
#layers  # a list of raster maps

a <- c("Downloaded Env Present Layers", toString(layers_choice_inp))
metadata<-rbind(metadata, a)

# see how many elements in 'layers':
length(layers)

# plot a couple of layers to see how they look:
names(layers)
par(mar = c(2, 2, 1, 0))  # change plotting parameters to make margins slightly larger
plot(layers[[1]], main = names(layers)[1])
plot(layers[[5]], main = names(layers)[5])

# find out if your layers have different extents or resolutions:
unique(pred_layers[pred_layers$dataset_code == "WorldClim", ]$cellsize_lonlat)  # in this case 0.08333333 - spatial resolution can then be coarsened as adequate for your species data and modelling region (see below)
unique(sapply(layers, extent))  # if you get more than one extent (which doesn't happen with WorldClim, but may happen with other datasets), you'll have to crop all layers to the minimum common extent before proceeding
# for example, if the first layer has the smallest extent:

#layers <- lapply(layers, crop, extent(layers[[1]]))

#note!! for MARSPEC these are not the same size, so I cropped layers to the mod_region, or my_region later on


# once all layers have the same extent and resolution, you can stack them:
layers <- stack(layers)
plot(layers)
#dev.copy(png, paste0("../outputs_KN/InvertEnvLayers.png"))
#dev.off()



# DELIMIT THE MODELLING REGION ####
occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_croppedandcleaned.csv"))

# convert species occurrences to a spatial object (like when you import a delimited text file into a GIS, you need to say which columns contain the spatial coordinates and what is the projection / coordinate reference system):
occurrences_spatial <- occurrences
names(occurrences_spatial)
coordinates(occurrences_spatial) <- occurrences[ , c("decimalLongitude", "decimalLatitude")]
crs(occurrences_spatial) <- "+proj=longlat"
plot(occurrences_spatial)



# add countries map:
plot(countries, border = "blue", lwd = 2, add = TRUE)
map.axes()
#dev.copy(png, paste0("../outputs_KN/EdittedWidePoints_", myspecies, ".png"))
#dev.off()


# select the modelling region, e.g. using the countries where this species has occurrence points (which means the species was surveyed in those countries):

#countries_with_points <- countries[occurrences_spatial, ]
#plot(countries_with_points, border = "red")
#map.axes()
#plot(occurrences_spatial, add = TRUE)
# judge if some countries are visibly insufficiently surveyed (in this dataset) for this species; compare with occurrence data from other sources, e.g. https://www.iucnredlist.org, national atlases, data papers

# for the example species, Spain seems to be the only evenly surveyed country in this GBIF dataset
# also, only the mainland should be included in the modelling region, as on islands species may be limited by dispersal
# so, assuming we can't complement the dataset with occurrences from other sources (which we could!), let's select mainland Spain for modelling:

# create a unique polygon identifier for 'countries_with_points' and add it as labels to the map, to see which polygon(s) we want to select:
#countries_with_points@data$my_id <- 1:length(countries_with_points)
#text(countries_with_points, labels = countries_with_points$my_id, col = "blue", font = 2, halo = TRUE)
# select only the desired polygon(s) for the modelling region (polygon 1 FOR THE EXAMPLE DATA - CHANGE AS APPROPRIATE!!!):
#mod_region <- subset(countries_with_points, my_id %in% c(1))
#plot(mod_region, border = "green", lwd = 4, add = TRUE)  # check that it is the desired polygon(s)

# add species points and filter them with the modelling region:
#plot(occurrences_spatial, col = "grey", add = TRUE)
#occurrences_spatial <- occurrences_spatial[mod_region, ]
#plot(occurrences_spatial, col = "darkblue", add = TRUE)


# if you can't select evenly surveyed countries (e.g. if you're working with marine species), you can delimit the modelling region as a buffer of a given distance -- e.g. 1 geographic degree, or 100 km, or the mean distance among points:

#mean_dist <- mean(spDists(occurrences_spatial))
#mean_dist  # in km
#pres_buff <- raster::buffer(occurrences_spatial, width = (mean_dist + 1000) * 1000, dissolve = TRUE)
#plot(pres_buff, lwd = 2)
#plot(occurrences_spatial, col = "blue", add = TRUE)
#plot(countries, border = "red", add = TRUE)

my_window <- c(176, 180, -21, -14)
#just Fiji

#work around
mod_region <- as(extent(my_window), "SpatialPolygons")
occurrences_spatial<- crop(occurrences_spatial, mod_region)
plot(occurrences_spatial, main = myspecies)
# add countries map:
plot(countries, border = "blue", lwd = 2, add = TRUE)
map.axes()
#dev.copy(png, paste0("../outputs_KN/EdittedPoints_FinalWindow_", myspecies, ".png"))
#dev.off()


ProtectedW<-raster("../data/ProtectedCropped.tif")
fD2<-raster("../data/DistanceCity.tif")

layers_mod <- mask(crop(layers, mod_region), mod_region)

fD2 <- mask(crop(fD2, mod_region),mod_region)


e <- intersect(intersect(extent(ProtectedW), extent(fD2)), extent(layers_mod))
r1e <- crop(layers_mod, e)
r2e <- resample(fD2, r1e)
r3e <- resample(ProtectedW, r1e)

t(sapply(c(r1e, r2e, r3e), function(i) as.vector(extent(i))))



#layers_mod <- lapply(layers, crop, mod_region)
layers_mod <- stack(r1e, r2e)
layers_mod <- stack(layers_mod, r3e)
names(layers_mod)
names(layers_mod)[12] <- 'Dist_City'
names(layers_mod)[13] <- 'Protected_Areas'
plot(layers_mod)

#dev.copy(png, paste0("../outputs_KN/AllInvertLayers.png"))
#dev.off()

a <- c("Stacked Present Layers", toString(names(layers_mod)))
metadata<-rbind(metadata, a)

a <- c("Layer Resolutions", toString(res(layers_mod)))
metadata<-rbind(metadata, a)



# if the buffer is to be COMBINED WITH previously selected countries / polygon(s), the modelling region should be:
#mod_region <- intersect(pres_buff, mod_region)

# if you don't have any previously selected polygons and want to use only the buffer, the modelling region should be:
#mod_region <- pres_buff

# IF YOU USED A LIMITED WINDOW OF COORDINATES to download the occurrence data, you need to intersect or crop with that too:
#mod_region <- crop(mod_region, extent(my_window))

plot(mod_region, border = "darkgreen", lwd = 3, add = TRUE)

b <- c("Left Long", my_window[1])
c <- c("Right Long", my_window[2])
d <- c("Bottom Lat", my_window[3])
e <- c("Top Lat", my_window[4])
f <-c("Final Number of Input GBIF Points", length(occurrences_spatial))
metadata<-rbind(metadata, b,c,d,e,f)

# now import and cut (crop + mask) the variable maps to the extent of the modelling region:

#this is now done above
#layers <- stack(list.files("../data/sdmpredictors_KN", pattern = "\\.tif$", full.names = TRUE))
#plot(layers)
#layers_mod <- mask(crop(layers, mod_region), mod_region)


plot(layers_mod)
names(layers_mod)
plot(layers_mod[[10]])
plot(countries, border = "darkgrey", add = TRUE)
plot(mod_region, add = TRUE)
plot(occurrences_spatial, add = TRUE)
# check that everything overlaps correctly! but DON'T resize the plotting window (e.g. by pressing the "zoom" button), as raster and vector data don't get resized proportionally


# SET THE APPROPRIATE SPATIAL RESOLUTION ####

# closely inspect your species data vs. the size of the variables' pixels:
plot(layers_mod[[1]], xlim = c(178, 180), ylim = c(-19, -16))
points(occurrences_spatial, cex = 0.5)
# plot within different x/y limits if necessary to see if presence point resolution matches pixel resolution (i.e., if you don't have evenly spaced points with pixels in between -- see lecture "04_predictor_variables.pdf")
# notice that, in the example data, pixels have approximately the same spatial resolution as the presence points, but they don't match exactly (i.e., points are not at the centroids of these pixels); ideally, you should find the original grid over which (most of) these presences were sampled, and extract the raster values to that grid

# if necessary (which is not the case for the example data), you can aggregate the layers, to e.g. a 5-times coarser resolution (choose the 'fact' value that best matches your presence data resolution to your variables' resolution):
#layers_aggr <- raster::aggregate(layers_mod, fact = 5, fun = mean)
#res(layers_aggr)
#plot(layers_aggr[[1]], xlim = range(occurrences_spatial$decimalLongitude), ylim = range(occurrences_spatial$decimalLatitude))
#points(occurrences_spatial, pch = ".")

# run the command below only if you did need to aggregate the layers:
#layers_mod <- layers_aggr


# save the modelling layers to a folder on disk:
#dir.create("../outputs_KN")
saveRDS(layers_mod, paste0("../outputs_KN/sdmpredictors_mod_SLR_", myspecies, ".rds"))


#save the editted datapoints, so can reload easily
#this is basically new, erased overlap occurences
df <- data.frame(x=coordinates(occurrences_spatial)[,1], y=coordinates(occurrences_spatial)[,2], occurrences_spatial@data)
# save the cleaned data to disk as a .csv file:
#write.csv(df, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_croppedandcleaned.csv"), row.names = FALSE)

# from now on, you don't need to download and clean the data again - you can just import them from the .csv:
#occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_croppedandcleaned.csv"))


# now make a dataframe of the species occurrence data gridded to the resolution of the raster variables
# i.e., one row per pixel with the values of the variables and the presence/absence of species records:

head(occurrences)
?gridRecords
gridded_data <- gridRecords(rst = layers_mod, pres.coords = occurrences[ , c("decimalLongitude", "decimalLatitude")])
head(gridded_data)

nrow(gridded_data)  # should be the same number as:
sum(!is.na(getValues(layers_mod[[1]])))

names(gridded_data)
myspecies

# plot the gridded records:
plot(layers_mod[[1]])
# plot the absences (pixels without presence records):
points(gridded_data[gridded_data[ , "presence"] == 0, c("x", "y")], col = "red", cex = 0.5)
# plot the presences (pixels with presence records):
points(gridded_data[gridded_data[ , "presence"] == 1, c("x", "y")], col = "blue", cex = 0.7)

#a <- c("Gridded as Absent", length(which(gridded_data$presence == 0)))
#f <-c("Gridded as Present", length(which(gridded_data$presence == 1)))
#metadata<-rbind(metadata, a, f)
#now done in 2nd day code

# plot within a narrower coordinate range to see closer:
plot(occurrences_spatial)
map.axes()
plot(layers_mod[[1]], xlim = c(176, 180), ylim = c(-19, -16))
points(gridded_data[gridded_data[ , "presence"] == 0, c("x", "y")], col = "red", cex = 0.5)
points(gridded_data[gridded_data[ , "presence"] == 1, c("x", "y")], col = "blue", pch = 20)


# save the modelling dataframe to a .csv file on disk:
write.csv(gridded_data, paste0("../outputs_KN/dat_SLR_", myspecies, ".csv"), row.names = FALSE)





########################################################################################

write.csv(metadata, paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_CodeInvFS.csv"), row.names = FALSE)
#need to read in every time
metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_Code1.csv"))

