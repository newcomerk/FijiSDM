Seagrass
library(rgbif)
library(maps)
library(rgdal)
library(scrubr)
library(sdmpredictors)
library(fuzzySim)
library(rgdal)
library(spatialEco)

myspecies <- c("Seagrass")
countries<-readOGR("data/countries/world_countries.shp")  # "../" means go up one level from the current folder or working directory

seagrass_occurences <- read.csv(paste0("../data/species_occurrence_KN/SeagrassDownload_20May2021_Points.csv"))
str(seagrass_occurences)
head(seagrass_occurences)

#latitude and longitude are swapped because I'm an idiot.
#fixed 25may2021

plot(countries, xlim = range(seagrass_occurences$Longitude), ylim = range(seagrass_occurences$Latitude))
map.axes()
points(seagrass_occurences[ , c("Longitude", "Latitude")], pch = 20, col = "red")  # compare e.g.with the range map of this species at https://www.iucnredlist.org to assess if the distribution is well represented

plot(mod_region, main = myspecies)
map.axes()
plot(countries, add=TRUE)
points(seagrass_occurences[ , c("Longitude", "Latitude")], pch = 20, col = "red", add=TRUE)  # compare e.g.with the range map of this species at https://www.iucnredlist.org to assess if the distribution is well represented
dev.copy(png, paste0("../outputs_KN/RawWidePoints_", myspecies, ".png"))
dev.off()

#too annoying to keep fixing
occurrences<-seagrass_occurences


metadata <-data.frame()
a <- c("Species", myspecies)
f <- c("Number of Raw GBIF Points in Window", nrow(occurrences))
metadata<-rbind(metadata, a,f)

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
occurrences <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(occurrences))))
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
write.csv(occurrences, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleaned.csv"), row.names = FALSE)

a <- c("Number of Editted Points in Large Window", nrow(occurrences))
metadata<-rbind(metadata, a)

# see the data you have on disk so far:
list.files("../data/species_occurrence_KN")

# from now on, you don't need to download and clean the data again - you can just import them from the .csv:
occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleaned.csv"))
head(occurrences)


# in addition to (or instead of) GBIF occurrence data, you can use presence (and absence) points for any species and source you may have, as long as they are in the same coordinate reference system (CRS) and they cover enough of the species' distribution to enable an informative model


#
#
#

layers_choice_sg <- c("BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                      "BO21_curvelmax_bdmax", "BO_bathymax", "BO_ph", "BO_cloudmean", "BO_damean", "BO2_lightbotrange_bdmax",
                      "BO2_lightbotmean_bdmax", "BO_parmean", "BO2_nitratemean_bdmax",
                      "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_bathy_5m", "MS_biogeo05_dist_shore_5m",
                      "MS_biogeo02_aspect_NS_5m")
layers_choice_sgp <- c("BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                      "BO21_curvelmax_bdmax", "BO_bathymax", 
                      "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_bathy_5m", 
                      "MS_biogeo02_aspect_NS_5m")
layers_choice_sgf <- c("BO21_RCP85_2100_tempmin_bdmax", "BO21_RCP85_2100_tempmax_bdmax", "BO21_RCP85_2100_temprange_bdmax",
                       "BO21_RCP85_2100_curvelmax_bdmax", "BO_bathymax", 
                       "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_bathy_5m", 
                       "MS_biogeo02_aspect_NS_5m")

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictors_Seagrass")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers <- load_layers(layers_choice_sg, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers  # a list of raster maps

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictors_Seagrassp")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers <- load_layers(layers_choice_sgp, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers  # a list of raster maps

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictors_Seagrassf")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layers <- load_layers(layers_choice_sgf, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layers  # a list of raster maps

a <- c("Downloaded Env Layers", toString(layers_choice_sg))
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
dev.copy(png, paste0("../outputs_KN/SeagrassEnvLayers.png"))
dev.off()


#
#
#

# DELIMIT THE MODELLING REGION ####

# convert species occurrences to a spatial object (like when you import a delimited text file into a GIS, you need to say which columns contain the spatial coordinates and what is the projection / coordinate reference system):
occurrences_spatial <- occurrences
names(occurrences_spatial)
coordinates(occurrences_spatial) <- occurrences[ , c("Longitude", "Latitude")]
crs(occurrences_spatial) <- "+proj=longlat"
plot(occurrences_spatial)



# add countries map:
plot(countries, border = "blue", lwd = 2, add = TRUE)
map.axes()
dev.copy(png, paste0("../outputs_KN/EdittedWidePoints_", myspecies, ".png"))
dev.off()



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
plot(occurrences_spatial, main = paste0(myspecies, " Overlapping Land"))
# add countries map:
plot(countries, border = "blue", lwd = 2, add = TRUE)
map.axes()
dev.copy(png, paste0("../outputs_KN/EdittedPoints_FinalWindow_overlapping", myspecies, ".png"))
dev.off()


# Erase points based on polygons
s.erase <- erase.point(occurrences_spatial, countries)
plot(s.erase, main = paste0(myspecies, " Land Removed"))
# add countries map:
plot(countries, border = "blue", lwd = 2, add = TRUE)
map.axes()
dev.copy(png, paste0("../outputs_KN/EdittedPoints_FinalWindow_LandRemoved", myspecies, ".png"))
dev.off()


#save the editted datapoints, so can reload easily
#this is basically new, erased overlap occurences
df <- data.frame(x=coordinates(s.erase)[,1], y=coordinates(s.erase)[,2], s.erase@data)
# save the cleaned data to disk as a .csv file:
write.csv(df, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleanedandcropped.csv"), row.names = FALSE)

# from now on, you don't need to download and clean the data again - you can just import them from the .csv:
occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleanedandcropped.csv"))
head(occurrences)


layers_mod <- mask(crop(layers, mod_region), mod_region)

#layers_mod <- lapply(layers, crop, mod_region)
layers_mod <- stack(layers_mod)

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
f <-c("Final Number of Input Points Overlapping", length(occurrences_spatial))
g <- c("Final Number of Input Points Land Removed", length(s.erase))
metadata<-rbind(metadata, b,c,d,e,f,g)


#
#
#
#


write.csv(metadata, paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_Code1.csv"), row.names = FALSE)
#need to read in every time
metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_Code1.csv"))


#
#
#


# now make a dataframe of the species occurrence data gridded to the resolution of the raster variables
# i.e., one row per pixel with the values of the variables and the presence/absence of species records:

head(occurrences)
?gridRecords
gridded_data <- gridRecords(rst = layers_mod, pres.coords = occurrences[ , c("Longitude", "Latitude")])
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

# plot within a narrower coordinate range to see closer:
plot(occurrences_spatial)
map.axes()
plot(layers_mod[[1]], xlim = c(171, 180), ylim = c(-21, -14))
points(gridded_data[gridded_data[ , "presence"] == 0, c("x", "y")], col = "red", cex = 0.5)
points(gridded_data[gridded_data[ , "presence"] == 1, c("x", "y")], col = "blue", pch = 20)


# save the modelling dataframe to a .csv file on disk:
write.csv(gridded_data, paste0("../outputs_KN/dat_", myspecies, ".csv"), row.names = FALSE)



# if you have a polygon grid on which (most of) your survey was based, it's probably better to use use that grid for modelling:

sort(table(occurrences$ownerInstitutionCode))  # in the case of our example species, most of the records come from 'MAGRAMA'
sort(unique(occurrences[occurrences$ownerInstitutionCode == "MAGRAMA", "coordinateuncertaintyinmeters"]))  # MAGRAMA uses a UTM 10x10 km2 grid (https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-especies-terrestres/inventario-nacional-de-biodiversidad/bdn-ieet-default.aspx):

mygrid <- readOGR("../data/utm10_Spain/Malla10km_p.shp")
plot(mygrid)

# project the grid to the same CRS of our other maps:
mygrid <- spTransform(mygrid, crs(occurrences_spatial))
plot(mygrid)

# delimit the grid with the modelling region:
plot(mod_region, border = "blue", lwd = 2, add = TRUE)
mygrid <- intersect(mygrid, mod_region)
plot(mygrid)
head(mygrid)

# plot the polygon grid on a section of the raster layers and the occurrence points:
plot(layers_mod[[1]], xlim = c(-7, -4), ylim = c(36, 39))
plot(mygrid, add = TRUE)
plot(occurrences_spatial, pch = 20, cex = 0.5, add = TRUE)

# get the polygon grid cells that overlap with occurrence points:
mygrid$my_id <- 1:nrow(mygrid@data)
mygrid_pres <- mygrid[occurrences_spatial, ]
plot(mygrid_pres)

# add a column to the grid map's attribute table with the species presence/absence:
mygrid$presence <- 0
mygrid$presence[mygrid$my_id %in% mygrid_pres$my_id] <- 1
table(mygrid$presence)  # numbers of presences and absences
# plot the entire grid with cells coloured according to this new column:
plot(mygrid, col = mygrid$presence)
plot(occurrences_spatial, pch = 20, cex = 0.2, col = "blue", add = TRUE)  # check if they overlap!

# extract the mean values of the environmental variables to the cells of the polygon grid
# to be done right, this should use finer resolutions than the ones available through the 'sdmpredictors' package! we'll use this to keep things manageable during the course, but for real work, you should download the variables from their original sources at finer resolutions and use those in the following 'extract'
mygrid_vars <- extract(layers_mod, mygrid, fun = mean, df = TRUE)  # can take time!
head(mygrid_vars)

mygrid_vars <- data.frame(mygrid$presence, coordinates(mygrid), mygrid_vars)
names(mygrid_vars)
names(mygrid_vars)[1:3] <- c("presence", "x", "y")
head(mygrid_vars)


# if you want to use this dataframe later for modelling, export it to a .csv file on disk:
write.csv(mygrid_vars, paste0("../outputs/dat_grid_", myspecies, ".csv"), row.names = FALSE)



