andara


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

# mind that data download takes a long time when there are many occurrences!
gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 50000)

# if your species is too widespread, you can download points only within a specified window of longitude and latitude coordinates:
#countries <- readOGR("../data/countries/world_countries.shp")  # "../" means go up one level from the current folder or working directory

countries<-readOGR("data/countries/world_countries.shp")  # "../" means go up one level from the current folder or working directory

par(mar = c(2, 2, 2, 2))
plot(countries)
map.axes()  # add coordinates on the plot axes
# xmin, xmax, ymin, ymax coordinates of your region of interest:
my_window <- c(144, 180, -21, -1)
plot(as(extent(my_window), "SpatialPolygons"), border = "red", lwd = 2, add = TRUE)  # check if it's where you want it on the map
# plot country borders within 'my_window' only:
plot(countries, xlim = my_window[1:2], ylim = my_window[3:4])
map.axes()
# if global data is too much, download GBIF data from this window only:
gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 50000, decimalLongitude = paste0(my_window[1:2], collapse = ", "), decimalLatitude = paste0(my_window[3:4], collapse = ", "))

gbif_data  # if "Records found" is larger than "Records returned", you need to increase the 'limit' argument above (or decrease the coordinate window size) -- see help(occ_data) for options and limitations

# NOTE: If you plan to use these data in any report or publication, download them directly from www.gbif.org (then import the .csv to R) and note down the DOI and citation for that particular dataset. It is very important to properly cite the data sources! GBIF is not a source, just a repository for many people who put in very hard work to collect these data and make them available.


# check how the data are organized:
names(gbif_data)
names(gbif_data$meta)
names(gbif_data$data)

occurrences <- gbif_data$data

unique(sapply(occurrences, class))
# some columns may be lists, which can not be saved in a .csv
which(sapply(occurrences, class) == "list")
# remove these list columns:
occurrences[which(sapply(occurrences, class) == "list")] <- NULL

# create a folder on disk and export the data as a .csv file:
dir.create("../data/species_occurrence_KN")
write.csv(occurrences, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_raw.csv"), row.names = FALSE)

# from here on, you don't need to download these data again from GBIF - you can just import them from the .csv:
occurrences <- read.csv(paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_raw.csv"))
head(occurrences)

# map the occurrence records to see how they look:
plot(countries, xlim = range(occurrences$decimalLongitude), ylim = range(occurrences$decimalLatitude), main = myspecies)
map.axes()
points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "red")  # compare e.g.with the range map of this species at https://www.iucnredlist.org to assess if the distribution is well represented
dev.copy(png, paste0("../outputs_KN/RawWidePoints_", myspecies, ".png"))
dev.off()


metadata <-data.frame()
a <- c("Species", myspecies)
f <- c("Number of Raw GBIF Points in Window", nrow(occurrences))
metadata<-rbind(metadata, a,f)


# CLEAN SPECIES OCCURRENCE DATA ####

# mind that data may contain many errors; careful mapping, inspection and cleaning are necessary!
# here we'll first remove records of absence or zero-abundance (if any):
names(occurrences)
sort(unique(occurrences$occurrenceStatus))  # check for different indications of "absent", which could be in different languages!
absence_rows <- which(occurrences$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
nrow(occurrences)
if (length(absence_rows) > 0)  occurrences <- occurrences[-absence_rows, ]
nrow(occurrences)

# let's do some further data cleaning with functions of the 'scrubr' package (but note this cleaning is not exhaustive!)
occurrences <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(occurrences))))
nrow(occurrences)

# add the cleaned occurrence data to the map:
points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "blue")  # excluded points are not added in blue colour

# also eliminate presences with reported coordinate uncertainty (location error, spatial resolution) larger than 10x10 km2 (7071 m is the distance from the centroid to the corner of a 10x10 km2 square):
occurrences <- coord_uncertain(occurrences, coorduncertainityLimit = 7071)
nrow(occurrences)
# but note that this will only discard records where coordinate uncertainty is adequately reported in the dataset, which may not always be the case! Careful mapping and visual inspection are necessary

# add these less uncertain occurrence records with a different colour on top of the previous ones:
points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "turquoise")


occurrences <- occurrences[grep("\\.[0-9][0-9][0-9]", occurrences$decimalLatitude), ]
occurrences <- occurrences[grep("\\.[0-9][0-9][0-9]", occurrences$decimalLongitude), ]
nrow(occurrences)


# add these less uncertain occurrence records with a different colour on top of the previous ones:
points(occurrences[ , c("decimalLongitude", "decimalLatitude")], pch = 20, col = "orange")

# save the cleaned data to disk as a .csv file:
write.csv(occurrences, paste0("../data/species_occurrence_KN/occurrences_", myspecies, "_cleaned.csv"), row.names = FALSE)

a <- c("Number of Editted GBIF Points in Large Window", nrow(occurrences))
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

# DELIMIT THE MODELLING REGION ####

# convert species occurrences to a spatial object (like when you import a delimited text file into a GIS, you need to say which columns contain the spatial coordinates and what is the projection / coordinate reference system):
occurrences_spatial <- occurrences
names(occurrences_spatial)
coordinates(occurrences_spatial) <- occurrences[ , c("decimalLongitude", "decimalLatitude")]
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
plot(occurrences_spatial, main = myspecies)
# add countries map:
plot(countries, border = "blue", lwd = 2, add = TRUE)
map.axes()
dev.copy(png, paste0("../outputs_KN/EdittedPoints_FinalWindow_", myspecies, ".png"))
dev.off()

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
f <-c("Final Number of Input GBIF Points", length(occurrences_spatial))
metadata<-rbind(metadata, b,c,d,e,f)


#
#
#
#


write.csv(metadata, paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_Code1.csv"), row.names = FALSE)
#need to read in every time
metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_CodeInv.csv"))

