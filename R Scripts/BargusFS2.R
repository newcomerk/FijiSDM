#Hatra2 future

#present and future projections

# R scripts for the course "Species distribution and ecological niche modelling in R"
# A. Marcia Barbosa (https://modtools.wordpress.com)


# don't forget to DO THIS FIRST:
# Session -> Set Working Directory -> To Source File Location


# LOAD PACKAGES ####

library(fuzzySim)
library(modEvA)
library(dismo)
library(maxnet)
library(gam)
library(randomForest)
library(gbm)
library(corrplot)


# IMPORT DATA AND DEFINE SOME SETTINGS ####

myspecies <- c("Bohadschia argus")
# import the data saved during the previous practical:
dat <- read.csv(paste0("../outputs_KN/dat_SLR_", myspecies, ".csv"))
# or (depending on which dataset you want to use):
#dat <- read.csv(paste0("../outputs_KN/dat_grid_", myspecies, ".csv"))
head(dat)
layers_mod <- readRDS(paste0("../outputs_KN/sdmpredictors_mod_SLR_", myspecies, ".rds"))
plot(layers_mod)


# define a colour palette and colour breaks, so that all maps of model predictions show a comparable gradient:
clrs <- hcl.colors(10)
brks <- seq(0, 1, by = 0.1)

a <- c("Cells in Grid before na omit", nrow(dat))
b <-c("presence cells in grid before", length(which(dat$presence == "1")))
c <-c("absense cells in grid before", length(which(dat$presence == "0")))
metadata<-rbind(metadata, a, b, c)

dat<-na.omit(dat)

a <- c("Cells in Grid after na omit", nrow(dat))
b <-c("presence cells in grid after", length(which(dat$presence == "1")))
c <-c("absense cells in grid after", length(which(dat$presence == "0")))
metadata<-rbind(metadata, a, b, c)

# just to check when we are actually modelling presence data only, let's make a version of the 'layers_mod' RasterStack with pixel values only at the locations of the presence points:

names(dat)
presence_centroids <- dat[dat$presence == 1, ]
coordinates(presence_centroids) <- presence_centroids[ , c("x", "y")]
crs(presence_centroids) <- "+proj=longlat"
plot(presence_centroids)

layers_mod_presonly <- mask(layers_mod, presence_centroids)
plot(layers_mod_presonly, col = clrs)
plot(layers_mod_presonly[[1]], col = clrs)

absence_centroids <- dat[dat$presence == 0, ]
coordinates(absence_centroids) <- absence_centroids[ , c("x", "y")]
crs(absence_centroids) <- "+proj=longlat"
plot(absence_centroids)

#
#
#
# SELECT VARIABLES ####

# some bioclimatic variables were found to correlate poorly among present and past/future scenarios (https://doi.org/10.1371/journal.pone.0129037, https://doi.org/10.1016/j.gloplacha.2013.04.005); for actual work, you may want to look at these papers and see if this affects your study region and time periods


names(dat)
#vars <- names(dat)[grep("alt|bio", names(dat))]
vars <- names(dat)[grep("BO|MS|Dist|Protected", names(dat))]
#vars <- names(dat)[grep("BO", names(dat))]
vars  # check if OK for your dataset!

# if you want to exclude particular variables:
#exclude <- names(dat)[grep("bio3|bio14|bio15", names(dat))]
#exclude  # check if OK for your dataset!
#vars <- setdiff(vars, exclude)
#vars  # check if OK for your dataset!

# many methods are affected by having too many variables and/or high correlations among them
# so, let's select a subset of not highly correlated variables to use with all models (so that they are comparable)
# (note this is just one possible way of selecting variables!)
??corSelect
vars_sel <- corSelect(data = dat, sp.cols = "presence", var.cols = vars, cor.thresh = 0.8, select = "AIC")
vars_sel
vars_sel$strongest.remaining.corr

a<- c("Corr Method", "AIC, 0.8, corSelect")
c <-c("Strongest Remaining Corr", vars_sel$strongest.remaining.corr)
metadata<-rbind(metadata, a, c)

vars_sel <- vars_sel$selected.vars
vars_sel


c <-c("Final Env Variables", toString(vars_sel))
metadata<-rbind(metadata, c)


# PRESENCE-ONLY MODELLING: BIOCLIM ####

?bioclim

bioclim_mod_po <- bioclim(x = layers_mod_presonly[[vars_sel]], p = presence_centroids)  # presence-only
bioclim_mod <- bioclim(x = layers_mod[[vars_sel]], p = presence_centroids)
all.equal(bioclim_mod, bioclim_mod_po)  # TRUE, so we confirm that we get the same model whether or not we provide pixels without presence records, showing that Bioclim is a true presence-only method

bioclim_mod
plot(bioclim_mod)  # the axes represent only the first 2 variables in the model, but the response includes all variables in the model


# compute and map Bioclim predictions across the study region
# and overlay the modelled presences:

bioclim_pred <- predict(layers_mod, bioclim_mod)

par(mar = c(2, 2, 1, 1))
plot(bioclim_pred, col = clrs, breaks = brks, main = "Bioclim")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)


# PRESENCE-ONLY MODELLING: DOMAIN ####

?domain

domain_mod_po <- domain(x = layers_mod_presonly[[vars_sel]], p = presence_centroids)  # presence-only
domain_mod <- domain(x = layers_mod[[vars_sel]], p = presence_centroids)
all.equal(domain_mod, domain_mod_po)  # TRUE, so Domain also provides the same model whether or not we give it the values of the pixels without presence, which means it is a true presence-only method

domain_mod


# compute and map Domain predictions and overlay the modelled presences:
domain_pred <- predict(layers_mod[[vars_sel]], domain_mod)  # can take time!

par(mar = c(2, 2, 1, 1))
plot(domain_pred, col = clrs, breaks = brks, main = "Domain")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)


# PRESENCE/BACKGROUND MODELLING: MAXENT ####

?maxent  # 'maxent' function in 'dismo' package

#maxent_mod_po <- maxent(x = layers_mod_presonly[[vars_sel]], p = presence_centroids)  
# ERROR: it won't compute with the presence-only pixels, it needs to generate background points at non-presence pixels too, so it's not a presence-only method!
maxent_mod <- maxent(x = layers_mod[[vars_sel]], p = presence_centroids)
# you can also use the data frame rather than the raster maps + presence points:
#maxent_mod <- maxent(x = dat[ , vars_sel], p = dat$presence)

# if you get an error like "MaxEnt is missing or incompatible with your version of Java" (or something similar), try copying the 'maxent.jar' file provided in the 'practicals' folder to the folder that you get with:
#system.file("java", package = "dismo")
# then try the 'maxent' command above again. If it still doesn't work, go to Session - Restart R" and run the script up to this point again. And if it still doesn't work, don't worry - we'll also build Maxent models without Java below.

# let's look at the contents of the resulting object:
str(maxent_mod)
nrow(maxent_mod@presence)
nrow(maxent_mod@absence)  # it creates absences from the pixels without presence points! so it does use absence data

# compute and map maxent predictions:
maxent_pred <- predict(layers_mod, maxent_mod)
plot(maxent_pred, col = clrs, breaks = brks, main = "Maxent")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)


?maxnet  # 'maxnet' function in 'maxnet' package, which uses 'glmnet' rather than the Java version of Maxent

# let's first create a version of 'dat' with only the presence data:
dat_presonly <- dat[dat$presence == 1, ]

#maxnet_mod_po <- maxnet(p = dat_presonly$presence, data = dat_presonly[ , vars_sel], maxnet.formula(p = dat_presonly$presence, data = dat_presonly[ , vars_sel], classes = "lq"))  
# ERROR: also won't work with a presence-only dataframe; it needs non-presence rows too:

maxnet_mod <- maxnet(p = dat$presence, data = dat[ , vars_sel], maxnet.formula(p = dat$presence, data = dat[ , vars_sel]))

# compute and map maxnet predictions:
maxnet_pred <- predict(layers_mod, maxnet_mod, type = "cloglog")
plot(maxnet_pred, col = clrs, breaks = brks, main = "Maxnet")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)


# PRESENCE/ABSENCE MODELLING: GLM ####

?glm

# let's first make the formula required by the 'glm' function:
glm_form <- as.formula(paste("presence ~", paste(vars_sel, collapse = " + ")))
glm_form

glm_mod <- glm(formula = glm_form, family = binomial, data = dat)
summary(glm_mod)

glm_pred <- predict(layers_mod, glm_mod, type = "response")  # "response" transforms the linear predictor with the appropriate link function, yielding results as probability values
plot(glm_pred, col = clrs, breaks = brks, main = "GLM")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)

# note that the predictions of GLM and other presence-absence models are of presence PROBABILITY, which incorporates the effects of the predictor variables AND the baseline prevalence (proportion of presences) of the species in the model training data
# probabilities are generally low for relatively rare species, and generally higher for widespread species

# convert probability to prevalence-independent favourability:
glm_fav <- Fav(pred = glm_pred, sample.preval = prevalence(glm_mod$y))
plot(glm_fav, col = clrs, breaks = brks, main = "GLM favourability")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)


# PRESENCE/ABSENCE MODELLING: GAM ####

?gam

gam_form <- as.formula(paste("presence ~", paste0("s(", vars_sel, ")", collapse = "+")))  # GAM with smoothing splines ('s')
gam_form

gam_mod <- gam(formula = gam_form, family = binomial, data = dat)
summary(gam_mod)


gam_pred <- predict(layers_mod, gam_mod, type = "response")
plot(gam_pred, col = clrs, breaks = brks, main = "GAM")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)

# convert probability to prevalence-independent favourability:
gam_fav <- Fav(pred = gam_pred, sample.preval = prevalence(gam_mod$y))
plot(gam_fav, col = clrs, breaks = brks, main = "GAM favourability")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)


# PRESENCE/ABSENCE MODELLING: RANDOM FOREST (RF) ####

?randomForest
presence_fact <- as.factor(dat[ , "presence"])  # we need to convert the "presence" column from integer to factor (categorical variable), otherwise 'randomForest' would do regression as if for a continuous response variable (rather than binary presence/absence)

rf_form <- as.formula(paste("presence_fact ~", paste(vars_sel, collapse = "+")))
rf_form

rf_mod <- randomForest(formula = rf_form, data = dat, na.action = na.exclude)  # I've used the defaults, but you should read the help to explore important parameters if you're interested in using RF
rf_mod

rf_pred <- 1 - predict(layers_mod, rf_mod, type = "prob")
plot(rf_pred, col = clrs, breaks = brks, main = "RF")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)  # note that random forests tend to overfit to the observed records

# convert probability to prevalence-independent favourability:
rf_fav <- Fav(pred = rf_pred, sample.preval = prevalence(rf_mod$y))
plot(rf_fav, col = clrs, breaks = brks, main = "RF favourability")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)

# notice how RF overfits to stripes of presences missing from the main data source:
plot(rf_fav, xlim = c(171, 180), ylim = c(-21, -14), col = clrs, breaks = brks, main = "RF favourability")
plot(presence_centroids, pch = 20, col = "red", add = TRUE)


# PRESENCE/ABSENCE MODELLING: BOOSTED REGRESSION TREES (BRT) ####
?gbm
gbm_form <- glm_form

gbm_mod <- gbm(formula = gbm_form, data = dat)  # I've used the defaults, but you should read the help to explore important parameters like 'shrinkage' (learning rate) and 'interaction.depth' (tree complexity) if you're interested in using GBM/BRT
gbm_mod

gbm_pred <- predict(layers_mod, gbm_mod, type = "response")
plot(gbm_pred, col = clrs, breaks = brks, main = "GBM")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)

# convert probability to prevalence-independent favourability:
gbm_fav <- Fav(pred = gbm_pred, sample.preval = prevalence(gbm_mod$data$y))
plot(gbm_fav, col = clrs, breaks = brks, main = "GBM favourability")
plot(presence_centroids, pch = ".", col = "red", add = TRUE)


# plot the raster map predictions together:

par(mar = c(2, 2, 2, 1), mfrow = c(4, 2))
plot(bioclim_pred, col = clrs, main = "Bioclim")
plot(domain_pred, col = clrs, main = "Domain")
plot(maxent_pred, col = clrs, main = "Maxent")
plot(maxnet_pred, col = clrs, main = "Maxnet")
plot(glm_fav, col = clrs, main = "GLM fav")
plot(gam_fav, col = clrs, main = "GAM fav")
plot(rf_fav, col = clrs, main = "RF fav")
plot(gbm_fav, col = clrs, main = "GBM fav")


# APPLY PREDICTIONS TO THE DATA TABLE ####
# (variables must have exact same names as in the models):

dat$bioclim_pred <- predict(bioclim_mod, dat)
dat$domain_pred <- predict(domain_mod, dat)
dat$maxent_pred <- predict(maxent_mod, dat)
dat$maxnet_pred <- as.vector(predict(maxnet_mod, dat, type = "cloglog"))
dat$glm_pred <- predict(glm_mod, newdata = dat, type = "response")
dat$glm_fav <- Fav(pred = dat$glm_p, sample.preval = prevalence(glm_mod$y))
dat$gam_pred <- predict(gam_mod, newdata = dat, type = "response")
dat$gam_fav <- Fav(pred = dat$gam_p, sample.preval = prevalence(gam_mod$y))
dat$rf_pred <- predict(rf_mod, newdata = dat, type = "prob")[ , "1"]  # RF prediction includes probabilities for each observed class; we want the probability of the presence class ("1")
dat$rf_fav <- Fav(pred = dat$rf_p, sample.preval = prevalence(rf_mod$y))
dat$gbm_pred <- predict(gbm_mod, newdata = dat, type = "response")
dat$gbm_fav <- Fav(pred = dat$gbm_p, sample.preval = prevalence(gbm_mod$data$y))

head(dat)

pred_cols <- c("bioclim_pred", "domain_pred", "maxent_pred", "maxnet_pred", "glm_fav", "gam_fav", "rf_fav", "gbm_fav")

# COMPARE PREDICTIONS OF DIFFERENT MODELS ####

# pair-wise scatterplots of predictions:
pairs(dat[ , pred_cols], pch = 20, cex = 0.1)  # takes time!

# correlations among predictions:
pred_corrs <- cor(dat[ , pred_cols])
pred_corrs
min(pred_corrs, na.rm = TRUE)

mydatasource <- "invertcurrent"

# visual correlation matrix:
par(mfrow = c(1, 1))
corrplot(pred_corrs, method = "ellipse", type = "upper", addCoef.col = "wheat3", addCoefasPercent = TRUE)


#
#
#


# map predictions from the data table:

# first we'll need to convert 'dat' to a spatial object:
dat_spatial <- dat
names(dat_spatial)
coordinates(dat_spatial) <- dat_spatial[ , c("x", "y")]

# map all prediction columns in one window:
spplot(dat_spatial, zcol = pred_cols, col.regions = clrs, cex = 0.1)

# map each prediction in turn:
spplot(dat_spatial, zcol = "bioclim_pred", cuts = brks, col.regions = clrs, cex = 0.5, main = "Bioclim")
spplot(dat_spatial, zcol = "domain_pred", cuts = brks, col.regions = clrs, cex = 0.5, main = "Domain")
spplot(dat_spatial, zcol = "maxent_pred", cuts = brks, col.regions = clrs, cex = 0.5, main = "Maxent")
spplot(dat_spatial, zcol = "maxnet_pred", cuts = brks, col.regions = clrs, cex = 0.5, main = "Maxnet")
spplot(dat_spatial, zcol = "glm_fav", cuts = brks, col.regions = clrs, cex = 0.5, main = "GLM fav")
spplot(dat_spatial, zcol = "gam_fav", cuts = brks, col.regions = clrs, cex = 0.5, main = "GAM fav")
spplot(dat_spatial, zcol = "rf_fav", cuts = brks, col.regions = clrs, cex = 0.5, main = "RF fav")
spplot(dat_spatial, zcol = "gbm_fav", cuts = brks, col.regions = clrs, cex = 0.5, main = "GBM fav")

# save all current objects to disk (YOU WILL NEED THEM LATER!):
save.image(paste0("../outputs_KN/models_SLR_",mydatasource,"_", myspecies, ".RData"))
#save.image(paste0("../outputs_KN/models_", myspecies, ".RData"))

#
#
#
library(modEvA)
library(ecospat)
library(blockCV)
library(raster)
library(dismo)
library(gam)
library(maxnet)
library(randomForest)
library(gbm)

########################################################################Day3

load(paste0("../outputs_KN/models_SLR_",mydatasource,"_", myspecies, ".RData"))


# EVALUATE MODELS ON THE TRAINING DATA ####

names(dat)


# area under the ROC Curve (AUC) ####

par(mfrow = c(4, 2), mar = c(3, 2, 2, 1))  # set plot margins and 3x2 plots in same window
?AUC
with(dat, AUC(obs = presence, pred = bioclim_pred, main = "Bioclim"))
with(dat, AUC(obs = presence, pred = domain_pred, main = "Domain"))
with(dat, AUC(obs = presence, pred = maxent_pred, main = "Maxent"))
with(dat, AUC(obs = presence, pred = maxnet_pred, main = "Maxnet"))
with(dat, AUC(obs = presence, pred = glm_pred, main = "GLM"))
with(dat, AUC(obs = presence, pred = gam_pred, main = "GAM"))
with(dat, AUC(obs = presence, pred = rf_pred, main = "RF"))
with(dat, AUC(obs = presence, pred = gbm_pred, main = "GBM"))


a <- with(dat, AUC(obs = presence, pred = maxent_pred, main = "Maxent"))
b <- with(dat, AUC(obs = presence, pred = maxnet_pred, main = "Maxnet"))
c <- with(dat, AUC(obs = presence, pred = glm_pred, main = "GLM"))
d <- with(dat, AUC(obs = presence, pred = gam_pred, main = "GAM"))
e <- with(dat, AUC(obs = presence, pred = gbm_pred, main = "GBM"))


f <-c("Maxent Model All Default data AUC", a$AUC)
g <-c("Maxnet Model All Default data AUC", b$AUC)
h <-c("GLM Model All Default data AUC", c$AUC)
i <-c("GAM Model All Default data AUC", d$AUC)
j <-c("GBM Model All Default data AUC", e$AUC)
metadata<-rbind(metadata, f,g,h,i,j)


# area under the precision-recall Curve (AUC-PR) ####

with(dat, AUC(obs = presence, pred = bioclim_pred, curve = "PR", main = "Bioclim"))
with(dat, AUC(obs = presence, pred = domain_pred, curve = "PR", main = "Domain"))
with(dat, AUC(obs = presence, pred = maxent_pred, curve = "PR", main = "Maxent"))
with(dat, AUC(obs = presence, pred = maxnet_pred, curve = "PR", main = "Maxnet"))
with(dat, AUC(obs = presence, pred = glm_pred, curve = "PR", main = "GLM"))
with(dat, AUC(obs = presence, pred = gam_pred, curve = "PR", main = "GAM"))
with(dat, AUC(obs = presence, pred = rf_pred, curve = "PR", main = "RF"))
with(dat, AUC(obs = presence, pred = gbm_pred, curve = "PR", main = "GBM"))


a <- with(dat, AUC(obs = presence, pred = maxent_pred, curve = "PR", main = "Maxent"))
b <- with(dat, AUC(obs = presence, pred = maxnet_pred, curve = "PR", main = "Maxnet"))
c <- with(dat, AUC(obs = presence, pred = glm_pred, curve = "PR", main = "GLM"))
d <- with(dat, AUC(obs = presence, pred = gam_pred, curve = "PR", main = "GAM"))
e <- with(dat, AUC(obs = presence, pred = gbm_pred, curve = "PR", main = "GBM"))


f <-c("Maxent Model Default All data AUC PR", a$AUC)
g <-c("Maxnet Model Default All data AUC PR", b$AUC)
h <-c("GLM Model Default All data AUC PR", c$AUC)
i <-c("GAM Model Default All data AUC PR", d$AUC)
j <-c("GBM Model Default All data AUC PR", e$AUC)
metadata<-rbind(metadata, f,g,h,i,j)



# threshold-based classification metrics ####

?threshMeasures
classif_metrics <- c("CCR", "Sensitivity", "Specificity", "Precision", "Recall", "TSS", "kappa")

# you can choose a threshold that optimizes classification performance for a particular measure (e.g. TSS) for each method
?optiThresh
my_thresh <- with(dat, optiThresh(obs = presence, pred = glm_pred, interval = 0.001, measures = "TSS", optimize = "each"))
my_thresh
my_thresh <- my_thresh$optimals.each$threshold
my_thresh

# or you can instead use species prevalence as the threshold for everyone:
my_thresh <- "preval"

par(mfrow = c(4, 2), mar = c(5, 2, 2, 1))
with(dat, threshMeasures(obs = presence, pred = bioclim_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "Bioclim"))
with(dat, threshMeasures(obs = presence, pred = domain_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "Domain"))
with(dat, threshMeasures(obs = presence, pred = maxent_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "Maxent"))
with(dat, threshMeasures(obs = presence, pred = maxnet_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "Maxnet"))
with(dat, threshMeasures(obs = presence, pred = glm_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "GLM"))
with(dat, threshMeasures(obs = presence, pred = gam_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "GAM"))
with(dat, threshMeasures(obs = presence, pred = rf_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "RF"))
with(dat, threshMeasures(obs = presence, pred = gbm_pred, thresh = my_thresh, measures = classif_metrics, ylim = c(0, 1), main = "GBM"))

# calculate and plot Miller calibration line:
?MillerCalib
par(mfrow = c(4, 2), mar = c(3, 2, 2, 1))
with(dat, MillerCalib(obs = presence, pred = bioclim_pred, main = "Bioclim"))
with(dat, MillerCalib(obs = presence, pred = domain_pred, main = "Domain"))
with(dat, MillerCalib(obs = presence, pred = maxent_pred, main = "Maxent"))
with(dat, MillerCalib(obs = presence, pred = maxnet_pred, main = "Maxnet"))
with(dat, MillerCalib(obs = presence, pred = glm_pred, main = "GLM"))
with(dat, MillerCalib(obs = presence, pred = gam_pred, main = "GAM"))
with(dat, MillerCalib(obs = presence, pred = rf_pred, main = "RF"))
with(dat, MillerCalib(obs = presence, pred = gbm_pred, main = "GBM"))


# COMPUTE BOYCE INDEX ####

?ecospat.boyce

# from raster variables + presence points:
ecospat.boyce(obs = coordinates(presence_centroids), fit = bioclim_pred)
ecospat.boyce(obs = coordinates(presence_centroids), fit = domain_pred)
ecospat.boyce(obs = coordinates(presence_centroids), fit = maxent_pred)
ecospat.boyce(obs = coordinates(presence_centroids), fit = maxnet_pred)
ecospat.boyce(obs = coordinates(presence_centroids), fit = glm_pred)
ecospat.boyce(obs = coordinates(presence_centroids), fit = gam_pred)
ecospat.boyce(obs = coordinates(presence_centroids), fit = rf_pred)
ecospat.boyce(obs = coordinates(presence_centroids), fit = gbm_pred)

# from the data frame with observed and predicted values:
ecospat.boyce(obs = dat[dat$presence == 1, "bioclim_pred"], fit = dat[ , "bioclim_pred"])
ecospat.boyce(obs = dat[dat$presence == 1, "domain_pred"], fit = dat[ , "domain_pred"])
ecospat.boyce(obs = dat[dat$presence == 1, "maxent_pred"], fit = dat[ , "maxent_pred"])
ecospat.boyce(obs = dat[dat$presence == 1, "maxnet_pred"], fit = dat[ , "bioclim_pred"])
ecospat.boyce(obs = dat[dat$presence == 1, "glm_pred"], fit = dat[ , "glm_pred"])
ecospat.boyce(obs = dat[dat$presence == 1, "gam_pred"], fit = dat[ , "gam_pred"])
ecospat.boyce(obs = dat[dat$presence == 1, "rf_pred"], fit = dat[ , "rf_pred"])
ecospat.boyce(obs = dat[dat$presence == 1, "gbm_pred"], fit = dat[ , "gbm_pred"])


# BUT THIS JUST EVALUATES HOW THE MODELS FIT THE SAME DATA ON WHICH THEY WERE TRAINED
# YOU CAN SET ASIDE SOME DATA TO LEAVE OUT OF MODEL TRAINING AND USE FOR TESTING OUT-OF-SAMPLE PREDICTIVE CAPACITY
# BLOCK CROSS-VALIDATION (below) IS CURRENTLY THE MOST APPROPRIATE METHOD
# THE R CODE IS A BIT COMPLEX, BUT DON'T WORRY IF YOU DON'T UNDERSTAND ALL OF IT
# it's basically the same things we've done so far, but looped over several folds of cross-validation blocks



# DIVIDE STUDY AREA INTO SPATIAL BLOCKS ####

?spatialBlock

names(dat)
# convert 'dat' to a spatial object with its coordinate reference system:
dat_spatial <- dat
coordinates(dat_spatial) <- dat[ , c("x", "y")]
crs(dat_spatial) <- crs(layers_mod)
par(mfrow = c(1, 1))  # reset to 1 plot per window
plot(dat_spatial)

## calculate the range of spatial autocorrelation in the modelling variables in the study area:
#?spatialAutoRange
#sarange <- spatialAutoRange(subset(layers_mod, vars_sel))  # you can also use an additional argument, speciesData = subset(dat_spatial, presence == 1), but mind that this is not always recommended; see ?spatialAutoRange for more info
## click on the 'Plots' pane for the graphic results!
#sarange$range
#
#c <-c("Spatial Auto Corr Block Size", sarange$range)
#metadata<-rbind(metadata, c)
#
#
## blocks based on the autocorrelation range may be too large for the models to be able to capture the species-environment relationship adequately
## get spatial blocks of 150 km instead:
#set.seed(123)  # set a particular seed of random numbers, so the next command alway provides the same set of blocks:
#blocks <- spatialBlock(speciesData = dat_spatial, rasterLayer = layers_mod[[1]], theRange = sarange$range, k = 5)  # you can use sarange$range as 'theRange' if you don't think it's too large
## you can also use an optional additional argument, species = "presence"; see ?spatialBlock
#
##set.seed(123)  # set a particular seed of random numbers, so the next command alway provides the same set of blocks:
##blocks <- spatialBlock(speciesData = dat_spatial, rasterLayer = layers_mod[[1]], theRange = 1436730, k = 5)  # you can use sarange$range as 'theRange' if you don't think it's too large
#
#blocks$folds
#blocks$foldID

#NEW Block Formation, with more presences per region

library(caret)
foldsr <-createFolds(factor(dat$presence), k = 5, list = FALSE)

# add the fold ID to the data frame:
#dat$foldID <- blocks$foldID
dat$foldIDr <- foldsr
head(dat)

dat_spatial$foldIDr <- foldsr
spplot(dat_spatial, "foldIDr")  # each fold has a different colour


#################################################
#################################################
#TUNE MODELS AND FIND TRUE MODEL PARAMETERS

#SDMtune
library(zeallot)
library(SDMtune)
library(plotROC)

###############maxent
p_coords <- dat[dat$presence == 1, ]
#check the number of columns to pick out ony x, y headers)
p_coords <- p_coords[ -c(1,4:30) ]
bg_coords <- dat[dat$presence == 0, ]
bg_coords <- bg_coords[ -c(1,4:30) ]

data.tune <- prepareSWD(species = myspecies, p = p_coords, a = bg_coords,
                        env = layers_mod[[vars_sel]])
model <- train(method = "Maxent", data = data.tune)
h <- list(reg = seq(0.2, 5, 0.2), fc = c("l", "lq", "lh", "lp", "lqp", "lqph"))

c(train, val, test) %<-% trainValTest(data.tune, val = 0.2, test = 0.2,
                                      only_presence = TRUE, seed = 61516)
model <- train("Maxnet", data = train)

exp_7 <- optimizeModel(model, hypers = h, metric = "auc", test = val, pop = 15,
                       gen = 2, seed = 798)

exp_7@results
index <- which.max(exp_7@results$test_AUC)
index
exp_7@models[[1]]
exp_7@models[[1]]


##Final model based on above results

fc <- "lqph"
reg <- 1.2
c <-c("Maxent fc", fc)
d <- c("Maxent reg", reg)
metadata<-rbind(metadata, c,d)


model.final.max <- train(method = "Maxent", data = data.tune, fc = fc, reg = reg, iter = 500)
pred.max <- predict(model.final.max, data = data.tune, type = "cloglog")
map <- predict(model.final.max, data = layers_mod[[vars_sel]], type = "cloglog")
plot(map, col = clrs, breaks = brks, main = paste0(myspecies, " Maxent Present Day"))
dev.copy(png, paste0("../outputs_KN/MaxentTunedModel_SLRPresent_", myspecies, ".png"))
dev.off()


auc(model.final.max)
plotROC(model.final.max)
tss(model.final.max)

d <- c("Maxent AUC for whole tuned Model", auc(model.final.max))
e <- c("Maxent TSS for whole tuned Model", tss(model.final.max))
metadata<-rbind(metadata, d, e)

#but lets check with a cross-validation
folds <- randomFolds(data.tune, k = 5, only_presence = TRUE, seed = 25)
cv_model <- train("Maxent", data = data.tune, fc = fc, reg = reg, folds = folds)
cv_model
#mean of training
auc(cv_model)
#mean of testing
auc(cv_model, test = TRUE)
tss(cv_model, test = TRUE)

d <- c("Maxent Average AUC for 5fold tuned Model", auc(cv_model, test = TRUE))
e <- c("Maxent Average TSS for 5fold tuned Model", tss(cv_model, test = TRUE))
metadata<-rbind(metadata, d, e)

###########BRT
model2 <- train("BRT", data = train)

g<-list(n.trees = seq(500, 8000, 500), shrinkage = seq(0.1, 0.001, -0.001) )

exp_8 <- optimizeModel(model2, hypers = g, metric = "auc", test = val, pop = 15, gen = 2, seed = 798)

exp_8@results
index2 <- which.max(exp_8@results$test_AUC)
index2
exp_8@models[[1]]

##Final model based on above results

ntree <- 5500   ## add number from above
shrink <-  0.003   ## add number from above
distribution <- "bernoulli"
interaction.depth <- 1
bag <- 0.05
c <-c("BRT ntree", ntree)
d <- c("BRT shrinkage", shrink)
e <-c("BRT distribution (not changed)", distribution)
f <- c("BRT interaction depth (not changed)", interaction.depth)
g <- c("BRT bag fraction (not changed)", bag)
metadata<-rbind(metadata, c,d,e,f,g)


model.final.brt <- train(method = "BRT", data = data.tune, n.trees = ntree, shrinkage = shrink)
pred.brt <- predict(model.final.brt, data = data.tune)
map.brt <- predict(model.final.brt, data = layers_mod[[vars_sel]])
plot(map.brt, col = clrs, breaks = brks, main = paste0(myspecies, " BRT Present Day"))
dev.copy(png, paste0("../outputs_KN/BRTTunedModel_SLRPresent_", myspecies, ".png"))
dev.off()


auc(model.final.brt)
plotROC(model.final.brt)
tss(model.final.brt)

d <- c("BRT AUC for whole tuned Model", auc(model.final.brt))
e <- c("BRT TSS for whole tuned Model", tss(model.final.brt))
metadata<-rbind(metadata, d, e)

#but lets check with a cross-validation

cv_modelbrt <- train("BRT", data = data.tune, n.trees = ntree, shrinkage = shrink, folds = folds)
cv_modelbrt
#mean of training
auc(cv_modelbrt)
#mean of testing
auc(cv_modelbrt, test = TRUE)
tss(cv_modelbrt, test = TRUE)

d <- c("BRT Average AUC for 5fold tuned Model", auc(cv_modelbrt, test = TRUE))
e <- c("BRT Average TSS for 5fold tuned Model", tss(cv_modelbrt, test = TRUE))
metadata<-rbind(metadata, d, e)

#################################
#gam
######################################
#gam tuning with biomod

#trial dataset to see how things happen
#DataSpecies <- read.csv(system.file("external/species/mammals_table.csv",
#                                    package="biomod2"))
#library(biomod2)
#
#myRespName <- myspecies
#myResp <- as.numeric(dat[,"presence"])
#myRespXY <- dat[,c("x","y")]
#myExpl<- layers_mod[[vars_sel]]
#myExpl <- stack(myExpl)
#
#myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
#                                     expl.var = myExpl,
#                                     resp.xy = myRespXY,
#                                     resp.name = myRespName)
#
#Biomod.tuning <- BIOMOD_tuning(myBiomodData, models = "GAM")
#Biomod.tuning
#
#select <- "TRUE"
#method <- "GCV.Cp"
#c <-c("GAM select", select)
#d <- c("GAM method", method)
#metadata<-rbind(metadata, c,d)
#
##This uses 100% of the data, runs it 1 time, spits out ROC and TSS hopefully
#myBiomodModelOut <- BIOMOD_Modeling(myBiomodData, 
#                                    models = "GAM", 
#                                    models.options = Biomod.tuning$models.options,
#                                    NbRunEval=1, 
#                                    DataSplit=100, 
#                                    VarImport=0, 
#                                    models.eval.meth = c('ROC', 'TSS'),
#                                    do.full.models=TRUE,
#                                    modeling.id="test")
#
#myBiomodModelOut
#get_evaluations(myBiomodModelOut)
#
#d <- c("GAM AUC for whole tuned Model", 0.986) ##insert AUC/ROC!!
#e <- c("GAM TSS for whole tuned Model", 0.956) ##insert tss!!
#metadata<-rbind(metadata, d, e)
#
##cross validation
#DataSplotTable<- BIOMOD_cv(myBiomodData, k = 5, repetition = 1, do.full.models = TRUE,
#                           stratified.cv = FALSE, stratify = "both", balance = "pres")
#
#myBiomodModelOut2 <- BIOMOD_Modeling(myBiomodData, 
#                                     models = "GAM", 
#                                     models.options = Biomod.tuning$models.options,
#                                     DataSplitTable = DataSplotTable,
#                                     NbRunEval=1, 
#                                     DataSplit=100, 
#                                     VarImport=0, 
#                                     models.eval.meth = c('ROC', 'TSS'),
#                                     do.full.models=TRUE,
#                                     modeling.id="test")
#
#
#eval <- get_evaluations(myBiomodModelOut2,as.data.frame=T)
#
#d <- c("GAM Average AUC for 5fold tuned Model", mean(eval$Testing.data[eval$Eval.metric=="ROC"& eval$Sensitivity==100]))
#e <- c("GAM Average TSS for 5fold tuned Model", mean(eval$Testing.data[eval$Eval.metric=="TSS"& eval$Sensitivity==100]))
#metadata<-rbind(metadata, d, e)
#
#myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
#                                        new.env = myExpl,
#                                        proj.name = 'current',
#                                        selected.models = 'all',
#                                        binary.meth =  NULL,
#                                        compress = FALSE,
#                                        build.clamping.mask = FALSE)
#myBiomodProjection
#
#plot(myBiomodProjection)
#mod_proj <- get_predictions(myBiomodProjection)
#plot(mod_proj, col=clrs, breaks = brks, main =  paste0(myspecies, " GAM Present Day"))
#dev.copy(png, paste0("../outputs_KN/GAMTunedModel_Present_", myspecies, ".png"))
#dev.off()
#
#

###############################################################
#project to the future

#RCP85

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictorsInvertsFut")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layersfs <- load_layers(layers_choice_infs, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layersfs  # a list of raster maps

a <- c("Downloaded Future Env Layers", toString(layers_choice_infs))
metadata<-rbind(metadata, a)

names(layersfs) <- c("BO2_salinitymean_bdmax", "BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                     "BO21_curvelmax_bdmax", 
                     "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_biogeo05_dist_shore_5m",
                     "MS_biogeo02_aspect_NS_5m")

layersfs <- stack(layersfs)
plot(layersfs)

layersfs_mod <- mask(crop(layersfs, mod_region), mod_region)

BathyMaxFuture<-raster("../data/BathyMaxFuture.tif")
BathyFuture<-raster("../data/BathyFuture.tif")

layersfs_mod<- stack(layersfs_mod, BathyMaxFuture, BathyFuture)

fD2 <- mask(crop(fD2, mod_region),mod_region)

e <- intersect(intersect(extent(ProtectedW), extent(fD2)), extent(layersfs_mod))
r1e <- crop(layersfs_mod, e)
r2e <- resample(fD2, r1e)
r3e <- resample(ProtectedW, r1e)

t(sapply(c(r1e, r2e, r3e), function(i) as.vector(extent(i))))

#layers_mod <- lapply(layers, crop, mod_region)
layersfs_mod <- stack(r1e, r2e)
layersfs_mod <- stack(layersfs_mod, r3e)
names(layersfs_mod)
names(layersfs_mod)[10] <- 'BO_bathymax'
names(layersfs_mod)[11] <- 'MS_bathy_5m'
names(layersfs_mod)[12] <- 'Dist_City'
names(layersfs_mod)[13] <- 'Protected_Areas'
plot(layersfs_mod)

data.tune.f <- prepareSWD(species = myspecies, p = p_coords, a = bg_coords,
                          env = layersfs_mod[[vars_sel]])

#maxent
pred.max.fut.slr <- predict(model.final.max, data = data.tune.f, type = "cloglog")
map.max.fut.slr <- predict(model.final.max, data = layersfs_mod[[vars_sel]], type = "cloglog")
plot(map.max.fut.slr, col = clrs , breaks = brks, main = paste0(myspecies, " Maxent Future Projection RCP 8.5 2100 w SLR"))
dev.copy(png, paste0("../outputs_KN/MaxentTunedModel_FutureProjection85_SLR", myspecies, ".png"))
dev.off()

#brt
pred.brt.fut.slr <- predict(model.final.brt, data = data.tune.f, type = "cloglog")
map.brt.fut.slr <- predict(model.final.brt, data = layersfs_mod[[vars_sel]], type = "cloglog")
plot(map.brt.fut.slr, col = clrs, breaks = brks, main = paste0(myspecies, " BRT Future Projection RCP 8.5 2100 w SLR"))
dev.copy(png, paste0("../outputs_KN/BRTTunedModel_FutureProjection85_SLR", myspecies, ".png"))
dev.off()

##GAM
#myRespName <- myspecies
#myResp <- as.numeric(dat[,"presence"])
#myRespXY <- dat[,c("x","y")]
#myExpl2<- layersf_mod[[vars_sel]]
#myExpl2 <- stack(myExpl2)
#
#myBiomodData2 <- BIOMOD_FormatingData(resp.var = myResp,
#                                     expl.var = myExpl2,
#                                     resp.xy = myRespXY,
#                                     resp.name = myRespName)
#
#myBiomodProjection2 <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
#                                        new.env = myExpl2,
#                                        proj.name = 'future 85',
#                                        selected.models = 'all',
#                                        binary.meth =  NULL,
#                                        compress = FALSE,
#                                        build.clamping.mask = FALSE)
#myBiomodProjection2
#
#plot(myBiomodProjection2)
#mod_proj2 <- get_predictions(myBiomodProjection2)
#
##if this raster is being mean, and doesn't change the scale. just divide by 1000
#mod_proj3 <- mod_proj2 / 1000
#plot(mod_proj3, col=clrs, breaks = brks, main = paste0(myspecies, " GAM Future Projection RCP 8.5 2100"))
#dev.copy(png, paste0("../outputs_KN/GAMTunedModel_FutureProjection85", myspecies, ".png"))
#dev.off()
#

#RCP 26

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictorsInvertsFut1")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layersfs1 <- load_layers(layers_choice_infs1, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layersfs1  # a list of raster maps

a <- c("Downloaded Future Env Layers", toString(layers_choice_infs1))
metadata<-rbind(metadata, a)

names(layersfs1) <- c("BO2_salinitymean_bdmax", "BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                      "BO21_curvelmax_bdmax", 
                      "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_biogeo05_dist_shore_5m",
                      "MS_biogeo02_aspect_NS_5m")

layersfs1 <- stack(layersfs1)
plot(layersfs1)

layersfs1_mod <- mask(crop(layersfs1, mod_region), mod_region)
fD2 <- mask(crop(fD2, mod_region),mod_region)

BathyMaxFuture<-raster("../data/BathyMaxFuture.tif")
BathyFuture<-raster("../data/BathyFuture.tif")

layersfs1_mod<- stack(layersfs1_mod, BathyMaxFuture, BathyFuture)

fD2 <- mask(crop(fD2, mod_region),mod_region)

e <- intersect(intersect(extent(ProtectedW), extent(fD2)), extent(layersfs1_mod))
r1e <- crop(layersfs1_mod, e)
r2e <- resample(fD2, r1e)
r3e <- resample(ProtectedW, r1e)

t(sapply(c(r1e, r2e, r3e), function(i) as.vector(extent(i))))

#layers_mod <- lapply(layers, crop, mod_region)
layersfs1_mod <- stack(r1e, r2e)
layersfs1_mod <- stack(layersfs1_mod, r3e)
names(layersfs1_mod)
names(layersfs1_mod)[10] <- 'BO_bathymax'
names(layersfs1_mod)[11] <- 'MS_bathy_5m'
names(layersfs1_mod)[12] <- 'Dist_City'
names(layersfs1_mod)[13] <- 'Protected_Areas'
plot(layersfs1_mod)

data.tune.f <- prepareSWD(species = myspecies, p = p_coords, a = bg_coords,
                          env = layersfs1_mod[[vars_sel]])

#maxent
pred.max.fut.slr <- predict(model.final.max, data = data.tune.f, type = "cloglog")
map.max.fut.slr <- predict(model.final.max, data = layersfs1_mod[[vars_sel]], type = "cloglog")
plot(map.max.fut.slr, col = clrs , breaks = brks, main = paste0(myspecies, " Maxent Future Projection RCP 2.6 2100 w SLR"))
dev.copy(png, paste0("../outputs_KN/MaxentTunedModel_FutureProjection26_SLR", myspecies, ".png"))
dev.off()

#brt
pred.brt.fut.slr <- predict(model.final.brt, data = data.tune.f, type = "cloglog")
map.brt.fut.slr <- predict(model.final.brt, data = layersfs1_mod[[vars_sel]], type = "cloglog")
plot(map.brt.fut.slr, col = clrs, breaks = brks, main = paste0(myspecies, " BRT Future Projection RCP 2.6 2100 w SLR"))
dev.copy(png, paste0("../outputs_KN/BRTTunedModel_FutureProjection26_SLR", myspecies, ".png"))
dev.off()

#RCP 4.5

# define folder for downloading / fetching the variables' map layers:
options(sdmpredictors_datadir = "../data/sdmpredictorsInvertsFut2")
# load the layers to the current R session (downloading them if they aren't already in the folder defined above):
layersfs2 <- load_layers(layers_choice_infs2, rasterstack = FALSE)  # rasterstack=TRUE gives error when there are layers with different extent
layersfs2  # a list of raster maps

a <- c("Downloaded Future Env Layers", toString(layers_choice_infs2))
metadata<-rbind(metadata, a)

names(layersfs2) <- c("BO2_salinitymean_bdmax", "BO2_tempmin_bdmax", "BO2_tempmax_bdmax", "BO2_temprange_bdmax",
                      "BO21_curvelmax_bdmax", 
                      "MS_biogeo06_bathy_slope_5m", "MS_biogeo01_aspect_EW_5m", "MS_biogeo05_dist_shore_5m",
                      "MS_biogeo02_aspect_NS_5m")

layersfs2 <- stack(layersfs2)
plot(layersfs2)

layersfs2_mod <- mask(crop(layersfs2, mod_region), mod_region)
fD2 <- mask(crop(fD2, mod_region),mod_region)

BathyMaxFuture<-raster("../data/BathyMaxFuture.tif")
BathyFuture<-raster("../data/BathyFuture.tif")

layersfs2_mod<- stack(layersfs2_mod, BathyMaxFuture, BathyFuture)

fD2 <- mask(crop(fD2, mod_region),mod_region)

e <- intersect(intersect(extent(ProtectedW), extent(fD2)), extent(layersfs2_mod))
r1e <- crop(layersfs2_mod, e)
r2e <- resample(fD2, r1e)
r3e <- resample(ProtectedW, r1e)

t(sapply(c(r1e, r2e, r3e), function(i) as.vector(extent(i))))

#layers_mod <- lapply(layers, crop, mod_region)
layersfs2_mod <- stack(r1e, r2e)
layersfs2_mod <- stack(layersfs2_mod, r3e)
names(layersfs2_mod)
names(layersfs2_mod)[10] <- 'BO_bathymax'
names(layersfs2_mod)[11] <- 'MS_bathy_5m'
names(layersfs2_mod)[12] <- 'Dist_City'
names(layersfs2_mod)[13] <- 'Protected_Areas'
plot(layersfs2_mod)

data.tune.f <- prepareSWD(species = myspecies, p = p_coords, a = bg_coords,
                          env = layersfs2_mod[[vars_sel]])

#maxent
pred.max.fut.slr <- predict(model.final.max, data = data.tune.f, type = "cloglog")
map.max.fut.slr <- predict(model.final.max, data = layersfs2_mod[[vars_sel]], type = "cloglog")
plot(map.max.fut.slr, col = clrs , breaks = brks, main = paste0(myspecies, " Maxent Future Projection RCP 4.5 2100 w SLR"))
dev.copy(png, paste0("../outputs_KN/MaxentTunedModel_FutureProjection45_SLR", myspecies, ".png"))
dev.off()

#brt
pred.brt.fut.slr <- predict(model.final.brt, data = data.tune.f, type = "cloglog")
map.brt.fut.slr <- predict(model.final.brt, data = layersfs2_mod[[vars_sel]], type = "cloglog")
plot(map.brt.fut.slr, col = clrs, breaks = brks, main = paste0(myspecies, " BRT Future Projection RCP 4.5 2100 w SLR"))
dev.copy(png, paste0("../outputs_KN/BRTTunedModel_FutureProjection45_SLR", myspecies, ".png"))
dev.off()


###############################################################
########################################################################
# save all current objects to disk (YOU WILL NEED THEM LATER!): 
save.image(paste0("../outputs_KN/models/futureSLRmodels_",mydatasource,"_", myspecies, ".RData")) 

load(paste0("../outputs_KN/models/futureSLRmodels_",mydatasource,"_", myspecies, ".RData")) 

################################################################ 
#variable importance in models 

#Maxent 

varimp_max <- varImp(model.final.max) 
varimp_max[1,] 
varimp_max[2,] 
varimp_max[3,] 

a<-c("Most Important Variable MAX", toString(varimp_max[1,])) 
b<-c("Second Most Important Variable MAX", toString(varimp_max[2,])) 
c<-c("Third Most Important Variable MAX", toString(varimp_max[3,])) 
metadata<-rbind(metadata, a, b, c) 


varimp_brt <- varImp(model.final.brt) 
varimp_brt[1,] 
varimp_brt[2,] 
varimp_brt[3,] 

a<-c("Most Important Variable BRT", toString(varimp_brt[1,])) 
b<-c("Second Most Important Variable BRT", toString(varimp_brt[2,])) 
c<-c("Third Most Important Variable BRT", toString(varimp_brt[3,])) 
metadata<-rbind(metadata, a, b, c) 


#################################################################################


write.csv(metadata, paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_CodeInvFS.csv"), row.names = FALSE)
#need to read in every time
metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_CodeInvFS.csv"))


