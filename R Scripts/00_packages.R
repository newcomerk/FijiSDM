# INSTALL NECESSARY R PACKAGES (IF THEY ARE NOT ALREADY INSTALLED) ####

# you may need recent versions of R and RStudio

if(!("uuid" %in% rownames(installed.packages())))
  install.packages("uuid")
if(!("wk" %in% rownames(installed.packages())))
  install.packages("wk")
if(!("httpcode" %in% rownames(installed.packages())))
  install.packages("httpcode")
if(!("rgbif" %in% rownames(installed.packages())))
  install.packages("rgbif", repos = "https://dev.ropensci.org")  # this installs the development version, which fixes recent bugs
if(!("maps" %in% rownames(installed.packages())))
  install.packages("maps")
if(!("rgdal" %in% rownames(installed.packages())))
  install.packages("rgdal")
if(!("scrubr" %in% rownames(installed.packages()))) install.packages("scrubr")
if(!("raster" %in% rownames(installed.packages()))) install.packages("raster")
if(!("sdmpredictors" %in% rownames(installed.packages()))) install.packages("sdmpredictors")

if(!("dismo" %in% rownames(installed.packages()))) install.packages("dismo")
if(!("lattice" %in% rownames(installed.packages()))) install.packages("lattice")
if(!("maxnet" %in% rownames(installed.packages()))) install.packages("maxnet")
if(!("fuzzySim" %in% rownames(installed.packages()))) install.packages("fuzzySim")
if(!("gam" %in% rownames(installed.packages()))) install.packages("gam")
if(!("randomForest" %in% rownames(installed.packages()))) install.packages("randomForest")
if(!("gbm" %in% rownames(installed.packages()))) install.packages("gbm")

if(!("blockCV" %in% rownames(installed.packages()))) install.packages("blockCV")
if(!("modEvA" %in% rownames(installed.packages()))) install.packages("modEvA")
