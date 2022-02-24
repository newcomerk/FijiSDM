#gam check

#myspecies <- c("Bohadschia argus")
#myspecies <- c("Charonia tritonis")
#myspecies <- c("Scylla serrata")
myspecies <- c("Anadara antiquata") 
#myspecies <- c("Holothuria edulis")
#Tridacna maxima #already done
#myspecies <- c("Tridacna squamosa")
#Holothuria atra #already done
#myspecies <- c("Coral")
#myspecies <- c("Seagrass")
#myspecies <- c("Tridacna gigas") #Tridacna gigas #ran for 2 hours
#mangrove #already done


#andara future
#bargus future
#hatra future
#hedulis future
#tmaxima future
#tsquamosa future

load(paste0("../outputs_KN/models/presentmodels_",mydatasource,"_", myspecies, ".RData"))
load(paste0("../outputs_KN/models/presentmodels_",mydatasource,"_", myspecies, "_gam.RData"))

##need to read in every time
#metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_CodeInv.csv"))
#
#clrs <- hcl.colors(10)
#brks <- seq(0, 1, by = 0.1)
#
#
## import the data saved during the previous practical:
#dat <- read.csv(paste0("../outputs_KN/dat_", myspecies, ".csv"))
## or (depending on which dataset you want to use):
##dat <- read.csv(paste0("../outputs_KN/dat_grid_", myspecies, ".csv"))
#head(dat)
#layers_mod <- readRDS(paste0("../outputs_KN/sdmpredictors_mod_", myspecies, ".rds"))
#plot(layers_mod)
#
#dat<-na.omit(dat)
#
#names(dat)
##vars <- names(dat)[grep("alt|bio", names(dat))]
#vars <- names(dat)[grep("BO|MS|Dist|Protected", names(dat))]
##vars <- names(dat)[grep("BO", names(dat))]
#vars  # check if OK for your dataset!
#
## if you want to exclude particular variables:
##exclude <- names(dat)[grep("bio3|bio14|bio15", names(dat))]
##exclude  # check if OK for your dataset!
##vars <- setdiff(vars, exclude)
##vars  # check if OK for your dataset!
#
## many methods are affected by having too many variables and/or high correlations among them
## so, let's select a subset of not highly correlated variables to use with all models (so that they are comparable)
## (note this is just one possible way of selecting variables!)
#??corSelect
#vars_sel <- corSelect(data = dat, sp.cols = "presence", var.cols = vars, cor.thresh = 0.8, select = "AIC")
#vars_sel
#vars_sel$strongest.remaining.corr
#
#
#vars_sel <- vars_sel$selected.vars
#vars_sel
#
#dat_spatial <- dat
#coordinates(dat_spatial) <- dat[ , c("x", "y")]
#crs(dat_spatial) <- crs(layers_mod)
#par(mfrow = c(1, 1))  # reset to 1 plot per window
#plot(dat_spatial)
#foldsr <-createFolds(factor(dat$presence), k = 5, list = FALSE)
#
## add the fold ID to the data frame:
##dat$foldID <- blocks$foldID
#dat$foldIDr <- foldsr
#head(dat)
#
#dat_spatial$foldIDr <- foldsr
#spplot(dat_spatial, "foldIDr") 

#gam
#################################
#gam
######################################
#gam tuning with biomod

#trial dataset to see how things happen
#DataSpecies <- read.csv(system.file("external/species/mammals_table.csv",
#                                    package="biomod2"))
library(biomod2)

myRespName <- myspecies
myResp <- as.numeric(dat[,"presence"])
myRespXY <- dat[,c("x","y")]
myExpl<- layers_mod[[vars_sel]]
#myExpl <- stack(myExpl)

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)

Biomod.tuning <- BIOMOD_tuning(myBiomodData, models = "GAM")



Biomod.tuning

select <- "FALSE"
method <- "GCV.Cp"
 c <-c("GAM select", select)
d <- c("GAM method", method)
metadata<-rbind(metadata, c,d)

#This uses 100% of the data, runs it 1 time, spits out ROC and TSS hopefully
myBiomodModelOut <- BIOMOD_Modeling(myBiomodData, 
                                    models = "GAM", 
                                    models.options = Biomod.tuning$models.options,
                                    NbRunEval=1, 
                                    DataSplit=100, 
                                    VarImport=0, 
                                    models.eval.meth = c('ROC', 'TSS'),
                                    do.full.models=TRUE,
                                    modeling.id="test")

myBiomodModelOut
get_evaluations(myBiomodModelOut)

d <- c("GAM AUC for whole tuned Model", 1) ##insert AUC/ROC!!
e <- c("GAM TSS for whole tuned Model", 1) ##insert tss!!
metadata<-rbind(metadata, d, e)

#cross validation
DataSplotTable<- BIOMOD_cv(myBiomodData, k = 5, repetition = 1, do.full.models = TRUE,
                           stratified.cv = FALSE, stratify = "both", balance = "pres")

myBiomodModelOut2 <- BIOMOD_Modeling(myBiomodData, 
                                     models = "GAM", 
                                     models.options = Biomod.tuning$models.options,
                                     DataSplitTable = DataSplotTable,
                                     NbRunEval=1, 
                                     DataSplit=100, 
                                     VarImport=0, 
                                     models.eval.meth = c('ROC', 'TSS'),
                                     do.full.models=TRUE,
                                     modeling.id="test")




eval <- get_evaluations(myBiomodModelOut2,as.data.frame=T)
eval
d <- c("GAM Average AUC for 5fold tuned Model", mean(eval$Testing.data[eval$Eval.metric=="ROC"]))
e <- c("GAM Average TSS for 5fold tuned Model", mean(eval$Testing.data[eval$Eval.metric=="TSS"]))
metadata<-rbind(metadata, d, e)

myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = myExpl,
                                        proj.name = 'current',
                                        selected.models = 'all',
                                        binary.meth =  NULL,
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)
myBiomodProjection

plot(myBiomodProjection)
mod_proj <- get_predictions(myBiomodProjection)
#if this raster is being mean, and doesn't change the scale. just divide by 1000
mod_projf <- mod_proj / 1000
plot(mod_projf, col=clrs, breaks = brks, main =  paste0(myspecies, " GAM"))
dev.copy(png, paste0("../outputs_KN/GAMTunedModel_AUS_", myspecies, ".png"))
dev.off()


###############################################################

###############################################################


# save all current objects to disk (YOU WILL NEED THEM LATER!):
save.image(paste0("../outputs_KN/models/presentmodels_",mydatasource,"_", myspecies, "_gam.RData"))
load(paste0("../outputs_KN/models/presentmodels_",mydatasource,"_", myspecies, "_gam.RData"))

###############################################################
#gam
#MyModels_var_import <- get_variables_importance(myBiomodModelOut)
#MyModels_var_import <- variables_importance(myBiomodModelOut, data = layers_mod[[vars_sel]])
#dimnames(MyModels_var_import)
#mVarImp <- apply(MyModels_var_import, c(1,2), median) 
#mVarImp <- apply(mVarImp, 2, function(x) x*(1/sum(x))) # standardize the data
#mVarImp 
#write.table(mVarImp, file="modelVarImp.txt", sep="\t", quote = FALSE)


#bm.mod <- get(load(myBiomodModelOut@models.out.obj@link))


em.mods.names <- BIOMOD_LoadModels(myBiomodModelOut)
em.mods.names

em.vi.list <- lapply(em.mods.names,
                     function(emn) {
                       variables_importance(get(emn), data = get_formal_data(myBiomodModelOut,'expl.var'))
                     })
names(em.vi.list) <- em.mods.names
varimp_gam<-as.data.frame(em.vi.list[[1]]$mat)
total<- sum(varimp_gam$rand1)
varimp_gam$percentage<-(varimp_gam$rand1 / total)*100
varimp_gam <- varimp_gam[with(varimp_gam,order(-percentage)),]
varimp_gam$rowname<- rownames(varimp_gam)
varimp_gam[1,]
varimp_gam[2,]
varimp_gam[3,]

a<-c("Most Important Variable GAM", toString(varimp_gam[1,]))
b<-c("Second Most Important Variable GAM", toString(varimp_gam[2,]))
c<-c("Third Most Important Variable GAM", toString(varimp_gam[3,]))
metadata<-rbind(metadata, a, b, c)


##############################################################


write.csv(metadata, paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_CodeInv2.csv"), row.names = FALSE)
metadata <- read.csv(paste0("../data/species_occurrence_KN/Metadata_", myspecies, "_CodeInv.csv"))

#####################
#to project to fiji for tgigas
my_window <- c(176, 180, -21, -14)
#just Fiji
options(sdmpredictors_datadir = "../data/sdmpredictors_Inverts")
layers <- load_layers(layers_choice_in, rasterstack = FALSE)

#work around
mod_region2 <- as(extent(my_window), "SpatialPolygons")


ProtectedW<-raster("../data/ProtectedCropped.tif")
fD2<-raster("../data/DistanceCity.tif")

layers<-stack(layers)
layers_mod2 <- mask(crop(layers, mod_region2), mod_region2)

e <- intersect(extent(ProtectedW), extent(layers_mod2))
r1e <- crop(layers_mod2, e)
r3e <- resample(ProtectedW, r1e)

t(sapply(c(r1e,r3e), function(i) as.vector(extent(i))))


#layers_mod <- lapply(layers, crop, mod_region)
layers_mod2 <- stack(r1e, r3e)
names(layers_mod2)
names(layers_mod2)[16] <- 'Protected_Areas'
plot(layers_mod2)

layers_mod2<-stack(layers_mod2)

myExpl2<- layers_mod2[[vars_sel]]

myBiomodProjectionF <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = myExpl2,
                                        proj.name = 'fiji',
                                        selected.models = 'all',
                                        binary.meth =  NULL,
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)
myBiomodProjectionF

plot(myBiomodProjectionF)
mod_proj2 <- get_predictions(myBiomodProjectionF)
#if this raster is being mean, and doesn't change the scale. just divide by 1000
mod_projf2 <- mod_proj2 / 1000
plot(mod_projf2, col=clrs, breaks = brks, main =  paste0(myspecies, " GAM"))
dev.copy(png, paste0("../outputs_KN/GAMTunedModel_", myspecies, ".png"))
dev.off()
