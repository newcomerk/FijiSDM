#extracting data for comparison

library(raster)

metadata.proj <-data.frame()
headers<- c("Species", "Model Era", "Category", "Value")
metadata.proj<-rbind(metadata.proj, headers)


###########################################################################

#start


# !!! Skip tgigas and scylla for now
myspecies<- c("Tridacna gigas")

#check seagrass

#current projections####################################################################


#load(paste0("../outputs_KN/models/presentmodels_",mydatasource,"_", myspecies, ".RData"))
load(paste0("../outputs_KN/models/presentmodels_",mydatasource,"_", myspecies, "_gam.RData"))


era<- c("best present")

bestpresent.max<-values(map)
df.bestpresent.max<-as.data.frame(bestpresent.max)
df.bestpresent.max$era <- era
df.bestpresent.max$model <- c("maxent")

a<-length(which(bestpresent.max > 0.7))
b<-length(which(bestpresent.max > 0.4))
c<- length(bestpresent.max)
          
d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

bestpresent.brt<-values(map.brt)
df.bestpresent.brt<-as.data.frame(bestpresent.brt)
df.bestpresent.brt$era <- era
df.bestpresent.brt$model <- c("brt")


a<-length(which(bestpresent.brt > 0.7))
b<-length(which(bestpresent.brt > 0.4))
c<- length(bestpresent.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


bestpresent.gam<-values(mod_projf2)
#mod_projf2 for gigas/scylla
df.bestpresent.gam<-as.data.frame(bestpresent.gam)
df.bestpresent.gam$era <- era
df.bestpresent.gam$model <- c("gam")

a<-length(which(bestpresent.gam > 0.7))
b<-length(which(bestpresent.gam > 0.4))
c<- length(bestpresent.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)



###future projections####################################################################

  
          
#load(paste0("../outputs_KN/models/futuremodels_",mydatasource,"_", myspecies, ".RData"))
load(paste0("../outputs_KN/models/futuremodels_",mydatasource,"_", myspecies, "_gam.RData"))

era<- c("future present")

present.max<-values(map)
df.present.max<-as.data.frame(present.max)
df.present.max$era <- era
df.present.max$model <- c("maxent")

a<-length(which(present.max > 0.7))
b<-length(which(present.max > 0.4))
c<- length(present.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

present.brt<-values(map.brt)
df.present.brt<-as.data.frame(present.brt)
df.present.brt$era <- era
df.present.brt$model <- c("brt")

a<-length(which(present.brt > 0.7))
b<-length(which(present.brt > 0.4))
c<- length(present.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

present.gam<-values(mod_projf)
#mod_projf2 fpr gigas and scylla
df.present.gam<-as.data.frame(present.gam)
df.present.gam$era <- era
df.present.gam$model <- c("gam")

a<-length(which(present.gam > 0.7))
b<-length(which(present.gam > 0.4))
c<- length(present.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)




#85
era<- c("future 85")
map.max.fut <- predict(model.final.max, data = layersf_mod[[vars_sel]], type = "cloglog")
map.brt.fut <- predict(model.final.brt, data = layersf_mod[[vars_sel]], type = "cloglog")

#gigas and scylla
#map.max.fut <- predict(model.final.max, data = layersf_mod2[[vars_sel]], type = "cloglog")
#map.brt.fut <- predict(model.final.brt, data = layersf_mod2[[vars_sel]], type = "cloglog")

future85.max<-values(map.max.fut)
df.future85.max<-as.data.frame(future85.max)
df.future85.max$era <- era
df.future85.max$model <- c("maxent")

a<-length(which(future85.max > 0.7))
b<-length(which(future85.max > 0.4))
c<- length(future85.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


future85.brt<-values(map.brt.fut)
df.future85.brt<-as.data.frame(future85.brt)
df.future85.brt$era <- era
df.future85.brt$model <- c("brt")

a<-length(which(future85.brt > 0.7))
b<-length(which(future85.brt > 0.4))
c<- length(future85.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

future85.gam<-values(mod_projf85)
df.future85.gam<-as.data.frame(future85.gam)
df.future85.gam$era <- era
df.future85.gam$model <- c("gam")

a<-length(which(future85.gam > 0.7))
b<-length(which(future85.gam > 0.4))
c<- length(future85.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


#26
era<- c("future 26")
map.max.fut <- predict(model.final.max, data = layersf1_mod[[vars_sel]], type = "cloglog")
map.brt.fut <- predict(model.final.brt, data = layersf1_mod[[vars_sel]], type = "cloglog")

#gigas and scylla
#map.max.fut <- predict(model.final.max, data = layersf1_mod2[[vars_sel]], type = "cloglog")
#map.brt.fut <- predict(model.final.brt, data = layersf1_mod2[[vars_sel]], type = "cloglog")


future26.max<-values(map.max.fut)
df.future26.max<-as.data.frame(future26.max)
df.future26.max$era <- era
df.future26.max$model <- c("maxent")

a<-length(which(future26.max > 0.7))
b<-length(which(future26.max > 0.4))
c<- length(future26.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

future26.brt<-values(map.brt.fut)
df.future26.brt<-as.data.frame(future26.brt)
df.future26.brt$era <- era
df.future26.brt$model <- c("brt")

a<-length(which(future26.brt > 0.7))
b<-length(which(future26.brt > 0.4))
c<- length(future26.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

future26.gam<-values(mod_projf26)
df.future26.gam<-as.data.frame(future26.gam)
df.future26.gam$era <- era
df.future26.gam$model <- c("gam")

a<-length(which(future26.gam > 0.7))
b<-length(which(future26.gam > 0.4))
c<- length(future26.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


#45
era<- c("future 45")
map.max.fut <- predict(model.final.max, data = layersf2_mod[[vars_sel]], type = "cloglog")
map.brt.fut <- predict(model.final.brt, data = layersf2_mod[[vars_sel]], type = "cloglog")

#gigas and scylla
#map.max.fut <- predict(model.final.max, data = layersf2_mod2[[vars_sel]], type = "cloglog")
#map.brt.fut <- predict(model.final.brt, data = layersf2_mod2[[vars_sel]], type = "cloglog")


future45.max<-values(map.max.fut)
df.future45.max<-as.data.frame(future45.max)
df.future45.max$era <- era
df.future45.max$model <- c("maxent")

a<-length(which(future45.max > 0.7))
b<-length(which(future45.max > 0.4))
c<- length(future45.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

future45.brt<-values(map.brt.fut)
df.future45.brt<-as.data.frame(future45.brt)
df.future45.brt$era <- era
df.future45.brt$model <- c("brt")

a<-length(which(future45.brt > 0.7))
b<-length(which(future45.brt > 0.4))
c<- length(future45.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

future45.gam<-values(mod_projf45)
df.future45.gam<-as.data.frame(future45.gam)
df.future45.gam$era <- era
df.future45.gam$model <- c("gam")

a<-length(which(future45.gam > 0.7))
b<-length(which(future45.gam > 0.4))
c<- length(future45.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


###sea level projections####################################################################
era<- c("SLR present")


#load(paste0("../outputs_KN/models/futureSLRmodels_",mydatasource,"_", myspecies, ".RData"))
load(paste0("../outputs_KN/models/futureSLRmodels_",mydatasource,"_", myspecies, "_gam.RData"))

present.max<-values(map)
df.SLRpresent.max<-as.data.frame(present.max)
df.SLRpresent.max$era <- era
df.SLRpresent.max$model <- c("maxent")

a<-length(which(present.max > 0.7))
b<-length(which(present.max > 0.4))
c<- length(present.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

present.brt<-values(map.brt)
df.SLRpresent.brt<-as.data.frame(present.brt)
df.SLRpresent.brt$era <- era
df.SLRpresent.brt$model <- c("brt")

a<-length(which(present.brt > 0.7))
b<-length(which(present.brt > 0.4))
c<- length(present.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

present.gam<-values(mod_projf)
#mod_projf2 fpr gigas and scylla
df.SLRpresent.gam<-as.data.frame(present.gam)
df.SLRpresent.gam$era <- era
df.SLRpresent.gam$model <- c("gam")

a<-length(which(present.gam > 0.7))
b<-length(which(present.gam > 0.4))
c<- length(present.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)



#85
era<- c("SLR 85")
map.max.fut <- predict(model.final.max, data = layersf_mod[[vars_sel]], type = "cloglog")
map.brt.fut <- predict(model.final.brt, data = layersf_mod[[vars_sel]], type = "cloglog")

#gigas and scylla
#map.max.fut <- predict(model.final.max, data = layersf_mod2[[vars_sel]], type = "cloglog")
#map.brt.fut <- predict(model.final.brt, data = layersf_mod2[[vars_sel]], type = "cloglog")

SLR85.max<-values(map.max.fut)
df.SLR85.max<-as.data.frame(SLR85.max)
df.SLR85.max$era <- era
df.SLR85.max$model <- c("maxent")

a<-length(which(SLR85.max > 0.7))
b<-length(which(SLR85.max > 0.4))
c<- length(SLR85.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


SLR85.brt<-values(map.brt.fut)
df.SLR85.brt<-as.data.frame(SLR85.brt)
df.SLR85.brt$era <- era
df.SLR85.brt$model <- c("brt")

a<-length(which(SLR85.brt > 0.7))
b<-length(which(SLR85.brt > 0.4))
c<- length(SLR85.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

SLR85.gam<-values(mod_projf85)
df.SLR85.gam<-as.data.frame(SLR85.gam)
df.SLR85.gam$era <- era
df.SLR85.gam$model <- c("gam")

a<-length(which(SLR85.gam > 0.7))
b<-length(which(SLR85.gam > 0.4))
c<- length(SLR85.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


#26
era<- c("SLR 26")
map.max.fut <- predict(model.final.max, data = layersf1_mod[[vars_sel]], type = "cloglog")
map.brt.fut <- predict(model.final.brt, data = layersf1_mod[[vars_sel]], type = "cloglog")

#gigas and scylla
#map.max.fut <- predict(model.final.max, data = layersf1_mod2[[vars_sel]], type = "cloglog")
#map.brt.fut <- predict(model.final.brt, data = layersf1_mod2[[vars_sel]], type = "cloglog")


SLR26.max<-values(map.max.fut)
df.SLR26.max<-as.data.frame(SLR26.max)
df.SLR26.max$era <- era
df.SLR26.max$model <- c("maxent")

a<-length(which(SLR26.max > 0.7))
b<-length(which(SLR26.max > 0.4))
c<- length(SLR26.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

SLR26.brt<-values(map.brt.fut)
df.SLR26.brt<-as.data.frame(SLR26.brt)
df.SLR26.brt$era <- era
df.SLR26.brt$model <- c("brt")

a<-length(which(SLR26.brt > 0.7))
b<-length(which(SLR26.brt > 0.4))
c<- length(SLR26.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

SLR26.gam<-values(mod_projf26)
df.SLR26.gam<-as.data.frame(SLR26.gam)
df.SLR26.gam$era <- era
df.SLR26.gam$model <- c("gam")

a<-length(which(SLR26.gam > 0.7))
b<-length(which(SLR26.gam > 0.4))
c<- length(SLR26.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)


#45
era<- c("SLR 45")
map.max.fut <- predict(model.final.max, data = layersf2_mod[[vars_sel]], type = "cloglog")
map.brt.fut <- predict(model.final.brt, data = layersf2_mod[[vars_sel]], type = "cloglog")

#gigas and scylla
#map.max.fut <- predict(model.final.max, data = layersf2_mod2[[vars_sel]], type = "cloglog")
#map.brt.fut <- predict(model.final.brt, data = layersf2_mod2[[vars_sel]], type = "cloglog")


SLR45.max<-values(map.max.fut)
df.SLR45.max<-as.data.frame(SLR45.max)
df.SLR45.max$era <- era
df.SLR45.max$model <- c("maxent")

a<-length(which(SLR45.max > 0.7))
b<-length(which(SLR45.max > 0.4))
c<- length(SLR45.max)

d<-  c(myspecies, era, "Highly Suitable Habitat Maxent" , a/c)
e<-  c(myspecies, era, "Suitable Habitat Maxent"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

SLR45.brt<-values(map.brt.fut)
df.SLR45.brt<-as.data.frame(SLR45.brt)
df.SLR45.brt$era <- era
df.SLR45.brt$model <- c("brt")

a<-length(which(SLR45.brt > 0.7))
b<-length(which(SLR45.brt > 0.4))
c<- length(SLR45.brt)

d<-  c(myspecies, era, "Highly Suitable Habitat BRT" , a/c)
e<-  c(myspecies, era, "Suitable Habitat BRT"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)

SLR45.gam<-values(mod_projf45)
df.SLR45.gam<-as.data.frame(SLR45.gam)
df.SLR45.gam$era <- era
df.SLR45.gam$model <- c("gam")

a<-length(which(SLR45.gam > 0.7))
b<-length(which(SLR45.gam > 0.4))
c<- length(SLR45.gam)

d<-  c(myspecies, era, "Highly Suitable Habitat GAM" , a/c)
e<-  c(myspecies, era, "Suitable Habitat GAM"  , b/c)
metadata.proj<-rbind(metadata.proj, d,e)





#stop



############################################### 

#resaving 20aug21

names(df.bestpresent.brt)[1] = "Value"
names(df.bestpresent.max)[1] = "Value"
names(df.bestpresent.gam)[1] = "Value"

df.projections.pres<-rbind(df.bestpresent.brt, df.bestpresent.gam, df.bestpresent.max)
df.projections.pres$species<-myspecies


write.csv(df.projections.pres, paste0("../data/RawRasterData/", myspecies, "_PresentProjections_Data.csv"))

#

names(df.present.brt)[1] = "Value"
names(df.present.max)[1] = "Value"
names(df.present.gam)[1] = "Value"
names(df.future26.brt)[1] = "Value"
names(df.future26.max)[1] = "Value"
names(df.future26.gam)[1] = "Value"
names(df.future45.brt)[1] = "Value"
names(df.future45.max)[1] = "Value"
names(df.future45.gam)[1] = "Value"
names(df.future85.brt)[1] = "Value"
names(df.future85.max)[1] = "Value"
names(df.future85.gam)[1] = "Value"



df.projections.fut<-rbind(df.present.brt, df.present.gam, df.present.max,
                      df.future26.brt, df.future26.gam, df.future26.max, df.future45.brt, df.future45.gam, df.future45.max,
                      df.future85.max, df.future85.gam, df.future85.brt)

df.projections.fut$species<-myspecies


write.csv(df.projections.fut, paste0("../data/RawRasterData/", myspecies, "_FutureProjections_Data.csv"))




#############





#At the very end
write.csv(metadata.proj, paste0("../data/ProjectionData_26Jul2021.csv"), row.names = FALSE)

names(df.bestpresent.brt)[1] = "Value"
names(df.bestpresent.max)[1] = "Value"
names(df.bestpresent.gam)[1] = "Value"
names(df.present.brt)[1] = "Value"
names(df.present.max)[1] = "Value"
names(df.present.gam)[1] = "Value"
names(df.future26.brt)[1] = "Value"
names(df.future26.max)[1] = "Value"
names(df.future26.gam)[1] = "Value"
names(df.future45.brt)[1] = "Value"
names(df.future45.max)[1] = "Value"
names(df.future45.gam)[1] = "Value"
names(df.future85.brt)[1] = "Value"
names(df.future85.max)[1] = "Value"
names(df.future85.gam)[1] = "Value"
names(df.SLRpresent.brt)[1] = "Value"
names(df.SLRpresent.max)[1] = "Value"
names(df.SLRpresent.gam)[1] = "Value"
names(df.SLRfuture26.brt)[1] = "Value"
names(df.SLRfuture26.max)[1] = "Value"
names(df.SLRfuture26.gam)[1] = "Value"
names(df.SLRfuture45.brt)[1] = "Value"
names(df.SLRfuture45.max)[1] = "Value"
names(df.SLRfuture45.gam)[1] = "Value"
names(df.SLRfuture85.brt)[1] = "Value"
names(df.SLRfuture85.max)[1] = "Value"
names(df.SLRfuture85.gam)[1] = "Value"

df.projections<-rbind(df.bestpresent.brt, df.bestpresent.gam, df.bestpresent.max, df.present.brt, df.present.gam, df.present.max,
                      df.future26.brt, df.future26.gam, df.future26.max, df.future45.brt, df.future45.gam, df.future45.max,
                      df.future85.max, df.future85.gam, df.future85.brt, df.SLRpresent.brt, df.SLRpresent.max, 
                      df.SLRpresent.gam, df.SLR26.gam, df.SLR26.brt, df.SLR26.max, df.SLR45.brt, df.SLR45.gam,
                      df.SLR45.max, df.SLR85.brt, df.SLR85.gam, df.SLR85.max)

df.projections.mini<-rbind(df.present.brt, df.present.gam, df.present.max,df.future85.max, df.future85.gam, df.future85.brt)
df.projections.mini$species<-myspecies


write.csv(df.projections, paste0(myspecies, "_Projections_Data.csv", row.names = FALSE))


