library(ggplot2)
library(ggridges)
library(tidyr)
library(dplyr)
dat<-read.csv("../../Data Files/ProjectionsData_RawValues_17Aug21.csv")
dat.supp<-read.csv("../../Data Files/ProjectionsData_RawValues_19Aug21.csv")
View(dat)
#need all species together, y should be species
# we should pick which model we want to show (like all maxent or something)



dat.supp2$Value<-round(dat.supp2$Value, digits = 6)
dat$Value<-round(dat$Value, digits = 6)

new<- dat.supp2 %>% group_by(species, model, era) %>% tally()
dat.supp2<-dat.supp[-c(471745:483840),]

dat.supp2$list <- paste(dat.supp2$species, dat.supp2$era, dat.supp2$model)

dat$list <- paste(dat$species, dat$era, dat$model)

dat2 <- dat[ ! dat$list %in% dat.supp2$list, ]

dat2 <-rbind(dat.supp2, dat2)

new<- dat2 %>% group_by(species, model, era) %>% tally()


### mini test new future data

df.projections.mini$list <- paste(df.projections.mini$species, df.projections.mini$era, df.projections.mini$model)
dat3 <- dat2[ ! dat2$list %in% df.projections.mini$list, ]
dat3<-dat3[-c(1)]

dat3 <-rbind(dat3, df.projections.mini)


###


a<- read.csv("../data/RawRasterData/Anadara antiquata_FutureProjections_Data.csv")
b<- read.csv("../data/RawRasterData/Bohadschia argus_FutureProjections_Data.csv")
c<- read.csv("../data/RawRasterData/Charonia tritonis_FutureProjections_Data.csv")
d<- read.csv("../data/RawRasterData/Holothuria atra_FutureProjections_Data.csv")
e<- read.csv("../data/RawRasterData/Holothuria edulis_FutureProjections_Data.csv")
f<- read.csv("../data/RawRasterData/Tridacna gigas_FutureProjections_Data.csv")
g<- read.csv("../data/RawRasterData/Tridacna squamosa_FutureProjections_Data.csv")
h<- read.csv("../data/RawRasterData/Tridacna maxima_FutureProjections_Data.csv")
i<- read.csv("../data/RawRasterData/Scylla serrata_FutureProjections_Data.csv")
j<- read.csv("../data/RawRasterData/Mangrove_FutureProjections_Data.csv")
k<- read.csv("../data/RawRasterData/Seagrass_FutureProjections_Data.csv")
l<- read.csv("../data/RawRasterData/Coral_FutureProjections_Data.csv")

dat4 <-rbind(a,b,c,d,e,f,g,h,i,j,k,l)
write.csv(dat4,"AllTaxa_RawRasterFile.csv")

new<- dat4 %>% group_by(species, model, era) %>% tally()

dat4$list <- paste(dat4$species, dat4$era, dat4$model)

dat5 <- dat3[ ! dat3$list %in% dat4$list, ]
dat4<-dat4[-c(1)]


dat5 <-rbind(dat5, dat4)

a2<- read.csv("../data/RawRasterData/Anadara antiquata_PresentProjections_Data.csv")
b2<- read.csv("../data/RawRasterData/Bohadschia argus_PresentProjections_Data.csv")
c2<- read.csv("../data/RawRasterData/Charonia tritonis_PresentProjections_Data.csv")
d2<- read.csv("../data/RawRasterData/Holothuria atra_PresentProjections_Data.csv")
e2<- read.csv("../data/RawRasterData/Holothuria edulis_PresentProjections_Data.csv")
f2<- read.csv("../data/RawRasterData/Tridacna gigas_PresentProjections_Data.csv")
g2<- read.csv("../data/RawRasterData/Tridacna squamosa_PresentProjections_Data.csv")
h2<- read.csv("../data/RawRasterData/Tridacna maxima_PresentProjections_Data.csv")
i2<- read.csv("../data/RawRasterData/Scylla serrata_PresentProjections_Data.csv")
j2<- read.csv("../data/RawRasterData/Mangrove_PresentProjections_Data.csv")
k2<- read.csv("../data/RawRasterData/Seagrass_PresentProjections_Data.csv")
l2<- read.csv("../data/RawRasterData/Coral_PresentProjections_Data.csv")

dat6 <-rbind(a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
write.csv(dat6,"AllTaxaPresent_RawRasterFile.csv")

new<- dat4 %>% group_by(species, model, era) %>% tally()

dat6$list <- paste(dat6$species, dat6$era, dat6$model)

dat7 <- dat5[ ! dat5$list %in% dat6$list, ]

dat6<-dat6[-c(1)]


dat7 <-rbind(dat7, dat6)



###


dat.inverts <- dat7 %>% filter(species == "Tridacna maxima" | species == "Tridacna gigas" |species == "Tridacna squamosa" | species == "Holothuria atra"|
                                species =="Holothuria edulis" | species =="Anadara antiquata" | species =="Bohadschia argus"| species =="Charonia tritonis"|
                                species == "Scylla serrata")

dat.inverts.max<- dat.inverts %>% filter(model == "maxent")
trial2<-dat.inverts %>% group_by(species,era,model) %>% tally()
  
dat.habitats <- dat7 %>% filter(species == "Coral" | species == "Mangrove" | species == "Seagrass")
trial<-dat.habitats %>% group_by(species,era,model) %>% tally()

dat.habitats.brt <- dat.habitats %>% filter(model == "brt")
  
  
h.atrahis<- dat2 %>% filter(species=="Holothuria atra" & era == "SLR 45")    
h.atrahisf<- dat2 %>% filter(species=="Holothuria atra" & era == "future 45")    
h.atrahisfp<- dat2 %>% filter(species=="Holothuria atra" & era == "future present" & model == "maxent") 
h.atrahisfsp<- dat2 %>% filter(species=="Holothuria atra" & era == "SLR present" & model == "maxent") 
  
hist(h.atrahisfsp$Value, xlim = c(0.2, 1), ylim= c(0,500))
hist(table1$`values(map)`, xlim = c(0.2, 1), ylim= c(0,500))  

hist(h.atrahisfp$Value, xlim = c(0.2, 1), ylim= c(0,500))
hist(table2$`values(map)`, xlim = c(0.2, 1), ylim= c(0,500))  


dat.inverts.max2<-na.omit(dat.inverts.max)
write.csv(dat.inverts.max2, "Invert_RawValues_Maxent.csv")





#################
require(tidyverse)
dat.inverts.max %>%
  filter(Value>0.8)%>% # adjust to see the big values
  ggplot(aes(x=Value, fill=as.factor(species))) +
  geom_histogram(bins=15) + # added more bins but can reduce, changing this changes what you 'see' in terms of how many are big
  #xlim(.3, 1.01) +
  #ylim(0,300) +
  facet_grid(cols= vars(era), rows = vars(species), scales = "free")+
  labs(y='Frequency of Rating',
       x='Habitat Suitability') +
  theme(axis.text.x=element_text(size = 10, angle=45))





###############################

ggplot(dat.habitats.brt, aes(x=Value, y=factor(era), fill=as.factor(species))) +
  
  geom_density_ridges(alpha=0.7, scale=0.9, size=1.2) +
  
  xlim(.1, 1) +
  
  theme(axis.text.x = element_text(size=12),
        
        axis.text.y = element_text(size=12),
        
        text = element_text(size=14)) +
        
      #  axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 20, l = 0)),
        
      #  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  
  scale_fill_manual(name = "Demographic\nscenario", values = c("mediumblue","goldenrod1","maroon")) +
  
  scale_y_discrete(expand = c(0, 0)) +
      #             , labels=c("2.6","4.5","6.0","8.5")) +
      
  coord_cartesian(clip = "off") +
  
  facet_grid(cols= vars(species))+
  
  labs(y='Era',
       
       x='Remaining population (%)')

########################################

ggplot(dat.inverts.max, aes(x=Value, y=factor(era), fill=as.factor(species))) +
  
  geom_density_ridges(alpha=0.7, scale=0.9, size=1.2) +
  
  xlim(.1, 1) +
  
  theme(axis.text.x = element_text(size=12),
        
        axis.text.y = element_text(size=12),
        
        text = element_text(size=14)) +
  
  #  axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 20, l = 0)),
  
  #  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  
  scale_fill_manual(name = "Demographic\nscenario", values = c("mediumblue","goldenrod1","maroon", "aquamarine3", 
                                                               "brown1", "chartreuse3", "darkorange", "darkorchid",
                                                               "cyan1")) +
  
  scale_y_discrete(expand = c(0, 0)) +
  #             , labels=c("2.6","4.5","6.0","8.5")) +
  
  coord_cartesian(clip = "off") +
  
  facet_grid(cols= vars(species))+
  
  labs(y='Era',
       
       x='Remaining population (%)')


#####################

ggplot(dat.inverts.max, aes(x=Value, fill=as.factor(species))) +
  
  geom_histogram(bins=5) +
  
  xlim(.3, 1.01) +
  
  ylim(0,300) +
  
   facet_grid(cols= vars(era), rows = vars(species))+
  
  labs(y='Frequency of Rating',
       
       x='Habitat Suitability')




################

dat.inverts.max2<-na.omit(dat.inverts.max)
dat.inverts.max2$Order <- ifelse(dat.inverts.max2$era == "best present", 1, 
                             ifelse(dat.inverts.max2$era == "future present", 2,
                                    ifelse(dat.inverts.max2$era == "future 26", 3,
                                           ifelse(dat.inverts.max2$era == "future 45", 4,
                                                  ifelse(dat.inverts.max2$era == "future 85", 5,
                                                         ifelse(dat.inverts.max2$era == "SLR present", 6,
                                                                ifelse(dat.inverts.max2$era == "SLR 26", 7,
                                                                       ifelse(dat.inverts.max2$era == "SLR 45",8,
                                                                              ifelse(dat.inverts.max2$era == "SLR 85", 9, 10)))))))))



dat.inverts.max2 %>% group_by(species, Order, model) %>% summarize(mean_value = mean(Value)) %>%
  ggplot(aes(x=Order, y = mean_value)) + geom_col()+ ylab("Average") + ggtitle("Average Habitat Suitability") +
  scale_x_discrete(limits = c('Best Present', 'Future Present', 'Future 26', 'Future 45', 'Future 85', 'SLR Present', 'SLR 26', 'SLR 45', 'SLR 85'))+
  facet_grid(vars(species), scales="free")  

dat.habitats.brt2<-na.omit(dat.habitats.brt)
dat.habitats.brt2$Order <- ifelse(dat.habitats.brt2$era == "best present", 1, 
                                 ifelse(dat.habitats.brt2$era == "future present", 2,
                                        ifelse(dat.habitats.brt2$era == "future 26", 3,
                                               ifelse(dat.habitats.brt2$era == "future 45", 4,
                                                      ifelse(dat.habitats.brt2$era == "future 85", 5,
                                                             ifelse(dat.habitats.brt2$era == "SLR present", 6,
                                                                    ifelse(dat.habitats.brt2$era == "SLR 26", 7,
                                                                           ifelse(dat.habitats.brt2$era == "SLR 45",8,
                                                                                  ifelse(dat.habitats.brt2$era == "SLR 85", 9, 10)))))))))



dat.habitats.brt2 %>% group_by(species, Order, model) %>% summarize(mean_value = mean(Value)) %>%
  ggplot(aes(x=Order, y = mean_value)) + geom_col()+ ylab("Average") + ggtitle("Average Habitat Suitability") +
  scale_x_discrete(limits = c('Best Present', 'Future Present', 'Future 26', 'Future 45', 'Future 85', 'SLR Present', 'SLR 26', 'SLR 45', 'SLR 85'))+
  facet_grid(vars(species), scales="free")  



################################################################

Habitat.dat<- read.csv("../../Data Files/ProjectionData_17Aug21.csv")

Habitat.supp<- read.csv("../../Data Files/ProjectionData_19Aug21.csv")

Habitat.dat<-Habitat.dat[-c(1),]
Habitat.supp<-Habitat.supp[-c(1),]


Habitat.dat$X.Value.<-as.numeric(Habitat.dat$X.Value.)
Habitat.dat$Percentage<-Habitat.dat$X.Value. * 100

Habitat.dat$Model <- gsub(".* ", "", Habitat.dat$X.Category.)
Habitat.dat$HabType <- gsub(" [^ ]*$", "", Habitat.dat$X.Category.)

Habitat.supp$X.Value.<-as.numeric(Habitat.supp$X.Value.)
Habitat.supp$Percentage<-Habitat.supp$X.Value. * 100

Habitat.supp$Model <- gsub(".* ", "", Habitat.supp$X.Category.)
Habitat.supp$HabType <- gsub(" [^ ]*$", "", Habitat.supp$X.Category.)

new<- Habitat.supp %>% group_by(X.Species., Model, X.Model.Era.) %>% tally()
#remove gigas SLR present
Habitat.supp2<-Habitat.supp %>% distinct(X.Species., Model, X.Model.Era., HabType, .keep_all = TRUE)
new<- Habitat.supp2 %>% group_by(X.Species., Model, X.Model.Era.) %>% tally()
Habitat.supp2$list <- paste(Habitat.supp2$X.Species., Habitat.supp2$X.Model.Era., Habitat.supp2$X.Category.)

Habitat.dat$list <- paste(Habitat.dat$X.Species., Habitat.dat$X.Model.Era., Habitat.dat$X.Category.)

Habitat.dat2 <- Habitat.dat[ ! Habitat.dat$list %in% Habitat.supp2$list, ]
Habitat.dat2 <-rbind(Habitat.supp2, Habitat.dat2)
new<- Habitat.dat2 %>% group_by(X.Species., Model, X.Model.Era.) %>% tally()

Habitat.dat2$Order <- ifelse(Habitat.dat2$X.Model.Era. == "best present", 1, 
                      ifelse(Habitat.dat2$X.Model.Era. == "future present", 2,
                             ifelse(Habitat.dat2$X.Model.Era. == "future 26", 3,
                                    ifelse(Habitat.dat2$X.Model.Era. == "future 45", 4,
                                           ifelse(Habitat.dat2$X.Model.Era. == "future 85", 5,
                                                  ifelse(Habitat.dat2$X.Model.Era. == "SLR present", 6,
                                                         ifelse(Habitat.dat2$X.Model.Era. == "SLR 26", 7,
                                                                ifelse(Habitat.dat2$X.Model.Era. == "SLR 45",8,
                                                                       ifelse(Habitat.dat2$X.Model.Era. == "SLR 85", 9, 10)))))))))
                                                                              


ggplot(Habitat.dat2, aes(x=Order, y=Percentage, fill = HabType)) + 
  geom_bar(position = 'identity', stat='identity', alpha = .5) +
  facet_grid(rows= vars(X.Species.), cols = vars(Model), scales="free")



Habitat.dat.fut <- Habitat.dat2  %>% filter(X.Model.Era. == "future present" | X.Model.Era. =="future 26" |X.Model.Era. == "future 45" |X.Model.Era. == "future 85") %>%
  group_by(X.Species., Model, HabType) %>%
  mutate(Change = Percentage - Percentage[X.Model.Era. == "future present"])

Habitat.dat.fut <- Habitat.dat.fut  %>%
  group_by(X.Species., Model, HabType) %>%
  mutate(Percent.Change = Change / Percentage[X.Model.Era. == "future present"] *100)
Habitat.dat.fut$Percent.Change[is.na(Habitat.dat.fut$Percent.Change)] <- 0


Habitat.dat.fut %>% filter(X.Species. == "Tridacna maxima" | X.Species. == "Tridacna gigas" |X.Species. == "Tridacna squamosa" | X.Species. == "Holothuria atra"|
                             X.Species. =="Holothuria edulis" | X.Species. =="Anadara antiquata" | X.Species. =="Bohadschia argus"| X.Species. =="Charonia tritonis"|
                             X.Species. == "Scylla serrata") %>% filter(Model == "Maxent") %>%

ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.), scales="free") 


Habitat.dat.fut %>% filter(X.Species. == "Mangrove" | X.Species. == "Seagrass" |X.Species. == "Coral" )  %>%
                            filter(Model == "BRT") %>%
  
  ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.)) + ylim (-100,100)


Habitat.dat.slr <- Habitat.dat2  %>% filter(X.Model.Era. == "SLR present" | X.Model.Era. =="SLR 26" |X.Model.Era. == "SLR 45" |X.Model.Era. == "SLR 85") %>%
  group_by(X.Species., Model, HabType) %>%
  mutate(Change = Percentage - Percentage[X.Model.Era. == "SLR present"])

Habitat.dat.slr <- Habitat.dat.slr  %>%
  group_by(X.Species., Model, HabType) %>%
  mutate(Percent.Change = Change / Percentage[X.Model.Era. == "SLR present"] *100)
Habitat.dat.slr$Percent.Change[is.na(Habitat.dat.slr$Percent.Change)] <- 0


Habitat.dat.slr %>% filter(X.Species. == "Tridacna maxima" | X.Species. == "Tridacna gigas" |X.Species. == "Tridacna squamosa" | X.Species. == "Holothuria atra"|
                             X.Species. =="Holothuria edulis" | X.Species. =="Anadara antiquata" | X.Species. =="Bohadschia argus"| X.Species. =="Charonia tritonis"|
                             X.Species. == "Scylla serrata") %>% filter(Model == "Maxent") %>%
  
  ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.), scales = "free") 


Habitat.dat.slr %>% filter(X.Species. == "Mangrove" | X.Species. == "Seagrass" |X.Species. == "Coral" )  %>%
  filter(Model == "BRT") %>%
  
  ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.)) + ylim (-100,100)



Habitat.dat2 %>% filter(X.Species. == "Tridacna maxima" | X.Species. == "Tridacna gigas" |X.Species. == "Tridacna squamosa" | X.Species. == "Holothuria atra" |
                             X.Species. =="Holothuria edulis" | X.Species. =="Anadara antiquata" | X.Species. == "Bohadschia argus"| X.Species. =="Charonia tritonis"|
                             X.Species. == "Scylla serrata") %>% filter(Model == "Maxent") %>%
                    filter(X.Model.Era.== "best present" | X.Model.Era. == "future present" | X.Model.Era. == "SLR present") %>%
  
 ggplot(aes(x=X.Model.Era., y=Percentage, fill = HabType)) + 
  geom_bar(position = 'identity', stat='identity', alpha = .5) +
  facet_grid(rows= vars(X.Species.), scales="free")



Habitat.dat2$Time<- gsub(".* ", "", Habitat.dat2$X.Model.Era.)
Habitat.dat.chng <- Habitat.dat2  %>% filter(X.Model.Era. == "future present" |
                                               X.Model.Era. =="future 26" |X.Model.Era. == "future 45" |
                                               X.Model.Era. == "future 85" | X.Model.Era. == "SLR present" |
                                               X.Model.Era. =="SLR 26" | X.Model.Era. == "SLR 45" |
                                               X.Model.Era. == "SLR 85")%>%

  group_by(X.Species., Model, Time, HabType) %>%
  mutate(SLRDiff = Percentage - Percentage[Order == 4 | Order ==5 | Order ==2 | Order == 3])

Habitat.dat.chng$PercSLRDiff <- Habitat.dat.chng$SLRDiff/Habitat.dat.chng$Percentage*100


SLRTable<-Habitat.dat.chng %>% filter(PercSLRDiff < 0 | PercSLRDiff > 0) 


ggplot(SLRTable, aes(x=list, y=PercSLRDiff, fill = HabType, label = Model)) +geom_col() + 
   facet_grid(rows = vars(X.Species.)) + geom_hline(yintercept = 0) + geom_text


#################

Habitat.dat.fut %>% filter(X.Species. == "Seagrass" |X.Species. == "Coral" )  %>%
  filter(Model == "Maxent") %>%
  
  ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.)) + ylim (-100,100)


Habitat.dat.slr %>% filter( X.Species. == "Seagrass" |X.Species. == "Coral" )  %>%
  filter(Model == "Maxent") %>%
  
  ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.)) + ylim (-100,100)

###########

Habitat.dat.fut %>% filter( X.Species. == "Holothuria atra"| X.Species. =="Bohadschia argus") %>% filter(Model == "GAM") %>%
  
  ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.), scales="free") 


Habitat.dat.slr %>% filter(X.Species. == "Tridacna maxima" | X.Species. == "Tridacna gigas" | X.Species. == "Holothuria atra"|
                             X.Species. =="Holothuria edulis" | X.Species. =="Bohadschia argus"| 
                             X.Species. == "Scylla serrata") %>% filter(Model == "GAM") %>%
  
  ggplot(aes(x=Order, y=Percent.Change)) + geom_point(aes(colour= HabType, size = 1, alpha = 0.5)) + facet_wrap(vars(X.Species.), scales = "free") 


###############################

#averages over time from values