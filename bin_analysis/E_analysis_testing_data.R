# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(lattice)
library(boot)
library(sjPlot)
library(car)

# ------------------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ----------------------------------------------
# load final models
# ----------------------------------------------
load("./results_train/mixedEf_final_all.R") # full model
load("./results_train/mixedEf_final_no_landscape.R") # no landscape variables


# ---------------------------------------------
# load data and select non-correlated variables (see Correlation Viz)
# ---------------------------------------------
d0<-read_csv("./results_test/14_IndpVar_Pts_test.csv") %>%
  filter(Depth_m>-6)%>%
  # reassigning geomorphology based on depth
  mutate(Geomorphic2=if_else(-2.5>Depth_m,"Reef Slope",Geomorphic))%>%
  mutate(Geomorphic2=if_else(-.5<Depth_m,"Reef Flat",Geomorphic2))%>%
  
  # removing coastal zone -------------------
filter(EcoZone2!="Coastal")%>%
  glimpse()


#  variables ------------------
d1<-d0%>%
  # make distance categories
  mutate(distance_mg2=if_else(distance_mg=="250" | distance_mg=="500" | distance_mg== "750","250-750", distance_mg))%>% # condense mg categories based on graphs below
  
  mutate(MPA=as.factor(if_else(mpa=="unprotected","open",mpa)),
         longitude=as.factor(longitude),
         ecological_zone=as.factor(EcoZone2),
         municipality=as.factor(Id_MunWtr),
         Depth_m=Depth_m*-1, #)%>%
         Reef_state=as.factor(Id_resil),
         Geomorphic=as.factor(Geomorphic),
         Geomorphic2=as.factor(Geomorphic2))%>%
  mutate(distance_sg=factor(distance_sg),
         distance_mg=factor(distance_mg),
         distance_mg2=factor(distance_mg2)
  )%>%
  glimpse()

# check new assignment of reef flat etc
cbind(d1$Geomorphic,d1$Geomorphic2,d1$Depth_m)

# graph updated variable ------------------
ggplot(d1,aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()

d1%>%
  filter(Geomorphic2=="Reef Slope")%>%
  ggplot(aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()

d1%>%
  filter(Geomorphic2=="Reef Flat")%>%
  ggplot(aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()



range(d1$Depth_m,na.rm=T)

# combine terrestrial island and coastal (done earlier)
unique(d0$EcoZone2)
d1$EcoZone2[d1$EcoZone2=="Terrestrial Island"]<-"Coastal"
unique(d1$EcoZone2)

# with(d1,xyplot(Id_resil~Depth_m|EcoZone2,type=c('g','p','l'),
# layout=c(3,1), index.cond = function(x,y)max(y)))


# check depth representation # ---------------------------------------------
min(d1$Depth_m, na.rm=T)
max(d1$Depth_m, na.rm=T)

# ---------------------------------------------
# subset data and make factors
# ---------------------------------------------
d2a<-d1%>%
  dplyr::select(PtID2,Reef_state,ecological_zone,longitude, mpa_barangay,MPA,mpa_area_ha,municipality, mpa_area_ha,Depth_m,sg_minDist_100,mg_minDist_100,co_minDist_100, distance_sg,distance_mg, distance_mg2,PopRskDecay, PopRskDecay.Nrm,fYrLag30A, cum_blast00, cum_FA_blast00,cum_FA_blast10,cum_kaykay00, cum_poison00,divGen2010,CoRuArea,CoRuEdg2Area,FRAC,PROX,SHAPE, PARA,CoRuEdg2Area,Geomorphic,Geomorphic2,x,y)%>%
  glimpse()

d2<-na.omit(d2a)




# ---------------------------------------------
# further subset, remove outliers
# ---------------------------------------------
d3<-d2%>%
  dplyr::select(PtID2,Reef_state,ecological_zone,MPA,Depth_m,sg_minDist_100,mg_minDist_100,distance_sg,distance_mg, distance_mg2,PopRskDecay, PopRskDecay.Nrm,fYrLag30A, cum_FA_blast10,cum_kaykay00, cum_poison00,divGen2010,CoRuArea,CoRuEdg2Area,Geomorphic,Geomorphic2,SHAPE,PARA,x,y)%>%
  glimpse()



# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)

with(d3,plot(Depth_m,cum_FA_blast10))


# check centering and scaling - .cs better than z. scores because filtered data -----------
t0<-cbind(cs.(d1$Depth_m),d1$z.Depth_m)
t0
colMeans(t0)
apply(t0,2,sd)


# subset data, clean data names ----------------------------
# in original file, remove outliers before this step
d5<-d3%>%
  mutate(Depth=cs.(Depth_m)[,1], # [,1] calls it out of the matrix
         Seagrass_isolation=cs.(I(sg_minDist_100^2))[,1],
         Patch_complexity=cs.(SHAPE)[,1],
         Population_risk=cs.(PopRskDecay.Nrm)[,1],
         Fishing_legacy_1980_2000=cs.(fYrLag30A)[,1],
         Blast_fishing_2010_2000=cs.(cum_FA_blast10)[,1])%>%
  dplyr::select(
    Reef_state,Depth, Seagrass_isolation,Patch_complexity,Population_risk,
    Fishing_legacy_1980_2000,Blast_fishing_2010_2000,MPA,Ecological_zone=ecological_zone,x,y,PtID2)%>%
  glimpse()


# ----------------------------------------
# Re-Build model with new data
# ----------------------------------------
d5$p_m_final<- round(predict(m_final, newdata = d5, type = "response"),3)%>%
  glimpse()
qplot(d5$p_m_final)
 

d5$p_m_final_no_l<- round(predict(m_final_no_landscape, newdata = d5, type = "response"),3)%>%
  glimpse()
qplot(d5$p_m_final_no_l)


# from checking model from Sept meeting with SG - much worse than other models!
# d5$Pm11<- round(predict(m.me2b, newdata = d5, type = "response"),3)
glimpse(d5)


######################
# Save predicted results to examine in GIS
write_csv(d5,"./results_test/m_final_test_data.csv")



