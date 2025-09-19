# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  exporting residuals from final model

##########################
library (arm)
library(ggplot2)
library(tidyverse)
library(sf)

# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")

# ------------------------------------------------------


# load final models
load("./results_train/model_full.R") #full model
load("./results_train/model_no_landscape.R") #full model, no landscape variables


# # load data 
d2<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>%
  mutate(MPA=as.factor(MPA),Ecological_zone=as.factor(ecological_zone))%>%
  glimpse()




##############
# extract residuals from model
# http://r.789695.n4.nabble.com/R-lme4-package-Fitted-values-and-residuals-td812638.html
fitted(m_all2) 
resid(m_all2)
sigma(m_all2) 
fixef(m_all2)
ranef(m_all2)

fit<-data.frame(cbind(resid(m_all2),fitted(m_all2)))
names(fit)<-c("resid","fitVal")
head(fit)

# plot
# par(mar=c(4,4,4,4))
with(fit,plot(fit$resid~fit$fitVal))

#merge residuals with orig data
d3<-cbind(d2,fit)
plot(d3$resid~d3$fitVal)

names(d3)
d3<-dplyr::select(d3,x,y,point_id,resid,fitVal)%>%
  glimpse()
head(d3)

# Identify outliers (bright spots and dark spots)
d3$BrightSpots<-0
d3$BrightSpots[d3$resid>2] <-1

d3$DarkSpots<-0
d3$DarkSpots[-2>d3$resid] <-1

# variable for any large residual
d3$lgresid<-0
d3$lgresid[d3$BrightSpots ==1]<-1
d3$lgresid[d3$DarkSpots ==1]<-1

# residuals that are not outliers
d3$smResid<-0
d3$smResid[d3$lgresid==0]<-1

# make spatial file
d_spatial<-d3%>%
  st_as_sf(coords=c("x","y"), crs=32651)%>%
  glimpse()

print(st_crs(d_spatial))
plot(d_spatial[4])

#########
# save residuals
write_csv(d3, "./results_train/full_model_residuals.csv")

st_write(d_spatial,"./results_train/full_model_residuals.shp", delete_layer=T)
st_write(d_spatial,"./results_train/full_model_residuals.gpkg", layer="model",delete_layer=T)

# Next Step: Make figure in ArcGIS that includes 3 maps depicting point locations and values from d3. 
# Maps: (1) Small residuals (2) Bright spots (3) Dark spots


# repeat and export a version with all data ----------------------
# Identify outliers (bright spots and dark spots)
d7<-d3
d7$BrightSpots<-0
d7$BrightSpots[d7$resid>2] <-1

d7$DarkSpots<-0
d7$DarkSpots[-2>d7$resid] <-1

# variable for any large residual
d7$lgresid<-0
d7$lgresid[d7$BrightSpots ==1]<-1
d7$lgresid[d7$DarkSpots ==1]<-1

# residuals that are not outliers
d7$smResid<-0
d7$smResid[d7$lgresid==0]<-1

# make spatial file
d_spatial_all<-d7%>%
  st_as_sf(coords=c("x","y"), crs=32651)%>%
  glimpse()

print(st_crs(d_spatial_all))
plot(d_spatial_all[4])

st_write(d_spatial_all,"./results_train/full_model_residuals_data2.shp", delete_layer=T)
st_write(d_spatial_all,"./results_train/full_model_residuals2.gpkg", layer="all",delete_layer=T)
