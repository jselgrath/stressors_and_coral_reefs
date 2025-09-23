# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  exporting residuals from final model

# for averaged models, averaging only combines coefficients, not the actual fitted likelihoods and random effects structures across models

# >>>using residuals from the global model

##########################
library (arm)
library(ggplot2)
library(tidyverse)
library(sf)

# -------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------

# load final models
load("./results_train/model_full.R") #full model
load("./results_train/model_no_landscape.R") #full model, no landscape variables


# # load data 
d2<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>%
  mutate(MPA=as.factor(MPA),
         Ecological_zone=as.factor(ecological_zone))%>%
  glimpse()




##############
# extract residuals from model
# http://r.789695.n4.nabble.com/R-lme4-package-Fitted-values-and-residuals-td812638.html
fitted(m_all2) 
resid(m_all2)
sigma(m_all2) 
fixef(m_all2)
ranef(m_all2)

# full
fit<-data.frame(cbind(resid(m_all2),fitted(m_all2)))
names(fit)<-c("resid_full","fitVal_full")
head(fit)

# no landscape
fit3<-data.frame(cbind(resid(m_all3),fitted(m_all3)))
names(fit3)<-c("resid_nl","fitVal_nl")
head(fit3)

# plot
# par(mar=c(4,4,4,4))
with(fit,plot(fit$resid_full~fit$fitVal_full))
with(fit3,plot(fit3$resid_nl~fit3$fitVal_nl))

#merge residuals with orig data
d3<-cbind(d2,fit,fit3)
plot(d3$resid_full~d3$fitVal_full)
plot(d3$resid_nl~d3$fitVal_nl)

names(d3)
d3<-dplyr::select(d3,x,y,point_id,resid_full:fitVal_nl)%>%
  glimpse()
head(d3)

# Identify outliers (bright spots and dark spots)
d3$BrightSpots_full<-0
d3$BrightSpots_full[d3$resid_full>2] <-1

d3$DarkSpots_full<-0
d3$DarkSpots_full[-2>d3$resid_full] <-1

# variable for any large residual
d3$lgresid_full<-0
d3$lgresid_full[d3$BrightSpots_full ==1]<-1
d3$lgresid_full[d3$DarkSpots_full ==1]<-1

# residuals that are not outliers
d3$smResid_full<-0
d3$smResid_full[d3$lgresid_full==0]<-1

# make spatial file
d_spatial<-d3%>%
  st_as_sf(coords=c("x","y"), crs=32651)%>%
  glimpse()

print(st_crs(d_spatial))
plot(d_spatial[4])

#########
# save residuals
write_csv(d3, "./results_train/all_model_residuals.csv")

st_write(d_spatial,"./results_train/all_model_residuals.shp", delete_layer=T)
st_write(d_spatial,"./results_train/all_model_residuals.gpkg", layer="models",delete_layer=T)

# Next Step: Make figure in ArcGIS that includes 3 maps depicting point locations and values from d3. 
# Maps: (1) Small residuals (2) Bright spots (3) Dark spots


# # repeat and export a version with all data ----------------------
# # this is less relevant since I excluded large residuals
# # Identify outliers (bright spots and dark spots)
# d7<-d3
# d7$BrightSpots<-0
# d7$BrightSpots[d7$resid>2] <-1
# 
# d7$DarkSpots<-0
# d7$DarkSpots[-2>d7$resid] <-1
# 
# # variable for any large residual
# d7$lgresid<-0
# d7$lgresid[d7$BrightSpots ==1]<-1
# d7$lgresid[d7$DarkSpots ==1]<-1
# 
# # residuals that are not outliers
# d7$smResid<-0
# d7$smResid[d7$lgresid==0]<-1
# 
# # make spatial file
# d_spatial_all<-d7%>%
#   st_as_sf(coords=c("x","y"), crs=32651)%>%
#   glimpse()
# 
# print(st_crs(d_spatial_all))
# plot(d_spatial_all[4])
# 
# st_write(d_spatial_all,"./results_train/full_model_residuals_data2.shp", delete_layer=T)
# st_write(d_spatial_all,"./results_train/full_model_residuals2.gpkg", layer="all",delete_layer=T)
