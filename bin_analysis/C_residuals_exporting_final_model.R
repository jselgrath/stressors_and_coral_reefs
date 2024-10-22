# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  exporting residuals from final model

##########################
library (arm)
library(ggplot2)
library(tidyverse)

# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------


# load final models
load("./results_train/mixedEf_final_all.R") #full model
load("./results_train/mixedEf_final_no_landscape.R") #full model, no landscape variables

# # load data 
d5<-read_csv("./results_train/15_IndpVar_Pts_train_for_models_subset.csv")%>%
  mutate(MPA=as.factor(MPA),Ecological_zone=as.factor(Ecological_zone))%>%
  glimpse()




##############
# extract residuals from model
# http://r.789695.n4.nabble.com/R-lme4-package-Fitted-values-and-residuals-td812638.html
fitted(m_final) 
resid(m_final)
sigma(m_final) 
fixef(m_final)
ranef(m_final)

fit<-data.frame(cbind(resid(m_final),fitted(m_final)))
names(fit)<-c("resid","fitVal")
head(fit)

# plot
# par(mar=c(4,4,4,4))
with(fit,plot(fit$resid~fit$fitVal))

#merge residuals with orig data
d5<-cbind(d5,fit)
plot(d5$resid~d5$fitVal)

names(d5)
d3<-dplyr::select(d5,x,y,PtID2,resid,fitVal)%>%
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
st_write(d_spatial,"./results_train/full_model_residuals.gpkg", delete_layer=T)

# Next Step: Make figure in ArcGIS that includes 3 maps depicting point locations and values from d3. 
# Maps: (1) Small residuals (2) Bright spots (3) Dark spots