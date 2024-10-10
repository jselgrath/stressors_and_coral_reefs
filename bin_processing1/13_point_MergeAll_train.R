# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# GOAL: Join random points to output from fishing and fragstat joins

# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# -------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/")

#########################################################################
# Load and organize files 
#########################################################################

# # load points with hab, resilience, and some indp data as ESRI shapefile
pts<-st_read("./results_train/8_IndpVarOther_Pts_RS.gpkg")%>%
  glimpse()
plot(pts)

# load csv points from fishing, fragstats, distance, and prox data
# here not data from categorical fishing impact script
# normalized fishing impact (by that year, not all years)
f1<-read_csv("./results_train/2_pts_FishingImpact_normalized.csv")%>% 
  glimpse()

# Cumulative fishing years, or with lag
# select for values standardized against max for all years (A)
f2<- read_csv("./results_train/3_pts_FishingYrs_2cumulative.csv")%>% glimpse()

# g1n measures
# cumulative
f3<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_blast.csv")%>% 
  glimpse()

f4<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_kaykay.csv")%>%
  glimpse()

f5<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_poison.csv")%>%
  glimpse()

f6<- read_csv("./results_train/5_pts_FishingYrsDest_normalized.csv")%>%
  glimpse()


f7<- read_csv("./results_train/6_pts_FishingYrs_destructive_cumulative.csv")%>% 
  glimpse()


f8<-read_csv("./results_train/7_pts_fragstats.csv")%>%
  glimpse()

f9<- read_csv("./results_train/11_pts_PopRsk_Norm.csv")%>% 
  glimpse()


#version with LEK SG in inner channel and MG on olango
# coral uses dist from fragstats, rubble uses dist from ArcGIS
f10<-read_csv("./results_train/12_2pts_Co_Mg_Sg_minDist2.csv")%>%
  glimpse()



####################################
# join new tables to original pt table
####################################
# head(pt@data)
pts2<-f1%>% #f7p, f4p,f5p,,f17p,f18p,f19p,f20p,f21p,f22p, f11p, f12p,f13p,f6p,
  full_join(f2)%>%
  full_join(f3)%>%
  full_join(f4)%>%
  full_join(f5)%>%
  full_join(f6)%>%
  full_join(f7)%>%
  full_join(f8)%>%
  full_join(f9)%>%
  full_join(f10)%>%
  select(-geom)%>%
  glimpse()

# merge with sf and remove fishing NAs
# spatial
pts3<-pts%>%
  full_join(pts2)%>%
  dplyr::filter(allEffort2010>=0&destEffort2010>=0&SIGen2010>=0)%>%
  glimpse
str(pts3)


############### 
# join CID and x y data to data.frame
# pt@data$CID
coord<-data.frame(st_coordinates(pts3))#"CID"
names(coord)<-c("x","y") 

# make dataframe - non-spatial
pt4<-data.frame(cbind(pts3,coord)) %>%#confirmed same CID
  glimpse()


####################
# Save files

# geopackage
st_write(pts3,"./results_train/13_IndpVar_Pts_all.gpkg", delete_layer=T)

# csv
write_csv(pt4,"./results_train/13_IndpVar_Pts_all.csv")

# ----------------------------------------
# To satisfy reviewers complaint that there were too many points, I exported this file to GIS and then I subsampled the point data using create random points and restricting the points to the IndpVar shapefile. Two versions: (1) 1000 points and (2) 500 points 250 m apart (this might build spatial structure into data so I think is less ideal, but this is what the reviewers wanted so I am creating the sample anyways...)  This did not change results in a meaninful way so not using

