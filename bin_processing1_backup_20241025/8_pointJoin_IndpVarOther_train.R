# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# GOAL: Join random points to other indp data in shapefile format (indp var)
###########################################

# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# -------------------------------------------
# OBJECTIVE 1: Load and organize files 
# -------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

############################
# TASK 1.1 Load point files

#read in shapefile of random point with resilience data
pt<-st_read("./results_train/hab_Resil_Pts_RS.gpkg")%>%
  select(PtID2,Geomorphic,geom)%>%
  glimpse()


############################
# Task 1.2 load shapefiles with indp data 

#1. depth/bathymetry # changing to continuous variable
#read in shapefile of depth data
v1<-st_read("./gis/0most_shp/depth/depth_splinelandshallow_20160726_dis.shp")%>%
  glimpse()


v1p_s <- pt%>%
  st_join(v1)%>%
  arrange(PtID2)%>%
  glimpse()

# check depth representation
min(v1p_s$Depth_m, na.rm=T)


filter(v1p_s,Depth_m<= -10)

# convert to data frame
v1p<- as.data.frame(v1p_s)%>%
  filter(Depth_m>=-10)%>%   # remove deep points
  glimpse()


#2. Ecol Zones
# ogrInfo(".","EcologicalZones_FA_Land_20160810"); 
v2<-st_read("./gis/0most_shp/ecological_zones/EcologicalZones_FA_Land_20160810.shp")%>%
  dplyr::select(-Id,-ORIG_FID,-EcoZ_Id2,-ZoneID)%>%
  dplyr::select(EcoZone=Zone,EcoZone2, geometry)%>%
  glimpse()

# extract polygon data to points
v2p_a <- pt%>%
  st_join(v2)%>%
  glimpse()

# convert to data frame
v2p<- as.data.frame(v2p_a)%>%
  glimpse()# or .x?

#3. LongitudeZones  -------------------------------------------------
# ogrInfo(".","longzoneFA_20160525"); 
v3<-st_read("./gis/0most_shp/longitude_zones/longzoneFA_20160525.shp")%>%
  mutate(longitude=Id)%>%
  select(-Id)%>%
  glimpse()

# extract polygon data to points
v3p_a <- pt%>%
  st_join(v3)%>%
  glimpse()

# convert to data frame
v3p<- as.data.frame(v3p_a)%>%
  glimpse()

#4. MPAs in focal area (FA)
# ogrInfo(".","MPA_FA_20160525_2");
v4<-st_read("./gis/0most_shp/marine_protected_areas_focal_area/MPA_FA_20160525_3.shp")%>%
  mutate(mpa=if_else(Id_binary==601,"protected","unprotected"))%>%
  select(mpa,mpa_id=Id,mpa_name=NAME,mpa_desig=DESIG,mpa_status=STATUS,mpa_area_ha=AREA_R_HA,mpa_barangay=Brgy)%>%
  glimpse()

# extract polygon data to points
v4p_a <- pt%>%
  st_join(v4)%>% 
  glimpse()


# convert to data frame
v4p<- as.data.frame(v4p_a)%>%
  glimpse()



#MunicipalWaters
# ogrInfo(".","MunWaterLine3_Polyg6");
v5<-st_read("./gis/0most_shp/municipal_water/MunWaterLine3_Polyg6.shp")%>%
  glimpse()

# extract polygon data to points
v5p_a <- pt%>%
  st_join(v5)%>%
  dplyr::select(PtID2,Id_MunWtr,geom)%>%
  glimpse()

# convert to data frame
v5p<- as.data.frame(v5p_a)%>%
  glimpse


# var.names<-c("Depth","EcoZone","LongZone","MPA","MunWater","dRiver", "dMang","dSeagrs")

####################################
# join new tables to original pt table
glimpse(pt)

pt_b<-data.frame(pt)%>%
  glimpse()

pt3<-pt%>%
  full_join(v1p)%>%# v6p,
  full_join(v2p)%>%
  full_join(v3p)%>%
  full_join(v4p)%>%
  full_join(v5p)%>%
  # full_join(v7p)%>%
  # full_join(v8p)%>%
  glimpse()
str(pt3)

# write out a new shapefile (including .prj component)
st_write(pt3,"./results_train/8_IndpVarOther_Pts_RS.gpkg", delete_layer=T)





