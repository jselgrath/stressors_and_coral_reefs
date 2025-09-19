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
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")

# -------------------------------------------
#read in random points - # update this name if change sampling number and distance

# train --------
# pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train_extra_pts100")%>% 
#   glimpse()
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # 
  glimpse()

# test ------
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()

############################
# Task 1.2 load shapefiles with indp data 

#1. depth/bathymetry # changing to continuous variable
#read in shapefile of depth data
v1<-st_read("./gis2/depth/depth_shp/depth_splinelandshallow_20160726_dis.shp")%>%
  glimpse()

# train ------
v1p_s <- pts%>%
  st_join(v1)%>%
  arrange(Depth_m)%>%
  glimpse()

# check depth representation
min(v1p_s$Depth_m, na.rm=T)
filter(v1p_s,Depth_m<= -25)

# convert to data frame
v1p<- as.data.frame(v1p_s)%>%
  filter(Depth_m>=-25)%>%   # remove deep points
  glimpse()

# test -------------
v1p_s_te <- pts_te%>%
  st_join(v1)%>%
  arrange(Depth_m)%>%
  glimpse()

# convert to data frame
v1p_te<- as.data.frame(v1p_s_te)%>%
  filter(Depth_m>=-25)%>%   # remove deep points
  glimpse()


#2.various versions have different numbers of classes
v2<-st_read("./gis2/ecological_zones/EcoZones2_DB.shp")%>% 
  dplyr::select(ecological_zone=eco_zone2,ecological_zone2=eco_zn2_id,)%>%
  glimpse()
  
unique(v2$ecological_zone)


# extract polygon data to points
# train --------
v2p <- pts%>%
  st_join(v2)%>%
  as.data.frame()%>% #convert to data frame
  glimpse()

# test --------
# extract polygon data to points
v2p_te <- pts_te%>%
  st_join(v2)%>%
  as.data.frame()%>% #convert to data frame
  glimpse()


#3. LongitudeZones  -------------------------------------------------
v3<-st_read("./gis2/longitude_zones/longzoneFA_20160525.shp")%>%
  mutate(longitude=Id)%>%
  dplyr::select(-Id)%>%
  glimpse()

# extract polygon data to points
# train ---
v3p <- pts%>%
  st_join(v3)%>%
  as.data.frame()%>%
  glimpse()

# test ---
v3p_te <- pts_te%>%
  st_join(v3)%>%
  as.data.frame()%>%
  glimpse()



#4. MPAs in focal area (FA)
v4<-st_read("./gis2/mpa/MPA_FA_20160525_4_50m_buf.shp")%>% # was: MPA_FA_20160525_3.shp - updated to include 50m buffer
  mutate(mpa=if_else(Id_binary==601,"protected","unprotected"))%>%
  dplyr::select(mpa,mpa_id=Id,mpa_name=NAME,mpa_desig=DESIG,mpa_status=STATUS,mpa_area_ha=AREA_R_HA,mpa_barangay=Brgy)%>%
  glimpse()


# extract polygon data to points
# train --
v4p <- pts%>%
  st_join(v4)%>% 
  as.data.frame()%>%
  glimpse()

# test --
v4p_te <- pts_te%>%
  st_join(v4)%>% 
  as.data.frame()%>%
  glimpse()

#5. Municipal Waters
v5<-st_read("./gis2/municipal_waters/municipal_waters.shp")%>%
  glimpse()

# plot(v5)

# extract polygon data to points
# train --
v5p <- pts%>%
  st_join(v5)%>%
  dplyr::select(point_id,Id_MunWtr)%>%
  as.data.frame()%>%
  glimpse

# test --
v5p_te <- pts_te%>%
  st_join(v5)%>%
  dplyr::select(point_id,Id_MunWtr)%>%
  as.data.frame()%>%
  glimpse


# geomorphology
v6<-st_read("./gis2/geomorphology/geomorphology_19x22_20160523.shp")%>%
  glimpse()

# extract polygon data to points
# train --
v6p <- pts%>%
  st_join(v6)%>%
  dplyr::select(gridcode)%>%
  as.data.frame()%>%
  mutate(geomorphology_id=gridcode,
         geomorphology=if_else(gridcode==803,"reef flat","reef slope"))%>% #checked 16 points in deep area and ok to assign to slope
  glimpse()

# test --
v6p_te <- pts_te%>%
  st_join(v6)%>%
  dplyr::select(gridcode)%>%
  as.data.frame()%>%
  mutate(geomorphology_id=gridcode,
         geomorphology=if_else(gridcode==803,"reef flat","reef slope"))%>% 
  glimpse()

####################################
# join new tables to original pts table
glimpse(pts)

# train --
pts3<-pts%>%
  full_join(v1p)%>%# v6p,
  full_join(v2p)%>%
  full_join(v3p)%>%
  full_join(v4p)%>%
  full_join(v5p)%>%
  full_join(v6p)%>%
  glimpse()
str(pts3)

pts4<-data.frame(pts3)%>%
  glimpse()

# test --
pts3_te<-pts_te%>%
  full_join(v1p_te)%>%# v6p,
  full_join(v2p_te)%>%
  full_join(v3p_te)%>%
  full_join(v4p_te)%>%
  full_join(v5p_te)%>%
  full_join(v6p_te)%>%
  glimpse()
str(pts3_te)

pts4_te<-data.frame(pts3_te)%>%
  glimpse()

# save ---------------------------------
# gpkg
st_write(pts3,"./results/train.gpkg", layer="9_pts_IndpVarOther", delete_layer=T)
st_write(pts3_te,"./results/test.gpkg", layer="9_pts_IndpVarOther", delete_layer=T)

# .csv
write_csv(pts4,"./results_train/9_pts_IndpVarOther_pts.csv")
write_csv(pts4_te,"./results_test/9_pts_IndpVarOther_pts.csv")



