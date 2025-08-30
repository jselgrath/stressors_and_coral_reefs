# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: extract resilience variable to points
# Note: POINTS IN 19x22 Focal AREA (FA) ONLY.
# Random (training) points 100m apart
# Testing points 100m apart and >25m from training points

#########################
library(tidyverse)
library(sf)

#########################
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

########### 
# import point data from GIS - 5003 random points, 100m apart min, in Coral Area only
#https://www.nceas.ucsb.edu/scicomp/usecases/ReadWriteESRIShapeFiles

#read in shapefile of random points
pt<-st_read("./gis/random_points2/TestingPts_CoRu_19x22_NoOlango2_25m_buf.shp")%>%
  # dplyr::select(PtID=CID)%>%
  arrange(geometry)%>%
  glimpse()
pt$PtID2<-as.numeric(row.names(pt))
glimpse(pt)

plot(pt)

print(st_crs(pt)) # check projection


#read in shapefile of habitat and resilience data
# this has most of the detailed info
# folder may also be "habitat_map_remote_sensing"
hab<-st_read("./gis/resilience/RS_FocalArea_Updated_20160627b.shp")%>%
  glimpse()

# plot(hab)


# read in simplified polygons for length/area
habS<-st_read("./gis/resilience/CoRu_Smplfd_20160628.shp")%>%
  glimpse() 

# extract hab polygon data to points
pt2 <- pt%>%
  st_join(hab)%>%
  dplyr::select(-Shape_Leng,-Shape_Area,-Id_Hab1,-Id.x,-Id.y,-Id_geom)%>%
  glimpse()


# extract simplified habtiat polygon data to points
# rename variables 
# extract patch characteristics of coral and rubble habitat from shapefile data
pt12 <- pt%>%
  st_join(habS)%>%
  dplyr::select(CoRuLngth=Shape_Leng,CoRuArea=Shape_Area)%>%
  glimpse()

#combine pt data with new names (?and values?) for length
# this is the way that it was originally done, but note that some areas are much larger in the simplified file
pt3<-pt2%>%
  st_join(pt12)%>%
  mutate(Nm_Resil3=as.character(Nm_Resil))%>%
  glimpse()

plot(pt3)

# create a mixed habtiat class
unique(pt3$Hab1)
pt3$Nm_Resil3[pt3$Hab1=="Sand Rubble Coral"]<-"Mixed" 
pt3$Nm_Resil3[pt3$Hab1=="Sand Rubble Seagrass"]<-"Mixed" 
levels(as.factor(as.character(pt3$Nm_Resil3)))
str(pt3$Nm_Resil) 

# write out a new shapefile (including .prj component)
st_write(pt3,"./results_test/hab_Resil_Pts_RS.gpkg", delete_layer=T)


