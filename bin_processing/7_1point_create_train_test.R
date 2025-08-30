# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: extract points for landscape variables
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

# TESTING --------------------------------
#read in shapefile of random points

# in PhD: TestingPts_CoRu_19x22_NoOlango_50away.shp
# 2024 version with ~ 1k points: TestingPts_CoRu_19x22_NoOlango2_25m_buf.shp

pt<-st_read("./gis/random_points2/TestingPts_CoRu_19x22_NoOlango2_25m_buf.shp")%>%
  # dplyr::select(PtID=CID)%>%
  arrange(geometry)%>%
  # select(-PtID)%>%
  glimpse()
pt$PtID2<-as.numeric(row.names(pt))
glimpse(pt)
plot(pt)


st_write(pt,"./gis/landscape/test/test_points_fragstats.shp") # 2 is 50m away, withoug 2 = 25m


# TRAINING --------------------------------
#read in shapefile of random points
pt2<-st_read("./gis/random_points2/RanPts_RSonly_19x22_NoOlango2.shp")%>%
  # dplyr::select(PtID=CID)%>%
  arrange(geometry)%>%
  select(geometry)%>%
  glimpse()
pt2$PtID2<-as.numeric(row.names(pt2))
glimpse(pt2)
plot(pt2)

st_write(pt2,"./gis/landscape/train/train_points_fragstats.shp")
