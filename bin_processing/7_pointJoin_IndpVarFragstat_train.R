# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# GOAL: Join random points to fragstat data (indp var) and other metrics
###########################################


# ----------------------------------------------
library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)
# -------------------------------------------



####################################################################
# OBJECTIVE 1: Load and organize files 
####################################################################
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")

############################
# TASK 1.1 Load point files
pts0<-st_read("./results_train/hab_Resil_Pts_RS.gpkg")%>%
  select(PtID2,Id_resil,CoRuLngth,CoRuArea,geom)%>%
  mutate(CoRuEdg2Area=CoRuLngth/CoRuArea)%>% # edge/area
  glimpse()
plot(pts0)


#random point with dist to mg and sg data
# these were generated in 7_1point_create_train_test.R and then in Arcpro I used the NEAR tool to calculate distances to seagrass (sg) and mangroves (mg)
# pts<-st_read("./gis/landscape/train/train_points_fragstats.shp")%>%
#   glimpse()
# plot(pts)

#read in shapefile of coral/rubble AREA - from original analysis
# CA<-st_read("./gis/coralrubble/CoralRubArea.shp")

# 2025 version
CA<-st_read("./gis/0_2025/habitat/co_ru_rs_lek_reclass_20250615_area.shp")
plot(CA)

# CA 2025: this is the habitat - need to make area file - from updated file 2025
CA2<-st_read("./gis/0_2025/habitat/co_ru_focal_area_rs_lek_reclass_20250615_dis.shp")
plot(CA2)

############################
# Task 1.2 load shapefile with indp data from fragstats

# fragstats 2025: patch metrics = co_ru_fragstat_patchid_20250615.shp
frag<-st_read("./gis/0_2025/landscape/co_ru_fragstat_patchid_20250615b.shp") # b has background removed (-99999) and patch attributes saved
plot(frag)

##################################
# Task 1.3 load shapefile with landscape variables

# in GIS run model "Ch4_DistToHabitat/CoRuFrag_ToPts.tbx"
# this joins testing points (50 away) with CoRu_Frag_20160927.shp> CoRu_Frag_Pts.shp


frag<-st_read("./gis/landscape/CoRu_Frag_20160927.shp")%>%
  select(SHAPE=SHAPE_1,TYPE:geometry)%>%
  glimpse()
# plot(frag) #


###########################################
# Extract  Variables to Point Data
# intersect points 
pts2 <- pts0%>%
  st_join(frag)%>%
  glimpse()
plot(pts2)


# join to CoRuArea etc data 
# pts3<-pts2%>%
#   st_join(pts0)%>%
#   glimpse()

# check matching - all is good so removed in pts2 above since its a spatial join
# pts3%>%
#   filter(PtID2.x!=PtID2.y)

# convert to data frame, keeping your data
pts4<- as.data.frame(pts2)%>%
  glimpse()


########################333
#export table
write_csv(pts2,"./results_train/7_pts_fragstats.csv")

