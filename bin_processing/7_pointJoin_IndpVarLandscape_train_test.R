# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# GOAL: Join random points to landscape patch data 
###########################################

# note: older versions from fragsats
# fragstats 2025: patch metrics = co_ru_fragstat_patchid_20250615.shp
# frag<-st_read("./gis/0_2025/landscape/co_ru_fragstat_patchid_20250615b.shp") # b has background removed (-99999) and patch attributes saved


# ----------------------------------------------
library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)
# -------------------------------------------



# -------------------------------------------
# OBJECTIVE 1: Load and organize files 
# -------------------------------------------

remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")

# ----------------------------------------------------------
#read in gpkg layer of random points 
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # update this name if change sampling number and distance
  glimpse()
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()


# ----------------------------------------------------------
# load landscape  metrics calculated in R in 00_pointJoin_InpdVarLandscape (and ..2)
frag<-st_read("./results/habitat.gpkg",layer="habitat_all_db_landscape2")%>%
  glimpse
# plot(frag)

max(frag$patch_dist_to_coral_m)


###########################################
# Extract  Variables to Point Data
# intersect points 

# train ------
pts2 <- pts%>%
  st_join(frag)%>%
  # dplyr::select(point_id:patch_perimeter_m,
  #        patch_shape_index,# =shape_index,
  #        patch_dist_to_coral_m=enn_m,
  #        patch_dist_to_seagrass_m=dist_to_seagrass_m,
  #        patch_dist_to_mangrove_m=dist_to_mangrove_m)%>%
  glimpse()
plot(pts2)

# test ------
pts2_te <- pts_te%>%
  st_join(frag)%>%
  # dplyr::select(point_id:patch_perimeter_m,
  #        patch_shape_index,# =shape_index,
  #        patch_dist_to_coral_m=enn_m,
  #        patch_dist_to_seagrass_m=dist_to_seagrass_m,
  #        patch_dist_to_mangrove_m=dist_to_mangrove_m)%>%
  glimpse()
plot(pts2_te)


# check - all match so removing these two columns from one data set above
# pts2%>%
#   filter(hab_reclass.x!=hab_reclass.y)
# 
# pts2%>%
#   filter(patch_id.x!=patch_id.y)

# convert to data frame, keeping data
pts4<- as.data.frame(pts2)%>%
  glimpse()

pts4_te<- as.data.frame(pts2_te)%>%
  glimpse()


########################333
#export table
write_csv(pts4,   "./results_train/7_pts_landscape_patch.csv")
write_csv(pts4_te,"./results_test/7_pts_landscape_patch.csv")

st_write(pts2,"./results/train.gpkg",layer="7_pts_landscape_patch", delete_layer = TRUE)
st_write(pts2_te,"./results/test.gpkg",layer="7_pts_landscape_patch", delete_layer = TRUE)