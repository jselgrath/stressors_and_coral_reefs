# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: extract coral and rubble (resilience) variable to points
# Note: POINTS IN 19x22 Focal AREA (FA) and in coral and rubble habitat areas ONLY.
# set distance and number of points in code below

# -------------------------------------------
library(tidyverse)
library(sf)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")


# -------------------------------------------
#read in shapefile of habitat/resilience data
habitat<-st_read("./results/habitat.gpkg",layer="habitat_all_fa_reclass2")%>%
  mutate(resilience_id=if_else(hab_reclass=="Coral",1,0))%>%
  glimpse() 

#read in gpkg layer of random points - set with R code 

# we haphazardly added points in undersampled habitats in ArcPro to follow the rule from Plant (2020) that random effects needs >=10 samples.
# pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train_extra_pts100")%>% # update this name if change sampling number and distance
#   glimpse()
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # 
  glimpse()

pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()



# extract hab polygon data to points
pts2 <- pts%>%
  st_join(habitat)%>% 
  glimpse()

pts2_te <- pts_te%>%
  st_join(habitat)%>% 
  glimpse()

# save 
st_write(pts2,"./results/train.gpkg", layer="1_pts_habitat_tr", delete_layer=T) # hab_Resil_Pts_RS
st_write(pts2_te,"./results/test.gpkg", layer="1_pts_habitat_te", delete_layer=T)

