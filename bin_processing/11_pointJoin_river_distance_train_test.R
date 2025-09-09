# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Normalize fishing effort estimates from all years and extract to points
# -------------------------------------------
library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------
#read in random points
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # update this name if change sampling number and distance
  glimpse()
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()

#read in file of coral/rubble area only
CA<-st_read("./results/habitat.gpkg",layer="co_ru_fa_reclass2")%>%
  glimpse()
# plot(CA)


######################################
# distance to river mouths

s <- rast("./gis2/river_distance/DistRiver.tif")
names(s)
plot(s)

# Mask rasters so only evaluating fishing effort in coral areas
s2<-mask(s, CA) 
str(s2)

# plot(s[[1]])
plot(s2)

# Calculate max distance
mx<-tibble()
  val<-minmax(s2[[1]])
  val2<-cbind(val[1],val[2])
  file="river_distance"
  max<-cbind(val2,file)
  mx<-rbind(mx,max)
names(mx)<-c("river_dist_min","river_dist_max","file")
mx$river_dist_max<-as.numeric(mx$river_dist_max)
mx$river_dist_min<-as.numeric(mx$river_dist_min)
glimpse(mx)


###########################################
#Extract Raster Variables to Point Data
d1<-stars::st_as_stars(s2)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  dplyr::select(river_distance=DistRiver,point_id)%>%
  glimpse()

plot(d1[[1]])

# test
d1_te<-stars::st_as_stars(s2)%>%
  stars::st_extract(pts_te)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts_te)%>% # join to point data
  tibble()%>%
  dplyr::select(river_distance=DistRiver,point_id)%>%
  glimpse()

##########################
# Calculate normalized variables (value at a site/max value), See Maynard et al 2015
# NOTE: I Confirmed and NAs are in places with no fishing info...

# TRAIN -----------------------------------------------------------------
d2<-d1
d2$river_distance.nrm=d2$river_distance/mx$river_dist_max
glimpse(d2)
qplot(d2$river_distance.nrm)

# TEST -----------------------------------------------------------------
d2_te<-d1_te
d2_te$river_distance.nrm=d2_te$river_distance/mx$river_dist_max
glimpse(d2_te)
qplot(d2_te$river_distance.nrm)



########################
#export table
write_csv(d2,   "./results_train/11_pts_river_distance_1normalized.csv")
write_csv(d2_te,"./results_test/11_pts_river_distance_1normalized.csv")




