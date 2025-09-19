# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Normalize fishing effort estimates from all years IN CORAL AND RUBBLE AREAS ONLY and extract to points
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
#read in random points - # update this name if change sampling number and distance

# train --------
# pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train_extra_pts100")%>% 
#   glimpse()
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # 
  glimpse()

# test ------
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()





######################################
# Stack and organize the rasters of fishing effort

# list of files for fishing effort 

# normalized ---------------------
files.n<-list.files("./gis2/fishing/effort_fa_normalized","*\\.tif$", full.names = TRUE, ignore.case = TRUE); files.n; s.n<-rast(files.n)
plot(s.n)

# cumulative
files.c<-list.files("./gis2/fishing/effort_fa_cumulative","*\\.tif$", full.names = TRUE, ignore.case = TRUE); files.c; s.c<-rast(files.c)
plot(s.c)

# lag
files.l<-list.files("./gis2/fishing/effort_fa_lag","*\\.tif$", full.names = TRUE, ignore.case = TRUE); files.l; s.l<-rast(files.l)
plot(s.l)


###########################################
#Extract Raster Variables to Point Data
# normalized ------------------------------
# train
d1.n<-stars::st_as_stars(s.n)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1[[1]])

# test
d1_te.n<-stars::st_as_stars(s.n)%>%
  stars::st_extract(pts_te)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts_te)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1_te[[1]])



# cumulative ------------------------------
# train
d1.c<-stars::st_as_stars(s.c)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1[[1]])

# test
d1_te.c<-stars::st_as_stars(s.c)%>%
  stars::st_extract(pts_te)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts_te)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1_te[[1]])


# lag ------------------------------
# train
d1.l<-stars::st_as_stars(s.l)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1[[1]])

# test
d1_te.l<-stars::st_as_stars(s.l)%>%
  stars::st_extract(pts_te)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts_te)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1_te[[1]])



########################
#export table
write_csv(d1.n,   "./results_train/2_pts_fishingeffort_normalized.csv")
write_csv(d1_te.n,"./results_test/2_pts_fishingeffort_normalized.csv")

write_csv(d1.c,   "./results_train/3_pts_fishingeffort_cumulative.csv")
write_csv(d1_te.c,"./results_test/3_pts_fishingeffort_cumulative.csv")

write_csv(d1.l,   "./results_train/4_pts_fishingeffort_lag.csv")
write_csv(d1_te.l,"./results_test/4_pts_fishingeffort_lag.csv")

