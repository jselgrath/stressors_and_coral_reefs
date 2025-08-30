
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

pt2<-st_read("./results_test/hab_Resil_Pts_RS.gpkg")%>%
  select(PtID2,Geomorphic,geom)%>%
  glimpse()

# buffer files - seagrass and mangroves

d1<-st_read("./gis/0most_shp/seagrass_buffer/seagrass_buf_FA2_20160525.shp")%>%
  glimpse()
plot(d1)

d2<-st_read("./gis/0most_shp/mangrove_buffer/mangrove_buf_FA2_20160525.shp")%>%
  glimpse()
plot(d2)


# buffer files - train

d3<-pt%>%
  st_join(d1)%>%
  mutate(distance_sg=distance)%>%
  select(-distance,-Id,-Id_dSg)%>%
  st_join(d2)%>%
  mutate(distance_mg=distance)%>%
  select(-distance,-Id,-Id_dMg)%>%
  glimpse()

plot(d3)


# join files - test ---------------------

d3_te<-pt2%>%
  st_join(d1)%>%
  mutate(distance_sg=distance)%>%
  select(-distance,-Id,-Id_dSg)%>%
  st_join(d2)%>%
  mutate(distance_mg=distance)%>%
  select(-distance,-Id,-Id_dMg)%>%
  glimpse()

plot(d3_te)


# convert to dataframe
d4<-d3%>%
  data.frame()%>%
  glimpse()

d4_te<-d3_te%>%
  data.frame()%>%
  glimpse()

# save
st_write(d3,"./results_train/13_distance_sg_mg_buf.gpkg",append=FALSE)
st_write(d3_te,"./results_test/13_distance_sg_mg_buf.gpkg",append=FALSE)

write_csv(d4,"./results_train/13_distance_sg_mg_buf.csv")
write_csv(d4_te,"./results_test/13_distance_sg_mg_buf.csv")
