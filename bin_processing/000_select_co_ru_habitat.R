# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs 
# --------------------------------------------

# GOAL: select coral and rubble areas only from reclassified habitat map

#########################
library(tidyverse)
library(dplyr)
library(sf)
library(spatstat.geom)
library(spatstat.random)

#########################
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

########### 

# -- FOCAL AREA ----------------------------------------------------------------
#habitat/resilience data & select coral & rubble only
habitat<-st_read("./results/habitat.gpkg",layer="habitat_all_fa_reclass2")%>%
  filter(hab_reclass=="Coral" | hab_reclass=="Rubble")%>%
  glimpse() 

unique(habitat$hab_reclass)
plot(habitat)

st_write(habitat,"./results/habitat.gpkg",layer="co_ru_fa_reclass2", delete_layer = TRUE)



#habitat data & select seagrass only
sg<-st_read("./results/habitat.gpkg",layer="habitat_all_fa_reclass2")%>%
  filter(hab_reclass=="Seagrass")%>%
  glimpse() 
unique(sg$hab_reclass)
plot(sg)

st_write(sg,"./results/habitat.gpkg",layer="sg_fa_reclass2", delete_layer = TRUE)


#habitat data & select mangrove only
mg<-st_read("./results/habitat.gpkg",layer="habitat_all_fa_reclass2")%>%
  filter(hab_reclass=="Mangrove")%>%
  glimpse() 
unique(mg$hab_reclass)
plot(mg)

st_write(mg,"./results/habitat.gpkg",layer="mg_fa_reclass2", delete_layer = TRUE)



# -- FULL AREA ----------------------------------------------------------------
#read in shapefile of habitat/resilience data & select coral & rubble only
habitat<-st_read("./results/habitat.gpkg",layer="habitat_all_db_reclass2")%>%
  filter(hab_reclass=="Coral" | hab_reclass=="Rubble")%>%
  glimpse() 

unique(habitat$hab_reclass)
plot(habitat)

st_write(habitat,"./results/habitat.gpkg",layer="co_ru_db_reclass2", delete_layer = TRUE)



#read in shapefile of habitat/resilience data & select seagrass only
sg<-st_read("./results/habitat.gpkg",layer="habitat_all_db_reclass2")%>%
  filter(hab_reclass=="Seagrass")%>%
  glimpse() 
unique(sg$hab_reclass)
plot(sg)

st_write(sg,"./results/habitat.gpkg",layer="sg_db_reclass2", delete_layer = TRUE)


#read in shapefile of habitat/resilience data & select mangrove only
mg<-st_read("./results/habitat.gpkg",layer="habitat_all_db_reclass2")%>%
  filter(hab_reclass=="Mangrove")%>%
  glimpse() 
unique(mg$hab_reclass)
plot(mg)

st_write(mg,"./results/habitat.gpkg",layer="mg_db_reclass2", delete_layer = TRUE)