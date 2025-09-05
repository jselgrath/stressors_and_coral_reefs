# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: select coral and rubble areas only

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

#read in shapefile of habitat/resilience data & select coral & rubble only
habitat<-st_read("./results/habitat.gpkg",layer="habitat_all_fa_reclass2")%>%
  filter(hab_reclass=="Coral" | hab_reclass=="Rubble")%>%
  glimpse() 

unique(habitat$hab_reclass)
plot(habitat)

st_write(habitat,"./results/habitat.gpkg",layer="co_ru_fa_reclass2", delete_layer = TRUE)