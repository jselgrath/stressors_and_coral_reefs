# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# GOAL: Load raster with population risk estimates from study area and join to samble points

# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# old: #PopRsk2 is area with land masked out
# PopRskDecay has land, but ignore...

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")



# -------------------------------------------
# OBJECTIVE 1: Load and organize files 
# -------------------------------------------

# -------------------------------------------
# Load point files with habitat & mpa data
#read in gpkg layer of random points 
# train --
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # update this name if change sampling number and distance
  glimpse()
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()

# min and max values for rasters
mx_orig<-read_csv("./doc/population_risk_orig_range.csv")  # min and max values
mx_inhab<-read_csv("./doc/population_risk_inhab_range.csv")
mx_pop<-read_csv("./doc/population_risk_pop_range.csv")


# -------------------------------------------
# Stack and organize the rasters of poplation risk

# list of files with 'reef' in the name - clipped to reef area
files = list.files(path="./gis2/population_risk",pattern='*reef.*\\.tif$', full.names = TRUE)%>%
  glimpse()

s <- rast(files)
names(s)
nl <- 1
plot(s)
str(s)


#---------------------------------------
#Extract Raster Variables to Point Data
#---------------------------------------
# train --
d1<-stars::st_as_stars(s)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  data.frame()%>%
  dplyr::select(-geom)%>%
  
  # Calculate normalized variables (value at a site/max value), See Maynard et al 2015
  mutate(pop_risk_dens_inhab.nrm=round(pop_risk_dens_inhab/mx_inhab$max,4),
         pop_risk_dens_orig.nrm=round(pop_risk_dens_orig/mx_orig$max,4),
         pop_risk_pop.nrm=round(pop_risk_pop/mx_pop$max,4))%>%
  dplyr::select(point_id,pop_risk_dens_inhab:pop_risk_pop.nrm)%>% # arrange columns
  glimpse()


# test --
d1_te<-stars::st_as_stars(s)%>%
  stars::st_extract(pts_te)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts_te)%>% # join to point data
  data.frame()%>%
  dplyr::select(-geom)%>%
  
  # Calculate normalized variables (value at a site/max value), See Maynard et al 2015
  mutate(pop_risk_dens_inhab.nrm=round(pop_risk_dens_inhab/mx_inhab$max,4),
         pop_risk_dens_orig.nrm=round(pop_risk_dens_orig/mx_orig$max,4),
         pop_risk_pop.nrm=round(pop_risk_pop/mx_pop$max,4))%>%
  dplyr::select(point_id,pop_risk_dens_inhab:pop_risk_pop.nrm)%>% # arrange columns
  glimpse()



########################
#export table
write_csv(d1,"./results_train/10_pts_PopRsk_Norm.csv")
write_csv(d1_te,"./results_test/10_pts_PopRsk_Norm.csv")




