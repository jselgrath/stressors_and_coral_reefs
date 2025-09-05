# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: create polygon of focal area that matches fishing rasters
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
# EcoZone2
d1<-st_read("./gis2/ecological_zones/EcoZones2_DB.shp")%>% glimpse()
plot(d1)

# all fishing effort
files = list.files("./gis2/fishing/effort",pattern='\\.tif$', full.names = TRUE)%>%
  glimpse()
files2<-files[1] # manually check - should be all_YEAR 1960-2010
files2

r <- rast(files2)

ext_poly <- as.polygons(ext(r), crs = crs(r))
ext_poly_sf <- st_as_sf(ext_poly)

# make sure CRS is projected (so areas are meaningful, not lat/long & important for 250 m distance)
st_crs(ext_poly_sf) # WGS 84 / UTM zone 51N 

# check
plot(r)
plot(ext_poly_sf, add = TRUE, border = "red", lwd = 2)

# clip by ecol zones
d2<-st_intersection(ext_poly_sf,d1)%>%
  summarize()%>%
  glimpse()
plot(d2)

st_write(d2,"./gis2/focal_area/focal_area_sm.shp",delete_layer = TRUE) 
st_write(d2,"./results/basic_files.gpkg",layer="focal_area_sm", delete_layer = TRUE)