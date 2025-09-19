# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs 
# --------------------------------------------

# GOAL: clip files for figures
# -------------------------------------------
# Load packages

library(ggplot2)
library(terra)
library(tidyverse)
library(sf)
# ---------------------------------------

d1<-st_read("./results/basic_files.gpkg",layer="focal_area_sm") # focal area only
# d1_vect <- vect(d1)   # convert sf object to SpatVector
st_crs(d1)

# Ensure same CRS
# st_crs(d1)
# crs(r1)

# Reproject d1 if needed
# d1 <- st_transform(d1, crs(r1))

# population risk---------------------
p1<-rast("./gis2/population_risk/pop_risk_dens_inhab_fa.tif") # inhabited area); 
st_crs(p1)
p1_clip <- mask(crop(p1, vect(d1)), vect(d1)) # Crop to extent # Mask to polygon shape
writeRaster(p1_clip, "./results/figure2/clip_pop_risk_dens_inhab_fa.tif", overwrite=TRUE) 

# river distance
r1 <- rast("./gis2/river_distance/DistRiver.tif")
r1_clip <- mask(crop(r1, vect(d1)), vect(d1)) # Crop to extent # Mask to polygon shape
writeRaster(r1_clip, "./results/figure2/clip_dist_river.tif", overwrite=TRUE) 

# cumulative fishing
cf<-rast("./gis2/fishing/effort_fa_lag/lag_all_30.tif")
cf_clip <- mask(crop(cf, vect(d1)), vect(d1))
writeRaster(cf_clip, "./results/figure2/clip_fishing_lag30.tif", overwrite=TRUE) 

# blast fishing
cb<-rast("./gis2/fishing/effort_fa_cumulative/cumulative_blast_10.tif")
cb_clip <- mask(crop(cb, vect(d1)), vect(d1))
writeRaster(cb_clip, "./results/figure2/clip_fishing_blast10.tif", overwrite=TRUE) 

# co/ru patch compactness
pc<-st_read("./results/habitat.gpkg",layer="habitat_all_db_landscape1")%>%
  dplyr::filter(hab_reclass=="Coral"|hab_reclass=="Rubble")%>%
  glimpse()
pc_clip <- st_intersection(pc, d1)
st_write(pc_clip, "./results/figure2/clip_patch_compactness.shp", delete_layer = TRUE)

# seagrass
sg<-st_read("./results/habitat.gpkg",layer="sg_db_reclass2")%>%
  glimpse()
sg_clip <- st_intersection(sg, d1)
st_write(sg_clip, "./results/figure2/clip_sg.shp", delete_layer = TRUE)

# mangrove
sg<-st_read("./results/habitat.gpkg",layer="mg_db_reclass2")%>%
  glimpse()
mg_clip <- st_intersection(mg, d1)
st_write(mg_clip, "./results/figure2/clip_mg.shp", delete_layer = TRUE)


# distance2
dist<-st_read("./results/habitat.gpkg",layer="habitat_all_db_landscape2")%>%
  glimpse()
mg_clip <- st_intersection(mg, d1)
st_write(mg_clip, "./results/figure2/clip_mg.shp", delete_layer = TRUE)

plot(dist)

