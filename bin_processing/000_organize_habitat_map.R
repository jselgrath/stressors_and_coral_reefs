# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: clean names for habitats for all DB - note this map is from remote sensing and updated with some LEK information for missing areas where it is avaliable 
# map is reclassified based on a confusion matrix made by comparing the original habitat map to LTM surveys by Project Seahorse Foundation/ ZSL Philippines in May 2025 

#########################
library(tidyverse)
library(dplyr)
library(sf)
library(nngeo)
library(units)

#########################

remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# --read in shapefile of focal area  ----------------------------------
d0<-st_read("./gis2/focal_area/focal_area_sm.shp")%>%
  glimpse()
# plot(d0)

# make sure CRS is projected (so areas are meaningful, not lat/long & important for 250 m distance)
st_crs(d0) # WGS 84 / UTM zone 51N 

# --read in shapefile of habitats for entire area  ----------------------------------
d1<-st_read("./gis2/habitat/db_full_area/all_full_area/habitat_full_area_rs_lek_reclass_20250615_union_with_fa2.shp")%>%
  select(hab_reclass=Hab_Paper,hab_orig=Hab1,hab_orig_smpl=Hab2,geomorphic=Geomorphic, location=Location,map=Map,reclass)%>%
  arrange(hab_orig)%>%
    glimpse()
# plot(d1[1])

# - clean habitat categories ----------------------
# make habitat classes match
unique(d1$hab_reclass)
d1$hab_reclass[d1$hab_reclass=="Deep"]<-"DeepWater"
d1$hab_reclass[d1$hab_reclass=="NA"]<-"Cloud" # these areas are not classified
d1$hab_reclass[d1$hab_reclass=="Coral Reef Matrix"]<-"Coral" 


unique(d1$hab_orig_smpl)
d1$hab_orig_smpl[d1$hab_orig_smpl=="Deep"]<-"DeepWater"
d1$hab_orig_smpl[d1$geomorphic=="Deep"]<-"DeepWater"
d1$hab_orig_smpl[d1$hab_orig_smpl=="NA"]<-"Cloud" # to have fewer classes - these areas are not classified
d1$hab_orig_smpl[d1$hab_orig_smpl=="Coral Reef Matrix"]<-"Coral" # to have fewer classes - these areas are not classified

# confirm reclassified habitat
unique(d1$hab_reclass)
unique(d1$hab_orig_smpl)
unique(d1$hab_orig)
unique(d1$geomorphic)
d1$hab_reclass0<-d1$hab_reclass # backup


# -- reclassify based on confusion matrix analysis 2025 from PSF LTM data -------------------
d1$hab_reclass[d1$hab_orig_smpl=="Coral/Algae" & d1$geomorphic=="Reef Flat"&d1$location=="TerrestrialIsland"]<-"Rubble"

d1$hab_reclass[d1$hab_orig=="Sand Terrestrial" & d1$geomorphic=="Reef Slope"]<-"Coral"
d1$hab_reclass[d1$hab_orig=="Rubbe Coral"& d1$geomorphic=="Reef Slope"& d1$location=="TerrestrialIsland"]<-"Coral" 
d1$hab_reclass[d1$hab_orig=="CoralReefMatrix" & d1$geomorphic=="Reef Slope"]<-"Coral"  # based on visual


d1$hab_reclass[d1$hab_orig=="SeagrassLight"]<-"Sand" 



unique(d1$hab_reclass)

# --projections ----------------------------------
# make sure CRS is projected (so areas are meaningful, not lat/long & important for 250 m distance)
st_crs(d1) # WGS 84 / UTM zone 51N 

# Lambert Azimuthal Equal Area
# Minimal area distortion when you center it on your study region (here centered on Visayas)
# centered near the middle of the Visayas.
crs_laea <- "+proj=laea +lat_0=10.5 +lon_0=123.5 +datum=WGS84 +units=m +no_defs"

# target projection (WGS84 / UTM zone 51N) - for saving at end ---
crs_utm51 <- 32651   # EPSG code

# change projection to equal area
d1_proj <- st_transform(d1, crs_laea)


# get rid of merge with focal area which splits patches ---------------
# Merge adjacent polygons of the same habitat

d1_merged<-d1_proj%>%
  group_by(hab_reclass) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("POLYGON")%>%   # ensure multipart polygons become separate polygons
  mutate(patch_id = row_number())%>% # -- Assign unique IDs to each patch
  st_transform(crs_utm51)%>% # transform back to WGS84/ Zone 51N
  ungroup()%>%
  glimpse()
d1_merged

# plot(d1_merged[1])


# -------------------------------------
# clip habitat to focal  area (intersection)

d2<- st_intersection(d1_merged, d0)%>%
  select(-FID)
d2$hab_reclass[d2$hab_reclass=="Coral Reef Matrix"]<-"Coral"

# plot(st_geometry(d1_merged), border = "black")
# plot(st_geometry(d2), col = "lightblue", add = TRUE)





# -save ----------------------------

st_write(d1,"./results/habitat.gpkg",layer="habitat_all_db_reclass", delete_layer = TRUE) # can use to back calculate other features (e.g., map)
st_write(d1_merged,"./results/habitat.gpkg",layer="habitat_all_db_reclass2", delete_layer = TRUE)

st_write(d2,"./results/habitat.gpkg",layer="habitat_all_fa_reclass2", delete_layer = TRUE)

# save as shapefile
st_write(d1,"./gis2/habitat/db_full_area/all_full_area/habitat_updated2025.shp",delete_layer = TRUE) 

# create a readmefile for shapefile -----------------
readme_text <- c("readme",
                 "Jennifer Selgrath",
                 "",
  "The file /gis2/habitat/db_full_area/all_full_area/habitat_updated2025.shp is based on ./gis2/habitat/db_full_area/all_full_area/habitat_full_area_rs_lek_reclass_20250615_union_with_fa2.shp. Three habitats in the habitat_updated2025.shp file have been reclassified based on confusion matrix analysis 2025 from PSF LTM data.  See code in 000_organize_habitat_map.R")

# Write to a text file
writeLines(readme_text, con = "./gis2/habitat/db_full_area/all_full_area/readme.txt")


