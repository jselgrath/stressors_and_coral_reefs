# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: calculate habitat metrics for central db area (bigger than focus area of study to address edge effects)
# habitat map is reclassified based on a confusion matrix made by comparing the original habitat map to LTM surveys by Project Seahorse Foundation/ ZSL Philippines in May 2025 

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

d1<-st_read("./results/habitat.gpkg","habitat_all_db_reclass2")%>%
  glimpse()


unique(d1$hab_reclass)
plot(d1)

# --projections ----------------------------------

# make sure CRS is projected (so areas are meaningful, not lat/long & important for 250 m distance)
# st_crs(d1) # WGS 84 / UTM zone 51N 

# Lambert Azimuthal Equal Area
# Minimal area distortion when you center it on your study region (here centered on Visayas)
# centered near the middle of the Visayas.
crs_laea <- "+proj=laea +lat_0=10.5 +lon_0=123.5 +datum=WGS84 +units=m +no_defs"

# target projection (WGS84 / UTM zone 51N) - for saving at end ---
crs_utm51 <- 32651   # EPSG code

# change projection to equal area
# d1_proj <- st_transform(d1, crs_laea)

# ----------------------------------
# # chatgpt command to update code from ArcPro to R: 
# in R for a sf shapefile 'd1' with a variable 'hab_reclass' I want to merge any adjacent polygons of the same hab_reclass value, assign an id to each polygon, and then calculate: patch area, patch perimeter edge length, distance between the edge of a patch and the edge of the nearest patch with the same habitat, and the shape index ((0.25*perimeter)/(squareroot of the patch area))



# calculate landscape metrics -----------------------------------  

# -- Calculate patch area and perimeter ----------------
# area in m², perimeter in m (make sure CRS is projected in meters, e.g. UTM or equal-area)
# d2 <- d1 %>%
#   mutate(
#     patch_area = st_area(geom),
#     patch_perimeter = st_length(st_cast(geom, "MULTILINESTRING")),
#     shape_index = (0.25 * st_length(st_cast(geom, "MULTILINESTRING"))) / sqrt(st_area(geom))
#   )
# d2

# -- Calculate patch area and perimeter & nearest-patch edge-to-edge distance (same habitat) ---------
# area in m², perimeter in m (make sure CRS is projected in meters, e.g. UTM or equal-area)
analyze_patches <- function(sf_obj, hab_col, target_crs) {
  
  # check
  if (!inherits(sf_obj, "sf")) stop("Input must be an sf object")
  if (!hab_col %in% names(sf_obj)) stop("Column not found in sf object")
  
  # project to target CRS (important: meters and equal area)
  sf_proj <- st_transform(sf_obj, target_crs) # change projection to equal area
  
  # dissolve polygons by habitat
  # merged <- sf_proj %>%
  #   group_by(.data[[hab_col]]) %>%
  #   summarise(geometry = st_union(geometry), .groups = "drop") %>%
  #   st_cast("POLYGON") %>%
  #   mutate(patch_id = row_number())
  
  # patch area and perimeter -
  merged2 <- sf_proj %>%
    mutate(
      patch_area      = st_area(geom),
      patch_perimeter = st_length(st_cast(geom, "MULTILINESTRING")),
      shape_index     = as.numeric((0.25 * patch_perimeter) / sqrt(patch_area)) # from fragstats
    )
  
  # nearest same-hab distance (near neighbor - from edge to edge)
  nearest_same_hab <- vector("numeric", nrow(merged2))
  for (hab in unique(merged2[[hab_col]])) {
    subset <- merged2 %>% filter(.data[[hab_col]] == hab)
    if (nrow(subset) < 2) {
      nearest_same_hab[merged2[[hab_col]] == hab] <- NA
    } else {
      nn <- st_nn(subset, subset, k = 2, progress = FALSE)
      dists <- sapply(1:length(nn), function(i) {
        j <- nn[[i]][2]
        as.numeric(st_distance(subset[i, ], subset[j, ]))
      })
      nearest_same_hab[merged2[[hab_col]] == hab] <- dists
    }
  }
  
  merged2$nearest_same_hab_dist <- set_units(nearest_same_hab, "m")
  
  return(merged2)
}


# -- run area and near neighbor function ----------------------
d3 <- analyze_patches(d1, hab_col = "hab_reclass", target_crs = crs_laea)%>%
  st_transform(crs_utm51)%>% # return to WGS
  glimpse()


# update with unit in column name and round 
d3a<- d3%>%
  mutate(patch_shape_index=round(as.numeric(shape_index),2),
         patch_area_m2=round(as.numeric(patch_area),2),
         patch_perimeter_m=round(as.numeric(patch_perimeter),2),
         patch_dist_to_coral_m=round(as.numeric(nearest_same_hab_dist),2))%>% # euclidian nearest neighbor
  select(-patch_area,-patch_perimeter,-nearest_same_hab_dist,-shape_index) %>%
  glimpse()

# Inspect results
head(d3)

# save as geotiff & .shp  ---------------------------------
st_write(d3a,"./results/habitat.gpkg",layer="habitat_all_db_landscape1", delete_layer = TRUE)

d4<-d3%>%
  select(-patch_area,-patch_perimeter,-nearest_same_hab_dist)
st_write(d4,"./gis2/landscape/habitat_all_db_landscape1.shp", delete_layer = TRUE)
