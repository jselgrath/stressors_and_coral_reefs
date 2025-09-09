# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: calculate landscape distance metrics for central db area (bigger than focus area of study to address edge effects)
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


d1<-st_read("./results/habitat.gpkg","habitat_all_db_landscape1")%>%
  glimpse()
plot(d1)
d1

# d1<-d1%>%
#   mutate(shape_index=round(as.numeric(shape_index),2),
#          patch_area_m2=round(as.numeric(patch_area_m2),2),
#          patch_perimeter_m=round(as.numeric(patch_perimeter_m),2),
#          enn_m=round(as.numeric(enn_m),2))%>%
#   glimpse()
d1

# --projections ----------------------------------

# make sure CRS is projected (so areas are meaningful, not lat/long & important for 250 m distance)
# st_crs(d1) # WGS 84 / UTM zone 51N 

# Lambert Azimuthal Equal Area
# Minimal area distortion when you center it on your study region (here centered on Visayas)
# centered near the middle of the Visayas.
crs_laea <- "+proj=laea +lat_0=10.5 +lon_0=123.5 +datum=WGS84 +units=m +no_defs"

# target projection (WGS84 / UTM zone 51N) - for saving at end ---
crs_utm51 <- 32651   # EPSG code


# ----------------------------------------------------------------------------------
# -- calculate landscape variables: distance from coral and rubble patches to seagrass and mangrove patches (edge to edge)

# function ---------
calc_target_distances <- function(sf_obj, hab_col = "hab_reclass",
                                  source_vals = c("Coral", "Rubble"),
                                  target1 = "Seagrass",
                                  target2 = "Mangrove",
                                  target_crs) {
  # check
  if (!inherits(sf_obj, "sf")) stop("Input must be an sf object")
  if (!hab_col %in% names(sf_obj)) stop("Column not found in sf object")
  
  # project to a metric CRS
  sf_proj <- st_transform(sf_obj, target_crs)
  
  # dissolve adjacent polygons by habitat type
  # merged <- sf_proj %>%
  #   group_by(.data[[hab_col]]) %>%
  #   summarise(geometry = st_union(geometry), .groups = "drop") %>%
  #   st_cast("POLYGON") %>%
  #   mutate(patch_id = row_number())
  
  # identify sources and targets
  sources <- sf_proj %>% filter(.data[[hab_col]] %in% source_vals)
  tgt1    <- sf_proj %>% filter(.data[[hab_col]] == target1)
  tgt2    <- sf_proj %>% filter(.data[[hab_col]] == target2)
  
  # helper function to compute nearest distance
  nearest_dist <- function(src, tgt) {
    if (nrow(tgt) == 0) return(rep(NA, nrow(src)))
    nn <- st_nn(src, tgt, k = 1, progress = FALSE)
    sapply(1:length(nn), function(i) {
      j <- nn[[i]][1]
      as.numeric(st_distance(src[i, ], tgt[j, ]))
    })
  }
  
  # compute distances
  sources$patch_dist_to_seagrass_m <- round(set_units(nearest_dist(sources, tgt1), "m"),2)
  sources$patch_dist_to_mangrove_m <- round(set_units(nearest_dist(sources, tgt2), "m"),2)
  
  return(sources)
}



# run function ------------------------------------
# d3 is your merged habitat sf with hab_reclass

d2 <- calc_target_distances(d1,
                            target_crs  = crs_laea,
                            hab_col     = "hab_reclass",
                            source_vals = c("Coral", "Rubble"), # origion habitats
                            target1     = "Seagrass", # target habitat 1
                            target2     = "Mangrove") # target habitat 2

# check results
head(d2)
plot(d2)


# coordinate reference system
d3<-d2%>%
  st_transform(crs_utm51)#return to WGS
d3



# d4<-d3%>%
#   dplyr::select(hab_reclass:patch_perimeter_m,patch_dist_to_coral_m=enn_m, patch_dist_to_seagrass_m=dist_to_seagrass_m,patch_dist_to_mangrove_m=dist_to_mangrove_m)%>%
#   select(-shape_index)%>%
#   glimpse()
  



# Save to shapefile
st_write(d4, "./gis2/landscape/habitat_all_db_landscape2.shp", delete_layer = TRUE)
st_write(d4,"./results/habitat.gpkg",layer="habitat_all_db_landscape2", delete_layer = TRUE)
