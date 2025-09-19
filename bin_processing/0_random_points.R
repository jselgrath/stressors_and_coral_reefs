# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: set test and train points in focal area, not at Olango or Cabul-an

# -------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(spatstat.geom)
library(spatstat.random)

# -------------------------------------------

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")


# load files ------------------------------------------- 
# --read in shapefile of focal area  ----------------------------------
d0<-st_read("./gis2/focal_area/focal_area_sm.shp")%>%
  glimpse()

#read in shapefile of habitat/resilience data & select coral & rubble only
habitat<-st_read("./results/habitat.gpkg",layer="co_ru_fa_reclass2")%>%
  glimpse() 

unique(habitat$hab_reclass)

# - MPA shapefile
mpa<-st_read("./gis2/mpa/MPA_FA_20160525_3.shp")%>%
  st_intersection(d0)%>% # clip to smaller area
  filter(STATUS !="Undesignated")%>% # remove because unclear status (very small area in center of Mahanay mangroves)
  dplyr::select(status=Id_binary,YearEst,Id_MEAT,Brgy)%>%
  glimpse()

# plot(mpa)

# intersect
d1<-st_intersection(habitat,mpa)%>%
  glimpse()
# plot(d1)

# chatcpt command: in R I have a sf polygon file called 'd1' and want to stratify by the column 'status' to create a shapefile of random points that are located inside the polygons and which provide a stratified random sample. The number of points in each strata should be relative to the area of the strata and the points should be a minimum of 250 m apart. before saving, return projection to WGS 84 / UTM zone 51N

# make sure CRS is projected (so areas are meaningful, not lat/long & important for 250 m distance)
st_crs(d1) # WGS 84 / UTM zone 51N 


# Example for Visayas (Lambert Azimuthal Equal Area)
# Minimal area distortion when you center it on your study region.
# centered near the middle of the Visayas.
crs_laea <- "+proj=laea +lat_0=10.5 +lon_0=123.5 +datum=WGS84 +units=m +no_defs"

# target projection (WGS84 / UTM zone 51N) - for saving at end ---
crs_utm51 <- 32651   # EPSG code


# change projection to equal area
d1_proj <- st_transform(d1, crs_laea)

# Compute area by status of MPAs and habitats to ensure samples in MPAs and enough samples in rubble in inner areas ---------------------
status_area <- d1_proj %>%
  group_by(status,hab_reclass) %>%
  summarise(total_area = sum(st_area(geom))) %>%
  ungroup()






# ----------------------------------------------------------------------------------
# Total number of points you want ------------------------
n_total <- 1500 # 1000 does not fit with 250m distance if do not include olango and cabul-an

# minimum distance for points ------------------------
min_dist = 100
# ----------------------------------------------------------------------------------





# Allocate points proportional to area -------------------
status_area2 <- status_area %>%
  mutate(n_points = round(as.numeric(total_area) / sum(as.numeric(total_area)) * n_total))
glimpse(status_area2)



# Helper: sample points with minimum distance using spatstat
sample_points_stratum <- function(polygon, n_points, min_dist) {
  if (n_points == 0) return(NULL)
  
  # Convert sf polygon -> spatstat window
  win <- as.owin(st_as_sf(polygon))
  
  # Simple inhibition (SSI) process ensures minimum distance
  pp <- rSSI(r = min_dist, n = n_points, win = win, giveup = 1000)
  
  # If not enough points could be placed, pp will have fewer
  if (pp$n < n_points) {
    warning(paste("Only placed", pp$n, "of", n_points, "points (try reducing min_dist)"))
  }
  
  # Convert to sf
  st_as_sf(as.data.frame(pp), coords = c("x", "y"), crs = st_crs(polygon))
}


# TRAIN -------------------
set.seed(333) # train

# Loop over strata
points_list_train <- list()

for (i in 1:nrow(status_area2)) {
  status_i <- status_area2$status[i]
  n_i <- status_area2$n_points[i]
  polys_i <- d1_proj %>% filter(status == status_i) %>% st_union()
  
  pts_i <- sample_points_stratum(polys_i, n_i, min_dist)
  if (!is.null(pts_i)) pts_i$status <- status_i
  points_list_train[[i]] <- pts_i
}




# TEST --------------------------
set.seed(789) # test

# Loop over strata
points_list_test <- list()

for (i in 1:nrow(status_area2)) {
  status_i <- status_area2$status[i]
  n_i <- status_area2$n_points[i]
  polys_i <- d1_proj %>% filter(status == status_i) %>% st_union()
  
  pts_i <- sample_points_stratum(polys_i, n_i, min_dist)
  if (!is.null(pts_i)) pts_i$status <- status_i
  points_list_test[[i]] <- pts_i
}

# Combine all points
points_sf_train <- do.call(rbind, points_list_train)
points_sf_train$point_id<-as.numeric(row.names(points_sf_train))
points_sf_train<-points_sf_train%>%
  dplyr::select(-status)

points_sf_test <- do.call(rbind, points_list_test)
points_sf_test$point_id<-as.numeric(row.names(points_sf_test))
points_sf_test<-points_sf_test%>%
  dplyr::select(-status)


# --- Reproject sampled points back to UTM Zone 51N ---
points_utm51_tr <- st_transform(points_sf_train, crs_utm51)
plot(points_utm51_tr)
glimpse(points_utm51_tr)

points_utm51_te <- st_transform(points_sf_test, crs_utm51)
plot(points_utm51_te)
glimpse(points_utm51_te)

# --- Save to shapefiles ---
st_write(points_utm51_tr, paste0("./results_train/stratified_random_points_",n_total,"pts_",min_dist,"m_train.shp"), delete_layer = TRUE)
st_write(points_utm51_te, paste0("./results_test/stratified_random_points_",n_total,"pts_",min_dist,"m_test.shp"), delete_layer = TRUE)

#geodatabases

st_write(points_utm51_tr, "./results/basic_files.gpkg",layer=paste0("stratified_random_points_",n_total,"pts_",min_dist,"m_train"), delete_layer = TRUE)
st_write(points_utm51_te, "./results/basic_files.gpkg",  layer=paste0("stratified_random_points_",n_total,"pts_",min_dist,"m_test"), delete_layer = TRUE)
