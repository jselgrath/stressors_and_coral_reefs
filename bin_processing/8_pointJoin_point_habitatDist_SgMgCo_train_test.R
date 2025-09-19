# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: calculate distances between random points and habitat edges 
# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(tidyverse)
library(sf)
library(lwgeom)  # for st_distance to polygon boundaries

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")

# -------------------------------------------
# OBJECTIVE 1: Load and organize files 
# -------------------------------------------

# -------------------------------------------
#read in random points - # update this name if change sampling number and distance

# train --------
# pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train_extra_pts100")%>% 
#   glimpse()
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # 
  glimpse()

# test ------
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()



#----------------------------------------------
# load habitat patches of habitats
d1<-st_read("./results/habitat.gpkg",layer="habitat_all_db_reclass2" )%>% 
  glimpse()

# plot(d1[1])
unique(d1$hab_reclass)

# -------------------------------------------------

# create function ---------------------------
calc_point_edge_distances <- function(habitat, points,
                                      point_id_col = "point_id",
                                      hab_col = "hab_reclass",
                                      target_habs = c("Coral", "Rubble"),
                                      edge_habs = c("Coral", "Seagrass", "Mangrove")) {
  # Ensure same CRS
  if (st_crs(habitat) != st_crs(points)) {
    points <- st_transform(points, st_crs(habitat))
  }
  
  # 1. Filter points within coral or rubble
  # points_in <- st_join(points, d1[, c(hab_col)], join = st_intersects) %>%
  #   filter(.data[[hab_col]] %in% target_habs)
  # 
  # if (nrow(points_in) == 0) {
  #   stop("No points found inside coral or rubble habitats.")
  # }
  
  # 2. Extract polygon boundaries (edges) for habitats of interest
  edge_list <- lapply(edge_habs, function(h) {
    d1 %>%
      filter(.data[[hab_col]] == h) %>%
      st_union() %>%                # merge patches
      st_boundary() %>%             # get edges
      st_cast("MULTILINESTRING")    # ensure line geometry
  })
  names(edge_list) <- paste0("point_dist_", edge_habs)
  
  # 3. Calculate nearest distance from each point to each habitat edge
  dist_results <- lapply(edge_list, function(edge_geom) {
    st_distance(points, edge_geom) %>%
      apply(1, min) %>% as.numeric()
  })
  
  # 4. Bind results
  points_out <- points %>%
    st_drop_geometry() %>%
    cbind(dist_results) %>%
    left_join(points[, c(point_id_col, "geom")], by = point_id_col) %>%
    # mutate(point_dist_Coral=round(dist_Coral,2))%>%
    st_as_sf()
  
  return(points_out)
}

#train points
d2 <- calc_point_edge_distances(habitat=d1, points=pts)
d2

# test points
d3 <- calc_point_edge_distances(habitat=d1, points=pts_te)
d3

# to tibble
d2t<-tibble(d2)%>%
  dplyr::select(-geom)
d3t<-tibble(d3)%>%
  dplyr::select(-geom)

# save --------------------------
st_write(d2,"./results/train.gpkg", layer="8_pts_landscape_edge_dist", delete_layer = TRUE)
st_write(d3,"./results/test.gpkg",  layer="8_pts_landscape_edge_dist", delete_layer = TRUE)

write_csv(d2t,"./results_train/8_pts_landscape_edge_dist.csv")
write_csv(d3t,"./results_test/8_pts_landscape_edge_dist.csv")
