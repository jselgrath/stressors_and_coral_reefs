# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Calculate population risk across landscape following McPhearson et al. 2008

# analysis is bigger than focal area to include influence of  villages up to 10km from focal area
# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)
library(gdistance)
library(dplyr)

# -------------------------------------------
# -------------------------------------------
# OBJECTIVE 1: Load and organize files 
# -------------------------------------------

remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")


#  projections ----------------------------------------------------------------

# equal area projection - for calculations
crs_laea <- "+proj=laea +lat_0=10.5 +lon_0=123.5 +datum=WGS84 +units=m +no_defs"

# target projection (WGS84 / UTM zone 51N) - for saving at end ---
crs_utm51 <- 32651   # EPSG code


# load files ------------------------------------

# land and water across large area with villages 10 km from focal area--------------------
islands<-st_read("./gis2/focal_area/focal_area_lg_land2.shp")%>%   # land is binary variable of land and water - bigger than focal area to include outside villages
  glimpse() # or fa_sm_land2.tif
plot(islands)

# focal area -----------
focal_area<-st_read("./gis2/focal_area/focal_area_sm_land2.shp")%>%
                      vect() # Convert to SpatVector
plot(focal_area)                      

# villages -----
# area orig calc from 
villages<-st_read("./gis2/population/population.gpkg", layer = "barangay_pop2010_area")%>%
  dplyr::select(barangay, pop_2010:geom)%>%
  glimpse()

# as an fyi - these were files used for calculating area ----------------'

# ORIGINAL: barangay shapefile original population area is from ---------
# villages_orig<-st_read("./gis2/population/BarangayPopulation2010p.shp")%>% # village area and more attributes
#   tibble()%>%
#   dplyr::select(barangay=Barangay)%>%
#   glimpse()

# INHABITED: barangay shapefile with updated area for cays.  # removes uninhabited mangrove only islands from area calculations
# villages_inhab<-st_read("./gis2/population/BarangayPopulation2010p_inhabited_2025.shp")%>% 
#   tibble()%>%
#   dplyr::select(barangay=Barangay)%>%
#   glimpse
#  - values from shapefiles - 
#   area_m2_orig/inhab  = st_area(geometry),                                    # area in m²
# area_km2_orig/inhab = round(as.numeric(area_m2_orig) / 1e6,2),              # area in km²
# pop_dens_km2_orig/inhab = round(pop_2010 / area_km2_orig,2)                 # population density (people/km²)



# save  - this is the combined file
# write_csv(villages,"./doc/population_stats_2025.csv")




# -- function --------------------------------------------------------
make_risk_surface <- function(villages, islands,
                              crs_in = crs_laea, # Lambert Azimuthal Equal Area # centered near the middle of the Visayas.
                              crs_out = crs_utm51,
                              pop_var = "pop_2010",
                              res       = 20,            # raster resolution (m)
                              decay     = "sqrt",         # "sqrt", "linear", "exp"
                              alpha     = 0.002,          # only for decay="exp"
                              snap_radius_init = 200,    # initial search radius (m)
                              snap_radius_max  = 20000,   # max search radius (m)
                              include_shore = TRUE        # add centroid->shore distance
) {
  # --- Reproject inputs ---
  villages <- st_transform(villages, crs_in)
  islands  <- st_transform(islands,crs_in)
  
  # # --- Areas & population density ---
  villages <- villages %>%
    mutate(
      pop_risk = .data[[pop_var]]
    )
  
  # --- Rasterize islands water mask (water=1, land=NA already encoded) ---
  # Build a template that covers islands extent -1 (passable) and land (1) → NA (barrier).
  template <- rast(islands, resolution = res, crs = crs_in)
  water_rast <- rasterize(vect(islands), template, field = "water")
  # Ensure water is 1 and land is NA (in case there are stray zeros)
  water_rast[water_rast != 1] <- NA
  
  # --- Build gdistance transition on water only ---
  # gdistance::transition(), convert the SpatRaster to a RasterLayer because gdistanve only works on raster layers
  cost_r <- raster::raster(water_rast)  # SpatRaster -> RasterLayer
  tr <- transition(cost_r, function(x) 1/mean(x), directions = 8)
  tr <- geoCorrection(tr, type = "c")
  
  # --- Helper: snap a point to nearest water cell within growing buffer to account for non-movement over land---
  snap_to_water <- function(pt_xy) {
    radius <- snap_radius_init
    snapped_xy <- NULL
    shore_dist <- NA_real_
    
    while (radius <= snap_radius_max && is.null(snapped_xy)) {
      
      # crop a neighborhood of water around the point
      buf <- st_buffer(st_as_sf(data.frame(x=pt_xy[1], y=pt_xy[2]), coords = c("x","y"), crs = crs_in), radius)
      wcrop <- crop(water_rast, vect(buf))
      
      if (!is.null(wcrop) && ncell(wcrop) > 0) {
        cells <- which(!is.na(values(wcrop)))
        if (length(cells) > 0) {
          xy <- terra::xyFromCell(wcrop, cells)
          # nearest water cell by Euclidean distance
          d2 <- (xy[,1] - pt_xy[1])^2 + (xy[,2] - pt_xy[2])^2
          k  <- which.min(d2)
          snapped_xy <- xy[k,]
          shore_dist <- sqrt(d2[k])
        }
      }
      radius <- radius * 2
    }
    list(xy = snapped_xy, shore = shore_dist)
  }
  
  # --- Prepare centroids (start points) ---
  v_centroids <- villages # do not need centroids because using points
  cent_xy <- st_coordinates(v_centroids)
  
  # --- Risk raster (initialized to 0 over water) ---
  risk_r <- water_rast
  values(risk_r) <- 0
  
  # --- Loop over villages ---
  for (i in seq_len(nrow(cent_xy))) {
    start <- snap_to_water(cent_xy[i, ])
    if (is.null(start$xy)) {
      warning(sprintf("Village %d: could not find nearby water within %.0f m; skipping.", i, snap_radius_max))
      next
    }
    
    # least-cost distance over water from snapped start
    d <- accCost(tr, matrix(start$xy, ncol = 2))
    d <- rast(d)  # back to SpatRaster
    
    dvals <- values(d)
    
    # optionally include on-land distance from centroid to shore entry
    if (include_shore && !is.na(start$shore)) {
      dvals <- dvals + start$shore
    }
    
    # avoid division by zero at the start cell
    dvals[dvals == 0] <- 0.5
    
    # decay contribution
    contrib <- switch(decay,
                      "sqrt"   = villages$pop_risk[i] / sqrt(dvals),
                      "linear" = villages$pop_risk[i] / dvals,
                      "exp"    = villages$pop_risk[i] * exp(-alpha * dvals),
                       stop('decay must be one of: "sqrt", "linear", "exp", "pop"')
    )
    
    # accumulate
    vals <- values(risk_r)
    vals <- vals + contrib
    values(risk_r) <- vals
  }
  
  # keep only water cells in final output
  risk_r <- mask(risk_r, water_rast, maskvalues = NA)
  
  return(risk_surface = risk_r)
  
}


# run functions -------------------------------------------------------------------------

# original density values with base shapefile -------------
d1 <- make_risk_surface(
  villages    = villages,
  islands     = islands,
  pop_var     = "pop_dens_km2_orig", # or "pop_dens_km2_orig"  or "pop_dens_km2_inhab" or "pop_2010"
  decay       = "sqrt",     # or "linear", "exp", 
  alpha       = 0.002,      # only for "exp"
  res         = 20,
  include_shore = TRUE
)
# plot(d1)
names(d1)<-"pop_risk_dens_orig"

d1p<-project(d1,"EPSG:32651") # reproject
d1c <- mask(crop(d1p, focal_area), focal_area) # clip to focal area
plot(d1c)

# updated density values without uninhabited areas ------------
d2 <- make_risk_surface(
  villages    = villages,
  islands     = islands,
  pop_var     = "pop_dens_km2_inhab", # or "pop_dens_km2_orig"  or "pop_dens_km2_inhab" or "pop_2010"
  decay       = "sqrt",     # or "linear", "exp", 
  alpha       = 0.002,      # only for "exp"
  res         = 20,
  include_shore = TRUE
)
# plot(d2)
names(d2)<-"pop_risk_dens_inhab"

d2p<-project(d2,"EPSG:32651") # reproject
d2c <- mask(crop(d2p, focal_area), focal_area) # clip to focal area
plot(d2c)

# populations, not density ------------
d3 <- make_risk_surface(
  villages    = villages,
  islands     = islands,
  pop_var     = "pop_2010", # or "pop_dens_km2_orig"  or "pop_dens_km2_inhab" or "pop_2010"
  decay       = "sqrt",     # or "linear", "exp", 
  alpha       = 0.002,      # only for "exp"
  res         = 20,
  include_shore = TRUE
)
plot(d3)
names(d3)<-"pop_risk_pop"

d3p<-project(d3,"EPSG:32651") # reproject
d3c <- mask(crop(d3p, focal_area), focal_area) # clip to focal area
plot(d3c)




# save ---------------------------------------
# population density risk
writeRaster(d1c, "./gis2/population_risk/pop_risk_dens_orig_fa.tif", overwrite=TRUE)
writeRaster(d2c, "./gis2/population_risk/pop_risk_dens_inhab_fa.tif", overwrite=TRUE) # inhabited area
writeRaster(d3c, "./gis2/population_risk/pop_risk_pop_fa.tif", overwrite=TRUE)

#  villages with updated population information
st_write(villages,"./results/basic_files.gpkg",layer="barangay_pop2010_area",delete_layer = T)

st_write(villages,"./gis2/population/barangay_pop2010_area.shp",delete_layer = T)


#  villages with updated population information
write_csv(as.data.frame(villages),"./doc/barangay_pop2010_area.csv")



# chat gpt: for updating original data process in ArcMap to R: I have a file with point data of multiple village locations (origins), which includes values for their population and population density. I want to calculate distance decay of population risk with a square root decay as a raster surface with the risk from multiple villages being additive. Additionally, I want to use islands as barriers to the risk because people have to travel around the islands in boats. the goal is to calculate the risk from population density that decreases with distance to the communities (i.e., an environmental risk surface) (McPherson et al., 2008). in my land raster land=1 and water=0.