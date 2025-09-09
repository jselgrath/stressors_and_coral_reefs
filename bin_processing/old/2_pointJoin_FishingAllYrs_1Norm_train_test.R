# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Normalize fishing effort estimates from all years IN CORAL AND RUBBLE AREAS ONLY and extract to points
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
#read in random points
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # update this name if change sampling number and distance
  glimpse()
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()

#read in file of coral/rubble area only
CA<-st_read("./results/habitat.gpkg",layer="co_ru_fa_reclass2")%>%
  glimpse()
# plot(CA)


######################################
# Stack and organize the rasters of fishing effort

# list of files for fishing effort 
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis2/fishing")


# all fishing effort
files = list.files("./gis2/fishing/effort",pattern='\\.tif$', full.names = TRUE)%>%
  glimpse()
files2<-files[13:18] # manually check - should be all_YEAR 1960-2010
files2

s <- rast(files2)
names(s)
nl <- 6

# change names to something better
new_names = c('all1960','all1970', 'all1980','all1990','all2000',"all2010")

names(s) = new_names

plot(s[[1]]) # effort 1960
str(s[[1]])

# Mask rasters so only evaluating max fishing effort in coral areas
s2<-mask(s, CA) 
str(s2)

# plot(s[[1]])
plot(s2[[1]])

# ---------------------------------------------------------
# Calculate max for each year for Coral and Rubble Area only
mx<-tibble()
for (i in 1:6){
  val<-minmax(s2[[i]])
  val2<-cbind(val[1],val[2])
  file=new_names[i]
  max<-cbind(val2,file)
  mx<-rbind(mx,max)
}
mx
names(mx)<-c("min","max","file")

mx$max<-as.numeric(mx$max)
mx$min<-as.numeric(mx$min)
glimpse(mx)

# max for all years
mx.all<-max(mx$max)

write_csv(mx,"./doc/all_fishing_max_all_yr_coralarea.csv")

###########################################
#Extract Raster Variables to Point Data
d1<-stars::st_as_stars(s)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1[[1]])

# test
d1_te<-stars::st_as_stars(s)%>%
  stars::st_extract(pts_te)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts_te)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# plot(d1_te[[1]])


##########################
# Calculate normalized variables (value at a site/max value), See Maynard et al 2015
# NOTE: I Confirmed and NAs are in places with no fishing info...

# TRAIN -----------------------------------------------------------------
d2<-d1

######## normalized by all years ###########
d2$all1960.nrmA=d2$all1960/mx.all
d2$all1970.nrmA=d2$all1970/mx.all
d2$all1980.nrmA=d2$all1980/mx.all
d2$all1990.nrmA=d2$all1990/mx.all
d2$all2000.nrmA=d2$all2000/mx.all
d2$all2010.nrmA=d2$all2010/mx.all



# TEST -----------------------------------------------------------------
d2_te<-d1_te

######## normalized by all years ###########
d2_te$all1960.nrmA=d2_te$all1960/mx.all
d2_te$all1970.nrmA=d2_te$all1970/mx.all
d2_te$all1980.nrmA=d2_te$all1980/mx.all
d2_te$all1990.nrmA=d2_te$all1990/mx.all
d2_te$all2000.nrmA=d2_te$all2000/mx.all
d2_te$all2010.nrmA=d2_te$all2010/mx.all






########################
#export table
write_csv(d2,   "./results_train/2_pts_FishingYrs_1normalized.csv")
write_csv(d2_te,"./results_test/2_pts_FishingYrs_1normalized.csv")




