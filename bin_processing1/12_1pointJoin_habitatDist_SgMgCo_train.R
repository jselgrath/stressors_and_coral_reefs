# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Load fishing effort estimates from all years
# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# -------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

############################
# TASK 1.1 Load point files

#read in shapefile of random point with resilience data
pts<-st_read("./results_train/hab_Resil_Pts_RS.gpkg")%>%
  select(PtID2,geom)%>%
  glimpse()

plot(pts)

#read in shapefile of coral/rubble area only
CA<-st_read("./gis/coralrubble/CoralRubArea.shp") %>%
  glimpse()
# plot(CA)

############################
# TASK 1.1 Load point files

######################################
# Stack and organize the rasters of fishing effort
# list of files 
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis/prox")

files<-list.files(pattern=c('.tif$'))
files #should be 1: 1 co 1 mg  1 sg

# PopRskDecay has land, but ignore...
#PopRsk2 is just area with land masked out
s <- rast(files)
names(s)
nl <- 1
plot(s)

# change names to something better
new_names = c('co_minDist','mg_minDist','sg_minDist')
names(s) = new_names

# return wd to regular
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# Mask rasters so only evaluating fishnig effort in coral areas
s2<-mask(s, CA) #,inverse=TRUE
str(s2)

plot(s2) 


###########################################

# Calculate max for raster in coral area
mx<-tibble()
for (i in 1:3){
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

# This is updated in the next code chunk
# write_csv(mx,"./doc/habitat_distance_min_max_train.csv")


#Extract Raster Variables to Point Data
d1<-stars::st_as_stars(s2)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  data.frame()%>%
  dplyr::select(PtID2,co_minDist:sg_minDist,geom)%>%
  mutate(
    co_minDist_Nml=round(co_minDist/2230.00,4),
    mg_minDist_Nml=round(co_minDist/11797.06,4),
    sg_minDist_Nml=round(co_minDist/1951.82,4)
  )%>%
  mutate(co_minDist_100=round(co_minDist/100,4),
         mg_minDist_100=round(mg_minDist/100,4),
         sg_minDist_100=round(sg_minDist/100,4))%>%
  glimpse()

########################
#export table

write_csv(d1,"./results_train/12_1pts_Co_Mg_Sg_minDist.csv")
