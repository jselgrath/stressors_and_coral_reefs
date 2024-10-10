# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# GOAL: Load raster with population risk estimates from study area

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
pts<-st_read("./results_test/hab_Resil_Pts_RS.gpkg")%>%
  select(PtID2,geom)%>%
  glimpse()


#read in shapefile of coral/rubble area only
CA<-st_read("./gis/coralrubble/CoralRubArea.shp") 

######################################
# Stack and organize the rasters of fishing effort

# list of files 
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis/population")

files<-list.files(pattern=c('.tif$'))
files

# PopRskDecay has land, but ignore...
#PopRsk2 is area with land masked out
s <- rast(files)
names(s)
nl <- 1
plot(s)

# return wd to regular
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# Mask rasters so only evaluating fishnig effort in coral areas
s2<-mask(s, CA) #,inverse=TRUE
str(s2)
plot(s2)


# Calculate max for raster in coral area
mx<-tibble()
  val<-minmax(s2[[1]])
  mx<-cbind(val[1],val[2])
  mx<-as.data.frame(mx)
names(mx)<-c("min","max")
glimpse(mx)
mx$max<-as.numeric(mx$max)
mx$min<-as.numeric(mx$min)

glimpse(mx)

write_csv(mx,"./doc/population_risk_range_test.csv")

# maxRsk<-s[[1]]@data@max 
#88 for old method. 6.17 for new method

###########################################
#Extract Raster Variables to Point Data
d1<-stars::st_as_stars(s2)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  data.frame()%>%
  dplyr::select(PopRskDecay=PopRskDecay_NoClip,PtID2,geom)%>%
  mutate(PopRskDecay.Nrm=round(PopRskDecay/mx$max,4))%>%
  glimpse()


##########################
# Calculate normalized variables (value at a site/max value), See Maynard et al 2015

# below was a problem with sp/raster but not an issue here
# set NAs to 0 (sometimes showing up as 128)

# d1$PopRskDecay[d1$PopRskDecay>88]<-0
# d1$PopRskDecay[is.na(d1$PopRskDecay)]<-0
# head(d1)




########################
#export table
write_csv(d1,"./results_test/11_pts_PopRsk_Norm.csv")





