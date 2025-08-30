# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# GOAL: Join random points to fisheries data and normalize fishing frequency and intensity
###########################################
# Load packages

# spatial
library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)


######################
# Area with Coral/Rubble only (CA = coral area)
########################################################################
####  OBJECTIVE 1: Load and organize files 
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
############################
# TASK 1.1 Load point files

# version 1 - from ESRI shapefile 


#read in shapefile of random point with resilience data
pts<-st_read("./results_train/hab_Resil_Pts_RS.gpkg")%>%
  glimpse()


#same as CoRub_Simplified, but no attributes except id=0
# CA = coral area
CA<-st_read("./gis/fishing_cumulative/CoralRubArea.shp")%>%
  glimpse()
# range(CA$Id)


############################
# Task 1.2 load rasters with fishing data
# Task 1.3 stack and organize the rasters 

# example of loading a single raster
# s1<-rast("./gis/fishing2010/est_dayYr_all_2010_2cln_CA.tif")%>%
#   glimpse()
# plot(s1)


# list of files -----------------------------
# be sure they are "float" format in ArcGIS
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis/fishing2010/")

# list files
files = list.files(pattern='.tif$')%>%
  glimpse()

# stack rasters
s <- rast(files)
plot(s)
names(s)
# nl <- nlayers(s)
# dataType(s)


# return wd to regular
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# change names to something better
# diversity general (divGen) = g5 (gears, general classification)
# diversity specific (divSpec) = g1 (gears, specific classification)
# SIGen = ?? general
# non-dest effort is not in this folder
new_names = c('allEffort2010', 'destEffort2010','divGen2010','divSpec2010',"SIGen2010") # 'nonDestEffort2010',
names(s) = new_names

# plot
plot(s[[1]]) # effort - days per year


# set 99,999 to NA 
# http://www.inside-r.org/packages/cran/raster/docs/calc
fun <- function(x) { x[x==99999] <- NA; return(x) }
s2 <- app(s, fun)
plot(s2[[1]])
range(s2[[1]])
plot(s2)
names(s2)

# Mask rasters so only evaluating fishnig effort in coral areas
s3<-mask(s2, CA) #,inverse=TRUE
str(s3)



################
# Calculate max for each raster 
# or better for values within the range of the samples??
mx<-tibble()

for (i in 1:5){
  val<-minmax(s3[[i]])
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

write_csv(mx,"./doc/max_fishing_2010_train.csv")

###########################################
#Extract Raster Variables to Point Data > tibble
d1<-stars::st_as_stars(s3)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  glimpse()

plot(d1[[1]])



##########################
# Calculate normalized variables (value at a site/max value for that year), See Maynard et al 2015
##########################
mx
str(mx)

d2<-d1%>%
  mutate(
    freqAll.Norm=round(allEffort2010/mx$max[1],4), #all
    freqDest.Norm=round(destEffort2010/mx$max[2],4), #dest
    impact.Norm=freqAll.Norm+freqDest.Norm
  )%>%
  glimpse()

# d1$freqNonDest.Norm<-round(d1$nonDestEffort2010/mx[3],4)
# d1$freqNonDest.Norm

qplot(d2$impact.Norm)

# reduce variables
d3<-d2%>%
  dplyr::select(PtID2,allEffort2010:SIGen2010,freqAll.Norm:impact.Norm ,geom)%>%
  glimpse()
  


########################
#export table
write_csv(d3,"./results_train/2_pts_FishingImpact_normalized.csv")



