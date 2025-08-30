# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Load dest fishing effort estimates from all years
# ----------------------------------------------
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
#random point with resilience data
pts<-st_read("./results_train/hab_Resil_Pts_RS.gpkg")%>%
  select(PtID2,geom)%>%
  glimpse()

#read in shapefile of coral/rubble area only
CA<-st_read("./gis/coralrubble/CoralRubArea.shp") 

######################################
# Stack and organize the rasters of desctructuve fishing effort

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis/fishing/effort_estimates")

# dest 1 = destructive fishing reported
# 1960 is missing, but ok since I am not using it anyways
files<-list.files(pattern=c(".tif$","est_dayYr_dest1"))%>%
  glimpse()
files
files2<-files[36:40] # manually check - should be 
head(files2)

s <- rast(files2)
names(s)

# change names to something better
new_names = c('dest1970', 'dest1980','dest1990','dest2000',"dest2010")#'dest1960',

names(s) = new_names


# return wd to regular
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# Mask rasters so only evaluating fishnig effort in coral areas
s2<-mask(s, CA) #,inverse=TRUE
str(s2)
str(s)

plot(s[[1]])
plot(s2[[1]])


# Calculate max for each year for entire raster 
# or better for values within the range of the samples??
mx<-tibble()

for (i in 1:5){
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
mx.dest<-max(mx$max)

###########################################
#Extract Raster Variables to Point Data
d1<-stars::st_as_stars(s2)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  glimpse()



##########################
# Calculate normalized variables (value at a site/max value), See Maynard et al 2015
# NOTE: I Confirmed and NAs are in places with no fishing info...

######## these ones are normalized by the year they were in ###########
d2<-d1%>%
  glimpse()
# d2$dest1960.Nrm=d2$dest1960/mx[1]
d2$dest1970.Nrm=d2$dest1970/mx[1,2]
d2$dest1980.Nrm=d2$dest1980/mx[2,2]
d2$dest1990.Nrm=d2$dest1990/mx[3,2]
d2$dest2000.Nrm=d2$dest2000/mx[4,2]
d2$dest2010.Nrm=d2$dest2010/mx[5,2]

######## these ones are normalized by all years ###########
# d2$dest1960.NrmA=d2$dest1960/mx.dest
d2$dest1970.NrmA=d2$dest1970/mx.dest
d2$dest1980.NrmA=d2$dest1980/mx.dest
d2$dest1990.NrmA=d2$dest1990/mx.dest
d2$dest2000.NrmA=d2$dest2000/mx.dest
d2$dest2010.NrmA=d2$dest2010/mx.dest

# Round to 4 decimal places
glimpse(d2)
names(d2)
# d2[,c(22:31)] <-round(d2[,c(22:31)],4)
# view(d2)

qplot(d2$dest2010.NrmA)

########################
#export table
write_csv(d2,"./results_train/5_pts_FishingYrsDest_normalized.csv")


