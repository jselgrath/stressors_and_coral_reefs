# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Load fishing effort estimates from all years
# -------------------------------------------
library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# -------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# -------------------------------------------
# TASK 1.1: read in shapefile of random point with resilience data
pts<-st_read("./results_test/hab_Resil_Pts_RS.gpkg")%>%
  glimpse()

#read in shapefile of coral/rubble area only
CA<-st_read("./gis/coralrubble/CoralRubArea.shp") 


######################################
# Stack and organize the rasters of fishing effort

# list of files 
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis/fishing/effort_estimates")

files<-list.files(pattern=c('.tif$','all'))
files2<-files[12:17] # manually check - should be 1960-2010
files2

s <- rast(files2)
names(s)
nl <- 6

# return wd to regular
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")


# change names to something better
new_names = c('all1960','all1970', 'all1980','all1990','all2000',"all2010")

names(s) = new_names

plot(s[[1]]) # effort 1960
str(s[[1]])

# Mask rasters so only evaluating fishnig effort in coral areas
s2<-mask(s, CA) #,inverse=TRUE
str(s2)

plot(s[[1]])
plot(s2[[1]])

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

write_csv(mx,"./doc/fishing_by_year_max_test.csv")

###########################################
#Extract Raster Variables to Point Data
d1<-stars::st_as_stars(s2)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  glimpse()

plot(d1[[1]])


##########################
# Calculate normalized variables (value at a site/max value), See Maynard et al 2015
# NOTE: I Confirmed and NAs are in places with no fishing info...

######## these ones are normalized by the year they were in ###########
mx

d2<-d1
d2$all1960.Nrm=d2$all1960/mx[1,2]
d2$all1970.Nrm=d2$all1970/mx[2,2]
d2$all1980.Nrm=d2$all1980/mx[3,2]
d2$all1990.Nrm=d2$all1990/mx[4,2]
d2$all2000.Nrm=d2$all2000/mx[5,2]
d2$all2010.Nrm=d2$all2010/mx[6,2]

######## these ones are normalized by all years ###########
d2$all1960.NrmA=d2$all1960/mx.all
d2$all1970.NrmA=d2$all1970/mx.all
d2$all1980.NrmA=d2$all1980/mx.all
d2$all1990.NrmA=d2$all1990/mx.all
d2$all2000.NrmA=d2$all2000/mx.all
d2$all2010.NrmA=d2$all2010/mx.all

# Round to 4 decimal places
# d2[,c(23:34)] <-round(d2[,c(23:34)],4) #the "-1" excludes column 1
glimpse(d2)

qplot(d2$all2010.Nrm)


########################
#export table
write_csv(d2,"./results_test/3_pts_FishingYrs_1normalized.csv")





