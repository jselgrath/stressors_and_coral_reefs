# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Load dest fishing effort estimates from all years
# ----------------------------------------------
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------
#random points
pts<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_train")%>% # update this name if change sampling number and distance
  glimpse()
pts_te<-st_read("./results/basic_files.gpkg", layer="stratified_random_points_1500pts_100m_test")%>% # update this name if change sampling number and distance
  glimpse()

#read in file of coral/rubble area only
CA<-st_read("./results/habitat.gpkg",layer="co_ru_fa_reclass2")%>%
  glimpse()
# plot(CA)

# -------------------------------------------
# Stack and organize the rasters of destructive fishing effort
# -------------------------------------------

# dest 1 = destructive fishing reported
files<-list.files("./gis2/fishing/effort",pattern="est_dayYr_dest1.*\\.tif$", full.names = TRUE)%>%
  glimpse()
s <- rast(files)
names(s)

# change names to something better
new_names = c('dest1960','dest1970', 'dest1980','dest1990','dest2000',"dest2010")#

names(s) = new_names

# Mask rasters so only evaluating fishnig effort in coral areas
s2<-mask(s, CA) #,inverse=TRUE
str(s2)
str(s)

plot(s[[1]])
plot(s2[[1]])


# Calculate max for each year for entire raster 
# or better for values within the range of the samples??
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

mx$max<-round(as.numeric(mx$max),4)
mx$min<-round(as.numeric(mx$min),4)
glimpse(mx)

# max for all years
mx.dest<-max(mx$max)

# -------------------------------------------
#Extract Raster Variables to Point Data

# test ----
d1<-stars::st_as_stars(s)%>%
  stars::st_extract(pts)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# train ----
d1_te<-stars::st_as_stars(s)%>%
  stars::st_extract(pts_te)%>% # extract raster values at points
  st_as_sf()%>% # transform back to sf
  st_join(pts_te)%>% # join to point data
  tibble()%>%
  dplyr::select(-geom)%>%
  glimpse()

# -------------------------------------------
# Calculate normalized variables (value at a site/max value), See Maynard et al 2015
# NOTE: I Confirmed and NAs are in places with no fishing info...

# train ----------------
######## these ones are normalized by the year they were in ###########
d2<-d1%>%
  glimpse()
# d2$dest1960.nrm=d2$dest1960/mx[1]
# d2$dest1970.nrm=d2$dest1970/mx[1,2]
# d2$dest1980.nrm=d2$dest1980/mx[2,2]
# d2$dest1990.nrm=d2$dest1990/mx[3,2]
# d2$dest2000.nrm=d2$dest2000/mx[4,2]
# d2$dest2010.nrm=d2$dest2010/mx[5,2]

######## these ones are normalized by all years ###########
d2$dest1960.nrmA=d2$dest1960/mx.dest
d2$dest1970.nrmA=d2$dest1970/mx.dest
d2$dest1980.nrmA=d2$dest1980/mx.dest
d2$dest1990.nrmA=d2$dest1990/mx.dest
d2$dest2000.nrmA=d2$dest2000/mx.dest
d2$dest2010.nrmA=d2$dest2010/mx.dest

# Round to 4 decimal places
glimpse(d2)
names(d2)
# d2[,c(22:31)] <-round(d2[,c(22:31)],4)
# view(d2)

qplot(d2$dest2010.nrmA)



# test ----------------
######## these ones are normalized by the year they were in ###########
d2_te<-d1_te%>%
  glimpse()
# d2_te$dest1960.nrm=d2_te$dest1960/mx[1,2]
# d2_te$dest1970.nrm=d2_te$dest1970/mx[1,2]
# d2_te$dest1980.nrm=d2_te$dest1980/mx[2,2]
# d2_te$dest1990.nrm=d2_te$dest1990/mx[3,2]
# d2_te$dest2000.nrm=d2_te$dest2000/mx[4,2]
# d2_te$dest2010.nrm=d2_te$dest2010/mx[5,2]

######## these ones are normalized by all years ###########
d2_te$dest1960.nrmA=d2_te$dest1960/mx.dest
d2_te$dest1970.nrmA=d2_te$dest1970/mx.dest
d2_te$dest1980.nrmA=d2_te$dest1980/mx.dest
d2_te$dest1990.nrmA=d2_te$dest1990/mx.dest
d2_te$dest2000.nrmA=d2_te$dest2000/mx.dest
d2_te$dest2010.nrmA=d2_te$dest2010/mx.dest

# Round to 4 decimal places
glimpse(d2_te)
names(d2_te)
# d2_te[,c(22:31)] <-round(d2_te[,c(22:31)],4)
# view(d2)

qplot(d2_te$dest2010.nrmA)

########################
#export table
write_csv(d2,   "./results_train/5_pts_FishingYrsDest_normalized.csv")
write_csv(d2_te,"./results_test/5_pts_FishingYrsDest_normalized.csv")

