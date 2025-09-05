# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# GOAL: Load raster with population risk estimates from study area, calculate min and max values

# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# old: 
# PopRsk2 is area with land masked out
# PopRskDecay has land, but ignore...

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs")



# -------------------------------------------
# OBJECTIVE 1: Load and organize files 
# -------------------------------------------

# -------------------------------------------
#read in file of coral/rubble area only for clipping
CA<-st_read("./results/habitat.gpkg",layer="co_ru_fa_reclass2")%>%
  glimpse()
plot(CA)

# -------------------------------------------
# Stack and organize the rasters of population risk

# list of files 
files1 = list.files("./gis2/population_risk",pattern='\\.tif$', full.names = TRUE)%>%
  glimpse()

files <- files1[!grepl("reef", basename(files1), ignore.case = TRUE)] # make sure _reef files are not included if run this after the next code
files

s <- rast(files)
names(s)
nl <- 1
plot(s)

# Mask rasters so only evaluating population risk
s2<-mask(s, CA) #,inverse=TRUE
str(s2)
plot(s2)


# Calculate max for raster in coral area

# for original pop density ---------
mx_orig<-tibble()
val_orig<-minmax(s2[[2]])
mx_orig<-cbind(val_orig[1],val_orig[2])
mx_orig<-as.data.frame(mx_orig)
names(mx_orig)<-c("min","max")
glimpse(mx_orig)
mx_orig$max<-as.numeric(mx_orig$max)
mx_orig$min<-as.numeric(mx_orig$min)

glimpse(mx_orig)

write_csv(mx_orig,"./doc/population_risk_orig_range.csv")

# for inhabited pop density --------------------
mx_inhab<-tibble()
val_inhab<-minmax(s2[[1]])
mx_inhab<-cbind(val_inhab[1],val_inhab[2])
mx_inhab<-as.data.frame(mx_inhab)
names(mx_inhab)<-c("min","max")
glimpse(mx_inhab)
mx_inhab$max<-as.numeric(mx_inhab$max)
mx_inhab$min<-as.numeric(mx_inhab$min)

glimpse(mx_inhab)

write_csv(mx_inhab,"./doc/population_risk_inhab_range.csv")

# for population (not density) --------------------
mx_pop<-tibble()
val_pop<-minmax(s2[[3]])
mx_pop<-cbind(val_pop[1],val_pop[2])
mx_pop<-as.data.frame(mx_pop)
names(mx_pop)<-c("min","max")
glimpse(mx_pop)
mx_pop$max<-as.numeric(mx_pop$max)
mx_pop$min<-as.numeric(mx_pop$min)

glimpse(mx_pop)

write_csv(mx_pop,"./doc/population_risk_pop_range.csv")

# save masked versions ---------------------------------------
writeRaster(s2[[1]], "./gis2/population_risk/pop_risk_dens_inhab_reef.tif", overwrite=TRUE)
writeRaster(s2[[2]], "./gis2/population_risk/pop_risk_dens_orig_reef.tif", overwrite=TRUE) # inhabited area
writeRaster(s2[[3]], "./gis2/population_risk/pop_risk_pop_reef.tif", overwrite=TRUE)
