# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# # GOAL: Load g1n some g5n and Ch3 categories effort estimates from all years
# NOTE: some of these I calculated so I have them if I need them. Goal is to test blast fishing, kaykay and poison/aquarium fishing.
###########################################

# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)
# -------------------------------------------


#######################################################
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------
# TASK 1.1 Load point files
# -------------------------------------------
#read in shapefile of random point with resilience data
pts<-st_read("./results/train.gpkg", layer="pts1_habitat")%>%
  glimpse()

#read in file of coral/rubble area only
CA<-st_read("./results/habitat.gpkg",layer="co_ru_fa_reclass2")%>%
  glimpse()
plot(CA)


######################################
# rast and organize the rasters of fishing effort

# # loc1<-"C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/fishing/g1_normalized"
# loc2<-"C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis/fishing/g1_cumulative"
# # loc3<-"C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/fishing/g1_lag"

# fishing effort for different gear types (g1)
files = list.files("./gis2/fishing/cumulative_g1",pattern='\\.tif$', full.names = TRUE)%>%
  glimpse()
files[[1]]%>%glimpse()

# cumulative files - list, then stack
# reminder these are in reverse order so 2010 = [[1]]; 2010-1960=[[6]]

# setwd(loc2) 
files.b<-list.files("./gis2/fishing/cumulative_g1","blast.*\\.tif$", full.names = TRUE, ignore.case = TRUE); files.b; s.b<-rast(files.b)
names(s.b)<-gsub("\\.tif$", "", basename(files.b)) 
plot(s.b)

files.p<-list.files("./gis2/fishing/cumulative_g1","*._poison*", full.names = TRUE, ignore.case = TRUE);files.p;s.p<-rast(files.p)
names(s.p)<-gsub("\\.tif$", "", basename(files.p)) 
plot(s.p)

files.p2<-list.files("./gis2/fishing/cumulative_g1","*.5poison*", full.names = TRUE, ignore.case = TRUE);files.p2;s.p2<-rast(files.p2)
names(s.p2)<-gsub("\\.tif$", "", basename(files.p2)) 
plot(s.p2)

files.k<-list.files("./gis2/fishing/cumulative_g1","*.kaykay*", full.names = TRUE, ignore.case = TRUE);files.k;s.k<-rast(files.k)
names(s.k)<-gsub("\\.tif$", "", basename(files.k)) 
plot(s.k)

files.a<-list.files("./gis2/fishing/cumulative_g1","*act*", full.names = TRUE, ignore.case = TRUE);files.a;s.a<-rast(files.a)
names(s.a)<-gsub("\\.tif$", "", basename(files.a)) 
plot(s.a)

files.i<-list.files("./gis2/fishing/cumulative_g1","*_il*", full.names = TRUE, ignore.case = TRUE);files.i;s.i<-rast(files.i)
names(s.i)<-gsub("\\.tif$", "", basename(files.i)) 
plot(s.i)

files.nS<-list.files("./gis2/fishing/cumulative_g1","*nSel*", full.names = TRUE, ignore.case = TRUE);files.nS;s.nS<-rast(files.nS)
names(s.nS)<-gsub("\\.tif$", "", basename(files.nS)) 
plot(s.nS)


plot(s.b[[1]]) 

###########################################
#Extract Raster Variables to Point Data

cumulative_f<-function(x,group1,pnts=pts){
  grp=group1 #this names the gear subset calc
	nms<-names(x)
	x2<-stars::st_as_stars(x)%>%
	  stars::st_extract(pnts)%>% # extract raster values at points
	  st_as_sf()%>% # transform back to sf
	  st_join(pnts)%>% # join to point data
	  tibble()%>%
	  glimpse()
	return(x2)
}

p.b<- cumulative_f(x=s.b, group1="blast")%>% glimpse()
p.p<- cumulative_f(x=s.p, group1="poison")%>%glimpse()
p.p2<-cumulative_f(x=s.p2,group1="g5poison")%>% glimpse()
p.k<- cumulative_f(x=s.k, group1="kaykay")%>%glimpse()

p.i<- cumulative_f(x=s.i, group1="illegal")%>%glimpse()
p.a<- cumulative_f(x=s.a, group1="active")%>%glimpse()
p.nS<- cumulative_f(x=s.nS, group1="nonSel")%>%  glimpse()

########################
#export points with cumulative values

expt<-function(x,group1){
	write_csv(x,file=paste0("./results_train/4_pts_cumulative_fishing_g1n_",group1,".csv"))
}

expt(p.b, group1="blast")
expt(p.p, group1="poison")
expt(p.k, group1="kaykay")

expt(p.p2, group1="g5poison")
expt(p.nS, group1="nonSel")
expt(p.i, group1="illegal")
expt(p.a, group1= "active")

