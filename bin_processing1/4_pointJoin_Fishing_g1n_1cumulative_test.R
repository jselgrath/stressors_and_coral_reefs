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
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

############################
# TASK 1.1 Load point files
#read in shapefile of random point with resilience data
pts<-st_read("./results_test/hab_Resil_Pts_RS.gpkg")%>%
  select(PtID2,geom)%>%
  glimpse()

#same as CoRub_Simplified, but no attributes except id=0
# CA = coral area
CA<-st_read("./gis/fishing_cumulative/CoralRubArea.shp")%>%
  glimpse()


######################################
# rast and organize the rasters of fishing effort

# loc1<-"C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/fishing/g1_normalized"
loc2<-"C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/gis/fishing/g1_cumulative"
# loc3<-"C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs/fishing/g1_lag"


# cumulative files - list, then stack
setwd(loc2) 
files.b<-list.files(pattern=c('.*blast*.'));s.b<-rast(files.b);s.b
files.p<-list.files(pattern=c('.*_poison*.'));s.p<-rast(files.p);s.p
files.p2<-list.files(pattern=c('.*5poison*.'));s.p2<-rast(files.p2);s.p2
files.k<-list.files(pattern=c('.*kaykay*.'));s.k<-rast(files.k);s.k

files.a<-list.files(pattern=c('.*act*.'));s.a<-rast(files.a);s.a
files.i<-list.files(pattern=c('.*il*.'));s.i<-rast(files.i);s.i
files.nS<-list.files(pattern=c('.*nSel*.'));s.nS<-rast(files.nS);s.nS

plot(s.b[[1]]) # reminder these are in reverse order so 2010 = [[1]]; 2010-1960=[[6]]

###########################################
#Extract Raster Variables to Point Data

cumulative_f<-function(x=s.b,group1="blast",pnts=pts){
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

# return wd to regular
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

expt<-function(x,group1="blast"){
	write_csv(x,file=paste0("./results_test/4_pts_cumulative_fishing_g1n_",group1,".csv"))
}

expt(p.b, group1="blast")
expt(p.p, group1="poison")
expt(p.k, group1="kaykay")

expt(p.p2, group1="g5poison")
expt(p.nS, group1="nonSel")
expt(p.i, group1="illegal")
expt(p.a, group1= "active")

