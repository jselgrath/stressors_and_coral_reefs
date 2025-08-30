# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# Updating distance so for rubble is based on ArcGis distance tool and for coral based on ENN tool in Fragstats. That way its always distance of a location, not to itself.
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

# read in shapefile of random point with resilience data
pts<-st_read("./results_test/hab_Resil_Pts_RS.gpkg")%>%
  glimpse()
plot(pts)

# need resilience info for update below
f0p<-pts%>%
  data.frame()%>%
  dplyr::select(PtID2,Id_resil,geom)%>%
  glimpse()

# 

#version with LEK SG in inner channel and MG on olango
# f1p<-read_csv("./results_orig/RS_only/pts_Co_Mg_Sg_minDist.csv")%>% # orig version has more 
f1p<-read_csv("./results_test/12_1pts_Co_Mg_Sg_minDist.csv")%>%
  dplyr::select(PtID2,co_minDist:sg_minDist)%>%
  glimpse()

plot(f1p)

f1p2<-f1p%>%
  left_join(f0p)%>%
  glimpse()

############################
# Task 1.2 load shapefiles/rasters

# polygons
s1<-st_read("./gis/landscape/CoRu_Frag_20160927.shp")%>%
  glimpse()
plot(s1)


# join
s2<-pts%>%
  st_join(s1)%>%
  glimpse()

plot(s2)

# checking
s2%>%filter(Id_resil==1)%>%
  select(Id_resil,TYPE)

f3p<-f1p2%>%
  full_join(s2)%>% #merge, keeping all points
  mutate(co_minDist2=co_minDist)%>%
  mutate(co_minDist=ifelse(Id_resil==1,ENN,co_minDist))%>% #update co_minDist with ENN for Coral areas
  dplyr::select(PtID2:sg_minDist)%>%
  data.frame()%>%
  mutate(
    co_minDist_Nml=round(co_minDist/2190.091,4),
    mg_minDist_Nml=round(mg_minDist/11603.62,4),
    sg_minDist_Nml=round(sg_minDist/1944.865,4)
  )%>%
  mutate(
    co_minDist_100=round(co_minDist/100,4),
    mg_minDist_100=round(mg_minDist/100,4),
    sg_minDist_100=round(sg_minDist/100,4)
  )%>%
  glimpse() 



# checking max distances
t1<-round(max(f3p$co_minDist,na.rm=T),4)
t2<-round(max(f3p$sg_minDist,na.rm=T),4)
t3<-round(max(f3p$mg_minDist,na.rm=T),4)
t4<-round(min(f3p$co_minDist,na.rm=T),4)
t5<-round(min(f3p$sg_minDist,na.rm=T),4)
t6<-round(min(f3p$mg_minDist,na.rm=T),4)

max<-c(t1,t2,t3)
min<-c(t4,t5,t6)
hab<-c("co","sg","mg")

temp<-data.frame(cbind(max,min,hab))%>%
  glimpse()



# write table
write_csv(f3p,"./results_test/12_2pts_Co_Mg_Sg_minDist2.csv")

write_csv(temp,"./doc/habitat_distance_min_max_test.csv")
