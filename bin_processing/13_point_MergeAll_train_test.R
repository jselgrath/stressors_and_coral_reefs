# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# GOAL: Join random points to output from fishing and fragstat joins

# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------
# Load and organize files 
# -------------------------------------------

# # load points with  some indp data 
pts<-st_read("./results/train.gpkg", layer="1_pts_habitat_tr")%>%
  glimpse()
pts_te<-st_read("./results/test.gpkg", layer="1_pts_habitat_te")%>%
  glimpse()
plot(pts)

# -------------------------------------------
# load csv points from fishing, fragstats, distance, and prox data
# -------------------------------------------

# normalized fishing impact (by that year, not all years)
# select for values standardized against max for all years (A)
f1<-read_csv("./results_train/2_pts_FishingYrs_1normalized.csv")%>%   
  dplyr::select(point_id,all1960.nrmA:all2010.nrmA)%>% # select normalized by all years
  glimpse()
f1_te<-read_csv("./results_test/2_pts_FishingYrs_1normalized.csv")%>%   
  dplyr::select(point_id,all1960.nrmA:all2010.nrmA)%>%
  glimpse()

# Cumulative fishing years, or with lag
# select for values standardized against max for all years (A)
# cumulative impact from all fishing effort by adding normalized values across various subsets of gears 
# std by max for all years with 10 year lag
f2<- read_csv("./results_train/3_pts_FishingYrs_cumulative.csv")%>%   
  dplyr::select(point_id,fYr00A:fYrLag50A)%>%
  glimpse()
f2_te <- read_csv("./results_test/3_pts_FishingYrs_cumulative.csv")%>% 
  dplyr::select(point_id,fYr00A:fYrLag50A)%>%
  glimpse()

# g1n measures (g1n = summarized gear types)
# cumulative
f3<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_blast.csv")%>%   glimpse()
f3_te<- read_csv("./results_test/4_pts_cumulative_fishing_g1n_blast.csv")%>%   glimpse()

f4<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_kaykay.csv")%>%  glimpse()
f4_te<- read_csv("./results_test/4_pts_cumulative_fishing_g1n_kaykay.csv")%>%  glimpse()

f5<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_poison.csv")%>%  glimpse()
f5_te<- read_csv("./results_test/4_pts_cumulative_fishing_g1n_poison.csv")%>%  glimpse()

f6<- read_csv("./results_train/5_pts_FishingYrsDest_normalized.csv")%>%  glimpse()
f6_te<- read_csv("./results_test/5_pts_FishingYrsDest_normalized.csv")%>%  glimpse()

f7<- read_csv("./results_train/6_pts_FishingYrs_destructive_cumulative.csv")%>%   glimpse()
f7_te<- read_csv("./results_test/6_pts_FishingYrs_destructive_cumulative.csv")%>%   glimpse()

f8<-read_csv("./results_train/7_pts_landscape_patch.csv")%>%  glimpse()
f8_te<-read_csv("./results_test/7_pts_landscape_patch.csv")%>%  glimpse()

f9<-read_csv("./results_train/8_pts_landscape_edge_dist.csv")%>%  glimpse()
f9_te<-  read_csv("./results_test/8_pts_landscape_edge_dist.csv")%>%  glimpse()

f10<-read_csv("./results_train/9_pts_IndpVarOther_pts.csv")%>%  glimpse()
f10_te<-  read_csv("./results_test/9_pts_IndpVarOther_pts.csv")%>%  glimpse()

f11<- read_csv("./results_train/10_pts_PopRsk_Norm.csv")%>%   glimpse()
f11_te<- read_csv("./results_test/10_pts_PopRsk_Norm.csv")%>%   glimpse()

f12<- read_csv("./results_train/11_pts_river_distance_1normalized.csv")%>%   glimpse()
f12_te<- read_csv("./results_test/11_pts_river_distance_1normalized.csv")%>%   glimpse()



# -------------------------------------------
# join new tables to original pt table
# -------------------------------------------
# train
pts2<-f1%>% 
  full_join(f2)%>%
  full_join(f3)%>%
  full_join(f4)%>%
  full_join(f5)%>%
  full_join(f6)%>% # can mute
  full_join(f7)%>% # can mute
  full_join(f8)%>%
  full_join(f9)%>%
  full_join(f10)%>%
  full_join(f11)%>%
  full_join(f12)%>%
  dplyr::select(-geom)%>%
  mutate(resilience_id=if_else(hab_reclass=="Coral",1,0))%>%
  dplyr::select(point_id,patch_id,hab_reclass,resilience_id,all1960.nrmA:river_distance.nrm)%>% # reordering columns
  glimpse()


# test
pts2_te<-f1_te%>% 
  full_join(f2_te)%>%
  full_join(f3_te)%>%
  full_join(f4_te)%>%
  full_join(f5_te)%>%
  full_join(f6_te)%>%
  full_join(f7_te)%>%
  full_join(f8_te)%>%
  full_join(f9_te)%>%
  full_join(f10_te)%>%
  full_join(f11_te)%>%
  full_join(f12_te)%>%
  dplyr::select(-geom)%>%
  mutate(resilience_id=if_else(hab_reclass=="Coral",1,0))%>%
  dplyr::select(point_id,patch_id,hab_reclass,resilience_id,all1960.nrmA:river_distance.nrm)%>% # reordering columns
  glimpse()


# -------------------------------------------
# merge with sf 
# spatial

# train
pts3<-pts%>%
  full_join(pts2)%>%
  glimpse
str(pts3)

# test
pts3_te<-pts_te%>%
  full_join(pts2_te)%>%
  glimpse
str(pts3_te)

# -------------------------------------------------------------
# train --
# join CID and x y data to data.frame
# pt@data$CID
coord<-data.frame(st_coordinates(pts3))#"CID"
names(coord)<-c("x","y") 

# make dataframe - non-spatial
pt4<-data.frame(cbind(pts3,coord)) %>%
  dplyr::select(-geom)%>%
  glimpse()


# test  --
# join CID and x y data to data.frame
# pt@data$CID
coord_te<-data.frame(st_coordinates(pts3_te))#"CID"
names(coord_te)<-c("x","y") 

# make dataframe - non-spatial
pt4_te<-data.frame(cbind(pts3_te,coord_te)) %>%
  dplyr::select(-geom)%>%
  glimpse()

# -------------------------------------------------------------
# calc percent
pct<-pt4%>%
  group_by(hab_reclass)%>%
  summarize(n=n())%>%
  glimpse()

totalPt<-sum(pct$n)

pct_coral<-round(pct$n[1]/totalPt,3)
pct_rubble<-round(pct$n[2]/totalPt,3)
more_rubble<-round(pct$n[2]/pct$n[1],3)

names<-c("pct_coral","pct_rubble","more_rubble")
vals<-c(pct_coral,pct_rubble,more_rubble)
stats<-data.frame(cbind(names,vals))%>%
  glimpse()

write_csv(stats,"./doc/hab_percentage_stats_train.csv")



####################
# Save files

# geopackage
st_write(pts3,"./results_train/13_IndpVar_Pts_all.gpkg", delete_layer=T)
st_write(pts3_te,"./results_test/13_IndpVar_Pts_all.gpkg", delete_layer=T)

# csv
write_csv(pt4,"./results_train/13_IndpVar_Pts_all.csv")
write_csv(pt4_te,"./results_test/13_IndpVar_Pts_all.csv")


