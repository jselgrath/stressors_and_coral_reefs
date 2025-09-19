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




# -------------------------------------------
# load csv points from fishing, fragstats, distance, and prox data
# -------------------------------------------

# normalized fishing impact # select for values standardized against max for all years (A)
# - for all effort, blast fishing, and poison
f2<-read_csv("./results_train/2_pts_fishingeffort_normalized.csv")%>%   
  glimpse()
f2_te<-read_csv("./results_test/2_pts_fishingeffort_normalized.csv")%>%   
  glimpse()

# cumulative impact from all fishing effort by adding normalized values across various subsets of gears 
# - for all effort, blast fishing, and poison
f3<- read_csv("./results_train/3_pts_fishingeffort_cumulative.csv")%>%   
  glimpse()
f3_te <- read_csv("./results_test/3_pts_fishingeffort_cumulative.csv")%>% 
  glimpse()

# lag impact from all fishing effort by adding normalized values across various subsets of gears for 10 years ago, 10+20 years ago, etc
# - for all effort, blast fishing, and poison
f4<- read_csv("./results_train/4_pts_fishingeffort_lag.csv")%>%   
  glimpse()
f4_te <- read_csv("./results_test/4_pts_fishingeffort_lag.csv")%>% 
  glimpse()

# g1n measures (g1n = summarized gear types)
# cumulative
# f3<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_blast.csv")%>%   glimpse()
# f3_te<- read_csv("./results_test/4_pts_cumulative_fishing_g1n_blast.csv")%>%   glimpse()
# 
# f4<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_kaykay.csv")%>%  glimpse()
# f4_te<- read_csv("./results_test/4_pts_cumulative_fishing_g1n_kaykay.csv")%>%  glimpse()
# 
# f5<- read_csv("./results_train/4_pts_cumulative_fishing_g1n_poison.csv")%>%  glimpse()
# f5_te<- read_csv("./results_test/4_pts_cumulative_fishing_g1n_poison.csv")%>%  glimpse()
# 
# f6<- read_csv("./results_train/5_pts_FishingYrsDest_normalized.csv")%>%  glimpse()
# f6_te<- read_csv("./results_test/5_pts_FishingYrsDest_normalized.csv")%>%  glimpse()
# 
# f7<- read_csv("./results_train/6_pts_FishingYrs_destructive_cumulative.csv")%>%   glimpse()
# f7_te<- read_csv("./results_test/6_pts_FishingYrs_destructive_cumulative.csv")%>%   glimpse()

f7<-read_csv("./results_train/7_pts_landscape_patch.csv")%>%  dplyr::select(-geom)%>% glimpse()
f7_te<-read_csv("./results_test/7_pts_landscape_patch.csv")%>%  dplyr::select(-geom)%>%  glimpse()

f8<-read_csv("./results_train/8_pts_landscape_edge_dist.csv")%>%  glimpse()
f8_te<-  read_csv("./results_test/8_pts_landscape_edge_dist.csv")%>%  glimpse()

f9<-read_csv("./results_train/9_pts_IndpVarOther_pts.csv")%>%    dplyr::select(-geom)%>%   glimpse()
f9_te<-  read_csv("./results_test/9_pts_IndpVarOther_pts.csv")%>%  dplyr::select(-geom)%>%glimpse()

f10<- read_csv("./results_train/10_pts_PopRsk_Norm.csv")%>%   glimpse()
f10_te<- read_csv("./results_test/10_pts_PopRsk_Norm.csv")%>%   glimpse()

f11<- read_csv("./results_train/11_pts_river_distance_1normalized.csv")%>%   glimpse()
f11_te<- read_csv("./results_test/11_pts_river_distance_1normalized.csv")%>%   glimpse()



# -------------------------------------------
# join new tables to original pt table
# -------------------------------------------
# train
pts2<-f2%>% 
  left_join(f3)%>%
  left_join(f4)%>%
  # left_join(f5)%>%
  # left_join(f6)%>% 
  left_join(f7)%>% 
  left_join(f8)%>%
  left_join(f9)%>%
  left_join(f10)%>%
  left_join(f11)%>%
  dplyr::select(-gridcode)%>%
  dplyr::select(point_id,patch_id,all_1960_nrmA:point_dist_river.nrm)%>% # reordering columns
  glimpse()


# test
pts2_te<-f2_te%>% 
  left_join(f3_te)%>%
  left_join(f4_te)%>%
  # left_join(f5_te)%>%
  # left_join(f6_te)%>%
  left_join(f7_te)%>%
  left_join(f8_te)%>%
  left_join(f9_te)%>%
  left_join(f10_te)%>%
  left_join(f11_te)%>%
  dplyr::select(-gridcode)%>%
  dplyr::select(point_id,patch_id,all_1960_nrmA:point_dist_river.nrm)%>% # reordering columns
  glimpse()


# -------------------------------------------
# merge with sf 
# spatial

# train
pts3<-pts%>%
  left_join(pts2)%>%
  glimpse
str(pts3)

# test
pts3_te<-pts_te%>%
  left_join(pts2_te)%>%
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
  # dplyr::select(-geom)%>%
  glimpse()


# test  --
# join CID and x y data to data.frame
# pt@data$CID
coord_te<-data.frame(st_coordinates(pts3_te))#"CID"
names(coord_te)<-c("x","y") 

# make dataframe - non-spatial
pt4_te<-data.frame(cbind(pts3_te,coord_te)) %>%
  # dplyr::select(-geom)%>%
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
st_write(pts3,"./results/train.gpkg", layer="13_IndpVar_Pts_all.gpkg", delete_layer=T)
st_write(pts3_te,"./results/test.gpkg", layer="13_IndpVar_Pts_all.gpkg", delete_layer=T)

# csv
write_csv(pt4,"./results_train/13_IndpVar_Pts_all.csv")
write_csv(pt4_te,"./results_test/13_IndpVar_Pts_all.csv")

tail(pt4)
