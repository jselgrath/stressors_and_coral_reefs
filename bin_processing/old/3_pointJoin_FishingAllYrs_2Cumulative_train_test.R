# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# GOAL: Join random points to output from fishing and fragstat joins
###########################################
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
#random point with normalized fishing data
d1<-read_csv("./results_train/2_pts_FishingYrs_1normalized.csv")%>%
  glimpse()
d1_te<-read_csv("./results_test/2_pts_FishingYrs_1normalized.csv")%>%
  glimpse()
  
  
# train ------------------------------------
# calculate cumulative impact from all fishing effort by adding normalized values across various subsets of gears
d2<-mutate(d1,

           # std by max for all years
           fYr00A=all2010.nrmA, 
           fYr10A=all2010.nrmA+all2000.nrmA, 
           fYr20A=all2010.nrmA+all2000.nrmA+all1990.nrmA, 
           fYr30A=all2010.nrmA+all2000.nrmA+all1990.nrmA+all1980.nrmA, 
           fYr40A=all2010.nrmA+all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA, 
           fYr50A=all2010.nrmA+all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA+all1960.nrmA,
					 
					 # std by max for all years with 10 year lag
					 fYrLag10A=all2000.nrmA, 
					 fYrLag20A=all2000.nrmA+all1990.nrmA, 
					 fYrLag30A=all2000.nrmA+all1990.nrmA+all1980.nrmA, 
					 fYrLag40A=all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA, 
					 fYrLag50A=all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA+all1960.nrmA)%>%
  dplyr::select(point_id,fYr00A:fYrLag50A) #update if add back in other measures

head(d2)

# qplot(d2$fYr50)
qplot(d2$fYr50A)
qplot(d2$fYrLag50A)


# test -------------------------------------
# calculate cumulative impact from all fishing effort by adding normalized values across various subsets of gears
d2_te<-mutate(d1_te,

           # std by max for all years
           fYr00A=all2010.nrmA, 
           fYr10A=all2010.nrmA+all2000.nrmA, 
           fYr20A=all2010.nrmA+all2000.nrmA+all1990.nrmA, 
           fYr30A=all2010.nrmA+all2000.nrmA+all1990.nrmA+all1980.nrmA, 
           fYr40A=all2010.nrmA+all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA, 
           fYr50A=all2010.nrmA+all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA+all1960.nrmA,
           
           # std by max for all years with 10 year lag
           fYrLag10A=all2000.nrmA, 
           fYrLag20A=all2000.nrmA+all1990.nrmA, 
           fYrLag30A=all2000.nrmA+all1990.nrmA+all1980.nrmA, 
           fYrLag40A=all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA, 
           fYrLag50A=all2000.nrmA+all1990.nrmA+all1980.nrmA+all1970.nrmA+all1960.nrmA)%>%
  dplyr::select(point_id,fYr00A:fYrLag50A)

head(d2_te)

# qplot(d2_te$fYr50)
qplot(d2_te$fYr50A)
qplot(d2_te$fYrLag50A)



#save --------------------
write_csv(d2,   "./results_train/3_pts_FishingYrs_cumulative.csv")
write_csv(d2_te,"./results_test/3_pts_FishingYrs_cumulative.csv")
