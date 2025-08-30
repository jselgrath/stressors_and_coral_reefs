# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# GOAL: Join random points to output from fishing and fragstat joins
#       Subset data to 1000 pts use
# ----------------------------------------------
library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)
# -------------------------------------------



#########################################################################
# Load and organize files 
#########################################################################
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# # load points with fishing by decade'
d1<-read_csv("./results_train/5_pts_FishingYrsDest_normalized.csv")%>%
  glimpse()
head(d1)

# calculate cumulative impact from dest fishing effort by adding normalized values across various subsets of gears
d2<-d1%>%
  mutate(
           # std by yearly max
           dfYr00=dest2010.Nrm, 
           dfYr10=dest2010.Nrm+dest2000.Nrm, 
           dfYr20=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm, 
           dfYr30=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm, 
           dfYr40=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm+dest1970.Nrm, 
           # dfYr50=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm+dest1970.Nrm+dest1960.Nrm,
           
           # std by max for all years
           dfYr00A=dest2010.NrmA, 
           dfYr10A=dest2010.NrmA+dest2000.NrmA, 
           dfYr20A=dest2010.NrmA+dest2000.NrmA+dest1990.NrmA, 
           dfYr30A=dest2010.NrmA+dest2000.NrmA+dest1990.NrmA+dest1980.NrmA, 
           dfYr40A=dest2010.NrmA+dest2000.NrmA+dest1990.NrmA+dest1980.NrmA+dest1970.NrmA, 
           # dfYr50A=dest2010.NrmA+dest2000.NrmA+dest1990.NrmA+dest1980.NrmA+dest1970.NrmA+dest1960.NrmA,
					 
					 # std by max for all years with 10 year lag
					 dfYrLag10A=dest2000.NrmA, 
					 dfYrLag20A=dest2000.NrmA+dest1990.NrmA, 
					 dfYrLag30A=dest2000.NrmA+dest1990.NrmA+dest1980.NrmA, 
					 dfYrLag40A=dest2000.NrmA+dest1990.NrmA+dest1980.NrmA+dest1970.NrmA)%>% 
					 # dfYrLag50A=dest2000.NrmA+dest1990.NrmA+dest1980.NrmA+dest1970.NrmA+dest1960.NrmA)%>%
  dplyr::select(PtID2,dfYr00:dfYrLag40A)%>% #50
glimpse()

# view(d2)

qplot(d2$dfYr10)
qplot(d2$dfYr10A)
qplot(d2$dfYrLag10A)

#export table
write_csv(d2,"./results_train/6_pts_FishingYrs_destructive_cumulative.csv")



