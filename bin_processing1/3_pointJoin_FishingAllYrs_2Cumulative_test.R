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

#########################################################################
# Load and organize files 
#########################################################################
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

#read in shapefile of random point with resilience data
d1<-read_csv("./results_test/3_pts_FishingYrs_1normalized.csv")%>%
  glimpse()

# calculate cumulative impact from all fishing effort by adding normalized values across various subsets of gears
d2<-mutate(d1,
           # std by yearly max
           fYr00=all2010.Nrm, 
           fYr10=all2010.Nrm+all2000.Nrm, 
           fYr20=all2010.Nrm+all2000.Nrm+all1990.Nrm, 
           fYr30=all2010.Nrm+all2000.Nrm+all1990.Nrm+all1980.Nrm, 
           fYr40=all2010.Nrm+all2000.Nrm+all1990.Nrm+all1980.Nrm+all1970.Nrm, 
           fYr50=all2010.Nrm+all2000.Nrm+all1990.Nrm+all1980.Nrm+all1970.Nrm+all1960.Nrm,
           
           # std by max for all years
           fYr00A=all2010.NrmA, 
           fYr10A=all2010.NrmA+all2000.NrmA, 
           fYr20A=all2010.NrmA+all2000.NrmA+all1990.NrmA, 
           fYr30A=all2010.NrmA+all2000.NrmA+all1990.NrmA+all1980.NrmA, 
           fYr40A=all2010.NrmA+all2000.NrmA+all1990.NrmA+all1980.NrmA+all1970.NrmA, 
           fYr50A=all2010.NrmA+all2000.NrmA+all1990.NrmA+all1980.NrmA+all1970.NrmA+all1960.NrmA,
					 
					 # std by max for all years with 10 year lag
					 fYrLag10A=all2000.NrmA, 
					 fYrLag20A=all2000.NrmA+all1990.NrmA, 
					 fYrLag30A=all2000.NrmA+all1990.NrmA+all1980.NrmA, 
					 fYrLag40A=all2000.NrmA+all1990.NrmA+all1980.NrmA+all1970.NrmA, 
					 fYrLag50A=all2000.NrmA+all1990.NrmA+all1980.NrmA+all1970.NrmA+all1960.NrmA)%>%
  dplyr::select(PtID2,fYr00:fYrLag50A)

head(d2)

qplot(d2$fYr50)
qplot(d2$fYr50A)
qplot(d2$fYrLag50A)

#export table
write_csv(d2,"./results_test/3_pts_FishingYrs_2cumulative.csv")
