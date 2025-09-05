# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# GOAL: Join random points to normalized destructive fishing data
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

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# # load points with fishing by decade'
d1<-read_csv("./results_train/5_pts_FishingYrsDest_normalized.csv")%>%
  glimpse()
head(d1)

d1_te<-read_csv("./results_test/5_pts_FishingYrsDest_normalized.csv")%>%
  glimpse()
head(d1_te)


# calculate cumulative impact from dest fishing effort by adding normalized values across various subsets of gears

# train -----------------
d2<-d1%>%
  mutate(
           # std by yearly max
           # dfYr00=dest2010.Nrm, 
           # dfYr10=dest2010.Nrm+dest2000.Nrm, 
           # dfYr20=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm, 
           # dfYr30=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm, 
           # dfYr40=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm+dest1970.Nrm, 
           # dfYr50=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm+dest1970.Nrm+dest1960.Nrm,
           
           # std by max for all years
           dfYr00A=dest2010.nrmA, 
           dfYr10A=dest2010.nrmA+dest2000.nrmA, 
           dfYr20A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA, 
           dfYr30A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA+dest1980.nrmA, 
           dfYr40A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA, 
           dfYr50A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA+dest1960.nrmA,
					 
					 # std by max for all years with 10 year lag
					 dfYrLag10A=dest2000.nrmA, 
					 dfYrLag20A=dest2000.nrmA+dest1990.nrmA, 
					 dfYrLag30A=dest2000.nrmA+dest1990.nrmA+dest1980.nrmA, 
					 dfYrLag40A=dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA, 
					 dfYrLag50A=dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA+dest1960.nrmA)%>%
  # dplyr::select(point_id,dfYr00A:dfYrLag40A)%>% #50
glimpse()



# test -----------------
d2_te<-d1_te%>%
  mutate(
    # std by yearly max
    # dfYr00=dest2010.Nrm, 
    # dfYr10=dest2010.Nrm+dest2000.Nrm, 
    # dfYr20=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm, 
    # dfYr30=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm, 
    # dfYr40=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm+dest1970.Nrm, 
    # dfYr50=dest2010.Nrm+dest2000.Nrm+dest1990.Nrm+dest1980.Nrm+dest1970.Nrm+dest1960.Nrm,
    
    # std by max for all years
    dfYr00A=dest2010.nrmA, 
    dfYr10A=dest2010.nrmA+dest2000.nrmA, 
    dfYr20A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA, 
    dfYr30A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA+dest1980.nrmA, 
    dfYr40A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA, 
    dfYr50A=dest2010.nrmA+dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA+dest1960.nrmA,
    
    # std by max for all years with 10 year lag
    dfYrLag10A=dest2000.nrmA, 
    dfYrLag20A=dest2000.nrmA+dest1990.nrmA, 
    dfYrLag30A=dest2000.nrmA+dest1990.nrmA+dest1980.nrmA, 
    dfYrLag40A=dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA, 
    dfYrLag50A=dest2000.nrmA+dest1990.nrmA+dest1980.nrmA+dest1970.nrmA+dest1960.nrmA)%>%
    # dplyr::select(point_id,dfYr00:dfYrLag40A)%>% #50
    glimpse()

# qplot(d2$dfYr10)
qplot(d2_te$dfYr10A)
qplot(d2_te$dfYrLag10A)

#export table
write_csv(d2,    "./results_train/6_pts_FishingYrs_destructive_cumulative.csv")
write_csv(d2_te, "./results_test/6_pts_FishingYrs_destructive_cumulative.csv")


