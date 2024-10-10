# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Code to analyze the relationship between coral (Coral (1) vs. rubble (0)) and various threats and biophysical parameters.

# TAKE HOME 1: 
# NO difference between using all fishing time series of 30,40,or 50 years # see versions of old code for this conclusion

############################################################
library(lme4)
library(car) # function Anova - correct checking on models
library(ggplot2)
##########################

# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------
# load data 
d0<-read_csv("./results_test/14_IndpVar_Pts_test.csv")%>%
  mutate(EcoZone2=if_else(EcoZone2=="Terrestrial Island","Coastal",EcoZone2))%>% # combine terrestrial island and coastal ecological zones
  glimpse()

unique(d0$EcoZone2)  

d1<-na.omit(d0) #2784 - test


##########################
options(na.action = "na.fail")

# remove enormous outlier for earlier run of model PtID = 4272
# d2<-filter(d2,d2$CID!="4272") 


##############################
# NOTES:
# leave out mgProx & sgProx? -  model throws errors with mgProx
# # z.LCoRuEdg2Area #not better with edge/area composite
# using PopRsk instead of reefs at risk data because RaR data was nonsensical
# dist to river and mangroves are - correlated


##########
# save
write_csv(d1,"./results_test/14_IndpVar_Pts_test.csv")



