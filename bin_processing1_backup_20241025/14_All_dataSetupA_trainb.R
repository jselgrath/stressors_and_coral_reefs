# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

#####################################
# GOAL: set up variables for models - was code chunk 3 in orig code
# center and scale
#####################################


# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------

# load data 
d0<-read_csv("./results_train/13_IndpVar_Pts_all.csv")%>%
  glimpse()

# organising so that numbers are separate from factors, should be same number of variables
# dropping TYPE, ENN
d1<-d0%>%
  select(PtID2:Depth1,EcoZone:mpa_status,mpa_barangay, Id_MunWtr, distance_sg,distance_mg,geom,# character
         Id_resil,# response variable
         Depth_m,mpa_area_ha,allEffort2010:y)%>% # numbers
  select(-TYPE,-ENN,-ENN_1)%>%
  glimpse()


# subset numeric variables only, not log transformed variables
d2<-d1%>%
  dplyr::select(Depth_m:sg_minDist_100)%>%
  glimpse()
nm<- names(d2)

# calculate summary statistics
d4<- d2%>%
  pivot_longer(names_to = "vari",cols=c(Depth_m:sg_minDist_100))%>%
  group_by(vari)%>%
	summarize(
	  n=n(),
		u.var=round(mean(value,na.rm=T),4),
		sd.var=round(sd(value,na.rm=T),4),
		sem.var=round((sd.var/sqrt(n)),4))%>%
  glimpse()
	
########################
# Save
write_csv(d3,"./results_train/14_IndpVar_Pts_train.csv")
write_csv(d4,"./doc/14_IndpVar_Pts_MeanSD_train.csv")

