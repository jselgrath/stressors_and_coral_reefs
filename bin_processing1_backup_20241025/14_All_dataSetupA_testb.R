# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

#####################################
# GOAL: set up variables for models - was code chunk 3 in orig code
#####################################
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# load data 
d0<-read_csv("./results_test/13_IndpVar_Pts_all.csv")%>%
  glimpse()

# organising so that numbers are separate from factors, should be same number of variables
# dropping TYPE, ENN (same info as IdResil) and hab_dist
d1<-d0%>%
  select(PtID2:Depth1,EcoZone:mpa_status,mpa_barangay, Id_MunWtr, distance_sg,distance_mg, # character
         Id_resil,# response variable
         Depth_m,mpa_area_ha,allEffort2010:geom)%>% # numbers
  select(-TYPE,-ENN,-ENN_1)%>%
  glimpse()


#############################
# centering variables based on mean
# from gelman and hill p 55
# https://www.r-bloggers.com/a-faster-scale-function/
# or substract by mean and divide by max (0-1)
##############################
glimpse(d1)

# subset numeric variables only, not log transformed variables
d2<-d1%>%
  dplyr::select(Depth_m:sg_minDist_100)%>%
  glimpse()
nm<- names(d2)


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
write_csv(d2,"./results_test/14_IndpVar_Pts_test.csv")
write_csv(d4,"./doc/14_IndpVar_Pts_MeanSD_test.csv")

