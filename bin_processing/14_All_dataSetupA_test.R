# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal - organize so numeric and categorical variables are grouped


# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------

# load data 
d0<-read_csv("./results_test/13_IndpVar_Pts_all.csv")%>%
  glimpse()

# organising so that numbers are separate from factors, should be same number of variables
# dropping TYPE, ENN
d1<-d0%>%
  dplyr::select(PtID2:Depth1,EcoZone:mpa_status,mpa_barangay, Id_MunWtr,geom,# character
         Id_resil,# response variable
         Depth_m,mpa_area_ha,allEffort2010:y)%>% # numbers
  dplyr::select(-TYPE,-ENN,-ENN_1)%>%
  glimpse()


########################
# Save
write_csv(d1,"./results_test/14_IndpVar_Pts_test.csv")


