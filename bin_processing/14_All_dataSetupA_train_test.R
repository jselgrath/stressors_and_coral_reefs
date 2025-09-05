# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal - organize so numeric and categorical variables are grouped


# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------

# load data -------------------
# train
d0<-read_csv("./results_train/13_IndpVar_Pts_all.csv")%>%
  glimpse()

# test
d0_te<-read_csv("./results_test/13_IndpVar_Pts_all.csv")%>%
  glimpse()

# organising so that numbers are separate from factors, should be same number of variables
#train
d1<-d0%>%
  dplyr::select(point_id:resilience_id,
                Depth1,ecological_zone:ecological_zone_id,geomorphology, mpa:mpa_status,mpa_barangay,Id_MunWtr,# character
                # hab_reclass,resilience_id,# response variable
         Depth_m,mpa_area_ha,all1960.nrmA:y)%>% # numbers
  glimpse()

# test
d1_te<-d0%>%
  dplyr::select(point_id:resilience_id,
                Depth1,ecological_zone:ecological_zone_id, geomorphology, mpa:mpa_status,mpa_barangay,Id_MunWtr,# character
                # hab_reclass,resilience_id,# response variable
                Depth_m,mpa_area_ha,all1960.nrmA:y)%>% # numbers
  glimpse()

########################
# Save
write_csv(d1,"./results_train/14_IndpVar_Pts_train.csv")
write_csv(d1_te,"./results_test/14_IndpVar_Pts_test.csv")

