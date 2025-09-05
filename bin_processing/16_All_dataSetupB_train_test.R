# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal - organize so numeric and categorical variables are grouped

library(tidyverse)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------

# load data #
d1<-read_csv("./results_train/14_IndpVar_Pts_train.csv")%>%

  # change name of response variable and making factors
  mutate(Reef_state=as.factor(resilience_id),
         ecological_zone=as.factor(ecological_zone),
         MPA=as.factor(mpa), #(if_else(mpa=="unprotected","open",mpa)),
         municipality=as.factor(Id_MunWtr),
         geomorphology=as.factor(geomorphology),
         
         # set fishing NA as 0 - confirmed true in ArcPro by visualizing
         cumulative_blast10 =if_else(is.na(cumulative_blast10),0,cumulative_blast10), 
         fYrLag30A=if_else(is.na(fYrLag30A),0,fYrLag30A))%>%
 filter(point_id!=674&point_id!=546&point_id!=573&point_id!=403)%>% # coral next to Bohol - low replicates
    glimpse()


d1_te<-read_csv("./results_test/14_IndpVar_Pts_test.csv")%>%

    # change name of response variable and ecological zone
  mutate(Reef_state=as.factor(resilience_id),
         ecological_zone=as.factor(ecological_zone),
         MPA=as.factor(mpa), #(if_else(mpa=="unprotected","open",mpa)),
         municipality=as.factor(Id_MunWtr),
         geomorphology=as.factor(geomorphology),
         # set fishing NA as 0 - confirmed true in ArcPro by visualizing
         cumulative_blast10 =if_else(is.na(cumulative_blast10),0,cumulative_blast10), 
         fYrLag30A=if_else(is.na(fYrLag30A),0,fYrLag30A))%>%
  glimpse()



# check depth and geomorphology 
cbind(d1$geomorphology,d1$Depth_m)

# graph updated variable ------------------
ggplot(d1,aes(x,y,color=Depth_m,shape=geomorphology ))+geom_point()

d1%>%
  filter(geomorphology=="reef slope")%>%
  ggplot(aes(x,y,color=Depth_m,shape=geomorphology ))+geom_point()

d1%>%
  filter(geomorphology=="reef flat")%>%
  ggplot(aes(x,y,color=Depth_m,shape=geomorphology ))+geom_point()



range(d1$Depth_m,na.rm=T)


# ---------------------------------------------
# subset data 
# ---------------------------------------------
# train
d2<-d1%>%
  dplyr::select(point_id,patch_id,resilience_id,hab_reclass, ecological_zone,mpa_barangay, mpa_id, MPA,mpa_status, municipality,point_dist_Seagrass,point_dist_Mangrove, geomorphology, point_dist_Coral, patch_shape_index,pop_risk_dens_inhab, pop_risk_dens_orig,pop_risk_pop,cumulative_blast00,cumulative_blast10,fYrLag10A,fYrLag20A,fYrLag30A,mpa_area_ha,Depth_m,river_distance,x,y)%>%
  glimpse()

#test
d2_te<-d1_te%>%
  dplyr::select(point_id,patch_id,resilience_id,hab_reclass, ecological_zone,mpa_barangay, mpa_id, MPA,mpa_status, municipality,point_dist_Seagrass,point_dist_Mangrove, geomorphology, point_dist_Coral, patch_shape_index,pop_risk_dens_inhab, pop_risk_dens_orig,pop_risk_pop,cumulative_blast00,cumulative_blast10,fYrLag10A,fYrLag20A,fYrLag30A,mpa_area_ha,Depth_m,river_distance,x,y)%>%
  glimpse()


# ---------------------------------------------
# futher subset data -  select non-correlated variables (see Correlation Viz)
# ---------------------------------------------
d3<- d2%>%
  dplyr::select(point_id,patch_id,Reef_state=resilience_id,hab_reclass, ecological_zone, geomorphology,
                mpa_barangay, mpa_id, MPA,mpa_status, municipality,
                point_dist_Seagrass,point_dist_Mangrove, point_dist_Coral, patch_shape_index,
                pop_risk_dens_inhab, pop_risk_dens_orig,pop_risk_pop,
                cumulative_blast10,fYrLag30A,
                mpa_area_ha,Depth_m,river_distance,x,y)%>%
  glimpse()
 
d3_te<- d2_te%>%
  dplyr::select(point_id,patch_id,Reef_state=resilience_id,hab_reclass, ecological_zone, geomorphology,
                mpa_barangay, mpa_id, MPA,mpa_status, municipality,
                point_dist_Seagrass,point_dist_Mangrove, point_dist_Coral, patch_shape_index,
                pop_risk_dens_inhab, pop_risk_dens_orig,pop_risk_pop,
                cumulative_blast10,fYrLag30A,
                mpa_area_ha,Depth_m,river_distance,x,y)%>%
  glimpse()   




########################
# Save
write_csv(d3,"./results_train/16_IndpVar_Pts_train_all.csv")
write_csv(d3_te,"./results_test/16_IndpVar_Pts_train_all.csv")
# write_csv(d4,"./results_train/16_IndpVar_Pts_train_noNA.csv")
# write_csv(d4_te,"./results_test/16_IndpVar_Pts_train_noNA.csv")