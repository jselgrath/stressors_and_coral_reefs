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
  mutate(municipality=as.factor(Id_MunWtr)) %>%  # update name
glimpse()


d1_te<-read_csv("./results_test/14_IndpVar_Pts_test.csv")%>%
  mutate(municipality=as.factor(Id_MunWtr))%>%   # update name
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
  dplyr::select(point_id,patch_id,resilience_id,hab_reclass, ecological_zone,mpa_barangay, 
                mpa_id, mpa, mpa_status, mpa_area_ha, municipality, Depth_m, river_distance,
                point_dist_Coral, point_dist_Seagrass,point_dist_Mangrove, 
                geomorphology, patch_shape_index,
                pop_risk_dens_inhab, pop_risk_dens_orig,pop_risk_pop,
                all_2010_nrmA:blast_2010_nrmA,
                cumulative_all_00:cumulative_poison_50,
                lag_all_10:lag_all_50,
                x,y)%>%
  glimpse()

#test
d2_te<-d1_te%>%
  dplyr::select(point_id,patch_id,resilience_id,hab_reclass, ecological_zone,mpa_barangay, 
                mpa_id, mpa, mpa_status, mpa_area_ha, municipality, Depth_m, river_distance,
                point_dist_Coral, point_dist_Seagrass,point_dist_Mangrove, 
                geomorphology, patch_shape_index,
                pop_risk_dens_inhab, pop_risk_dens_orig,pop_risk_pop,
                all_2010_nrmA:blast_2010_nrmA,
                cumulative_all_00:cumulative_poison_50,
                lag_all_10:lag_all_50,
                x,y)%>%
  glimpse()




########################
# Save
write_csv(d2,"./results_train/16_IndpVar_Pts_train_all.csv")
write_csv(d2_te,"./results_test/16_IndpVar_Pts_train_all.csv")
