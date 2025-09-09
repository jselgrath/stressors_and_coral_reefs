# Jennifer Selgrath
# August 3, 2016
# Coral resilience to fishing impacts

# GOAL: visualizing correlation of variables

# -------------------------------------------
library(corrplot)
library(ggplot2)
library(dplyr)
# -------------------------------------------

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------

# load data #
d0<-read_csv("./results_train/14_IndpVar_Pts_train.csv")%>%
  glimpse()

d0_te<-read_csv("./results_test/14_IndpVar_Pts_test.csv")%>%
  glimpse()

# 249 is NA for both
is.na(d0$cumulative_all_00)
is.na(d0$all_2010_nrmA)

# about 3 NAs
is.na(d0$pop_risk_dens_orig)
is.na(d0$pop_risk_dens_inhab)

# drop NAs -------------------------
d1<-d0%>%
  na.omit()%>% # loose ~ 12 points
  glimpse()

d1_te<-d0_te%>%
  na.omit()%>% # loose ~ 15 points
  glimpse()



# -- Correlations between variables ----------------------------------
cor(d1$point_dist_Seagrass,d1$point_dist_Mangrove)
cor(d1$pop_risk_dens_orig,d1$pop_risk_dens_inhab) # high
cor(d1$cumulative_all_00,d1$all_2010_nrmA) # same


#  pop risk variables -----------------
names(d1)

d.f<-d1%>%
  dplyr::select(pop_risk_dens_inhab:pop_risk_pop.nrm)
dfcor<-round(cor(d.f, use="complete"),2)
corrplot(dfcor, method="number", type = "lower")
corrplot(dfcor)
corrplot(dfcor, method="number", type = "lower")

# pop and pop_risk_inhabited cor at 0.59, others high


# landscape
d.f2<-d1%>%
  dplyr::select(patch_shape_index,patch_dist_to_coral_m:point_dist_Mangrove,river_distance.nrm)
dfcor2<-round(cor(d.f2, use="complete"),2)
corrplot(dfcor2, method="number", type = "lower")
corrplot(dfcor2)
# point and patch sg metrics are correlated. otherwise, not.
# river and distance to mangroves correlated

# fishing variables -----------------
names(d1)
d.f3<-d1%>%
  dplyr::select(all_1960_nrmA:lag_poison_50)
dfcor3<-round(cor(d.f3, use="complete"),2)
corrplot(dfcor3, method="number", type = "lower")
corrplot(dfcor3)
# blast fishing least correlated with other fishing variables

# other - removed ones that were correlated >0.7
d.f5<-d1%>%
  dplyr::select(Depth_m:y)

dfcor5<-round(cor(d.f5, use="complete"),2)
corrplot(dfcor5, method="number",type = "lower")
corrplot(dfcor5)
# population and mangroves negatively cor -0.88 & population and the coordinates (x with pop density original, y with population - inhab is ok with both)
# of this list, similar things are correlated(fishing lag), but otherwise only pop and mangroves are cor >0.7

# >.7 is threshold here

dfcor5_d<-data.frame(dfcor5)

# save
write_csv(dfcor5_d,"./doc/correlations_train_pt7_thresh.csv")

# 0.7 threshold. 
# fishing and dest fishing stop being highly correlated for 20+ years ago (e.g. fYr20A)

