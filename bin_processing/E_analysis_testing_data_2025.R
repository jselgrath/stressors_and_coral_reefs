# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(lattice)
library(boot)
library(sjPlot)
library(car)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------

# ----------------------------------------------
# load final models
# ----------------------------------------------
load("./results_train/model_full.R") #full model
load("./results_train/model_no_landscape.R") #full model, no landscape variables
#         


# ---------------------------------------------
# load data and select non-correlated variables (see Correlation Viz)
# ---------------------------------------------
d0<-read_csv("./results_test/16_IndpVar_Pts_test_all.csv")%>%
  mutate(Depth_m=if_else(Depth_m<0,Depth_m,-.1),   # just in case
         Depth_m=Depth_m*-1)%>%   # made depth positive for easier interpretation
  mutate(MPA=as.factor(mpa_id),     # make factors
         ecological_zone=as.factor(ecological_zone),
         ecological_zone2=as.factor(ecological_zone2), # not updated
         geomorphology=as.factor(geomorphology),
         Reef_state=resilience_id
  )%>%
  filter(!is.na(Depth_m))%>% # one point
  glimpse()

d0$ecological_zone

# check depth representation # ---------------------------------------------
range(d0$Depth_m)

# graph of depth
with(d0,xyplot(Reef_state~Depth_m|ecological_zone,type=c('g','p','l'),
               layout=c(4,1), index.cond = function(x,y)max(y)))

# check depth representation # ---------------------------------------------
max(d1$Depth_m, na.rm=T)
  
  # blast fishing in deep water - using to check for high blast fishing in shallow areas
  
d0%>%
    filter(Depth_m>12)%>%
    ggplot(aes(x,y,color=Depth_m,size=cumulative_blast_10))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2) #- one deep area is an atoll. fixed below
  
# remove NAs and make extreme depths shallower  - assuming spline errors
d1<-d0 %>%
    mutate(Depth_m=if_else(Depth_m>-15,Depth_m,-15))%>%#~8 deep outliers - probably misclassified in depth map. 
  glimpse()
    
d1<-data.frame(na.omit(d1))%>%# 27 NAs -  in population risk variables 
    glimpse()
  
d1%>%
    ggplot(aes(x,y,color=Reef_state))+geom_point()#+geom_label(aes(label = point_id), nudge_y=0.2)
  
  plot(d1$ecological_zone)

# calc percent
d1%>%
  group_by(Reef_state)%>%
  summarize(n=n())%>%
  glimpse()
#464 coral, 1019 - rubble = 31% coral in testing data

#############################
# centering variables based on mean
# from gelman and hill p 55
# https://www.r-bloggers.com/a-faster-scale-function/
# or substract by mean and divide by max (0-1)
##############################

# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)



# ---------------------------------------------
# subset data, clean data names to match final model
# ---------------------------------------------
# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)

# run cs fxn on variables
d1$depth<-as.numeric(cs. (d1$Depth_m))
d1$sg<-as.numeric(cs. (d1$point_dist_Seagrass))
d1$mg<-as.numeric(cs. (d1$point_dist_Mangrove))
d1$river<-as.numeric(cs. (d1$point_dist_river))
d1$psi<-as.numeric(cs. (d1$patch_shape_index))
d1$pr_orig<-as.numeric(cs. (d1$pop_risk_dens_orig))#pop_risk_dens_orig  #inhab
d1$pr_inhab<-as.numeric(cs. (d1$pop_risk_dens_inhab))
d1$fishing_30lag<-as.numeric(cs. (d1$lag_all_30))
d1$fishing_20lag<-as.numeric(cs. (d1$lag_all_20))
d1$fishing_10lag<-as.numeric(cs. (d1$lag_all_10))
d1$fishing10<-as.numeric(cs. (d1$cumulative_all_00))
d1$fishing20<-as.numeric(cs. (d1$cumulative_all_10))
d1$blast10<-as.numeric(cs. (d1$cumulative_blast_10))
d1$poison10<-as.numeric(cs. (d1$cumulative_poison_10))
d1$pr<-as.numeric(cs. (d1$pop_risk_pop))  # or point_dist_Mangrove or river_distance.nrm or river_distance
d1$psi2<- -1*d1$psi # patch complexity

# ---------------------------------------------
# precompute interactions
# ---------------------------------------------
# precompute
d1$sg2<-d1$sg^2
d1$pri_f30<-d1$pr_inhab*d1$fishing_30lag
d1$pri_b10<-d1$pr_inhab*d1$blast10

glimpse(d1)

# set to same name as fxn
d2<-d1

# ----------------------------------------
# Re-Build model with new data
# ----------------------------------------
d2$p_m_final<- round(predict(m_all2, newdata = d2, type = "response"),3)%>%
  glimpse()
qplot(d2$p_m_final)


d2$p_m_final_no_l<- round(predict(m_all3, newdata = d2, type = "response"),3)%>%
  glimpse()
qplot(d2$p_m_final_no_l)


glimpse(d2)


######################
# Save predicted results to examine in GIS
write_csv(d2,"./results_test/m_final_test_data.csv")



