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

# ------------------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ----------------------------------------------
# load final models
# ----------------------------------------------
load("./results_train/mixedEf_final_all.R") # full model
load("./results_train/mixedEf_final_no_landscape.R") # no landscape variables


# ---------------------------------------------
# load data and select non-correlated variables (see Correlation Viz)
# ---------------------------------------------
d1<-read_csv("./results_test/16_IndpVar_Pts_test.csv")%>%
  mutate(MPA=as.factor(MPA), 
         ecological_zone=as.factor(ecological_zone),
         Depth_m=Depth_m*-1, 
         Geomorphic2=as.factor(Geomorphic2))%>%
  
  # filter deep areas
  # filter(Depth_m<=6)%>%
  glimpse()

# check depth representation # ---------------------------------------------
max(d1$Depth_m, na.rm=T)

# calc percent
d1%>%
  group_by(Reef_state)%>%
  summarize(n=n())%>%
  glimpse()
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

with(d1,plot(Depth_m,cum_FA_blast10))

# ---------------------------------------------
# subset data, clean data names to match final model
# ---------------------------------------------
d5<-d1%>%
  mutate(Depth=cs.(Depth_m)[,1], # [,1] calls it out of the matrix
         Seagrass_isolation=cs.(I(sg_minDist_100^2))[,1],
         Patch_compactness=cs.(-SHAPE)[,1],
         Population_risk=cs.(PopRskDecay.Nrm)[,1],
         Fishing_legacy_1980_2000=cs.(fYrLag30A)[,1],
         Blast_fishing_2010_2000=cs.(cum_FA_blast10)[,1])%>%
  dplyr::select(
    Reef_state,Depth, Seagrass_isolation,Patch_compactness,Population_risk,
    Fishing_legacy_1980_2000,Blast_fishing_2010_2000,MPA,Ecological_zone=ecological_zone,x,y,PtID2)%>%
  glimpse()




# ----------------------------------------
# Re-Build model with new data
# ----------------------------------------
d5$p_m_final<- round(predict(m_final, newdata = d5, type = "response"),3)%>%
  glimpse()
qplot(d5$p_m_final)
 

d5$p_m_final_no_l<- round(predict(m_final_no_landscape, newdata = d5, type = "response"),3)%>%
  glimpse()
qplot(d5$p_m_final_no_l)


# from checking model from Sept meeting with SG - much worse than other models!
# d5$Pm11<- round(predict(m.me2b, newdata = d5, type = "response"),3)
glimpse(d5)


######################
# Save predicted results to examine in GIS
write_csv(d5,"./results_test/m_final_test_data.csv")



