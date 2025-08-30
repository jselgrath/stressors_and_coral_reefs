# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# correlation of variables

# GOAL: visualizing data
############################################################################
library(corrplot)
library(ggplot2)
library(dplyr)
##########################################################################

# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------

# load data #
d1<-read_csv("./results_train/13_IndpVar_Pts_all.csv")%>%  # changed to do after merging 
  glimpse()

########################
# Correlations between variables
cor(d1$sg_minDist,d1$mg_minDist)
cor(d1$PopRskDecay,d1$mg_minDist) # these are correlated


# 2010 variables -----------------
names(d1)

d.f<-d1%>%
  dplyr::select(allEffort2010:freqDest.Norm)
dfcor<-round(cor(d.f, use="complete"),2)
corrplot(dfcor, method="number", type = "lower")

# landscape
d.f2<-d1%>%
  dplyr::select(CoRuLngth:PopRskDecay)
dfcor2<-round(cor(d.f2, use="complete"),2)
corrplot(dfcor2, method="number")
corrplot(dfcor2)

# fishing variables -----------------
# all highly correlated
names(d1)
d.f3<-d1%>%
  dplyr::select(fYr00A:fYrLag50A)
dfcor3<-round(cor(d.f3, use="complete"),2)
corrplot(dfcor3, method="number")
corrplot(dfcor3)

# fishing var - other
d.f4<-d1%>%
  dplyr::select(fYrLag30A,dfYr00:dfYrLag40A)
dfcor4<-round(cor(d.f4, use="complete"),2)
corrplot(dfcor4, method="number")
corrplot(dfcor4)

# other - removed ones that were correlated >0.7
d.f5<-d1%>%
  dplyr::select(sg_minDist,co_minDist_100,PopRskDecay,fYrLag30A,cum_blast00,cum_FA_blast00,cum_kaykay00,cum_poison00,divGen2010,mpa_area_ha,Depth_m,CoRuArea,CoRuEdg2Area,FRAC,PROX)

dfcor5<-round(cor(d.f5, use="complete"),2)
corrplot(dfcor5, method="number")
corrplot(dfcor5)



# Dest fishing 00 is not correlated  with fishing >= 20 years
# dest fishing lag 10 or lag 20 is not correlated with fishign at 00.
# >.7 is threshold here


# save
write_csv(dfcor5,file="./doc/correlations_train.csv")

# 0.7 threshold. SIGen not sig correl. could use FreqAll OR FreqDest+FreqNonDest
# fishing and dest fishing stop being highly correlated for 20+ years ago (e.g. fYr20A)

#distance to MG and SG var


d.h<-dplyr::select(d1,sg_minDist:sgProx,mg_minDist:mgProx,PopRsk.Nrm,ENN)
dhcor<-round(cor(d.h, use="complete"),2)
dhcor
corrplot(dhcor)
# mean and min distance correlated, others independent

