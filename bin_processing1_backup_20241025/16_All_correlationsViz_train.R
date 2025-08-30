# Jennifer Selgrath
# August 3, 2016
# Coral resilience to fishing impacts

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
d1<-read_csv("./results_train/14_IndpVar_Pts_train.csv")%>%
  glimpse()

########################
# Correlations between variables
cor(d1$z.sg_minDist,d1$z.mg_minDist)
cor(d1$z.PopRskDecay,d1$z.mg_minDist) # these are correlated


# 2010 variables -----------------
names(d1)

d.f<-d1%>%
  dplyr::select(z.allEffort2010:z.freqDest.Norm)
dfcor<-round(cor(d.f, use="complete"),2)
corrplot(dfcor, method="number", type = "lower")

# landscape
d.f2<-d1%>%
  dplyr::select(z.CoRuLngth:z.PopRskDecay)
dfcor2<-round(cor(d.f2, use="complete"),2)
corrplot(dfcor2, method="number")
corrplot(dfcor2)

# fishing variables -----------------
# all highly correlated
names(d1)
d.f3<-d1%>%
  dplyr::select(z.fYr00A:z.fYrLag50A)
dfcor3<-round(cor(d.f3, use="complete"),2)
corrplot(dfcor3, method="number")
corrplot(dfcor3)

# fishing var - other
d.f4<-d1%>%
  dplyr::select(z.fYrLag30A,z.dfYr00:z.dfYrLag40A)
dfcor4<-round(cor(d.f4, use="complete"),2)
corrplot(dfcor4, method="number")
corrplot(dfcor4)

# other - removed ones that were correlated >0.7
d.f5<-d1%>%
  dplyr::select(z.sg_minDist,z.co_minDist_100,z.PopRskDecay,z.fYrLag30A,z.cum_blast00,z.cum_FA_blast00,z.cum_kaykay00,z.cum_poison00,z.divGen2010,z.mpa_area_ha,z.Depth_m,z.CoRuArea,z.CoRuEdg2Area,z.FRAC,z.PROX)

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

