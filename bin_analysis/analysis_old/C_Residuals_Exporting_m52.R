# Multiple stressors on coral reefs
# goal:  exporting residuals from final model

##########################
# library (arm)
# library(ggplot2)
# library(dplyr)

##########################
remove(list=ls())

loc=("C:/wd/Resilience/")
setwd(loc)

# load final models
load("./results/RS_only/Q41_model_52.R") #blast x pop
load("./results/RS_only/Q41_model_50.R") #blast x pop no landscape

# # load data 
d2<-read.csv("IndpVar_Ptsf_RS.csv",header = T)
d2$MPA<-as.factor(d2$MPA)
str(d2)


##############
# extract residuals from model
# http://r.789695.n4.nabble.com/R-lme4-package-Fitted-values-and-residuals-td812638.html
fitted(m.me_52) #m.me2b
resid(m.me_52)
sigma(m.me_52) #1
fixef(m.me_52)
ranef(m.me_52)

fit<-data.frame(cbind(resid(m.me_52),fitted(m.me_52)))
names(fit)<-c("resid","fitVal")
head(fit)

# plot
par(mar=c(4,4,4,4))
with(fit,plot(fit$resid~fit$fitVal))

#merge residuals with orig data
d2<-cbind(d2,fit)
plot(d2$resid~d2$fitVal)

names(d2)
d3<-dplyr::select(d2,x,y,PtID=CID,resid,fitVal)
head(d3)

# Identify outliers (bright spots and dark spots)
d3$BrightSpots<-0
d3$BrightSpots[d3$resid>2] <-1

d3$DarkSpots<-0
d3$DarkSpots[d3$resid<-2] <-1

# variable for any large residual
d3$lgresid<-0
d3$lgresid[d3$BrightSpots ==1]<-1
d3$lgresid[d3$DarkSpots ==1]<-1

# residuals that are not outliers
d3$smResid<-0
d3$smResid[d3$lgresid==0]<-1


#########
# save residuals

setwd(loc)
write.table(d3, file = "./results/RS_only/ModelResiduals_m52.csv",sep=",", col.names = T, row.names = F)

setwd("C:/wd/Resilience/bin/")

# Next Step: Make figure in ArcGIS that includes 3 maps depicting point locations and values from d3. 
# Maps: (1) Small residuals (2) Bright spots (3) Dark spots