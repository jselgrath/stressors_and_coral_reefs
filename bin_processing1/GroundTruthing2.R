# Jennifer Selgrath
# Histograms of ground truthing
# 20170104

#######################
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(tidyr)

par(mfrow=c(1,1))
par(mar=c(3,3,3,3))

#################
remove(list=ls())
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/data/groundtruthing")

d1<-read.csv("GT_Combined_20100301_edited.csv",header=T,sep=",", na.strings=c("NA","NaN", " ",""),stringsAsFactors = F) 
head(d1)
# 

d2<-select(d1,X,Y,hab1=HABITAT1, hab2=HABITAT2,hab3=HABITAT3,hab1.l=COVER1, hab2.l=COVER2,method=METHOD,survey=SURVEY__,cluster=CLUSTER,location=LOCATION,date=DATE)%>%
	arrange(location) %>%
  filter(hab1!="DW",hab1!="MGR",method!=".") # remove algae farms, deep water,  mangroves, and unknown methods of data collection

head(d2)


#clean names (condense and fix errors)
levels(as.factor(d2$hab1)) # check levels
d2$hab1<-gsub("DCA","AG",d2$hab1) 
d2$hab1<-gsub("SA","SI",d2$hab1) 
d2$hab1<-gsub("SI","SASI",d2$hab1) 
d2$hab1<-gsub("SASISASI","SASI",d2$hab1)
# d2$hab1<-gsub("RUB","R",d2$hab1) 
# d2$hab1<-gsub("CCR","CC",d2$hab1) 
# d2$hab1<-gsub("DCA","AG",d2$hab1) 
levels(as.factor(d2$hab1)) # check levels

# method
levels(as.factor(d2$method))
d2$method<-gsub("Bucket View","bucketView",d2$method)
d2$method<-gsub("bucketviews","bucketView",d2$method)
d2$method<-gsub("side view","bucketView",d2$method)
d2$method<-gsub("Side View","bucketView",d2$method)
d2$method<-gsub("side view/ in","bucketView",d2$method)
d2$method<-gsub("/ in","",d2$method)
levels(as.factor(d2$method))

tail(d2)


#export this
write.table(d2,file="GTcombined_cleaned1.csv",sep=",",row.names = F,col.names = T)

##########
# NEXT STEP IN ARCGIS...
# in gis select only points within focal area (could do in R too)
# export this as .csv to same folder
