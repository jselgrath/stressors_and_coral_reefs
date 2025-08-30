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


########
# reduce to bucket view 
# NOTE: DONT USE THIS FOR PAPER - USE 2009 DATA ONLY BECAUSE HAS CONSISTENT H M L RANKINGS THAT MAKE IT BETTER SUITED FOR COMPARING TO PIT METHOD
##################################
remove(list=ls())
dateToday=Sys.Date()

setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/data/groundtruthing")

d1<-read.csv("GTcombined_cleaned1_FA.csv",header=T,sep=",", na.strings=c("NA","NaN", " ",""),stringsAsFactors = F) 
head(d1)

# bucketview - cluster of points 20m from other points. From 2009
d1$date<-gsub("/","_",d1$date)
dte<-d1$date
dte2<-colsplit(dte,"_", c('mo','dy','yr')) # split up month, day, year
head(dte2)
range(dte2$dy) #checking labelled correctly
dte2$yr<-gsub(" 0:00:00","",dte2$yr) #remove blank time code

d3<-cbind(d1,dte2)
head(d3)


d4<-	filter(d3,method=="bucketView")%>% # | method == "snorkel" #, yr == 2009 
	select(location,survey,cluster,hab1,hab2,hab3,hab1_l,hab2_l)
d4$hab3_l<-NA
d4$hab3_l[!is.na(d4$hab3)]<-"L"
head(d4)
d4$location<-as.factor(d4$location)
d4$survey<-as.factor(d4$survey)
d4$cluster<-as.factor(d4$cluster)
d4$ID<-row.names(d4)

head(d4)

# condense for graphing

d4a<-select(d4,ID,location,survey,cluster,hab1:hab3)

keycol<-"hab.ordr"
valuecol<-"habitat"
measurement<-c("hab1","hab2","hab3")

d5<-gather_(d4a,keycol, valuecol, measurement) #%>%
# arrange(ID) #location,survey,cluster,hab.ordr
head(d5)

d5[c(1:50),]

#again for level
d4b<-select(d4,ID,location,survey,cluster,hab1_l:hab3_l); head(d4b)
keycol2<-"cover.ordr"
valuecol2<-"cover"
measurement2<-c("hab1_l","hab2_l","hab3_l")

d6<-gather_(d4b,keycol2, valuecol2, measurement2) %>%
	select(cover)
head(d6)

d7<-cbind(d5,d6)
d7[c(1:50),]

d8<-na.omit(d7)
head(d8)
d8$location<-as.factor(as.character(d8$location))
d8[c(1:50),]

# only FA sites

# relabel
d8$location<-as.character(d8$location)
d8$location[d8$location=="Jagoliao?"]<-"Jagoliao"

levels(as.factor(d8$location))

#################
# Save table
write.table(d8,file="BucketViewAll_FA.csv",sep=",",col.names=T,row.names = F)

d8[c(1:50),]
##########
# rearrange for graphing
head(d8)

# rename
d8$habitat<-as.character(d8$habitat)
d8$habitat[d8$habitat=="AG"]<-"Rubble" # combine with rubble to match map
d8$habitat[d8$habitat=="CCR"]<-"Coral"
d8$habitat[d8$habitat=="RUB"]<-"Rubble"
d8$habitat[d8$habitat=="DCA"]<-"Rubble"
d8$habitat[d8$habitat=="SASI"]<-"Sand & Silt"
d8$habitat[d8$habitat=="SA"]<-"Sand & Silt"
d8$habitat[d8$habitat=="SG"]<-"Seagrass"

levels(as.factor(d8$habitat))

# # points per habitat
# d8b<-filter(d8,hab.ordr=="hab1") # select only main hab so no duplicates for points
d9b<-ddply(d8,.(habitat,cover),summarize,
					pts=length(cover))
head(d9b)
range(d9b$pts)
sum(d9b$pts) #437
d9b$pct<-round((d9b$pts/437)*100,0)

#########################
# set which dataset option to graph

d10<-d9b # for points per habitat
# d10<-d10a # points per cluster

######################
# order the varaibles sensically and make label
# levels L(<30%); M(31-70%); H (71-100%)
d10$cover<-reorder(as.factor(d10$cover), as.numeric(d10$ordr), FUN=mean)
levels(d10$cover)

d10$ordr<-1; d10$ordr[d10$cover=="M"]<-2;d10$ordr[d10$cover=="H"]<-3
d10$cover2<-"Low (<30%)"
d10$cover2[d10$cover=="M"]<-"Medium (31-70%)"
d10$cover2[d10$cover=="H"]<-"High (>70%)"
d10$cover2

d10$cover2<-reorder(as.factor(d10$cover2), as.numeric(d10$ordr), FUN=mean)
levels(d10$cover2)

head(d10)


###############
# for habitat from all surveys
d11<-d10
d11$obsv<-1
d11

###############
# graphing setup
deets2<- theme(
	plot.title=element_text(size=rel(3), colour="black"), 
	legend.title=element_text(size=rel(2.5), colour="black"),
	legend.text =  element_text(size=rel(2), colour="black"), 
	axis.text =  element_text(size=rel(1.75), colour="black"), 
	axis.title=element_text(size=rel(2.5),colour="black"), 
	panel.grid.minor=element_blank(), 
	panel.grid.major=element_line(colour = "white"),#element_blank(),
	panel.background=element_rect(fill="#f7f7f7"),
	panel.border=element_rect(fill=NA, colour = "black"),
	axis.line =  element_line(size=1),
	axis.ticks = element_line(size=1),
	strip.text = element_text(size = rel(2.5))#,, colour = "orange", angle = 90
	# legend.position="none"  # use legend for clusters
)

# The palette with grey: (color blind)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###################
# graph
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/doc/")
ppi=300
tiff(paste("GroundTruthing_BV_allYrs_",dateToday,"_%d.tiff",sep=""), width = (3)*ppi, height=(1.55)*ppi)

g1<- 	ggplot(d11,aes(x=cover,y=pts, fill=cover2))+geom_bar(position="dodge",stat = "identity") #binwidth=10,

g1+
	labs(title ="Spotcheck (Bucketview)")+
	xlab("Habitat cover")+
	ylab("Count of observations")+
	facet_grid(.~habitat)+
	deets2+ 
	scale_fill_manual(values=cbPalette,name="Habitat cover")


dev.off()