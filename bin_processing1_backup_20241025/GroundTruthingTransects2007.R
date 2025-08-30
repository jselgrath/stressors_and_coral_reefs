# Jennifer Selgrath
# Histograms of ground truthing
# 20170105

#######################
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)

par(mfrow=c(1,1))
par(mar=c(3,3,3,3))

#################
remove(list=ls())
dateToday<-Sys.Date()
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/data/groundtruthing")

d1<-read.csv("LIsurveys2007.csv",header=T,sep=",", na.strings=c("NA","NaN", " ",""),stringsAsFactors = F) 
head(d1)

# remove errors
d2<-select(d1,Site,Transect,Meter,Habitat,FocArea)%>%
	filter(FocArea==1, Habitat!=".", Meter!=".",Meter!="a",Meter!="20T")

#make distance numeric
d2$Meter<-as.numeric(as.character(d2$Meter))
head(d2)
str(d2)
levels(as.factor(d2$Meter))
range(d2$Meter)

# categorize habitats
d2$Habitat2<-d2$Habitat
d2$Habitat2[d2$Habitat2=="ANEN"]<-"OTHER"
d2$Habitat2[d2$Habitat2=="BR"]<-"CORAL"
d2$Habitat2[d2$Habitat2=="CB"]<-"CORAL"
d2$Habitat2[d2$Habitat2=="CO"]<-"CORAL"
d2$Habitat2[d2$Habitat2=="DCA"]<-"RUB"   # or ag. Large influence on Ag/Rub dist depending on how plot because this is a big catgory
d2$Habitat2[d2$Habitat2=="FLC"]<-"CORAL"
d2$Habitat2[d2$Habitat2=="FO"]<-"CORAL"
d2$Habitat2[d2$Habitat2=="GUZ"]<-"SWF"
d2$Habitat2[d2$Habitat2=="MA"]<-"CORAL"
d2$Habitat2[d2$Habitat2=="CM"]<-"CORAL"

d2$Habitat2[d2$Habitat2=="POLYP"]<-"CORAL"
d2$Habitat2[d2$Habitat2=="RKC"]<-"RUB"
d2$Habitat2[d2$Habitat2=="RUB "]<-"RUB"
d2$Habitat2[d2$Habitat2=="SAA"]<-"SANDSILT"
d2$Habitat2[d2$Habitat2=="SA"]<-"SANDSILT"
d2$Habitat2[d2$Habitat2=="SI"]<-"SASI"
d2$Habitat2[d2$Habitat2=="SASI"]<-"SANDSILT"

d2$Habitat2[d2$Habitat2=="SC"]<-"OTHER"
d2$Habitat2[d2$Habitat2=="SCA"]<-"OTHER"
d2$Habitat2[d2$Habitat2=="SO"]<-"OTHER"
d2$Habitat2[d2$Habitat2=="SP"]<-"OTHER"

d2$Habitat2[d2$Habitat2=="SE"]<-"OTHER"
d2$Habitat2[d2$Habitat2=="MO"]<-"OTHER"

# order for graphing
d2$ordr<-6 # for SWF, which I get rid of below
d2$ordr[d2$Habitat2=="RUB"]<-2
d2$ordr[d2$Habitat2=="CORAL"]<-1
d2$ordr[d2$Habitat2=="OTHER"]<-5
d2$ordr[d2$Habitat2=="AG"]<-2
d2$ordr[d2$Habitat2=="CORAL"]<-1
d2$ordr[d2$Habitat2=="SANDSILT"]<-3
d2$ordr[d2$Habitat2=="SG"]<-4

# Names for graph
d2$Habitat2[d2$Habitat2=="RUB"]<-"Rubble"
d2$Habitat2[d2$Habitat2=="CORAL"]<-"Coral"
d2$Habitat2[d2$Habitat2=="OTHER"]<-"Other"
d2$Habitat2[d2$Habitat2=="AG"]<-"Rubble"
d2$Habitat2[d2$Habitat2=="CORAL"]<-"Coral"
d2$Habitat2[d2$Habitat2=="SANDSILT"]<-"Sand & Silt"
d2$Habitat2[d2$Habitat2=="SG"]<-"Seagrass"

levels(as.factor(d2$Habitat2))


# Fix dublicate transect names
filter(d2,Site=="Handumon MPA",Transect==1)
filter(d2,Site=="Handumon MPA",Transect==2)
filter(d2,Site=="Handumon MPA",Transect==3)
d2$Transect[d2$Site=="Handumon MPA"& d2$Transect==1]

tr1<-c(rep(1,39),rep(11,41)); tr1; length(tr1)
tr2<-c(rep(2,40),rep(21,41)); tr2; length(tr2)
tr3<-c(rep(3,40),rep(31,41)); tr3; length(tr3)

length(d2$Transect[d2$Site=="Handumon MPA"& d2$Transect==1])
d2$Transect[d2$Site=="Handumon MPA"& d2$Transect==1]<-tr1
d2$Transect[d2$Site=="Handumon MPA"& d2$Transect==2]<-tr2
d2$Transect[d2$Site=="Handumon MPA"& d2$Transect==3]<-tr3

# remove SWF
d2b<-filter(d2,Habitat2!="SWF")

# check
d3<-ddply(d2b,.(Site,Transect),summarize,
					pts=length(Meter));d3

head(d2b)
head(d3)
d4<-merge(d2b,d3);head(d4)

# order the varaibles sensically
d4$Habitat2<-reorder(as.factor(d4$Habitat2), as.numeric(d4$ordr), FUN=mean)
levels(d4$Habitat2)

# counts for habitats
d5<-ddply(d4,.(Site,Transect,Habitat2,pts),summarize, 
					habCount=length(Habitat2),
					pctCvr=round(habCount/max(pts)*100,0))%>%
	select(Site,Transect,Habitat2,pctCvr) # remove intermediate var

head(d5)

d5$cover<-"2M"
d5$cover[d5$pctCvr>70]<- "3H"
d5$cover[d5$pctCvr<30]<- "1L"
head(d5)
str(d5)
d5$cover<-as.factor(d5$cover)

#################
# Save table
# write.table(d5,file="TrsctPctCvr2007_FA.csv",sep=",",col.names=T,row.names = F)


###############
# graphing setup
deets2<- theme(
	plot.title=element_text(size=rel(3), colour="black"), 
	axis.text =  element_text(size=rel(1.75), colour="black"), 
	axis.title=element_text(size=rel(2.5),colour="black"), 
	panel.grid.minor=element_blank(), 
	panel.grid.major=element_line(colour = "white"),#element_blank(),
	panel.background=element_rect(fill="#f7f7f7"),
	panel.border=element_rect(fill=NA, colour = "black"),
	axis.line =  element_line(size=1),
	axis.ticks = element_line(size=1),
	strip.text = element_text(size = rel(2.5)),#,, colour = "orange", angle = 90
	legend.position="none")


# The palette with grey: (color blind)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###################
# graph

setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/doc/")
ppi=300
tiff(paste("GroundTruthing_PI_2007_",dateToday,"_%d.tiff",sep=""), width = (3)*ppi, height=(1.55)*ppi)

# with bins similar to bucket view 
g1<-ggplot(d5,aes(pctCvr, fill=cover))+geom_histogram(bins=3,breaks=c(0,30,70,100)) #binwidth=33.3)

g1+
	labs(title ="Point Intercept Transects")+
	xlab("Percent cover")+
	ylab("Count of observations")+
	facet_wrap(~Habitat2, ncol=5)+ #make label bigger
	deets2+
	scale_fill_manual(values=cbPalette,name="Habitat")+
	scale_x_continuous(breaks=seq(0,100,33))#,limits=c(0,13))

#bins of 10
g2<-ggplot(d5,aes(pctCvr, fill=cover))+geom_histogram(binwidth=10)

g2+
	labs(title ="Point Intercept Transects")+
	xlab("Percent cover")+
	ylab("Count of observations")+
	facet_wrap(~Habitat2, ncol=5)+ #make label bigger
	deets2+
	scale_fill_manual(values=cbPalette,name="Habitat")+
	scale_x_continuous(breaks=c(0,30,70,100))
										 	
	
dev.off()

