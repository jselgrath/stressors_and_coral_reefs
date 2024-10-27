# Multiple stressors on coral reefs
# goal: graphing predictive ability of models with and without landscape variables
####################################################################

##########################
remove(list=ls())

dateToday<-Sys.Date()

loc=("C:/wd/Resilience/")
setwd(loc)

# load data 
d2<-read.csv("./results/RS_only_test/Q51_data_test.csv", header=T,stringsAsFactors = F)
head(d2)
str(d2)

P.u<-mean(d2$Pm52)
P.sd<-sd(d2$Pm52)
P.sem<-P.u/sqrt(length(d2$Pm52))

P.stats<-cbind(P.u,P.sd,P.sem)
P.stats

# names(d2)


#########################
# loc2=("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch5/Resilience/doc/MixedEf1_Q51")
# setwd(loc2)


###########################################
# Graph with estimates

# full model
gg52<-ggplot(data=d2, aes(x=Pm52))+geom_histogram(binwidth = .02)+  ylab("Count")+
	xlab("Predicted Probability of Coral")+
	theme_classic() 
gg50<-ggplot(data=d2, aes(x=Pm50))+geom_histogram(binwidth = .02)+  ylab("Count")+
	xlab("Predicted Probability of Coral")+
	theme_classic() 

#######################3
# graph box plot for pm13 and pm41
#########################
# reorganize data
names(d2)

d3<-dplyr::select(d2,EcoZone2:Depth,Patch.shape,Market.proximity,Pm52,Pm50)%>%
	tidyr::gather(model,estimate,Pm52:Pm50,factor_key=TRUE)
str(d3)
d3$MPA<-as.factor(d3$MPA)
d3$State<-as.factor(d3$State)
d3$Reef.state<-as.factor(d3$Reef.state)
d3$EcoZone2<-as.factor(d3$EcoZone2)
str(d3)
head(d3)

# create missing level for coastal, rubble. Set y value above range of graph's y value

temp1<-c("Coastal","0","0", "600",0,0,0,0,0,0,"Pm52",5)
temp2<-c("Coastal","0","0", "600",0,0,0,0,0,0,"Pm50",5)
d4<-rbind(d3,temp1,temp2)
tail(d4)
str(d4)
d4$Fishing.legacy.1980.2000<-as.numeric(d4$Fishing.legacy.1980.2000)
d4$Seagrass.isolation<-as.numeric(d4$Seagrass.isolation)
d4$Depth<-as.numeric(d4$Depth)
d4$Patch.shape<-as.numeric(d4$Patch.shape)
d4$Market.proximity<-as.numeric(d4$Market.proximity)
d4$estimate<-as.numeric(d4$estimate)
str(d4)
range(d4$estimate)

# name for reef state
d4$Reef.state<-"Rubble"
d4$Reef.state[d4$State=="1"]<-"Coral"
d4$Reef.state<-as.factor(d4$Reef.state)

# label for facet
d4$fct<-"" #a
# d4$fct[d4$model=="Pm50"]<-"b"

# theme info
deets2<- theme(
	axis.text =  element_text(size=rel(3.2), colour="black"), 
	axis.title=element_text(size=rel(3.5),colour="black"), 
	panel.grid=element_blank(), 
	panel.background=element_rect(fill="#f7f7f7"),
	panel.border=element_rect(fill=NA, colour = "black"),
	strip.background = element_rect(colour=NA, fill=NA),
	strip.text = element_text(hjust=0.02,size=rel(3.2), colour="black"),
	legend.title = element_text(size=rel(3.5)),#, face="bold"
	legend.text = element_text(size=rel(3.2)))

# facet labels
# flabels<-c(Pm52=" Full model", Pm50=" Simplified model")
flabels<-c(Pm52="", Pm50="")

#######################
# graphs
# setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/doc/")
ppi=300
tiff(paste("./doc/Fig3c3d_",dateToday,".tiff",sep=""), width = 3*ppi, height=4*ppi)

# full model
gg2<-ggplot(data=d4, aes(x=EcoZone2, y=estimate, fill=Reef.state))+ geom_boxplot(colour="black", fatten = 1.2, size=1,outlier.size = 2,na.rm = F)+geom_text(data=d4,aes(x=3.2,y=1,label=fct),size=7) #PtID_orig Source

gg2+
	ylab("Predicted probability of living coral\n")+
	xlab("Ecological Zone")+
	coord_cartesian(ylim=c(-0.01,1.01))+ # set y lim to data not including missing level spaceholder
	theme_linedraw()+
	facet_grid(model~.,labeller=labeller(model=flabels))+
	scale_fill_manual(name="Mapped habitat",
										values=c("#a6bddb", "#8856a7"))+
										# black and white: values=c("#bdbdbd", "black"))+
	deets2+
	scale_x_discrete(breaks=c("Coastal", "Inner Reef", "Outer Reef"),labels = c("Coastal", "Inner\nreef", "Outer\nreef"))

dev.off()



# when indluded other models:
# less variability with m12, m13 predicts higher prob in places with coral

#############
#return wd
setwd("C:/wd/Resilience/bin/")


