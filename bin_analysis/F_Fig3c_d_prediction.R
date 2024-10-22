# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# goal: graphing predictive ability of models with and without landscape variables
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
# load data 
d2<-read_csv("./results_test/m_final_test_data.csv")%>%
  glimpse()


P.u<-mean(d2$p_m_final)
P.sd<-sd(d2$p_m_final)
P.sem<-P.u/sqrt(length(d2$p_m_final))

P.stats<-cbind(P.u,P.sd,P.sem)
P.stats

# names(d2)


#########################
# loc2=("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch5/Resilience/doc/MixedEf1_Q51")
# setwd(loc2)


###########################################
# Graph with estimates

# full model
gg_full<-ggplot(data=d2, aes(x=p_m_final))+geom_histogram(binwidth = .02)+  ylab("Count")+
	xlab("Predicted Probability of Coral")+
	theme_classic() 
gg_full


gg_final_no_l<-ggplot(data=d2, aes(x=p_m_final_no_l ))+geom_histogram(binwidth = .02)+  ylab("Count")+
	xlab("Predicted Probability of Coral")+
	theme_classic() 
gg_final_no_l




#######################3
# graph box plot for pm13 and pm41
#########################
# reorganize data
names(d2)

d3<-d2%>%
	tidyr::gather(model,estimate,p_m_final:p_m_final_no_l ,factor_key=TRUE)%>%
  glimpse()


d3$MPA<-as.factor(d3$MPA)
d3$Reef_state<-as.factor(d3$Reef_state)
d3$State<-d3$Reef_state
d3$Ecological_zone<-as.factor(d3$Ecological_zone)
glimpse(d3)
head(d3)

# create missing level for coastal, rubble. Set y value above range of graph's y value

# temp1<-c("Coastal","0","0", "600",0,0,0,0,0,0,"p_m_final",5)
# temp2<-c("Coastal","0","0", "600",0,0,0,0,0,0,"p_m_final_no_l ",5)
# d4<-rbind(d3,temp1,temp2)
# tail(d4)
# str(d4)

# if no coastal
d4<-d3

# name for reef state
d4$Reef_state<-"Rubble"
d4$Reef_state[d4$State=="1"]<-"Coral"
d4$Reef_state<-as.factor(d4$Reef_state)

glimpse(d4)

# label for facet
d4$fct<-"" #a
# d4$fct[d4$model=="p_m_final_no_l "]<-"b"

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
# flabels<-c(p_m_final=" Full model", p_m_final_no_l =" Simplified model")
flabels<-c(p_m_final="", p_m_final_no_l ="")

#######################
# graphs
# setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/doc/")
ppi=300
tiff("./doc/Fig3c3d_.tiff", width = 3*ppi, height=4*ppi)

# full model
gg2<-ggplot(data=d4, aes(x=Ecological_zone, y=estimate, fill=Reef_state))+ geom_boxplot(colour="black", fatten = 1.2, size=1,outlier.size = 2,na.rm = F)+geom_text(data=d4,aes(x=3.2,y=1,label=fct),size=7) #PtID_orig Source

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








