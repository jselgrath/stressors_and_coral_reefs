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
library(colorspace)


# ------------------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ----------------------------------------------
# load data 
d2<-read_csv("./results_test/m_final_test_data.csv")%>%
  glimpse()

unique(d2$Ecological_zone )

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



#########################
# reorganize data
d3<-d2%>%
	tidyr::gather(model,estimate,p_m_final:p_m_final_no_l)%>% # ,factor_key=TRUE
  glimpse()


d3$MPA<-as.factor(d3$MPA)
d3$Reef_state<-as.factor(d3$Reef_state)
d3$State<-d3$Reef_state
d3$Ecological_zone<-as.factor(d3$Ecological_zone)
glimpse(d3)
head(d3)

# create missing level for coastal, rubble. Set y value above range of graph's y value

temp1<-c("Coastal",0,0,"p_m_final",0)
temp2<-c("Coastal",0,0,"p_m_final_no_l",0)
# create missing level for coastal, rubble. Set y value above range of graph's y value

d4<-d3%>%
  dplyr::select(Ecological_zone,State,Reef_state,model,estimate)%>%
  rbind(temp1,temp2)%>%
  mutate(Ecological_zone=factor(Ecological_zone),State=as.numeric(State),model=factor(model),estimate=as.numeric(estimate))%>%
  glimpse()

range(d4$estimate)


# if no coastal
# d4<-d3

# name for reef state
d4$Reef_state<-"Rubble"
d4$Reef_state[d4$State=="1"]<-"Coral"
d4$Reef_state<-as.factor(d4$Reef_state)

glimpse(d4)



#######################
# graphs

d4$fct<-""



source("./bin_analysis/deets.R")

ggplot(data=d4, aes(x=Ecological_zone, y=estimate, fill=Reef_state))+ geom_boxplot(colour="black", size=1,outlier.size = 2,na.rm = F)+ #fatten = 1.1, 
  geom_text(data=d4,aes(x=3.2,y=1,label=fct),size=2) +
  deets5+
	ylab("Predicted Probability of Living Coral\n")+
	xlab("Ecological Zone")+
	coord_cartesian(ylim=c(-0.01,1.01))+ # set y lim to data not including missing level spaceholder
	# facet_grid(model~.,labeller=labeller(model=flabels))+
  facet_grid(model~.,labeller=labeller(model=""))+
  scale_fill_discrete_diverging(palette = "Berlin",nmax=5,order=c(5,4),name="Mapped Habitat")+
										# values=c("#a6bddb", "#8856a7"))+
										# values=c("#bdbdbd", "black"))+ #black and white: 
	scale_x_discrete(breaks=c("Coastal", "Inner Reef", "Outer Reef"),labels = c("Coastal", "Inner\nReef", "Outer\nReef"))

ggsave("./doc/fig_3c_3d.tif", width = 6, height=8)








