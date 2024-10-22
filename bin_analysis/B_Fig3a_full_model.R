# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal: figure 3a
# Effects are standardized
# -------------------------------------
library(tidyverse)
library(ggplot2)

# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------


# load data ------------------
d5<-read_csv("./results_train/15_IndpVar_Pts_train_for_models_subset.csv")

# load final model  ---------------
load("./results_train/mixedEf_final_all.R")

############################
# calculate se and CI######

# http://www.talkstats.com/showthread.php/25420-Confidence-Interval-for-model-parameters-(in-R-lme4)
summary(m_final)
tab_model (m_final, show.df = TRUE) 
models<-list(m_final)
models2<-c("m_final")
models3<-c( "(a) Multi-year fishing, interaction")

summary(m_final)
plot_model(m_final, vline.color = "lightgrey") 

# run this before loop
est<-data.frame()

# calculating lower and upper confidence interval using the sd of the estimate
# mean model estimate: coef(summary(models[[1]]))[,1]
# model sd: coef(summary(models[[1]]))[,2]
lower <- coef(summary(models[[1]]))[,1] + qnorm(.025)*coef(summary(models[[1]]))[,2]
upper <- coef(summary(models[[1]]))[,1] + qnorm(.975)*coef(summary(models[[1]]))[,2]

# combine
e1<-data.frame(cbind(coef(summary(models[[1]])), lower, upper))
e1$Parameter<-row.names(e1)
e1$model<-models2[1]
e1$grph<-models3[1]
est<-rbind(est,e1)

rownames(est) <- c()
est

est$grph<-as.factor(est$grph)
levels(est$grph)

est$Parameter<-as.factor(est$Parameter)
levels(est$Parameter)

est

#
# Make patch shape postive
# est$Estimate[est$Parameter=="Patch_complexity"]<-est$Estimate[est$Parameter=="Patch_complexity"]*-1
# est$lower[est$Parameter=="Patch_complexity"]<-est$lower[est$Parameter=="Patch_complexity"]*-1
# est$upper[est$Parameter=="Patch_complexity"]<-est$upper[est$Parameter=="Patch_complexity"]*-1

# order the varaibles sensically
est$ordr<--1*(c(1,2,4,3,8,6,7,5,9,10))

est$Parameter<-reorder(as.factor(est$Parameter), as.numeric(est$ordr), FUN=mean)
levels(est$Parameter)
dplyr::select(est,ordr, Parameter, model)

# create labels for graphs
est$labl<-as.character(est$Parameter)
est$labl<-gsub("Population_risk","Population density risk",est$labl)
est$labl<-gsub("MPAprotected","MPA (protected)",est$labl)
est$labl<-gsub("Seagrass_isolation","Seagrass isolation",est$labl)
est$labl<-gsub("Fishing_legacy_1980_2000","Fishing legacy, 1980-2000",est$labl)
est$labl<-gsub("Blast_fishing_2010_2000","Blast fishing, 2000-2010",est$labl)
est$labl<-gsub("GeomorphicReef Slope","Geomorphic (Reef slope)",est$labl)
est$labl<-gsub("Patch_complexity","Patch complexity",est$labl)



# check labels
est$labl

# make column for insignificant values
est$sig<-1
est$sig[est$labl == "Population density risk" & est$model=="m_final"]<-0
# est$sig[est$labl == "Fishing legacy, 1980-2000" & est$model=="m.me_52"]<-0
est$sig<-as.factor(est$sig)

est


# remove intercept for graph
est2<-filter(est,Parameter!="(Intercept)")



#############################
# graphing setup##############

deets2<- theme(
	axis.text =  element_text(size=rel(2.8), colour="black"), 
	axis.title=element_text(size=rel(3),colour="black"), 
	panel.grid=element_blank(), 
	# panel.grid.major=element_line(colour = "white"),#element_blank(),
	panel.background=element_rect(fill="#f7f7f7"),
	panel.border=element_rect(fill=NA, colour = "black"),
	# axis.line =  element_line(size=2),
	# axis.ticks = element_line(size=1),
	legend.position="none"
)


########################
# Graphing##############
ppi=300

# source("./bin_analysis/deets.R")
tiff("./doc/Fig3a_final_model.tiff", width = (4)*ppi, height=(2)*ppi)

# est2 doesn't have intercept
g1<- 	ggplot(data = est2, aes(x=Parameter, y = Estimate,ymin=lower, ymax=upper, colour=sig)) + geom_point(size=5)

g1+
	deets2+
	ylab("Standardized effect size")+
	xlab("Socio-economic        Biophysical")+
	ylim(c(-3,3))+
	geom_errorbar(position = position_dodge(width = 0.2), width = 0.2) +
	coord_flip() + #flips graph
	scale_x_discrete(breaks = est$Parameter, labels=est$labl )+
	geom_hline(yintercept=0, linetype=5)+
	scale_colour_manual(values=c("#bdbdbd", "black"))
# annotate("text", x=.6,y=.035, label="Biophysical", size=10) 


dev.off()

################
