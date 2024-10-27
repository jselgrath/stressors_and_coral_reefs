# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal: make figure 3b - reduced model
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

# load final model - no landscape variables ---------------
load("./results_train/mixedEf_final_no_landscape.R") #reduced model
m_final_no_landscape


summary(m_final_no_landscape)
tab_model (m_final_no_landscape, show.df = TRUE)


##############
# calculate se and CI

# http://www.talkstats.com/showthread.php/25420-Confidence-Interval-for-model-parameters-(in-R-lme4)

models<-list(m_final_no_landscape)
models2<-"m_final_no_landscape"
# models3<-c("(c) Fishing legacy","(d) Market proximity"," (a) Fishing legacy, interaction"," (b) Market proximity, interaction")

# run this before loop
est<-data.frame()
# for (i in 1:1){
lower <- coef(summary(models[[1]]))[,1] + qnorm(.025)*coef(summary(models[[1]]))[,2]
upper <- coef(summary(models[[1]]))[,1] + qnorm(.975)*coef(summary(models[[1]]))[,2]
e1<-data.frame(cbind(coef(summary(models[[1]])), lower, upper))
e1$Parameter<-row.names(e1)
e1$model<-models2[1]
# e1$grph<-models3[i]
est<-rbind(est,e1)

rownames(est) <- c()
est

# est$grph<-as.factor(est$grph)
# levels(est$grph)

est$Parameter<-as.factor(est$Parameter)
levels(est$Parameter)

# order the varaibles sensically
est$ordr<--1*(c(1,2,3,6,4,5,7,8)) #
est
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



# check labels
est$labl

# make column for insignificant values
est$sig<-1
# est$sig[est$labl == "Population density"]<-0
# est$sig[est$labl == "Fishing legacy, 1980-2000:Population density"]<-0
est$sig[est$labl == "Fishing legacy, 1980-2000"]<-0
est$sig<-as.factor(est$sig)

# remove intercept for graph
est2<-filter(est,Parameter!="(Intercept)")

est2

###############
# graphing setup
deets2<- theme(
	axis.text =  element_text(size=rel(1.9), colour="black"), 
	axis.title=element_text(size=rel(2),colour="black"), 
	    panel.grid=element_blank(), 
			panel.background=element_rect(fill="#f7f7f7"),
			panel.border=element_rect(fill=NA, colour = "black"),
			axis.line =  element_line(size=2),
			# axis.ticks = element_line(size=1),
	# linewidth=1,
	legend.position="none"
			)


##########
# Graphing
ppi=300
tiff("./doc/Fig3b_final_model_no_landscape.tiff", width = (3)*ppi, height=(1.55)*ppi)

# est2 doesn't have intercept
g1<- 	ggplot(data = est2, aes(x=Parameter, y = Estimate,ymin=lower, ymax=upper, colour=sig)) + geom_point(size=5)

g1+
	deets2+
	ylab("Standardized effect size")+
	xlab("  Socio-economic                 Biophysical")+
	ylim(c(-3,3))+
	geom_errorbar(position = position_dodge(width = 0.2), width = 0.2) +
	coord_flip() +
	scale_x_discrete(breaks = est$Parameter, labels=est$labl )+
	geom_hline(yintercept=0, linetype=5)+
	scale_colour_manual(values=c("#bdbdbd", "black")) 


dev.off()


