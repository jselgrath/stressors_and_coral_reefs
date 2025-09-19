# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal: figure 3a
# Effects are standardized
# -------------------------------------
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(ggtext)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------


# load data ------------------
d2<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>%
  mutate(MPA=as.factor(mpa), 
         # Depth=Depth*-1,
         Ecological_zone=as.factor(ecological_zone)
         )%>%
  glimpse()

# load final model  ---------------
load("./results_train/mixedEf_final_all.R")
m_final

############################
# calculate se and CI######

# http://www.talkstats.com/showthread.php/25420-Confidence-Interval-for-model-parameters-(in-R-lme4)
summary(m_final)

tab_model (m_final, show.df = TRUE)
get_model_data(m_final,type = c("est"))

tab_model (m_final, show.df = TRUE) 

models<-list(m_final)
models2<-c("m_final")
models3<-c( "(a) Multi-year fishing, interaction")


# plots odds rations
summary(m_final)
plot_model(m_final, type = "est", vline.color = "lightgrey")  # Fixed effects # using sjPlot
plot_model(m_final, type = "re")  # Random effects
#   type = c("est", "re", "eff", "emm", "pred", "int", "std", "std2", "slope", "resid", "diag"),



# visualize aspects of final model ------------------------
# conditional plot - all of the terms in the model must be specified (or use the median as a default)
# By default, the reference value in visreg for a numeric variable is its mean, and for a factor its first, or reference, level

# contrast plot - consider relative changes, or as they are called in statistics, contrasts
# contrast models are better for logistic regression and random effect models 
# https://pbreheny.github.io/visreg/articles/web/contrast.html
visreg(m_final, "depth", "ecological_zone", type="contrast", ylab=expression(Delta*'Reef State'),
       points=list(col="#55555540", cex=0.25))
visreg(m_final,"psi2","ecological_zone", xlab="Patch looseness",type="contrast", ylab=expression(Delta*'Reef State'),         points=list(col="#55555540", cex=0.25))
visreg(m_final,"sg","ecological_zone", xlab="Seagrass isolation",type="contrast", ylab=expression(Delta*'Reef State'),         points=list(col="#55555540", cex=0.25))
visreg(m_final,"pr_inhab","ecological_zone", xlab="Population_risk",type="contrast", ylab=expression(Delta*'Reef State'),         points=list(col="#55555540", cex=0.25))
visreg(m_final,"fishing_30lag","ecological_zone", xlab="Fishing_legacy_1980_2000",type="contrast", ylab=expression(Delta*'Reef State'),         points=list(col="#55555540", cex=0.25))
visreg(m_final,"blast10","ecological_zone", xlab="Blast_fishing_2010_2000",type="contrast", ylab=expression(Delta*'Reef State'),         points=list(col="#55555540", cex=0.25))

# random effects
# v <- visreg(m_final, "depth", "ecological_zone", re.form=~(1|"ecological_zone"), plot=FALSE)
# plot(v, ylab="Reef State")

summary(m_final)
plot_model(m_final, vline.color = "lightgrey") 





# calculating lower and upper confidence interval using the sd of the estimate
# mean model estimate: coef(summary(models[[1]]))[,1]
# model sd: coef(summary(models[[1]]))[,2]
lower <- coef(summary(models[[1]]))[,1] + qnorm(.025)*coef(summary(models[[1]]))[,2]
lower
upper <- coef(summary(models[[1]]))[,1] + qnorm(.975)*coef(summary(models[[1]]))[,2]


# combine
est<-data.frame()
e1<-round(data.frame(cbind(coef(summary(models[[1]])), lower, upper)),2)
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


# probability 
lower_p<-round(plogis(fixef(m_final))+ qnorm(.025)*plogis(fixef(m_final))%>%glimpse(),2)
upper_p<-round(plogis(fixef(m_final))+ qnorm(.975)*plogis(fixef(m_final))%>%glimpse(),2)

# combine
est_p<-data.frame()
e1p<-data.frame(cbind(plogis(coef(summary(models[[1]]))), lower_p, upper_p))
# e1p<-data.frame(cbind(round(plogis(fixef(m_final)),2), lower_p, upper_p))
e1p$Parameter<-row.names(e1p)
e1p$model<-models2[1]
e1p$grph<-models3[1]
est_p<-rbind(est_p,e1p)
est_p

# use probability ------------------------
est<-est_p


# Make patch shape postive
# est$Estimate[est$Parameter=="Patch_complexity"]<-est$Estimate[est$Parameter=="Patch_complexity"]*-1
# est$lower[est$Parameter=="Patch_complexity"]<-est$lower[est$Parameter=="Patch_complexity"]*-1
# est$upper[est$Parameter=="Patch_complexity"]<-est$upper[est$Parameter=="Patch_complexity"]*-1

# order the varaibles sensically
# est$ordr<--1*(c(1,3,2,4,8,7,10,6,9,5))# pos to neg by data type
# est$ordr<--1*(c(1,3,2,4,7,6,8,5,10,9)) # pos to neg, interactions at bottom
# est$ordr<--1*(c(1,4,2,6,7,5,8,3,9)) # pos to neg, interactions at bottom
est$ordr<--1*(c(1,3,4,7,6,8,5,10,9)) # pos to neg, interactions at bottom

est_p$Parameter<-reorder(as.factor(est$Parameter), as.numeric(est$ordr), FUN=mean)
levels(est$Parameter)
dplyr::select(est,ordr, Parameter)

# create labels for graphs
est$labl<-as.character(est$Parameter)
est$labl<-gsub("depth","Depth",est$labl)
est$labl<-gsub("pr_inhab","Population density risk",est$labl)

est$labl<-gsub("MPAprotected","MPA (protected)",est$labl)
est$labl<-gsub("I(sg^2)","Distance to Seagrass<sup>2</sup>",est$labl)
est$labl<-gsub("sg","Distance to Seagrass",est$labl)
est$labl<-gsub("fishing_30lag","Fishing legacy, 1980-2000",est$labl)
est$labl<-gsub("blast10","Blast fishing, 2000-2010",est$labl)
# est$labl<-gsub("GeomorphicReef Slope","Geomorphic (Reef slope)",est$labl)
est$labl<-gsub("psi","Patch complexity",est$labl)

# est$labl<-gsub("Population density risk: Fishing legacy, 1980-2000","Fishing legacy, 1980-2000:Population density risk",est$labl)
est$labl<-gsub("pr_inhab:blast10","Blast fishing, 2000-2010:Population density risk",est$labl)

# check labels
est$labl

# make column for insignificant values
est$sig<-1
est$sig[est$Parameter =="pr_inhab"]<-0
est$sig[est$Parameter =="(Intercept)"]<-0  
# est$sig[est$labl == "Population density risk" & est$model=="m_final"]<-0
# est$sig[est$labl == "Fishing legacy, 1980-2000" & est$model=="m.me_52"]<-0
est$sig<-as.factor(est$sig)

est2<-est%>%arrange(ordr)


# remove intercept for graph
est2<-filter(est,Parameter!="(Intercept)")
est2$type=c("b","b","b","s","s","s","s","s","s")



########################
# Graphing##############
source("./bin_analysis/deets.R")

# ppi=300
# tiff("./doc/fig_3a_full_model.tif", width = (3)*ppi, height=(1.55)*ppi)

# est2 doesn't have intercept
g1<- 	ggplot(data = est2, aes(x=Parameter, y = Estimate,ymin=lower, ymax=upper, colour=type)) + geom_point(size=3)

g1+
	ylab("Standardized effect size")+
	xlab("")+ #Socio-economic    Biophysical
	ylim(c(-3,3))+
	geom_errorbar(position = position_dodge(width = 0.2), width = 0.2) +
	coord_flip() + #flips graph
	scale_x_discrete(breaks = est2$Parameter, labels=est2$labl)+
  deets4+
	geom_hline(yintercept=0, linetype=5)+
  scale_color_discrete_diverging(palette = "Berlin",nmax=5,order=c(1,2),name="Data Type")

ggsave("./doc/fig_3a_full_model.tif", width = 8, height=4)

################
