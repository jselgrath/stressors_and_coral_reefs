# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Code to analyze the relationship between coral (Coral (1) vs. rubble (0)) and various threats and biophysical parameters.

# Mixed Effects Models
# Using forward stepping model because full model with all variables would not converge
# model with 1 random effect
# ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(lattice)
library(boot)
library(sjPlot)
library(car)

# ------------------------------------------------------------------
# driver
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ch5")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd_ch5/resilience_2024")


# load data 
d2<-read_csv("./results/IndpVar_Ptse_RS.csv")%>%
  mutate(MPA=as.factor(MPA),
         Reef.state=as.factor(Reef.state),
         Depth=Depth,
         z.Depth_m=z.Depth_m)%>%
  glimpse()

with(d2,xyplot(State~z.Depth_m|EcoZone2,type=c('g','p','l'),
layout=c(3,1), index.cond = function(x,y)max(y)))

#############################################
# models
m1<-lme4::glmer(State ~ z.Depth_m +(1|EcoZone2), # was z.depth - changing because log z score
					family=binomial(link=logit), data=d2)
summary(m1)

# graphs to visually assess fit
# http://127.0.0.1:29737/library/sjPlot/doc/plot_model_estimates.html
# http://127.0.0.1:29737/library/sjPlot/doc/plot_interactions.html
# 2017 version: https://www.rdocumentation.org/packages/sjPlot/versions/2.4.1/topics/sjp.glmer
# The default is type = "fe", which means that fixed effects (model coefficients) are plotted.


plot_model(m1, vline.color = "lightgrey") 
tab_model(m1, show.df = TRUE)

# old graphs
#sjp.glmer(m1, type = "re.qq") #graphing for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)
# plot_model(m1, type = "fe.pc") # sjp.glmer(m1, type = "fe.pc") #fixed effects
# sjp.glmer(m1, type = "ri.pc", facet.grid = FALSE) #random effects1
# sjp.glmer(m1,y.offset = .4,fade.ns=T) #random effects2
# sjp.glmer(m1,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)



# seagrass (sg) isolation
m2<-update(m1,~.+z.sg_minDist100) 
anova(m1,m2,test="Chi", REML = F) 
plot_model(m2, vline.color = "lightgrey") 
tab_model(m2, show.df = TRUE)

# graphs to visually assess fit
# sjp.glmer(m2, type = "ri.pc", facet.grid = FALSE) #random effects1
# sjp.glmer(m2, type = "fe.pc") #fixed effects
# sjp.glmer(m2,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

# Habitat Patch SHAPE	
m3<-update(m2,~.+z.SHAPE_1)
anova(m2,m3,test="Chi",REML=F)
Anova(m3)
plot_model(m3, vline.color = "lightgrey") 
tab_model(m3, show.df = TRUE)


# sjp.glmer(m3, type = "fe.pc") #fixed effects
# sjp.glmer(m3, type = "ri.pc", facet.grid = FALSE) #random effects1
# sjp.glmer(m3,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

#dist to towns # removed because not sig in final model
# m4<-update(m3,~.+z.distTown.km)
# summary(m4) #
# anova(m3,m4,test="Chi", REML = F) 
# plot_model(m4, vline.color = "lightgrey") 
# tab_model(m4, show.df = TRUE)

# graphs to visually assess fit
# sjp.glmer(m4, type = "fe.pc") #fixed effects
# sjp.glmer(m4, type = "ri.pc", facet.grid = FALSE) #random effects1
# sjp.glmer(m4,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

# population density risk
m5<-update(m3,~.+z.PopRsk.Nrm)
summary(m5) 
Anova(m5)
plot_model(m5, vline.color = "lightgrey") 
tab_model(m5, show.df = TRUE)

# sjp.glmer(m5, type = "fe.pc") #fixed effects
# sjp.glmer(m5, type = "ri.pc", facet.grid = FALSE) #random effects1
# sjp.glmer(m5,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

anova(m3,m5,test="Chi", REML = F)  # m5 better
# anova(m3,m6,test="Chi", REML = F)  # m6 better
# anova(m4,m5,test="Chi", REML = F) #adding just town or pop risk not significantly better

# distance to villages (Visayan: barangays) # not sig so did not update
# m7<-update(m5,~.+z.distBgy.km)
# anova(m4,m7,test="Chi", REML = F) 
# Anova(m7)
# anova(m5,m7,test="Chi") #no dif


# so far very little difference for geom zones so removed (note: previously tested this and no significant effect)

# fishing
m8<-update(m5,~.+z.fYr30A)
summary(m8) 
Anova(m8)
anova(m5,m8,test="Chi", REML = F)
tab_model(m8, show.df = TRUE)
plot_model(m8, vline.color = "lightgrey") 



# destructive fishing
m9<-update(m5,~.+z.dfYr00A) #30 not significant either
summary(m9) 
Anova(m9)
anova(m5,m9,test="Chi", REML = F)
plot_model(m9, vline.color = "lightgrey") 
tab_model(m9, show.df = TRUE)


#MPA
m10<-update(m5,~.+MPA)#+
summary(m10)
anova(m5,m10,test="Chi", REML = F) #marginally better
Anova(m10) #but significantly improves model
plot_model(m10, vline.color = "lightgrey") 
tab_model(m10, show.df = TRUE)

# distance to coral
m11<-update(m10,~.+z.co_minDist100) 
summary(m11)
anova(m5,m11,test="Chi", REML = F) 
Anova(m11)
tab_model(m11, show.df = TRUE)
plot_model(m11, vline.color = "lightgrey") 


##########
# #strong driver but is this a response or a symptom? what about deep coral that the shallow habitat maps did not include? That makes too much room for error...
# this throws off rest of models which otherwise have fairly consistent results, indicating collinearity or some other interaction (Zuur 2012) so not using...
############

#isolation from city market # ns so did not update
# m12<-update(m10,~.+z.distMarket.km) 
# summary(m12) #makes MPA totally ns so used m9 instead of m10
# anova(m9,m12,test="Chi")
# Anova(m12) 
# sjp.glmer(m12, type = "fe.pc") #fixed effects
# sjp.glmer(m12, type = "ri.pc", facet.grid = FALSE) #random effects1
# sjp.glmer(m12,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

###############
# In this section examine effects of variations on fishing pressure: short term, cumulative over 1-5 decades, cumulative with a 10 year lag, change in fishing pressure from 10 years ago to present. Examined effect for all fishing gears and for destructive fishing gears alone

# fishing  lag
m13<-update(m10,~.+z.fYrLag30A)
Anova(m13)
anova(m10,m13,test="Chi", REML = F)
summary(m13)
tab_model(m13, show.df = TRUE)
plot_model(m13, vline.color = "lightgrey") 



# fishing  change 2010 -2000
# m13a<-update(m10,~.+z.allDif_10_00)
# Anova(m13a)
# anova(m10,m13a,test="Chi", REML = F)
# summary(m13a)
# tab_model(m13a, show.df = TRUE)
# plot_model(m13a, vline.color = "lightgrey") 

# fishing  2010
m13b<-update(m10,~.+d2$z.fYr00A)
Anova(m13b)
anova(m10,m13b,test="Chi", REML = F)
summary(m13b)
tab_model(m13b, show.df = TRUE)
plot_model(m13b, vline.color = "lightgrey") 


# fishing with no lag
m14<-update(m10,~.+z.fYr30A)
anova(m10,m14,test="Chi")
anova(m13,m14,test="Chi")
Anova(m14) # fishing not quite significant
tab_model(m14, show.df = TRUE)
plot_model(m14, vline.color = "lightgrey") 

# destructive fishing with no lag
m14b<-update(m10,~.+z.dfYr30)  # was 30A not sure what the A is?
anova(m10,m14b,test="Chi")
Anova(m14b) 
tab_model(m14b, show.df = TRUE)
plot_model(m14b, vline.color = "lightgrey") 

# destructive fishing with lag
m14c<-update(m10,~.+z.dfYrLag30)
anova(m10,m14c,test="Chi")
anova(m14b,m14c,test="Chi")
Anova(m14c) 
tab_model(m14c, show.df = TRUE)
plot_model(m14c, vline.color = "lightgrey") 

# changes in dest fishing pressure 
m15<-update(m10,~.+z.destDif_10_00)  
summary(m15)
anova(m12,m15,test="Chi")
Anova(m15)


# 2010 dest  fishing pressure 
m15a<-update(m11,~.+z.dfYr00A)  #was 12
summary(m15a)
anova(m12,m15a,test="Chi")
Anova(m15a)
tab_model(m15a, show.df = TRUE)
plot_model(m15a, vline.color = "lightgrey") 

sjp.glmer(m15a, type = "fe.pc") #fixed effects
sjp.glmer(m15a, type = "ri.pc", facet.grid = FALSE) #random effects1
sjp.glmer(m15a,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
# significant...


# River
m16<-update(m13,~.+z.distRiver.km)  
summary(m16)
anova(m13,m16,test="Chi")
sjp.glmer(m16, type = "re.qq")
sjp.glmer(m16,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
#ns


# mangroves 
m17<-update(m13,~.+z.mg_minDist100) 
summary(m17) 
sjp.glmer(m17, type = "fe.pc") #fixed effects
sjp.glmer(m17, type = "ri.pc", facet.grid = FALSE) #random effects1
sjp.glmer(m17,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
# ns

###############
# interactions using fishing base model
################

# mpa*fishing
m18<-update(m13,~.+z.fYrLag30A:MPA) #using lag fishing
sjp.glmer(m18,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
summary(m18)
# mpa becomes ns and big interactive effect...
# this consistently happens in all models

# river*MPA  #ns
m19<-update(m13,~.+z.distRiver.km*MPA)
sjp.glmer(m19,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
summary(m19) 

#river*fishing
m20<-update(m13,~.+z.distRiver.km*z.fYrLag30A)
sjp.glmer(m20,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
summary(m20) 
sjp.glmer(m20, type = "fe.pc") #fixed effects
# River and fishing is positive, sig

# mpa*fishing with river*fishing
# m21<-update(m20,~.+MPA:z.fYrLag30A) 
# sjp.glmer(m21,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

# pop risk and mpa = ns
m22<-update(m13,~.+z.PopRsk.Nrm:MPA)
sjp.glmer(m22,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
#ns

# pop risk and fishing lag
m23<-update(m13,~.+z.PopRsk.Nrm:z.fYrLag30A)
sjp.glmer(m23,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

# pop risk and fishing 
m23b<-update(m14,~.+z.PopRsk.Nrm:z.fYr30A)
sjp.glmer(m23b,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
anova(m23b,m23,test="Chi")

# pop risk and rivers
m24<-update(m13,~.+z.PopRsk.Nrm*z.distRiver.km)
sjp.glmer(m24,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

# pop*town #ns
m25<-update(m13,~.+z.PopRsk.Nrm*z.distTown.km)
sjp.glmer(m25,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
summary(m25)

anova(m20,m23,m24,m25,test="Chi")
anova(m13,m20,test="Chi")

# anova(m35,m24,test="Chi")
anova(m25,m24,test="Chi")




#######################
#  INTERACTIONS
# pop risk*market
m33<-update(m12,~.+z.PopRsk.Nrm:z.distMarket.km) 
sjp.glmer(m33,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
#ns

# river*market interaction
m34<-update(m12,~.+z.distRiver.km*z.distMarket.km) 
sjp.glmer(m34,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
#far from market and rivers less likely to have corals... odd..

# river*popRsk
m35<-update(m12,~.+z.distRiver.km*z.PopRsk.Nrm) 
sjp.glmer(m35,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
summary(m35)
#far from pop rsk and rivers less likely to have corals

# market*MPA #ns
m36<-update(m12,~.+z.distMarket.km*MPA) 
sjp.glmer(m36,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)



#############################
# In this section examine effects of variations on specific destructive fishing gears: blast fishing (fishing with explosives) and kaykay (fishing for abalone using a crowbar-like tool). 
# poison sample size is too small for this analysis so not included.
# time periods: short term, cumulative over 1-5 decades, cumulative with a 10 year lag, change in fishing pressure from 10 years ago to present.


############################
# blast - 2010 + 2000 is most informative
m50a<-update(m23,~.+z.cum_blast00) # significant
m50b<-update(m23,~.+z.cum_blast10) # most informative
m50c<-update(m23,~.+z.cum_blast20)
m50d<-update(m23,~.+z.cum_blast30)

anova(m50a,m50b,m50c,m50d,test="Chi")
anova(m23,m50b,test="chi")
Anova(m50b)

#no effect of lag 10, 20,30
m50f<-update(m23,~.+z.lag_blast30) 
Anova(m50f)
anova(m50b,m50f,test="chi")

# effect of difference
m50g<-update(m23,~.+z.dif_blast10_00) # ns 
Anova(m50g)

# best is m50b
summary(m50b)
sjp.glmer(m50b,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

# kaykay
m51a<-update(m23,~.+z.cum_kaykay00) # these are significant but positive
Anova(m51a)
m51b<-update(m23,~.+z.cum_kaykay10)
Anova(m51b)
m51c<-update(m23,~.+z.cum_kaykay20)
m51d<-update(m23,~.+z.cum_kaykay30) 
anova(m51a,m51b,m51c,test="Chi") #m51d,
Anova(m51a)
Anova(m51b)
sjp.glmer(m51c,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

anova(m51a,m50b,test="Chi") # blast more informative than kaykay, plus may be higher because moving into coral areas?

#fishing and pop interactions
m52a<-update(m13,~.+z.cum_kaykay00*z.cum_blast10) 
m52b<-update(m13,~.+z.cum_blast10:z.fYrLag30A+z.cum_blast10) 
m52c<-update(m13,~.+z.cum_blast10+z.cum_blast10:z.PopRsk.Nrm)
m52d<-update(m13,~.+z.cum_blast10+z.cum_blast10:MPA)
m52e<-update(m23,~.+z.cum_blast10+MPA:z.fYrLag30A)
m52f<-update(m23,~.+z.cum_blast10+z.cum_blast10:z.PopRsk.Nrm)
m52g<-update(m23,~.+z.cum_blast10+z.cum_blast10:MPA)

anova(m23,m50b,m52f)
anova(m50b,m52f,test="chi") #(m52a:e,g -> m52f always better, though often 0df in comparison so not enough power. Thus using simpler model - m50b)
anova(m23,m50b,m52a,m52b,m52c,m52d,m52e,m52f,m52g,test="Chi") 
# 50b, 52b,c, d, and f are most informative. 
# m50b is only one that has low AIC and > 0 df.: USE THIS ONE


sjp.glmer(m52f,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
# b - nothing with fishing sig



################
# Final Models: Full model and model with no landscape variables
#################
# fishingLag*PopRisk with blast* pop
m.me_52<-glmer(Reef.state~
							 	Fishing.legacy.1980.2000*Population.density+
							 	z.cum_blast10*Population.density+
							 	MPA+
							 	Patch.shape+
							 	Seagrass.isolation+
							 	Depth+
							 	(1|EcoZone2),family=binomial(link=logit), data=d2)
sjp.glmer(m.me_52,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)

# above, without landscape var (used this because landscape variables are usually not included in coral surveys. Used for analysis in paper, but not included here.)
m.me_50<-glmer(Reef.state~
							 	Fishing.legacy.1980.2000*Population.density+
							 	z.cum_blast10*Population.density+
							 	MPA+
							 	# Patch.shape+
							 	# Seagrass.isolation+
							 	Depth+
							 	(1|EcoZone2),family=binomial(link=logit), data=d2)
summary(m.me_50)



save(m.me_52,file="./results/RS_only/Q41_model_20161109_mixedEf1_52.R") # m23 with blast*pop #m52f from models above
save(m.me_50,file="./results/RS_only/Q41_model_20161109_mixedEf1_50.R") # m53 w/out landcape var


#save image to set options for probabilities in GT
save.image("C:/wd/Resilience/results/Q41_analysis_mixedEf1_blast.RData")



#############################################
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/Resilience/bin/")


