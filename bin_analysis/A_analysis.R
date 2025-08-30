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
library(MuMIn)
library(DHARMa)# residual diagnostics for heirarchical regression models
library(colorspace)
library(visreg)

# ------------------------------------------------------------------
# driver
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")


# ---------------------------------------------
# load data 
# ---------------------------------------------
d1<-read_csv("./results_train/16_IndpVar_Pts_train.csv")%>%
  mutate(MPA=as.factor(MPA), 
         ecological_zone=as.factor(ecological_zone),
         Depth_m=Depth_m*-1,
         Geomorphic2=as.factor(Geomorphic2))%>%
  glimpse()
  
# check depth representation # ---------------------------------------------
max(d1$Depth_m, na.rm=T)

# calc percent
d1%>%
  group_by(Reef_state)%>%
  summarize(n=n())%>%
  glimpse()



with(d1,xyplot(Reef_state~Depth_m|ecological_zone,type=c('g','p','l'),
layout=c(3,1), index.cond = function(x,y)max(y)))



# ---------------------------------------------
# centering variables based on mean
# from gelman and hill p 55
# https://www.r-bloggers.com/a-faster-scale-function/
# or substract by mean and divide by max (0-1)
# ---------------------------------------------

# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)

with(d1,plot(Depth_m,cum_FA_blast10))

# ---------------------------------------------
# models - small to make sure it works
# ---------------------------------------------
d1$depth_m_cs<-cs.(d1$Depth_m) # need this for visreg

m1<-lme4::glmer(Reef_state ~ depth_m_cs +(1|ecological_zone), # was depth - changing because log z score
					family=binomial(link=logit), data=d1)
summary(m1)
Anova(m1)
tab_model(m1, show.df = TRUE)
plot_model(m1, vline.color = "lightgrey") 

# graphs with visreg
visreg(m1,"depth_m_cs",by="ecological_zone", xlab="Depth",ylab="Reef State")




# another small model # ---------------------------------------------
m2<-lme4::glmer(Reef_state ~ depth_m_cs +MPA +(1|ecological_zone), 
                family=binomial(link=logit), data=d1)
summary(m2)
Anova(m2)
tab_model(m2, show.df = TRUE)
plot_model(m2, vline.color = "lightgrey") 


visreg(m2,"depth_m_cs",by="ecological_zone", xlab="Depth",ylab="Reef State")


# graphs to visually assess fit
# http://127.0.0.1:29737/library/sjPlot/doc/plot_model_estimates.html
# http://127.0.0.1:29737/library/sjPlot/doc/plot_interactions.html
# 2017 version: https://www.rdocumentation.org/packages/sjPlot/versions/2.4.1/topics/sjp.glmer
# The default is type = "fe", which means that fixed effects (model coefficients) are plotted.

# https://stackoverflow.com/questions/53193940/plotting-results-of-logistic-regression-with-binomial-data-from-mixed-effects-mo





# ---------------------------------------------
# FULL MODEL - log variables it is not sig dif using Anova, so removing
# ---------------------------------------------
m_all<-lme4::glmer(Reef_state ~ 
                     cs.(Depth_m) +
                     # cs.(sg_minDist_100)+
                     cs.(I(sg_minDist_100^2))+
                     cs.(-SHAPE)+
                     cs.(PopRskDecay.Nrm)+
                     cs.(fYrLag30A)+
                     cs.(cum_FA_blast10)+
                     cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                     cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                     MPA+
                     # Geomorphic2+
                     (1|ecological_zone), 
                family=binomial(link=logit), data=d1, na.action = "na.fail")

summary(m_all)
Anova(m_all)
tab_model(m_all, show.df = TRUE) 
plot_model(m_all, vline.color = "lightgrey") 

# ---------------------------------------------
# check model - all
# ---------------------------------------------
qqmath(m_all) 
qqmath(ranef(m_all))


# ---------------------------------------------
# OUTLIERS - all
# ---------------------------------------------
# table of residuals and IDs. use a cutoff of residuals ±1.96
# https://stats.stackexchange.com/questions/196724/how-to-identify-outliers-and-do-model-diagnostics-for-an-lme4-model
# https://stackoverflow.com/questions/24268031/unscale-and-uncenter-glmer-parameters?rq=1
# ---------------------------------------------
# view(cbind(residuals(m_all),d1$PtID2)) # outlier at PtID=1384
range(residuals(m_all,type="deviance"))

# ran this iteratively with m_all2 & d4
outliers<-data.frame(cbind(residuals(m_all,type="deviance"),d1$PtID2))%>%
  mutate(residuals=X1,
          PtID2=X2)%>%
  dplyr::select(-X1,-X2)%>%
  filter(abs(residuals)>2.5)%>%
  # filter(residuals<(-2))%>% #removed 3 more
  # filter(abs(residuals)>2)%>%
  arrange((residuals))%>% 
  glimpse()

outliers2<-outliers%>%
  left_join(d1)%>%
  glimpse()

# plot outliers - all places with coral
ggplot(outliers2,aes(x,y,color=as.factor(Reef_state),shape=Geomorphic2 ))+geom_point()
ggplot(outliers2,aes(x,y,color=Depth_m,shape=MPA ))+geom_point()


# remove large outliers --------------------------------------------------
d4<-d1%>%
  filter(PtID2!=131, PtID2!= 135, PtID2!= 127, PtID2!=1083, PtID2!= 1006, 
         PtID2!= 966, PtID2!= 993, PtID2!=43,PtID2!=161,PtID2!=126 ,PtID2!=1146,PtID2!=4140
         # , PtID2!=111, PtID2!=113, 
         )%>%
  glimpse()
  
m_all2<-lme4::glmer(Reef_state ~ 
                      cs.(Depth_m) +
                      # cs.(sg_minDist_100)+
                      cs.(I(sg_minDist_100^2))+
                      cs.(-SHAPE)+
                      cs.(PopRskDecay.Nrm)+
                      cs.(fYrLag30A)+
                      cs.(cum_FA_blast10)+
                      cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                      cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                      MPA+
                      # Geomorphic2+
                     (1|ecological_zone), 
                   family=binomial(link=logit), data=d4, na.action = "na.fail")

summary(m_all2)
Anova(m_all2)
tab_model(m_all2, show.df = TRUE) 
plot_model(m_all2, vline.color = "lightgrey") 
qqmath(m_all2) 

# ---------------------------------------------
# check model - all
# ---------------------------------------------
qqmath(m_all2) 
qqmath(ranef(m_all2))


# ---------------------------------------------
# Model selection ---------------------------------------
# ---------------------------------------------

# https://uoftcoders.github.io/rcourse/lec09-model-selection.html

# less than 2 are considered to be just as good as the top model and thus we shouldn’t just discount them

m_all_set<-dredge(m_all2, rank = "AICc")
head(m_all_set)


# Get top model --------------------
top_model <- get.models(m_all_set, subset = 1)[[1]]
top_model

# Get models with <2 delta AICc - all --------------------
top_m1 <- get.models(m_all_set,subset = delta<2)
top_m1

# average top models - all -------------------------
# The “full” coefficients are thus more conservative and it is best practice to interpret these
# m1_avg <- model.avg(top_m1,cumsum(weight) <= .95, fit = TRUE)
# cof<-data.frame(m1_avg$coefficients[1,])
# NOTE: TOP MODEL IS ONLY ONE WITH delta<2 - no need to average


# plot averaged model - all ----------------------
# tab_model(m1_avg, show.df = TRUE) 
# plot_model(m1_avg, vline.color = "lightgrey") 


# compared with and without log transformations - ns so removed from code above
# anova(m_all2,m_all2_nl) #ns





################################################
# --------------------------------------
# residual diagnostics 
# --------------------------------------
################################################

# calculates calculates randomized quantile residuals

# scaled residual value of 0.5 means that half of the simulated data are higher than the observed value, and half of them lower

# For a correctly specified model we would expect asymptotically 1) # a uniform (flat) distribution of the scaled residuals, 2) # uniformity in y direction if we plot against any predictor.


# --------------------------------------
# SET MODEL HERE - NEED TO MANUALLY UPDATE
# --------------------------------------


# set model - all ----------------------------
fittedModel<-m_all2
d_fitted<-d4



# --------------------------------------
# Simulations 
# --------------------------------------
# DHARMa: It is therefore highly recommended to first calculate the residuals once, using the simulateResiduals() function

simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F) 
# residuals(simulationOutput)
plot(simulationOutput) # By default, plotResiduals plots against predicted values


# against numeric predictors
plotResiduals(simulationOutput, form = cs.(d_fitted$Depth_m))
plotResiduals(simulationOutput, form = cs.(d_fitted$sg_minDist_100)) # not normal - maybe over dispersed?
plotResiduals(simulationOutput, form = cs.(d_fitted$PopRskDecay.Nrm))
plotResiduals(simulationOutput, form = cs.(d_fitted$fYrLag30A))
plotResiduals(simulationOutput, form = cs.(d_fitted$cum_FA_blast10)) # not normal - maybe over dispersed?
# plotResiduals(simulationOutput, form = cs.(log(d_fitted$CoRuEdg2Area+1)))
# against factors
plotResiduals(simulationOutput, form = (d_fitted$MPA))
# plotResiduals(simulationOutput, form = (d_fitted$Geomorphic2))


# test dispersions 
testDispersion(fittedModel)








# -------------------------------------------------------------
# old graphs
#sjp.glmer(m1, type = "re.qq") #graphing for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)
# plot_model(m1, type = "fe.pc") # sjp.glmer(m1, type = "fe.pc") #fixed effects
# sjp.glmer(m1, type = "ri.pc", facet.grid = FALSE) #random effects1
# sjp.glmer(m1,y.offset = .4,fade.ns=T) #random effects2
# sjp.glmer(m1,type="fe", expand.grid = TRUE, geom.colors = c("black","black"), axis.title= "Odds Ratio", fade.ns=T)
# ----------------------------------------------------------




################
# Final Models: Full model and model with no landscape variables
#################

# subset data, clean data names ----------------------------
d5<-d4%>%
  mutate(Depth=cs.(Depth_m)[,1], # [,1] calls it out of the matrix
         Seagrass_isolation=cs.(I(sg_minDist_100^2))[,1],
         # Seagrass_isolation=cs.(sg_minDist_100)[,1],
         # Patch_complexity=cs.(SHAPE)[,1],
         Patch_compactness=cs.(-SHAPE)[,1],
         Population_risk=cs.(PopRskDecay.Nrm)[,1],
         Fishing_legacy_1980_2000=cs.(fYrLag30A)[,1],
         Blast_fishing_2010_2000=cs.(cum_FA_blast10)[,1])%>%
  dplyr::select(
    Reef_state,Depth, Seagrass_isolation,Patch_compactness,Population_risk,
    Fishing_legacy_1980_2000,Blast_fishing_2010_2000,MPA,Ecological_zone=ecological_zone,x,y,PtID2)%>%
  glimpse()




# final full model ----------------------------
m_final<-lme4::glmer(Reef_state ~ 
                       Depth+
                       # Patch_complexity+
                       Patch_compactness+
                       Seagrass_isolation+
                       Population_risk+
                       Fishing_legacy_1980_2000+
                       Blast_fishing_2010_2000+
                       Population_risk:Fishing_legacy_1980_2000+
                       Population_risk:Blast_fishing_2010_2000+ 
                       MPA+
                       # Geomorphic+
                       (1|Ecological_zone), 
                     family=binomial(link=logit), data=d5, na.action = "na.fail")

summary   (m_final)
tab_model (m_final, show.df = TRUE) 
plot_model(m_final, vline.color = "lightgrey") 
t1<-tab_model (m_final, show.df = TRUE)%>%
  glimpse()

arm::display(m_final)

o1<-arm::display(m_final)
str(o1)

coef(m_final)  # estimated intercepts for each eco zone # see gelman and hill p303
fixef(m_final) #estimated averages over the eco zones # average coefficents
ranef(m_final) # group level errors for intercepts and slopes
# results of fixef and ranef add up to coef

# writm_final# write_csv(t1 ,"./doc/final_model_stats.csv")

# visualize aspects of final model ------------------------
visreg(m_final,"Depth","Ecological_zone", xlab="Depth",ylab="Reef State")
visreg(m_final,"Patch_compactness","Ecological_zone", xlab="Patch compactness",ylab="Reef State")
visreg(m_final,"Seagrass_isolation","Ecological_zone", xlab="Seagrass isolation",ylab="Reef State")
visreg(m_final,"Population_risk","Ecological_zone", xlab="Population_risk",ylab="Reef State")
visreg(m_final,"Fishing_legacy_1980_2000","Ecological_zone", xlab="Fishing_legacy_1980_2000",ylab="Reef State")
visreg(m_final,"Blast_fishing_2010_2000","Ecological_zone", xlab="Blast_fishing_2010_2000",ylab="Reef State")

# above, without landscape var (used this because landscape variables are usually not included in coral surveys. Used for analysis in paper, but not included here.)
m_final_no_landscape<-lme4::glmer(Reef_state ~ 
                       Depth+
                         # Patch_complexity+
                         # Patch_compactness+
                       # Seagrass_isolation+
                       Population_risk+
                       Fishing_legacy_1980_2000+
                       Blast_fishing_2010_2000+
                       Population_risk:Fishing_legacy_1980_2000+
                       Population_risk:Blast_fishing_2010_2000+ 
                       MPA+
                       # Geomorphic+
                       (1|Ecological_zone), 
                     family=binomial(link=logit), data=d5, na.action = "na.fail")

summary   (m_final_no_landscape)
tab_model (m_final_no_landscape, show.df = TRUE) 
plot_model(m_final_no_landscape, vline.color = "lightgrey") 
display(m_final_no_landscape)


# visualize aspects of final model, no landscape ------------------------
visreg(m_final_no_landscape,"Depth","Ecological_zone", xlab="Depth",ylab="Reef State")
visreg(m_final_no_landscape,"Population_risk","Ecological_zone", xlab="Population_risk",ylab="Reef State")
visreg(m_final_no_landscape,"Fishing_legacy_1980_2000","Ecological_zone", xlab="Fishing_legacy_1980_2000",ylab="Reef State")
visreg(m_final_no_landscape,"Blast_fishing_2010_2000","Ecological_zone", xlab="Blast_fishing_2010_2000",ylab="Reef State")


# ---------------------------------
# SAVE 
# ----------------------------------

# models --------------
save(m_all2,file="./results_train/mixedEf_final_all1.R") #same but uncleaned variable names 
save(m_final,file="./results_train/mixedEf_final_all.R") 
save(m_final_no_landscape,file="./results_train/mixedEf_final_no_landscape.R") 


#save image to set options for probabilities in GT
save.image("./results_train/mixedEf_final_all1.RData")

# save modified datasets -------------
# THESE ARE SIMILAR. d4 has all variables, d5 has updated names for clean final model - in d5 seagrass is exponated
d4a<-d4%>%select(-depth_m_cs)
write_csv(d4a,"./results_train/17_IndpVar_Pts_train_for_models_all.csv")
write_csv(d5,"./results_train/17_IndpVar_Pts_train_for_models_subset.csv")



