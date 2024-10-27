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

# ------------------------------------------------------------------
# driver
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ---------------------------------------------
# load data and select non-correlated variables (see Correlation Viz)
# ---------------------------------------------
d0<-read_csv("./results_train/14_IndpVar_Pts_train.csv")%>%
  
  # remove outlier - removing other ones below
  filter(PtID2!=1384)%>%
  
  filter(Depth_m>-6)%>%
  # reassigning geomorphology based on depth
  mutate(Geomorphic2=if_else(-2.5>Depth_m,"Reef Slope",Geomorphic))%>%
  mutate(Geomorphic2=if_else(-.5<Depth_m,"Reef Flat",Geomorphic2))%>%
  
  # removing coastal zone -------------------
  filter(EcoZone2!="Coastal")%>%
  glimpse()

d1<-d0%>%
  mutate(distance_mg2=if_else(distance_mg=="250" | distance_mg=="500" | distance_mg== "750","250-750", distance_mg))%>% # condense mg categories based on graphs below
  
  mutate(MPA=as.factor(if_else(mpa=="unprotected","open",mpa)),
         longitude=as.factor(longitude),
         ecological_zone=as.factor(EcoZone2),
         municipality=as.factor(Id_MunWtr),
         Depth_m=Depth_m*-1, #)%>%
         Reef_state=as.factor(Id_resil),
         Geomorphic=as.factor(Geomorphic),
         Geomorphic2=as.factor(Geomorphic2))%>%
  mutate(distance_sg=factor(distance_sg),
         distance_mg=factor(distance_mg),
         distance_mg2=factor(distance_mg2)
  )%>%
  glimpse()

# check new assignment of reef flat etc
cbind(d1$Geomorphic,d1$Geomorphic2,d1$Depth_m)

# graph updated variable ------------------
ggplot(d1,aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()

d1%>%
  filter(Geomorphic2=="Reef Slope")%>%
  ggplot(aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()

d1%>%
  filter(Geomorphic2=="Reef Flat")%>%
  ggplot(aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()



range(d1$Depth_m,na.rm=T)

unique(d0$EcoZone2)
d1$EcoZone2[d1$EcoZone2=="Terrestrial Island"]<-"Coastal"
unique(d1$EcoZone2)

with(d1,xyplot(Id_resil~Depth_m|EcoZone2,type=c('g','p','l'),
               layout=c(3,1), index.cond = function(x,y)max(y)))


# check depth representation # ---------------------------------------------
min(d1$Depth_m, na.rm=T)


# ---------------------------------------------
# subset data and make factors
# ---------------------------------------------
d2a<-d1%>%
    dplyr::select(PtID2,Reef_state,ecological_zone,longitude, mpa_barangay,MPA,mpa_area_ha,municipality, mpa_area_ha,Depth_m,sg_minDist_100,mg_minDist_100,co_minDist_100, distance_sg,distance_mg, distance_mg2,PopRskDecay, PopRskDecay.Nrm,fYrLag30A, cum_blast00, cum_FA_blast00,cum_FA_blast10,cum_kaykay00, cum_poison00,divGen2010,CoRuArea,CoRuEdg2Area,FRAC,PROX,SHAPE, PARA,CoRuEdg2Area,Geomorphic,Geomorphic2,x,y)%>%
  glimpse()

d2<-na.omit(d2a)




# ---------------------------------------------
# further subset, remove outliers
# ---------------------------------------------
d3<-d2%>%
  dplyr::select(PtID2,Reef_state,ecological_zone,MPA,Depth_m,sg_minDist_100,mg_minDist_100,distance_sg,distance_mg, distance_mg2,PopRskDecay, PopRskDecay.Nrm,fYrLag30A, cum_FA_blast10,cum_kaykay00, cum_poison00,divGen2010,CoRuArea,CoRuEdg2Area,Geomorphic,Geomorphic2,SHAPE,PARA,x,y)%>%
  
  # filter(PtID2!=1109 &PtID2!=956&PtID2!=944&PtID2!=1112 &PtID2!=957&PtID2!=1097&PtID2!=987)%>%
  # filter(PtID2!=1144,PtID2!=1127,PtID2!= 4096,PtID2!= 968,PtID2!= 155,PtID2!= 4093, PtID2!= 4095, PtID2!= 4090, PtID2!= 679, PtID2!= 1111,PtID2!= 954)%>%
  

  
  glimpse()



# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)

with(d3,plot(Depth_m,cum_FA_blast10))


# check centering and scaling - .cs better than z. scores because filtered data -----------
t0<-cbind(cs.(d1$Depth_m),d1$z.Depth_m)
t0
colMeans(t0)
apply(t0,2,sd)


# ---------------------------------------------
# models - small to make sure it works
# ---------------------------------------------
m1<-lme4::glmer(Reef_state ~ cs.(Depth_m) +(1|ecological_zone), # was depth - changing because log z score
					family=binomial(link=logit), data=d3)
summary(m1)
Anova(m1)
tab_model(m1, show.df = TRUE)
plot_model(m1, vline.color = "lightgrey") 



# another small model # ---------------------------------------------
m2<-lme4::glmer(Reef_state ~ cs.(Depth_m) +MPA +(1|ecological_zone), 
                family=binomial(link=logit), data=d3)
summary(m2)
Anova(m2)
tab_model(m2, show.df = TRUE)
plot_model(m2, vline.color = "lightgrey") 



# another small model # ---------------------------------------------
m3<-lme4::glmer(Reef_state ~ cs.(Depth_m) +MPA +distance_sg+(1|ecological_zone), 
                family=binomial(link=logit), data=d3)
summary(m3)
Anova(m3)
tab_model(m3, show.df = TRUE)
plot_model(m3, vline.color = "lightgrey") 

# another small model # ---------------------------------------------
m4<-lme4::glmer(Reef_state ~ cs.(Depth_m) +MPA +distance_mg2+(1|ecological_zone), 
                family=binomial(link=logit), data=d3)
summary(m4)
Anova(m4)
tab_model(m4, show.df = TRUE)
plot_model(m4, vline.color = "lightgrey") 



# graphs to visually assess fit
# http://127.0.0.1:29737/library/sjPlot/doc/plot_model_estimates.html
# http://127.0.0.1:29737/library/sjPlot/doc/plot_interactions.html
# 2017 version: https://www.rdocumentation.org/packages/sjPlot/versions/2.4.1/topics/sjp.glmer
# The default is type = "fe", which means that fixed effects (model coefficients) are plotted.

# https://stackoverflow.com/questions/53193940/plotting-results-of-logistic-regression-with-binomial-data-from-mixed-effects-mo







# ---------------------------------------------
# FULL MODEL - log variables it is not sig dif using Anova, so removing.  -----------------------------------
# ---------------------------------------------
m_all<-lme4::glmer(Reef_state ~ 
                     cs.(Depth_m) +
                     cs.(I(sg_minDist_100^2))+
                     cs.(SHAPE)+
                     cs.(PopRskDecay.Nrm)+
                     cs.(fYrLag30A)+
                     cs.(cum_FA_blast10)+
                     cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                     cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                     MPA+
                     Geomorphic2+
                     (1|ecological_zone), 
                family=binomial(link=logit), data=d3, na.action = "na.fail")

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
# view(cbind(residuals(m_all),d3$PtID2)) # outlier at PtID=1384
range(residuals(m_all,type="deviance"))

# ran this iteratively 
outliers<-data.frame(cbind(residuals(m_all,type="deviance"),d3$PtID2))%>%
  mutate(residuals=X1,
          PtID2=X2)%>%
  select(-X1,-X2)%>%
  filter(abs(residuals)>2.5)%>%
  # filter(residuals<(-2))%>% #removed 3 more
  # filter(abs(residuals)>2)%>%
  arrange((residuals))%>% 
  glimpse()

outliers2<-outliers%>%
  left_join(d3)%>%
  glimpse()

# plot outliers - all places with coral
ggplot(outliers2,aes(x,y,color=as.factor(Reef_state),shape=Geomorphic2 ))+geom_point()
ggplot(outliers2,aes(x,y,color=Depth_m,shape=MPA ))+geom_point()


# remove large outliers --------------------------------------------------
d4<-d3%>%
  filter(PtID2!= 966, PtID2!= 993, PtID2!= 1006, PtID2!=131   , PtID2!= 135, PtID2!= 127, PtID2!=161,
         PtID2!=4140, PtID2!=111, PtID2!=113, PtID2!=126, PtID2!=43, PtID2!=1083)%>%
  glimpse()
  
m_all2<-lme4::glmer(Reef_state ~ 
                      cs.(Depth_m) +
                      cs.(I(sg_minDist_100^2))+
                      cs.(SHAPE)+
                      cs.(PopRskDecay.Nrm)+
                      cs.(fYrLag30A)+
                      cs.(cum_FA_blast10)+
                      cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                      cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                      MPA+
                      Geomorphic2+
                     (1|ecological_zone), 
                   family=binomial(link=logit), data=d4, na.action = "na.fail")

summary(m_all2)
Anova(m_all2)
tab_model(m_all2, show.df = TRUE) 
plot_model(m_all2, vline.color = "lightgrey") 

# ---------------------------------------------
# check model - all
# ---------------------------------------------
qqmath(m_all2) 
qqmath(ranef(m_all2))



# Model selection ---------------------------------------
########this is effectively the same as the full model so using the full model######

m_all_set<-dredge(m_all, rank = "AICc")
m_all_set


# Get models with <10 delta AICc - all --------------------
top_m1 <- get.models(m_all_set,subset = delta<10)
top_m1

# average top models - all -------------------------
m1_avg <- model.avg(top_m1)
m1_avg

# plot averaged model - all ----------------------
tab_model(m1_avg, show.df = TRUE) 
plot_model(m1_avg, vline.color = "lightgrey") 



########best model (without coastal locations is without seagrass, though it is included in the avg model above)
# --------------------------------------

# model with reduced variables based on dredging
m_all3<-lme4::glmer(Reef_state ~ 
                      cs.(Depth_m) +
                      cs.(Ics.(Depth_m) +
                      # cs.(I(sg_minDist_100^2))+
                      cs.(SHAPE)+
                      cs.(PopRskDecay.Nrm)+
                      cs.(fYrLag30A)+
                      cs.(cum_FA_blast10)+
                      cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                      cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                      MPA+
                      Geomorphic2+(sg_minDist_100^2))+
                      cs.(SHAPE)+
                      cs.(PopRskDecay.Nrm)+
                      cs.(fYrLag30A)+
                      cs.(cum_FA_blast10)+
                      cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                      cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                      MPA+
                      Geomorphic2+
                     (1|ecological_zone), 
                   family=binomial(link=logit), data=d4, na.action = "na.fail")



summary(m_all3)
Anova(m_all3)
tab_model(m_all3, show.df = TRUE) 
plot_model(m_all3, vline.color = "lightgrey") 

# compare with and without log transformations - ns so removed from code above
# anova(m_all2,m_all2_nl) #ns


# --------------------------------------
# two models for reef flat and reef slope
# --------------------------------------

# --------------------------------------
# reef flat 
# --------------------------------------
d_flat<-d4%>%
  filter(Geomorphic2=="Reef Flat")%>%
  glimpse() 

range(d_flat$Depth_m) # 0-2.5
with(d_flat,plot(Depth_m,cum_FA_blast10))


m_f<-lme4::glmer(Reef_state ~ 
                   cs.(Depth_m) +
                   cs.(I(sg_minDist_100^2))+
                   cs.(SHAPE)+
                   cs.(PopRskDecay.Nrm)+
                   cs.(fYrLag30A)+
                   cs.(cum_FA_blast10)+
                   cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                   cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                   MPA+
                   # Geomorphic2+
                   (1|ecological_zone), 
                   family=binomial(link=logit), data=d_flat, na.action = "na.fail")

summary(m_f)
Anova(m_f)
plot_model(m_f, vline.color = "lightgrey") 
tab_model(m_f, show.df = TRUE) 

# step through models for reef flat
m_f_set<-dredge(m_f, rank = "AICc")
m_f_set

# Get models with <10 delta AICc
top_mf <- get.models(m_f_set,subset = delta<10)
top_mf

mf_avg <- model.avg(top_mf) 
mf_avg

# no seagrass - feef flat
m_f2<-lme4::glmer(Reef_state ~ 
                    cs.(Depth_m) +
                    cs.(I(sg_minDist_100^2))+
                    cs.(SHAPE)+
                    cs.(PopRskDecay.Nrm)+
                    cs.(fYrLag30A)+
                    cs.(cum_FA_blast10)+
                    cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                    cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                    MPA+
                    # Geomorphic2+
                   (1|ecological_zone), 
                 family=binomial(link=logit), data=d_flat, na.action = "na.fail")

summary(m_f2)
Anova(m_f2)
plot_model(m_f2, vline.color = "lightgrey") 
tab_model(m_f2, show.df = TRUE) 

# ---------------------------------------------
# check model - reef flat
# ---------------------------------------------
qqmath(m_f2) 




# ---------------------------------------------
# look at outliers - reef flat ------------------------------
# ---------------------------------------------
# set as m_f or m_f2
outliers_f<-data.frame(cbind(residuals(m_f,type="deviance"),d_flat$PtID2))%>%
  mutate(residuals=X1,
         PtID2=X2)%>%
  select(-X1,-X2)%>%
  filter(abs(residuals)>2.5)%>%
  # filter(residuals<(-2.5))%>%
  # filter(abs(residuals)>2)%>%
  arrange((residuals))%>% 
  glimpse()

outliers2_f<-outliers_f%>%
  left_join(d_flat)%>%
  glimpse()



# --------------------------------------
# reef slope
# --------------------------------------
d_slope<-d4%>%
  filter(Geomorphic2=="Reef Slope")%>%
 glimpse()

m_s<-lme4::glmer(Reef_state ~ 
                   cs.(Depth_m) +
                   cs.(I(sg_minDist_100^2))+
                   cs.(SHAPE)+
                   cs.(PopRskDecay.Nrm)+
                   cs.(fYrLag30A)+
                   cs.(cum_FA_blast10)+
                   cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                   cs.(PopRskDecay.Nrm):cs.(cum_FA_blast10)+ 
                   MPA+
                   # Geomorphic2+
                   (1|ecological_zone), 
                 family=binomial(link=logit), data=d_slope, na.action = "na.fail")

# check model - slope ---------------
summary(m_s)
Anova(m_s)
plot_model(m_s, vline.color = "lightgrey") 
tab_model(m_s, show.df = TRUE)

m_s_set<-dredge(m_s, rank = "AICc")
m_s_set

# Get models with <10 delta AICc -----------------
top_ms <- get.models(m_s_set,subset = delta<10)
top_ms

ms_avg <- model.avg(top_ms) # sg and mpa are different in the top 4 models
ms_avg


qqmath(m_s) 



# ---------------------------------------------
# look at outliers - reef slope 
# ---------------------------------------------
# set as m_s or m_s2
outliers_s<-data.frame(cbind(residuals(m_s,type="deviance"),d_slope$PtID2))%>%
  mutate(residuals=X1,
         PtID2=X2)%>%
  select(-X1,-X2)%>%
  filter(abs(residuals)>2.5)%>%
  # filter(residuals<(-2.5))%>%
  # filter(abs(residuals)>2)%>%
  arrange((residuals))%>% 
  glimpse()

outliers2_s<-outliers_s%>%
  left_join(d_slope)%>%
  glimpse()



# --------------------------------------
# residual diagnostics ------------------------------------
# --------------------------------------

# DHARMa: It is therefore highly recommended to first calculate the residuals once, using the simulateResiduals() function

# calculates calculates randomized quantile residuals

# scaled residual value of 0.5 means that half of the simulated data are higher than the observed value, and half of them lower

# For a correctly specified model we would expect asymptotically 1) # a uniform (flat) distribution of the scaled residuals, 2) # uniformity in y direction if we plot against any predictor.


# --------------------------------------
# SET MODEL HERE - NEED TO MANUALLY UPDATE
# --------------------------------------


# set model - all ----------------------------
fittedModel<-m_all2
d_fitted<-d4

# set model - reef flat----------------------------
fittedModel<-m_f2
d_fitted<-d_flat

# set model - reef slope----------------------------
fittedModel<-m_s2
d_fitted<-d_slope




# --------------------------------------
# Simulations 
# --------------------------------------
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = F) 
# residuals(simulationOutput)
plot(simulationOutput) # By default, plotResiduals plots against predicted values


# against numeric predictors
plotResiduals(simulationOutput, form = cs.(d_fitted$Depth_m))
plotResiduals(simulationOutput, form = cs.(d_fitted$sg_minDist_100)) # not normal - maybe over dispersed?
plotResiduals(simulationOutput, form = cs.(d_fitted$PopRskDecay.Nrm))
plotResiduals(simulationOutput, form = cs.(d_fitted$fYrLag30A))
plotResiduals(simulationOutput, form = cs.(log(d_fitted$cum_FA_blast10+1))) # not normal - maybe over dispersed?
plotResiduals(simulationOutput, form = cs.(log(d_fitted$CoRuEdg2Area+1)))
# against factors
plotResiduals(simulationOutput, form = (d_fitted$MPA))
plotResiduals(simulationOutput, form = (d_fitted$Geomorphic2))


# test dispersions 
testDispersion(fittedModel)








###################################################
# UNSCALE AND UNCENTER GLMER PARAMETERS
###################################################

# --------------------------------------------
# Unscale variables - full model
# --------------------------------------------

# https://stackoverflow.com/questions/24268031/unscale-and-uncenter-glmer-parameters?rq=1

# categorical variables
cat.vars<-subset(d4,select=c(MPA, Geomorphic2))%>%
  glimpse()

# unscaled variables 
unsc.vars<-subset(d4,select=c(
  Depth_m,
  sg_minDist_100, 
  SHAPE,
  PopRskDecay.Nrm,
  fYrLag30A,
  cum_FA_blast10))%>%
  glimpse()

# scaled variables
scl.vars<-d4%>%
  # select
  select(Depth_m,
         sg_minDist_100, 
         SHAPE,
         PopRskDecay.Nrm,
         fYrLag30A,
         cum_FA_blast10)%>%
  # scale
  mutate(Depth=cs.(Depth_m)[,1], # [,1] calls it out of the matrix
         Seagrass_isolation_2=cs.(I(sg_minDist_100^2))[,1],
         Patch_shape=cs.(SHAPE)[,1],
         Population_risk=cs.(PopRskDecay.Nrm)[,1],
         Fishing_legacy_1980_2000=cs.(fYrLag30A)[,1],
         Blast_fishing_2010_2000=cs.(cum_FA_blast10)[,1])%>%
 
  # remove unscaled values
  select(-Depth_m,-sg_minDist_100,
         -PopRskDecay.Nrm,
         -fYrLag30A,
         -cum_FA_blast10,
         -SHAPE)%>%
  data.frame()%>%
  glimpse()


# ----------------------------------------
# means for scaled and unscaled variables    
# ----------------------------------------
colMeans(scl.vars)
apply(scl.vars,2,sd)

cm <- colMeans(unsc.vars)
csd <- apply(unsc.vars,2,sd)


# ----------------------------------------
# resecale function
# ----------------------------------------
rescale.coefs <- function(beta,mu,sigma) {
  beta2 <- beta ## inherit names etc.
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1]  <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
  beta2
}   

# get scaling parameters from model matrix -------------
X <- model.matrix(Reef_state ~ 
                    Depth_m +
                    I(sg_minDist_100^2)+
                    SHAPE+
                    PopRskDecay.Nrm+
                    fYrLag30A+
                    cum_FA_blast10+
                    PopRskDecay.Nrm:fYrLag30A+
                    PopRskDecay.Nrm:cum_FA_blast10+ 
                    MPA+
                    Geomorphic2, data=d4)

cm2 <- colMeans(X)[-1]
csd2 <- apply(X,2,sd)[-1]                                            
(cc2 <- rescale.coefs(fixef(m_all2),mu=c(0,cm2),sigma=c(1,csd2)))
all.equal(unname(cc2),unname(fixef(m2)),tol=1e-3)  ## TRUE
 
# rescale coef 
cc <- rescale.coefs(fixef(m_all),mu=c(0,cm),sigma=c(1,csd))

fixef(m_all2)

# with interactions etc
X <- getME(m_all,"X")                                        
cm2 <- colMeans(X)[-1] # drop first intercept value
csd2 <- apply(X,2,sd)[-1]                                            
(cc2 <- rescale.coefs(fixef(m_all),mu=c(0,cm2),sigma=c(1,csd2)))
# all.equal(unname(cc2),unname(X),tol=1e-3)  ## TRUE


# NOTE: if length issues above:  compare length(fixef(m1.sc)), length(cm), length(csd)? I believe the latter should be one shorter than the former (the fixed effects include an intercept term; by design the mu and sigma arguments are the same length as beta, they contain one term for the scaling of the response plus terms for the scaling of each fixed-effect parameter). 
# --------------------------------------------------------




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
d5<-d4%>%
  mutate(Depth=cs.(Depth_m),
         Seagrass_isolation=cs.(sg_minDist_100),
         Population_risk=cs.(PopRskDecay.Nrm),
         Fishing_legacy_1980_2000=cs.(fYrLag30A), 
         Blast_fishing_2010_2000=cs.(cum_FA_blast10),
         Patch_shape=cs.(CoRuEdg2Area),
         Geomorphic)%>%
  glimpse()

m.me_52<-lme4::glmer(Reef_state ~ 
                       Depth_m+
                       MPA+
                       Seagrass_isolation+
                       Population_risk+
                       Fishing_legacy_1980_2000+
                       Blast_fishing_2010_2000+
                       Population_risk:Fishing_legacy_1980_2000+
                       Population_risk:Blast_fishing_2010_2000+ 
                       Patch_shape+
                       Geomorphic+
                       (1|ecological_zone), 
                     family=binomial(link=logit), data=d4, na.action = "na.fail")

# above, without landscape var (used this because landscape variables are usually not included in coral surveys. Used for analysis in paper, but not included here.)
m.me_50<-lme4::glmer(Reef_state ~ 
                       Depth_m+
                       MPA+
                       # Seagrass_isolation+
                       Population_risk+
                       Fishing_legacy_1980_2000+
                       Blast_fishing_2010_2000+
                       Population_risk:Fishing_legacy_1980_2000+
                       Population_risk:Blast_fishing_2010_2000+ 
                       # Patch_shape+
                       Geomorphic+
                       (1|ecological_zone), 
                     family=binomial(link=logit), data=d4, na.action = "na.fail")
summary(m.me_50)



save(m.me_52,file="./results_train/Q41_model_20161109_mixedEf1_52.R") # m23 with blast*pop #m52f from models above
save(m.me_50,file="./results_train/Q41_model_20161109_mixedEf1_50.R") # m53 w/out landcape var


#save image to set options for probabilities in GT
save.image("./results_train/Q41_analysis_mixedEf1_blast.RData")




