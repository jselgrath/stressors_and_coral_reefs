# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: To UNSCALE AND UNCENTER GLMER PARAMETERS from final model
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
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")
# ---------------------------------------------
# load model- m_all2
load("./results_train/mixedEf_final_all1.R") # unscaled data is scaled in model
summary(m_all2)
display(m_all2)

# load data - scaled & seagrass is exponated
d0<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_subset.csv")%>% # all
  mutate(MPA=as.factor(MPA),
         Ecological_zone=as.factor(Ecological_zone),
         Depth=Depth*-1)%>%
  glimpse()

# load data - unscaled
d1<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>% # all
  mutate(MPA=as.factor(MPA),
         ecological_zone=as.factor(ecological_zone),
         # Depth_m=Depth_m*-1,
         Geomorphic2=as.factor(Geomorphic2))%>%
  glimpse()

# do I want to link these: write_csv(w4,"./doc/TableS3_final_model_wald.csv")??

# ---------------------------------------
# calc means for data
# ---------------------------------------
# ----------------------------------------------
# subset numeric variables only, not log transformed variables
d2<-d1%>%
  dplyr::select(Depth_m,
                sg_minDist_100, 
                SHAPE, # this is negative in models
                PopRskDecay.Nrm,
                fYrLag30A,
                cum_FA_blast10)%>%
  glimpse()
nm<- names(d2)

# calculate summary statistics
d3<- d2%>%
  pivot_longer(names_to = "vari",cols=c(Depth_m:cum_FA_blast10))%>%
  group_by(vari)%>%
  summarize(
    n=n(),
    u.var=round(mean(value,na.rm=T),4),
    sd.var=round(sd(value,na.rm=T),4),
    sem.var=round((sd.var/sqrt(n)),4),
    min=min(value,na.rm=T),
    max=max(value,na.rm=T)
)%>%
  mutate(ordr=c(1,4,3,6,5,2))%>%
  arrange(ordr)%>%
  glimpse()
d3

write_csv(d3,"./doc/G_IndpVar_Pts_MeanSD_train.csv")

###################################################
# UNSCALE AND UNCENTER GLMER PARAMETERS
###################################################

# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)



# --------------------------------------------
# Unscale variables 
# --------------------------------------------

# https://stackoverflow.com/questions/24268031/unscale-and-uncenter-glmer-parameters?rq=1

# categorical variables ----------------------
cat.vars<-subset(d1,select=c(MPA, Geomorphic2))%>%
  glimpse()

# unscaled variables ----------------------
unsc.vars<-subset(d1,select=c(
  Depth_m,
  sg_minDist_100, 
  SHAPE, # this is negative in models
  PopRskDecay.Nrm,
  fYrLag30A,
  cum_FA_blast10))%>%
  glimpse()

# scaled variables ----------------------
# NOTE: this has sg^2 built into it
scl.vars<-d1%>%
  # select
  dplyr::select(Depth_m,
         sg_minDist_100, 
         SHAPE,
         PopRskDecay.Nrm,
         fYrLag30A,
         cum_FA_blast10)%>%
  # scale
  mutate(Depth=cs.(Depth_m)[,1], # [,1] calls it out of the matrix
         Seagrass_isolation_2=cs.(I(sg_minDist_100^2))[,1],
         Patch_compactness=cs.(-SHAPE)[,1],
         Population_risk=cs.(PopRskDecay.Nrm)[,1],
         Fishing_legacy_1980_2000=cs.(fYrLag30A)[,1],
         Blast_fishing_2010_2000=cs.(cum_FA_blast10)[,1])%>%
  
  # remove unscaled values
  dplyr::select(-Depth_m,-sg_minDist_100,
         -PopRskDecay.Nrm,
         -fYrLag30A,
         -cum_FA_blast10,
         -SHAPE)%>%
  data.frame()%>%
  glimpse()


# ----------------------------------------
# means for scaled and unscaled variables    
# ----------------------------------------
# NOTE: For this model, this won't work because of the sg^2 term and interaction terms
# cm <- colMeans(unsc.vars)
# csd <- apply(unsc.vars,2,sd)


# ----------------------------------------
# RESCALE FUNCTION
# ----------------------------------------
rescale.coefs <- function(beta,mu,sigma) {
  beta2 <- beta ## inherit names etc.
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1]  <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
  beta2
}   
# ------------------------------------------------------


# get scaling parameters from model matrix -------------
# this includes sg^2
X <- model.matrix(Reef_state ~ 
                    Depth_m +
                    I(sg_minDist_100^2)+
                    -SHAPE+
                    PopRskDecay.Nrm+
                    fYrLag30A+
                    cum_FA_blast10+
                    PopRskDecay.Nrm:fYrLag30A+
                    PopRskDecay.Nrm:cum_FA_blast10+ 
                    MPA, 
                  data=d1)


fixef(m_all2)

# with interactions etc -------------------------
X <- getME(m_all2,"X")                                        
cm2 <- colMeans(X)[-1] # drop first intercept value
csd2 <- apply(X,2,sd)[-1]      

# rescale coef --------------------
(coef_rescaled <- rescale.coefs(fixef(m_all2),mu=c(0,cm2),sigma=c(1,csd2)))
all.equal(unname(coef_rescaled),unname(X),tol=1e-3)  

coef_rescaled # # note: unscaledcoef are almost the same as original

# use divide by 4 rule to look at rough estimates of the influence of the variable
# divide by 4 rule - gellman & hill p304 & p82
cc2<-data.frame(coef_rescaled)%>%
  mutate(var=c("Intercept","Depth", 
               "Seagrass_isolation", 
               "Patch_compactness", 
               "Population_risk",
               "Fishing_legacy_1980_2000",
               "Blast_fishing_2010_2000",
               "MPA_protected",
               "Population_risk:Fishing_legacy_1980_2000",
               "Population_risk:Blast_fishing_2010_2000"))%>%
  mutate(est=coef_rescaled/4,  # look at relative effects -----------------------
         pct=round(est*100,1))%>%
  dplyr::select(var,coef_rescaled,est,pct)%>%
  glimpse()

row.names(cc2)<-1:10
cc2


# save these values 
write_csv(cc2,"./doc/unscaled_coef_pct.csv")




# ------------ 
# remove intercept

cc3<-cc2%>%
  filter(var!="Intercept")%>%
  glimpse()

row.names(cc3)<-1:9


cc3


# join means and coef
# tmp<-data.frame(cbind("MPA",3179,NA,NA,NA,NA,NA,7))
# tmp2<-data.frame(cbind("pop:fish",3179,NA,NA,NA,NA,NA,8))
# tmp3<-data.frame(cbind("pop:blast",3179,NA,NA,NA,NA,NA,9))
# colnames(tmp)<-colnames(d3);colnames(tmp2)<-colnames(d3);colnames(tmp3)<-colnames(d3)
# d4<-rbind(d3,tmp,tmp2,tmp3)%>%
#   glimpse()
# d4
# 
# 
# cc4<-cc3%>%
#   cbind(d4)%>%
#   glimpse()
# cc4


# --------------------------------------------------------
# NOTE: if length issues above:  compare length(fixef(m1.sc)), length(cm), length(csd)? I believe the latter should be one shorter than the former (the fixed effects include an intercept term; by design the mu and sigma arguments are the same length as beta, they contain one term for the scaling of the response plus terms for the scaling of each fixed-effect parameter). 
# --------------------------------------------------------


