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

d1<-read_csv("./results_train/15_IndpVar_Pts_train_for_models_all.csv")%>%
  glimpse()

# do I want to link these: write_csv(w4,"./doc/TableS3_final_model_wald.csv")??

# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)

# load model - m_all2
load("./results_train/mixedEf_final_all1.R")


###################################################
# UNSCALE AND UNCENTER GLMER PARAMETERS
###################################################

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
  SHAPE,
  PopRskDecay.Nrm,
  fYrLag30A,
  cum_FA_blast10))%>%
  glimpse()

# scaled variables ----------------------
# NOTE: this has sg^2 built into it
scl.vars<-d1%>%
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
                    SHAPE+
                    PopRskDecay.Nrm+
                    fYrLag30A+
                    cum_FA_blast10+
                    PopRskDecay.Nrm:fYrLag30A+
                    PopRskDecay.Nrm:cum_FA_blast10+ 
                    MPA+
                    Geomorphic2, data=d1)

# cm2 <- colMeans(X)[-1]
# csd2 <- apply(X,2,sd)[-1]                                            
# (cc2 <- rescale.coefs(fixef(m_all2),mu=c(0,cm2),sigma=c(1,csd2)))
# all.equal(unname(cc2),unname(fixef(m2)),tol=1e-3)  ## TRUE
# 
# 
# cc <- rescale.coefs(fixef(m_all),mu=c(0,cm2),sigma=c(1,csd2))
# 
# fixef(m_all2)

# with interactions etc -------------------------
X <- getME(m_all2,"X")                                        
cm2 <- colMeans(X)[-1] # drop first intercept value
csd2 <- apply(X,2,sd)[-1]      

# rescale coef --------------------
(cc2 <- rescale.coefs(fixef(m_all2),mu=c(0,cm2),sigma=c(1,csd2)))
# all.equal(unname(cc2),unname(X),tol=1e-3)  ## TRUE

cc2

# --------------------------------------------------------
# NOTE: if length issues above:  compare length(fixef(m1.sc)), length(cm), length(csd)? I believe the latter should be one shorter than the former (the fixed effects include an intercept term; by design the mu and sigma arguments are the same length as beta, they contain one term for the scaling of the response plus terms for the scaling of each fixed-effect parameter). 
# --------------------------------------------------------


