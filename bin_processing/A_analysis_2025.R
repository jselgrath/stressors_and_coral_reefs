# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: Code to analyze the relationship between coral (Coral (1) vs. rubble (0)) and various threats and biophysical parameters.

# Mixed Effects Models
# Using forward stepping model because full model with all variables would not converge
# model with 1 random effect

# maybe want to remove NAs - check which variables have NAs because loose ~280 points
# remove nas

# https://crd230.github.io/lab8.html
# https://rpubs.com/corey_sparks/111362
#https://stat.ethz.ch/pipermail/r-sig-geo/2009-November/007034.html


# ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(lattice)
library(boot)
library(car)
library(MuMIn)
library(DHARMa)# residual diagnostics for heirarchical regression models
library(colorspace)
library(visreg)
library(glmmTMB) #allows a binomial GLMM with spatial correlation structure
library(spdep)
library(MASS)   # for stepAIC



# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------


# ---------------------------------------------
# load data 
# ---------------------------------------------
d0<-read_csv("./results_train/16_IndpVar_Pts_train_all.csv")%>%
  
  # adjusting depth
  mutate(Depth_m=if_else(Depth_m>-135,Depth_m,-15))%>% #~8 deep outliers - probably misclassified in depth map. 8 at 15m slightly more at 12
  mutate(Depth_m=if_else(Depth_m<0,Depth_m,-.1))%>% # one value above water - also misclassified
  
  mutate(MPA=as.factor(MPA), 
         ecological_zone=as.factor(ecological_zone),
         Depth_m=Depth_m*-1,
         # Reef_state=as.factor(Reef_state),
         geomorphology=as.factor(geomorphology))%>%
  glimpse()

d1<-data.frame(na.omit(d0))# 13 NAs - all in population risk variables

d1%>%filter(point_id==371)%>% glimpse()  # 636

d1$Depth_m[d1$point_id==856]<-5  # outlier depth is wrong
d1$Depth_m[d1$point_id==371]<-2

# check depth representation # ---------------------------------------------
range(d1$Depth_m)

# graph of depth
with(d1,xyplot(Reef_state~Depth_m|ecological_zone,type=c('g','p','l'),
               layout=c(4,1), index.cond = function(x,y)max(y)))

# blast fishing in deep water

d1%>%
  filter(Depth_m>12)%>%
  ggplot(aes(x,y,color=Depth_m,size=cumulative_blast10 ))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

# ---------------------------------------------
# centering variables based on mean
# from gelman and hill p 55
# https://www.r-bloggers.com/a-faster-scale-function/
# or substract by mean and divide by max (0-1)
# ---------------------------------------------

# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=FALSE,scale=TRUE)

# run cs fxn on variables
d1$depth<-as.numeric(cs. (d1$Depth_m))
d1$sg<-as.numeric(cs. (d1$point_dist_Seagrass))
d1$mg<-as.numeric(cs. (d1$point_dist_Mangrove))
d1$psi<-as.numeric(cs. (d1$patch_shape_index))
d1$pr_orig<-as.numeric(cs. (d1$pop_risk_dens_orig))#pop_risk_dens_orig  #inhab
d1$pr_inhab<-as.numeric(cs. (d1$pop_risk_dens_inhab))
d1$fishing_30lag<-as.numeric(cs. (d1$fYrLag30A))
d1$blast10<-as.numeric(cs. (d1$cumulative_blast10))
d1$pr<-as.numeric(cs. (d1$pop_risk_pop))  # or point_dist_Mangrove or river_distance.nrm or river_distance

# precompute
d1$sg2<-d1$sg^2
d1$pri_f30<-d1$pr_inhab*d1$fishing_30lag
d1$pri_b10<-d1$pr_inhab*d1$blast10

glimpse(d1)
# ---------------------------------------------
# FULL MODEL - log variables it is not sig dif using Anova, so removing
# ---------------------------------------------



# build spatial matrix of coordaiates and neighbors list -------------------------------

# matrix of coordinates of sample locations projected into meters for distance
coords<-as.matrix(d1[, c("x", "y")])

# Build nearest neighbor list ------------

# OPTION1 -----------
# knearneigh = number of neighbors. # k = 5 means: for each point, find the 5 nearest neighbors
# knearneigh gives you a nearest-neighbor structure (which is not yet a full neighbor list)
# knn2nb() Converts the nearest-neighbor object from knearneigh() into an nb object (neighbor list) that can be used in spatial weights (nb2listw()
# If you want one fully connected graph (since there are subgraphs), switch to dnearneigh() with a radius chosen to cover the gaps.

nb <- knn2nb(knearneigh(coords, k = 5)) # 5 nearest neighbors # Warning: "neighbour object has 3 sub-graphs" - points form disconnected groups (clusters), so some parts of the neighbor graph are not connected to others. Here probably the three ecological zones → so you get multiple "sub-graphs". It’s not necessarily an error — it’s telling you the spatial graph is not fully connected.Moran’s I still works, but it’s applied within those sub-graphs.

# OPTION2 ------------
# dnearneigh = distance based near neighbors d1 = closest, d2 = furtherst in meters (if projected. if not projected in km)

# nb <- dnearneigh(coords, d1=0,d2=1000) #warning: neighbour object has 9 sub-graphs



# Convert to spatial weights
wts <- nb2listw(nb, style = "W") # assigns weights, creates listw object (Plant p93)



# base function # ---------------------------
fxn1<-as.formula(Reef_state ~ 
              depth+
              sg+
              sg2+
              mg+
              psi+
              # pr_orig+
              pr_inhab+
              fishing_30lag+
              blast10+
              # pr+ # OR point_dist_Mangrove or river_distance
              pri_f30+ 
              pri_b10+
              MPA+
              ecological_zone+
              geomorphology+
              municipality)



# -----------------------------------------------------------
fit_and_check <- function(formula, data, model_name, k = 5) {
  
  # 1. Fit logistic regression
  model <- glm(formula, data = data, family = binomial(link=logit), 
                                                       na.action = "na.fail")
  
  # 2. Pearson residuals
  resid <- residuals(model, type = "pearson")
  
  # 3. Spatial neighbors (k-nearest)
  coords <- as.matrix(data[, c("x", "y")])
  nb <- knn2nb(knearneigh(coords, k = k))
  lw <- nb2listw(nb, style = "W")
  
  # 4. Moran’s I test
  moran_out <- lm.morantest(lm(resid ~ 1), lw)
  
  # 5. DHARMa test
  dharma_res <- simulateResiduals(model)
  dharma_spatial <- testSpatialAutocorrelation(dharma_res, x = data$x, y = data$y)
  
  # 6. Store results in a list with names
  out <- list(
    model = model,
    resid = resid,
    lw = lw,
    moran = moran_out,
    dharma = dharma_spatial
  )
  names(out) <- paste0(names(out), "_", model_name)
  
  return(out)
}

#---------------------------------------
# Example 1: Full model m1
#---------------------------------------
m1_results <- fit_and_check(
  formula = fxn1,
  data = d1,
  model_name = "m1",
  k=8  # number of near neighbors
)

summary(m1_results$model_m1)
Anova(m1_results$model_m1)
m1_results$moran_m1
m1_results$dharma_m1 # no spatial autocorrelation. same for 5 and 8 neighbors



# check for outliers -------------------------------

# ---------------------------------------------
# OUTLIERS - all
# ---------------------------------------------
# table of residuals and IDs. use a cutoff of residuals ±1.96
# https://stats.stackexchange.com/questions/196724/how-to-identify-outliers-and-do-model-diagnostics-for-an-lme4-model
# https://stackoverflow.com/questions/24268031/unscale-and-uncenter-glmer-parameters?rq=1
# ---------------------------------------------
# view(cbind(residuals(m_all),d1$PtID2)) # outlier at point_id=1384
m1<-m1_results$model_m1
range(residuals(m1,type="pearson")) # pearson for GLM

# ran this iteratively with m_all2 & d4
outliers<-data.frame(cbind(residuals(m1,type="pearson"),d1$point_id))%>%
  mutate(residuals=X1,
         point_id=X2)%>%
  dplyr::select(-X1,-X2)%>%
  filter(abs(residuals)>2.5)%>%
  # filter(residuals<(-2))%>% #removed 3 more
  # filter(abs(residuals)>2)%>%
  arrange(-abs(residuals))%>% 
  glimpse()

outliers
outliers$point_id # 28 bigger than 2.5

# point_id=147 590 636 119 579 819 856 745 694 190 708 807 105 721 709 238 125 442 685 658 712 675 772 781 661 451 371  14 # remove

# look at spatially - many in NE
outliers2<-outliers%>%
  left_join(d1)%>%
  glimpse()

# plot outliers - all places with coral
ggplot(outliers2,aes(x,y,color=as.factor(Reef_state),shape=geomorphology ))+geom_point()
ggplot(outliers2,aes(x,y,color=Depth_m,size=residuals ))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)











# -------------------------------------------------------
# Stepwise backward selection (from m1)
m2 <- dredge(m1_results$model_m1)

# Check results again
m2_results <- fit_and_check(
  formula= m2,
  data = d1,
  model_name = "m2"
)

summary(m2_results$model_m2)
m2_results$moran_m2
m2_results$dharma_m2































#version1 - generalized linear model
m1<-glm(fxn1, 
            data=d1, 
            family=binomial(link=logit), 
            na.action = "na.fail")
Anova(m1)
summary(m1)

# Then check residual spatial autocorrelation:
# extract residuals from GLM
resid_m1 <- residuals(m1, type = "pearson")






# check for spatial autocorrelation -----------------------------------------
# METHOD 1
# morans I on residuals(spatial autocorrelation test for residuals) - but not great for glms - assumes linear residuals, so with GLM Pearson residuals it can over-detect autocorrelation
# see thread: https://stat.ethz.ch/pipermail/r-sig-geo/2009-November/007034.html "While lm.morantest() can be used on glm output objects, no work has been done to establish whether this is a sensible idea." - Roger Bivand
moran_res_m1 <- lm.morantest(lm(resid_m1 ~ 1), wts)
print(moran_res_m1)

# METHOD 2
# simulations to check residual spatial autocorrelation
#This uses permutations to check spatial structure in residuals and works even when distributional assumptions of lm.morantest are questionable. It is more robust for GLMs
sim_res_m1 <- simulateResiduals(m1, n = 1000)

# Test for spatial autocorrelation
spatial_test_m1 <- testSpatialAutocorrelation(
  sim_res_m1,
  x = d1$x,
  y = d1$y
)
print(spatial_test_m1)


# autocorrelated using first method, but not second. going with second since that is a better method for this model structure -------------
# -----------------------------------------------------------------------------






















m_all<-lme4::glmer(Reef_state ~ 
                     cs.(Depth_m) +
                     cs.(point_dist_Seagrass)+ 
                     cs.(I(point_dist_Seagrass^2))+
                     cs.(-patch_shape_index)+
                     cs.(pop_risk_dens_orig)+ #pop_risk_dens_inhab
                     cs.(fYrLag30A)+
                     cs.(cumulative_blast10)+
                     cs.(pop_risk_pop)+  # or point_dist_Mangrove or river_distance.nrm or river_distance
                     cs.(pop_risk_dens_orig):cs.(fYrLag30A)+
                     cs.(pop_risk_dens_orig):cs.(cumulative_blast10)+ 
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



  
m_all2<-lme4::glmer(Reef_state ~ 
                      cs.(Depth_m) +
                      # cs.(sg_minDist_100)+
                      cs.(I(sg_minDist_100^2))+
                      cs.(-SHAPE)+
                      cs.(PopRskDecay.Nrm)+
                      cs.(fYrLag30A)+
                      cs.(cumulative_FA_blast10)+
                      cs.(PopRskDecay.Nrm):cs.(fYrLag30A)+
                      cs.(PopRskDecay.Nrm):cs.(cumulative_FA_blast10)+ 
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
plotResiduals(simulationOutput, form = cs.(d_fitted$cumulative_FA_blast10)) # not normal - maybe over dispersed?
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
         Blast_fishing_2010_2000=cs.(cumulative_FA_blast10)[,1])%>%
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



