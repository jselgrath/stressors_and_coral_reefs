# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------


# --------------------------------------------
# GOAL: Code to analyze the relationship between coral (Coral (1) vs. rubble (0)) and various threats and biophysical parameters.
# --------------------------------------------


# --------------------------------------------
# -- Some Resources --

# https://crd230.github.io/lab8.html
# https://rpubs.com/corey_sparks/111362
#https://stat.ethz.ch/pipermail/r-sig-geo/2009-November/007034.html

# Plant: Spatial Data analysis in Ecology and Agriculture using R (2nd ed)
# Penguin Book
# gelman and hill


# ------------------------------------------------
# overview of methods for near neighbor list  --

# OPTION1 -----------
# knearneigh = number of neighbors. # k = 5 means: for each point, find the 5 nearest neighbors
# knearneigh gives you a nearest-neighbor structure (which is not yet a full neighbor list)
# knn2nb() Converts the nearest-neighbor object from knearneigh() into an nb object (neighbor list) that can be used in spatial weights (nb2listw()
# If you want one fully connected graph (since there are subgraphs), switch to dnearneigh() with a radius chosen to cover the gaps.

# nb <- knn2nb(knearneigh(coords, k = 5)) # 5 nearest neighbors # Warning: "neighbour object has 3 sub-graphs" - points form disconnected groups (clusters), so some parts of the neighbor graph are not connected to others. Here probably the three ecological zones → so you get multiple "sub-graphs". It’s not necessarily an error — it’s telling you the spatial graph is not fully connected.Moran’s I still works, but it’s applied within those sub-graphs.

# OPTION2 ------------
# dnearneigh = distance based near neighbors d1 = closest, d2 = furtherst in meters (if projected. if not projected in km)

# nb <- dnearneigh(coords, d1=0,d2=1000) #warning: neighbour object has 9 sub-graphs




# ------------------------------------------------
# Overview of spatial autocorrelation tests:
# METHOD 1
# morans I on residuals(spatial autocorrelation test for residuals) - but not great for glms - assumes linear residuals, so with GLM Pearson residuals it can over-detect autocorrelation
# see thread: https://stat.ethz.ch/pipermail/r-sig-geo/2009-November/007034.html "While lm.morantest() can be used on glm output objects, no work has been done to establish whether this is a sensible idea." - Roger Bivand

# Pearson residuals
# r1 <- residuals(m1, type = "pearson")

# test
# lm.morantest(lm(residuals ~ 1), wts) # wts from neighbor list above


# METHOD 2
# simulations to check residual spatial autocorrelation
#This uses permutations to check spatial structure in residuals and works even when distributional assumptions of lm.morantest are questionable. It is more robust for GLMs
# sim_res_m1 <- simulateResiduals(m1, n = 1000)

# Test for spatial autocorrelation
# testSpatialAutocorrelation(
#   sim_res_m1,
#   x = d1$x,
#   y = d1$y
# )




# ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(car)
library(MuMIn)
library(DHARMa)# residual diagnostics for heirarchical regression models
library(colorspace)
library(spdep)
library(lattice)
library(adespatial)
library(spatialreg)
library(sjPlot)
library(lme4)
library(visreg)
library(officer)


# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# -------------------------------------------


# ---------------------------------------------
# load data and organize variables
# ---------------------------------------------
d0<-read_csv("./results_train/16_IndpVar_Pts_train_all.csv")%>%

  mutate(Depth_m=if_else(Depth_m<0,Depth_m,-.1),   # adjusting depth # one value above water 
  Depth_m=Depth_m*-1)%>%   # made depth positive for easier interpretation
  mutate(MPA=as.factor(mpa_id),     # make factors
         ecological_zone=as.factor(ecological_zone),
         Reef_state=resilience_id
         )%>%
  glimpse()

d0$ecological_zone

# check depth representation # ---------------------------------------------
range(d0$Depth_m)

# graph of depth
with(d0,xyplot(Reef_state~Depth_m|ecological_zone,type=c('g','p','l'),
               layout=c(4,1), index.cond = function(x,y)max(y)))

# blast fishing in deep water - using to check for high blast fishing in shallow areas

d0%>%
  filter(Depth_m>12)%>%
  ggplot(aes(x,y,color=Depth_m,size=cumulative_blast_10))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2) #- one deep area is an atoll. fixed below


# correct outlier depth
d0%>%filter(point_id==371)%>% glimpse()  # outlier
d0$Depth_m[d0$point_id==371]<-5 # too shallow 

d0%>%filter(point_id==815)%>% glimpse()  # outlier
d0$Depth_m[d0$point_id==815]<-5 # atoll that the depth modeling missed 


 
# remove NAs and make extreme depths shallower  
d1<-d0 %>%
  # make extreme depth shallower - assuming spline errors
  mutate(Depth_m=if_else(Depth_m>-15,Depth_m,-15))%>%#~8 deep outliers - probably misclassified in depth map. 8 at 15m slightly more at 12
  
  #remove outliers 
# this group is in deep water or sand dominated
  filter(point_id!=687,point_id!=1037, point_id!=511, point_id!=1236,point_id!=266, point_id!=802)%>%
  glimpse()

d1<-data.frame(na.omit(d1))%>%# 13 NAs -  in population risk variables 
    glimpse()


d1%>%
   ggplot(aes(x,y,color=Reef_state))+geom_point()#+geom_label(aes(label = point_id), nudge_y=0.2)

plot(d1$ecological_zone)


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

# run cs fxn on variables
d1$depth<-as.numeric(cs. (d1$Depth_m))
d1$sg<-as.numeric(cs. (d1$point_dist_Seagrass))
d1$sg100<-as.numeric(cs. (d1$point_dist_Seagrass100))
d1$mg<-as.numeric(cs. (d1$point_dist_Mangrove))
d1$river<-as.numeric(cs. (d1$point_dist_river))
d1$river100<-as.numeric(cs. (d1$point_dist_river100))
d1$psi<-as.numeric(cs. (d1$patch_shape_index))
d1$pr_orig<-as.numeric(cs. (d1$pop_risk_dens_orig))#pop_risk_dens_orig  #inhab
d1$pr_inhab<-as.numeric(cs. (d1$pop_risk_dens_inhab))
d1$fishing_30lag<-as.numeric(cs. (d1$lag_all_30))
d1$fishing_20lag<-as.numeric(cs. (d1$lag_all_20))
d1$fishing_10lag<-as.numeric(cs. (d1$lag_all_10))
d1$fishing10<-as.numeric(cs. (d1$cumulative_all_00))
d1$fishing20<-as.numeric(cs. (d1$cumulative_all_10))
d1$blast10<-as.numeric(cs. (d1$cumulative_blast_10))
d1$poison10<-as.numeric(cs. (d1$cumulative_poison_10))
d1$pr<-as.numeric(cs. (d1$pop_risk_pop))  # or point_dist_Mangrove or river_distance.nrm or river_distance


d1$psi2<- -1*d1$psi
# ---------------------------------------------
# precompute interactions
# ---------------------------------------------
# precompute
d1$sg2<-d1$sg^2
d1$pri_f30<-d1$pr_inhab*d1$fishing_30lag
d1$pri_b10<-d1$pr_inhab*d1$blast10

glimpse(d1)



# ---------------------------------------------
# build spatial matrix of coordaiates and neighbors list -------------------------------
# ---------------------------------------------
# matrix of coordinates of sample locations projected into meters for distance
coords<-as.matrix(d1[, c("x", "y")])

# Build nearest neighbor list ------------

# OPTION1 -----------
# knearneigh = number of neighbors. # k = 5 means: for each point, find the 5 nearest neighbors
# knearneigh gives you a nearest-neighbor structure (which is not yet a full neighbor list)
# knn2nb() Converts the nearest-neighbor object from knearneigh() into an nb object (neighbor list) that can be used in spatial weights (nb2listw()
# If you want one fully connected graph (since there are subgraphs), switch to dnearneigh() with a radius chosen to cover the gaps.

nb <- knn2nb(knearneigh(coords, k = 7)) # 7 nearest neighbors # Warning: "neighbour object has 3 sub-graphs" - points form disconnected groups (clusters), so some parts of the neighbor graph are not connected to others. Here probably the three ecological zones → so you get multiple "sub-graphs". It’s not necessarily an error — it’s telling you the spatial graph is not fully connected.Moran’s I still works, but it’s applied within those sub-graphs.


# Convert to spatial weights
wts <- nb2listw(nb, style = "W") # assigns weights, creates listw object (Plant p93)




# -----------------------------------------------------
# function for testing spaitial autocorrelation using simulations
# -----------------------------------------------------
# note I originally ran both and MoransI was significant, but Moran's I is less accurate for glm so not using further
f_sp_autocor <- function(data,model) {
  
  # Pearson residuals
  resid <- residuals(model, type = "pearson")

  # DHARMa test
  dharma_res <- simulateResiduals(model)
  dharma_spatial <- testSpatialAutocorrelation(dharma_res, x = data$x, y = data$y) # tests for spatial autocorrelation in the residuals

  
  return(dharma_spatial)
}
# ---------------------------------------------


# ---------------------------------------------
# ---------------------------------------------
# FULL MODEL 
# ---------------------------------------------

# base function # ---------------------------
var1<-Reef_state ~ 
  depth+
  sg100+
  I(sg100^2)+
  psi2+
  pr_inhab+
  # pr+ # OR point_dist_Mangrove or river_distance
  # mg+
  river100+
  fishing_30lag+
  blast10+
  pr_inhab:fishing_30lag+
  pr_inhab:blast10+
  MPA+
  ecological_zone2

fxn1<-as.formula(var1)



#Fit logistic regression# -----------------------------------------------------------
m1<-glm(fxn1, 
        data=d1, 
        family=binomial(link=logit), 
        na.action = "na.fail")        # required by dredge
Anova(m1)
summary(m1)
r1<-residuals.glm(m1, type="pearson")
r1

d1_r<-cbind(d1,r1)

d1_r%>%
  filter(abs(r1)>2.5)%>%
  ggplot(aes(x,y,color=r1, size=abs(r1)))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

d1_r%>%
  filter(abs(r1)>2.5)


# Plant methods to check for best model with spatial autocorrelation  ----------------------------
m1_sac<-f_sp_autocor(data=d1,model=m1);m1_sac
# autocorrelated - one method for addressing this is mixed effects models (see Plant 2020)


# --------------------------------
# GLMER
# --------------------------------
# base function # ---------------------------
var2<-Reef_state ~ 
  depth+
  sg100+
  I(sg100^2)+
  psi2+
  pr_inhab+ 
  # pr+ # OR point_dist_Mangrove or river_distance
  # mg+
  river100+
  fishing_30lag+
  blast10+
  pr_inhab:fishing_30lag+
  pr_inhab:blast10+
  MPA+
  (1|ecological_zone) 
fxn2<-as.formula(var2)


# glmer full model ----------------
m_all<-lme4::glmer(fxn2,
                   family=binomial(link=logit), data=d1, na.action = "na.fail")

summary(m_all)
Anova(m_all)
tab_model(m_all, show.df = TRUE)
plot_model(m_all, vline.color = "lightgrey")

r_all<-residuals(m_all, type="pearson")
r_all

d1_m_all_r<-cbind(d1,r_all)


# ---------------------------------------------
# OUTLIERS - all
# ---------------------------------------------
# table of residuals and IDs. use a cutoff of residuals ±1.96
# https://stats.stackexchange.com/questions/196724/how-to-identify-outliers-and-do-model-diagnostics-for-an-lme4-model
# https://stackoverflow.com/questions/24268031/unscale-and-uncenter-glmer-parameters?rq=1
# ---------------------------------------------

# plot residuals for m_all ------------------------
r_all<-residuals(m_all, type="pearson")
r_all

d1_m_all_r<-cbind(d1,r_all)



# graph residuals
d1_m_all_r%>%
  filter(abs(r_all)>4)%>%
  ggplot(aes(x,y,color=r_all))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

d1_m_all_r%>%
  filter(abs(r_all)>2.5)%>%
  ggplot(aes(x,y,color=r_all))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

d1_m_all_r%>%
  # filter(abs(r_all)>2.5)%>%
  ggplot(aes(x,y,color=r_all))+geom_point()


# remove  large residuals
d1_m_all_r%>%
  filter(abs(r_all)>2.5)%>%dplyr::select(point_id)
  
d2<-d1%>%
  filter(point_id!=17&point_id!=63&point_id!=83&point_id!=132&point_id!=145&point_id!=154)%>%
  filter(point_id!=173&point_id!=180&point_id!=269&point_id!=323&point_id!=370&point_id!=381&point_id!=388)%>%
  filter(point_id!=476&point_id!=483&point_id!=578)%>%
  filter(point_id!=606&point_id!=611)%>%
  filter(point_id!=658&point_id!=717&point_id!=788&point_id!=795)%>%
  filter(point_id!=817&point_id!=898&point_id!=900&point_id!=1077&point_id!=1096&point_id!=1203&point_id!=1228)%>%
  filter(point_id!=1246&point_id!=1284&point_id!=1307&point_id!=1371&point_id!=1383&point_id!=1389)%>%
  filter(point_id!=1408&point_id!=1412&point_id!=1433&point_id!=1443&point_id!=1445&point_id!=1464)%>%
  filter(point_id!=1488)%>%
  glimpse()


# re-run cs fxn on variables
d2$depth<-as.numeric(cs. (d2$Depth_m))
d2$sg<-as.numeric(cs. (d2$point_dist_Seagrass))
d1$sg100<-as.numeric(cs. (d1$point_dist_Seagrass100))
d2$mg<-as.numeric(cs. (d2$point_dist_Mangrove))
d2$river<-as.numeric(cs. (d2$point_dist_river))
d2$river100<-as.numeric(cs. (d2$point_dist_river100))
d2$psi<-as.numeric(cs. (d2$patch_shape_index))
d2$pr_orig<-as.numeric(cs. (d2$pop_risk_dens_orig))#pop_risk_dens_orig  #inhab
d2$pr_inhab<-as.numeric(cs. (d2$pop_risk_dens_inhab))
d2$fishing_30lag<-as.numeric(cs. (d2$lag_all_30))
d2$fishing_20lag<-as.numeric(cs. (d2$lag_all_20))
d2$fishing_10lag<-as.numeric(cs. (d2$lag_all_10))
d2$fishing10<-as.numeric(cs. (d2$cumulative_all_00))
d2$fishing20<-as.numeric(cs. (d2$cumulative_all_10))
d2$blast10<-as.numeric(cs. (d2$cumulative_blast_10))
d2$poison10<-as.numeric(cs. (d2$cumulative_poison_10))
d2$pr<-as.numeric(cs. (d2$pop_risk_pop))  # or point_dist_Mangrove or river_distance.nrm or river_distance
d2$psi2<- -1*d2$psi




# save modified dataset (centered/scaled, outliers removed) -----------------------
write_csv(d2,"./results_train/17_IndpVar_Pts_train_for_models_all.csv")


# -----------------------------------------------
# Fit the  global model with reduced outliers
# -----------------------------------------------
m_all2<-lme4::glmer(fxn2,
                    family=binomial(link=logit), data=d2, na.action = "na.fail")


# when I changed river to river100 (dist to rivers/100) which is easier to interpret the model started converging at just under the threshold. Since the results are nearly identical and the extra time is 0.00029 vs 0.0002 I am proceeding.

# Warning message:
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.00293738 (tol = 0.002, component 1)
               
summary(m_all2)
Anova(m_all2)
tab_model(m_all2, show.df = TRUE)
plot_model(m_all2, vline.color = "lightgrey")


# Conditional R2 is the amount of explained variance for the entire model. In this case, both the fixed and random effects explain 77.8% of the variance of the outcome. The marginal R2 explains how much of this variance is attributed to the fixed effects alone- here 40.7%

# visually inspect residuals ------------------
# pull  residuals from model -------------------
r_all2<-residuals(m_all2, type="pearson") # pearson for GLM
r_all2

d2_m_all2_r<-cbind(d2,r_all2)


# ---------------------------------------------
# OUTLIERS
# ---------------------------------------------
# plot residuals for m_all2 ------------------------
d2_m_all2_r%>%
  filter(abs(r_all2)>4)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

d2_m_all2_r%>%
  filter(abs(r_all2)>3.5)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)


d2_m_all2_r%>%
  filter(abs(r_all2)>1.96)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()#+geom_label(aes(label = point_id), nudge_y=0.2)


d2_m_all2_r%>%
  # filter(abs(r_all)>2.5)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()


# ---------------------------------------------
# check model - all2
# ---------------------------------------------
qqmath(ranef(m_all2)) 
qqmath(fixef(m_all2))
coef(m_all2) # reults of fixed and random effects add to coef
fixef(m_all2)  #estimated averages over the eco zones # average coefficents
ranef(m_all2) # group level errors for intercepts and slopes

# visualize aspects of  model ------------------------
visreg(m_all2,"depth","ecological_zone", xlab="Depth",ylab="Reef State")
visreg(m_all2,"psi2","ecological_zone", xlab="Patch compactness",ylab="Reef State")
visreg(m_all2,"sg100","ecological_zone", xlab="Seagrass isolation",ylab="Reef State")
visreg(m_all2,"pr_inhab","ecological_zone", xlab="Population_risk",ylab="Reef State")
visreg(m_all2,"fishing_30lag","ecological_zone", xlab="Fishing_legacy_1980_2000",ylab="Reef State")
visreg(m_all2,"blast10","ecological_zone", xlab="Blast_fishing_2010_2000",ylab="Reef State")


# -------------------------------
# Dredge models and average
# -------------------------------

# https://uoftcoders.github.io/rcourse/lec09-model-selection.html
# less than 2 are considered to be just as good as the top model and thus we shouldn’t just discount them

m_all_set<-dredge(m_all2, rank = "AIC")
head(m_all_set)


# Get top model --------------------
top_model <- get.models(m_all_set, subset = 1)[[1]]
top_model

# Get models with <2 delta AICc - all --------------------
top_m1 <- get.models(m_all_set,subset = delta<=2)
top_m1
# 

# model averaging # -----------------------------------

# The “full” coefficients are thus more conservative and it is best practice to interpret these
m1_avg <- model.avg(top_m1,cumsum(weight) <= .95, fit = TRUE, subset = delta < 2) # fit = TRUE makes model.avg() keep fitted models
m1_avg


# -----------------------------------
# -- SAVE --------------
# -----------------------------------

# save full model to file -----------
save(m_all2,file="./results_train/model_full.R")

# Save averaged model to file
saveRDS(m1_avg, file = "./results_train/model_full_avg.rds")

#save image to set options for probabilities in GT
save.image("./results_train/mixedEf_final_all1.RData")

# or load image...
# load("./results_train/mixedEf_final_all1.RData")





# -----------------------------------
# interpreting output from model averaging 
# -----------------------------------
cof<-data.frame(m1_avg$coefficients[1,]) # full
cof
summary(m1_avg)            # shows (full average) & (conditional average)


coef_full <- coef(m1_avg, full = TRUE)      # Extract full-model average coefficients.  # estimated intercepts for each eco zone # see gelman and hill p303

summary(m1_avg, full = TRUE)$coefmat.full   # full model average with SE, z, p
confint(m1_avg, full = TRUE)                # 95% CI for full model average




# df = number of estimated parameters
# logLik = log-likelihood of the model
# AICc = corrected Akaike Information Criterion
# ΔAICc = difference from the best model
# weight = model weight (relative support, adds to 1 across models)
# When you run model.avg(), you’re telling R: don’t pick just one — combine them proportionally to their weights.

# Estimate = the model-averaged regression coefficient.
# If a variable only appears in some models, its coefficient is “shrinked” toward 0 depending on how often it appears.
# SE = unconditional standard error of the estimate.
# Adjusted SE = accounts for model selection uncertainty.
# z value and Pr(>|z|) = significance test, similar to glm summary.

# Full average if your goal is prediction (because it incorporates model selection uncertainty).
# Conditional average if your goal is inference about effect sizes (assuming the variable is “really in” the model).
# Burnham & Anderson 2004 recommend reporting both the importance and full-average estimates for transparency.



# --------------------------------------------------
#  -- MODEL WITH NO LANDSCAPE VARIABLES ------------
# without landscape var (used this because landscape variables are usually not included in coral surveys. Used for analysis in paper, but not included here.)
# --------------------------------------------------

# function ----------
var3<-Reef_state ~ 
  depth+
  pr_inhab+ 
  fishing_30lag+
  blast10+
  pr_inhab:fishing_30lag+
  pr_inhab:blast10+
  MPA+
  (1|ecological_zone) 

fxn3<-as.formula(var3)

#  -- run model with no landscape var ----------------------
m_all3<-lme4::glmer(fxn3,
                   family=binomial(link=logit), data=d2, na.action = "na.fail")

summary   (m_all3)
tab_model (m_all3, show.df = TRUE) 
plot_model(m_all3, vline.color = "lightgrey") 

# ---------------------------------------------
# check model - all3
# ---------------------------------------------
qqmath(ranef(m_all3))
qqmath(fixef(m_all3))

# visualize aspects of final model, no landscape ------------------------
visreg(m_all3,"depth","ecological_zone", xlab="Depth",ylab="Reef State")
visreg(m_all3,"pr_inhab","ecological_zone", xlab="Population_risk",ylab="Reef State")
visreg(m_all3,"fishing_30lag","ecological_zone", xlab="Fishing_legacy_1980_2000",ylab="Reef State")
visreg(m_all3,"blast10","ecological_zone", xlab="Blast_fishing_2010_2000",ylab="Reef State")


# ---------------------------------------------
# Model selection ---------------------------------------
# ---------------------------------------------

# https://uoftcoders.github.io/rcourse/lec09-model-selection.html

# less than 2 are considered to be just as good as the top model and thus we shouldn’t just discount them

m_all_set3<-dredge(m_all3, rank = "AIC")
head(m_all_set3)


# Get top model --------------------
top_model3 <- get.models(m_all_set3, subset = 1)[[1]]
top_model3

# Get models with <2 delta AICc - all --------------------
top_m3 <- get.models(m_all_set3,subset = delta<2)
top_m3
# 

# model averaging # -----------------------------------

# The “full” coefficients are thus more conservative and it is best practice to interpret these
m3_avg <- model.avg(top_m3,cumsum(weight) <= .95, fit = TRUE, subset = delta < 2) # fit = TRUE makes model.avg() keep fitted models
m3_avg

# ---------------------------------
# SAVE 
# ----------------------------------

# models --------------
# save full model to file -----------
save(m_all3,file="./results_train/model_no_landscape.R")

# Save averaged no landccape model to file
saveRDS(m3_avg, file = "./results_train/model_no_landscape_avg.rds")

#save updated image to set options for probabilities in GT
save.image("./results_train/mixedEf_final_all1_no_landscape.RData")




# outputs ---------------
cof3<-data.frame(m3_avg$coefficients[1,]) # full
cof3
summary(m3_avg)            # shows (full average) & (conditional average)
sw(m3_avg)                # relative importance of predictors

coef_full3 <- coef(m3_avg, full = TRUE)      # Extract full-model average coefficients - MPA and blast fishing less important

summary(m3_avg, full = TRUE)$coefmat.full   # full model average with SE, z, p
confint(m3_avg, full = TRUE)                # 95% CI for full model average



# Coefficients, SEs, z, p
coef_table3 <- summary(m3_avg, full = TRUE)$coefmat.full
coef_table3

# Confidence intervals
ci3 <- confint(m3_avg, full = TRUE)



# ---------------------------------
# SAVE (same as above, here in one place)
# ----------------------------------

# models --------------
# Save averaged object to file
saveRDS(m1_avg, file = "./results_train/model_full_avg.rds")
saveRDS(m3_avg, file = "./results_train/model_no_landscape_avg.rds")

# main model with all variables
save(m_all2,file="./results_train/model_full.R")
save(m_all3,file="./results_train/model_no_landscape.R")


#save image to set options for probabilities in GT
save.image("./results_train/mixedEf_final_all1.RData")

# reload if needed 
remove(list=ls())
load("./results_train/mixedEf_final_all1.RData")



# save modified dataset (centered/scaled, outliers removed) -----------------------
write_csv(d2,"./results_train/17_IndpVar_Pts_train_for_models_all.csv")


