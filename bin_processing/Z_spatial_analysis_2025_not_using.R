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
  # mutate(ecological_zone=if_else(ecological_zone=="Terrestrial Island","Coastal",ecological_zone))%>% # combine terrestrial islands and coastal so enough rubble samples for model
  mutate(MPA=as.factor(mpa),     # make factors
         ecological_zone=as.factor(ecological_zone),
         ecological_zone2=as.factor(ecological_zone2), # not updated
         geomorphology=as.factor(geomorphology),
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
  # filter(point_id!=402,point_id!=719, point_id!=876, point_id!=944, point_id!=1040, point_id!=1250,point_id!=1274)%>%
  # filter(point_id!=33,point_id!=535,point_id!=1241)%>%
  # point_id!=121,point_id!=293,point_id!=423, point_id!=481, point_id!=635,point_id!=657, point_id!=686,
  #        point_id!=815,point_id!=869
  #        point_id!=1008,point_id!=1422,point_id!=1350)%>%
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
cs.<- function(x) scale(x,center=FALSE,scale=TRUE)

# run cs fxn on variables
d1$depth<-as.numeric(cs. (d1$Depth_m))
d1$sg<-as.numeric(cs. (d1$point_dist_Seagrass))
d1$mg<-as.numeric(cs. (d1$point_dist_Mangrove))
d1$river<-as.numeric(cs. (d1$river_distance))
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
  sg+
  I(sg^2)+# sg2+
  psi+
  pr_inhab+
  # pr+ # OR point_dist_Mangrove or river_distance
  # mg+
  # river+
  fishing_20lag+
  # fishing_30lag+
  blast10+
  # I(pr_inhab*fishing_30lag)+
  I(pr_inhab*blast10)+
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


# Plant methods to check for best model with spatial autocorrelation  -----------------------------------------
m1_sac<-f_sp_autocor(data=d1,model=m1);m1_sac




# --------------------------------
# GLMER
# --------------------------------
# base function # ---------------------------
var2<-Reef_state ~ 
  depth+
  sg+
  I(sg^2)+# sg2+
  psi+
  pr_inhab+
  # pr+ # OR point_dist_Mangrove or river_distance
  # mg+
  # river+
  fishing_30lag+
  blast10+
  # I(pr_inhab*fishing_30lag)+
  I(pr_inhab*blast10)+
  # MPA+
  (1|ecological_zone) 

fxn2<-as.formula(var2)




# glmer full model ----------------
m_all<-lme4::glmer(fxn2,
                   family=binomial(link=logit), data=d1, na.action = "na.fail")
summary(m_all)
Anova(m_all)
tab_model(m_all, show.df = TRUE)
plot_model(m_all, vline.color = "lightgrey")

# m_all_sac<-f_sp_autocor(data=d1,model=m_all);m_all_sac # spatial autocorrelation of residuals

r_all<-residuals(m_all, type="pearson")
r_all

d1_m_all_r<-cbind(d1,r_all)

# plot residuals for m_all ------------------------
r_all<-residuals(m_all, type="pearson")
r_all

d1_m_all_r<-cbind(d1,r_all)



# graph residuals
d1_m_all_r%>%
  filter(abs(r_all)>5)%>%
  ggplot(aes(x,y,color=r_all))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

d1_m_all_r%>%
  filter(abs(r_all)>2.5)%>%
  ggplot(aes(x,y,color=r_all))+geom_point()#+geom_label(aes(label = point_id), nudge_y=0.2)

d1_m_all_r%>%
  # filter(abs(r_all)>2.5)%>%
  ggplot(aes(x,y,color=r_all))+geom_point()


# remove five large residuals

d1%>%
  filter(point_id==63)%>%
  glimpse()

d2<-d1%>%
  filter(point_id!=63&point_id!=1445&point_id!=1464&point_id!=388&point_id!=323)




# rerun model without large outliers--------------------------------
m_all2<-lme4::glmer(fxn2,
                    family=binomial(link=logit), data=d2, na.action = "na.fail")

# visualize results ------------------------
summary(m_all2)
Anova(m_all2)
tab_model(m_all2, show.df = TRUE)
# Conditional R2 is the amount of explained variance for the entire model. In this case, both the fixed and random effects explain 75.7% of the variance of the outcome. The marginal R2 explains how much of this variance is attributed to the fixed effects alone- here 38.2%

plot_model(m_all2, vline.color = "lightgrey")





# visually inspect residuals ------------------
# pull  residuals from model -------------------
r_all2<-residuals(m_all2, type="pearson")
r_all2

d2_m_all2_r<-cbind(d2,r_all2)

# plot residuals for m_all2 ------------------------
d2_m_all2_r%>%
  filter(abs(r_all2)>2.5)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

d2_m_all2_r%>%
  filter(abs(r_all2)>5)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2)

d2_m_all2_r%>%
  filter(abs(r_all2)>2.5)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()#+geom_label(aes(label = point_id), nudge_y=0.2)


d2_m_all2_r%>%
  # filter(abs(r_all)>2.5)%>%
  ggplot(aes(x,y,color=r_all2))+geom_point()






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
                      cs.(lag_all_30)+
                      cs.(cumulative_FA_blast10)+
                      cs.(PopRskDecay.Nrm):cs.(lag_all_30)+
                      cs.(PopRskDecay.Nrm):cs.(cumulative_FA_blast10)+ 
                      MPA+
                      # Geomorphic2+
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
































lm.RStests (m1,wts,test="all") # Plant p457

# tests for heteroscedasticity 
library(lmtest)# Plant p462

# breusch-pagan
bptest(m1)

# levene
fits.med<-median(fitted (m1))
fits.groups<-fitted(m1) <= fits.med
leveneTest(residuals(m1),factor(fits.groups))

# if sig, plot residuals against variables 







# fit spatial lag model
m1_lag<-lagsarlm(fxn1,data=d1,listw=wts) # 	package=spatialreg; Spatial simultaneous autoregressive model estimation by maximum likelihood
summary(m1_lag)

# fit spatial error model
m1_err<-efforsarlm(fxn1+d1,listw=wts)
summary(m1_err)

# pick option with lower AIC

# another option
m1_wb<-nb2listw(nb,style="B") #weights, different type
m1_car<-spautolm(fxn1,data-d1,family="CAR", listw=m1_wb)
summary(m1_car)

print(coef(m1_lag), digits=2)
print(coef(m1_err), digits=2)
print(coef(m1_car), digits=2)
# -----------------------------------------------------------------------------


# Moran eigenvector GLM filtering
# https://r-spatial.github.io/spatialreg/reference/ME.html

system.time(m1_me<-ME(fxn1, 
          data=d1, 
          family=binomial(link="logit"), 
          listw=wts,
          alpha=0.1, #0.5
          nsim=5, 
          verbose=TRUE, 
          na.action = "na.fail"))
  
glm_m1_me <- glm(fxn1 + fitted(m1_me), family=binomial(link="logit"))
#anova(glm_m1_me , test="Chisq")
coef(summary(glm_m1_me ))
anova(m1, glm_m1_me, test="Chisq")


#-------------------------------------
# add spatial eigenvector (dbMEM)
# -------------------------------------

# Build dbMEM from coordinates; choose a neighborhood so MEMs are meaningful
# Use minimum spanning tree + threshold distance to ensure connectivity:
dthresh <- nb |> spdep::nbdists(coords) |> unlist() |> max() 
# nb was spdep::nnearneigh(coords, k=1)
mem <- dbmem(coords, 
             thresh = dthresh,
             MEM.autocor="positive")  # spatial filters (columns) #distance-based Moran's eigenvector maps (dbMEM, also called dbMEM spatial eigenfunctions) from a geographic distance matrix, in view of spatial eigenfunction analysis

# Select informative MEMs using Pearson residuals from m1
resid_m1 <- residuals(m1, type = "pearson")

# Simple screening: keep MEMs correlated with residuals (p < 0.05)
pvals <- apply(as.matrix(mem), 2, function(z) summary(lm(resid_m1 ~ z))$coefficients[2,4])
keep  <- names(pvals)[pvals < 0.05]
mem_sel <- if (length(keep)) as.data.frame(mem[, keep, drop=FALSE]) else NULL



#  build mem formula ---------------------------------------------
if (!is.null(mem_sel)) {
  names(mem_sel) <- paste0("mem", seq_len(ncol(mem_sel)))
  d1_df <- bind_cols(d1, mem_sel)


# Build MEM formula terms
mem_terms <- paste(names(mem_sel), collapse = " + ")

# Extract response and base predictors from fxn1
base_formula <- paste(deparse(fxn1), collapse = " ") # deparse extracts the formula text as a character string

# Append MEM terms to the right-hand side
fxn2 <- as.formula(
  paste(base_formula, "+", mem_terms)
)
} else {
  d1_df <- d1
  fxn2  <- fxn1
}

# screen mems by explained variance (too many!) -----------------------
# Rank MEMs by R² of regression against residuals
mem_r2 <- apply(as.matrix(mem_sel), 2, function(z) summary(lm(resid_m1 ~ z))$r.squared)

# Keep the top 5 MEMs
top_mem <- names(sort(mem_r2, decreasing = TRUE))[1:5]
mem_sel_top <- mem_sel[, top_mem, drop = FALSE]
mem_terms2 <- paste(names(mem_sel_top), collapse = " + ")

# Append MEM terms to the right-hand side
fxn3 <- formula(
  paste(base_formula, "+", mem_terms2 )
)



#----------------------------------------
# Dredge to find best model #-------------
#----------------------------------------
m2 <- glm(fxn3, 
               data = d1_df, 
               family = binomial(link=logit),
               na.action = "na.fail")

# Autocorrelation  -------------------
m2_sac<-f_sp_autocor(data=d1_df,model=m2);m2_sac

system.time(dd <- dredge(m_start, rank = "AIC"))

# inspect top models
head(dd, 10)

# Best model (AICc)
m2 <- get.models(dd, subset = 1)[[1]]
summary(m2)

# Autocorrelation of top dredge model -------------------
m2_sac<-f_sp_autocor(data=d1,model=m2);m2_sac

# family = binomial, the coefficients are on the log-odds scale. exponentiate them to get odds ratios:
exp(coef(m2))



# -----------------------------------
# model averaging
system.time(m2_avg <- model.avg(dd, subset = delta < 2))
summary(m2_avg)            # shows (full average) & (conditional average)
importance(m2_avg)         # relative importance of predictors






# exponate coefficents
ec_m1<-exp(coef(m_avg))

ec_sac<-f_sp_autocor(data=d1,model=m_avg)

# output from model averaging ------------------------------------
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







#---------------------------------------
# Example 1: Full model m1
#---------------------------------------
#----------------------------------------
# Step 1: Full model m1
#----------------------------------------
m1_results <- fit_and_check(fxn1, d1, "m1")

summary(m1_results$model_m1)
m1_results$moran_m1
m1_results$dharma_m1

#----------------------------------------
# Step 2: Dredge to find best model
#----------------------------------------
m1<-m1_results$model_m1
d1_dredge <- dredge(m1, rank = "AIC")

# Best model = m2
m2 <- get.models(d1_dredge, subset = 1)[[1]]

# Residual check for best dredge model
m2_results <- fit_and_check(formula(m2), d1, "m2")

summary(m2_results$model_m2)
m2_results$moran_m2
m2_results$dharma_m2



























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




































m_all<-lme4::glmer(Reef_state ~ 
                     cs.(Depth_m) +
                     cs.(point_dist_Seagrass)+ 
                     cs.(I(point_dist_Seagrass^2))+
                     cs.(-patch_shape_index)+
                     cs.(pop_risk_dens_orig)+ #pop_risk_dens_inhab
                     cs.(lag_all_30)+
                     cs.(cumulative_blast_10)+
                     cs.(pop_risk_pop)+  # or point_dist_Mangrove or river_distance.nrm or river_distance
                     cs.(pop_risk_dens_orig):cs.(lag_all_30)+
                     cs.(pop_risk_dens_orig):cs.(cumulative_blast_10)+ 
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
                      cs.(lag_all_30)+
                      cs.(cumulative_FA_blast10)+
                      cs.(PopRskDecay.Nrm):cs.(lag_all_30)+
                      cs.(PopRskDecay.Nrm):cs.(cumulative_FA_blast10)+ 
                      MPA+
                      # Geomorphic2+
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
plotResiduals(simulationOutput, form = cs.(d_fitted$lag_all_30))
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
         Fishing_legacy_1980_2000=cs.(lag_all_30)[,1],
         Blast_fishing_2010_2000=cs.(cumulative_FA_blast10)[,1])%>%
  dplyr::select(
    Reef_state,Depth, Seagrass_isolation,Patch_compactness,Population_risk,
    Fishing_legacy_1980_2000,Blast_fishing_2010_2000,MPA,Ecological_zone=ecological_zone,x,y,point_id%>%
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



