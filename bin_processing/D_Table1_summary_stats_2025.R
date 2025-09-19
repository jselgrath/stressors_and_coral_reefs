# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# GOAL: calculate sd for understanding changes in model, and other summary statistics

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

# -------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------

# load data - original, unscaled
d1<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>% # all
  mutate(Depth_m=Depth_m)%>%   # made depth positive for easier interpretation
  mutate(MPA=as.factor(mpa_id),     # make factors
         ecological_zone=as.factor(ecological_zone),
         Reef_state=resilience_id
  )%>%
  # mutate(depth=-1*Depth_m)%>%
  mutate(patch_compactness=patch_shape_index*-1)%>% # negative in models
  glimpse()

d1$Depth_m

# table to add values to
d0<-read_csv("./doc/model_avg_odds_ratios_full.csv")%>%
  glimpse()
d0


# -----------------------------
# Fishing Calculations - lag 
# ------------------------------

# to back transform normalized fishing data to regular scale --------------------------
# max value for fishing
fa<-read_csv("./doc/fishing_all_max_all_yr_coralarea.csv")%>%
  summarize(max_all=max(max_a))%>%
  glimpse()

names(d1)

# back-transform per row to raw days (three-year total)
fishing_days_sum <- d1%>%
  mutate(lag_all_30_raw=lag_all_30 * fa$max_all)%>%
  summarize(sd_lag_all_30_raw=round(sd(lag_all_30_raw,na.rm=T),2))%>%
  glimpse()

# back transform to raw days per year - per-year standard deviations.
sd_year1 <- sd(d1$all_1980_nrmA * fa$max_all, na.rm = TRUE)
sd_year2 <- sd(d1$all_1990_nrmA * fa$max_all, na.rm = TRUE)
sd_year3 <- sd(d1$all_2000_nrmA * fa$max_all, na.rm = TRUE)

# single sd averaged across all years
mean_sd_years <- mean(c(sd_year1, sd_year2, sd_year3))%>%
  glimpse()

# -----------------------------
# Fishing Calculations - cumulative blast fishing 
# ------------------------------

# to back transform normalized fishing data to regular scale --------------------------
# max value for blast fishing
fb<-read_csv("./doc/fishing_blast_max_all_yr_coralarea.csv")%>%
  summarize(max_blast=max(max_b))%>%
  glimpse()

names(d1)

# back-transform per row to raw days (three-year total)
fishing_days_sum_b <- d1%>%
  mutate(cumulative_blast_10_raw=cumulative_blast_10 * fb$max_blast)%>%
  summarize(cumulative_blast_10_sd=sd(cumulative_blast_10_raw,na.rm=T))%>%
  glimpse()

# back transform to raw days per year - per-year standard deviations.
sd_year1_b <- sd(d1$blast_2000_nrmA * fb$max_blast, na.rm = TRUE)
sd_year2_b <- sd(d1$blast_2010_nrmA * fb$max_blast, na.rm = TRUE)


# single sd averaged across all years
mean_sd_years_b <- mean(c(sd_year1_b, sd_year2_b))%>%
  glimpse()



# ---------------------------------------
# calc means for data
# ---------------------------------------
# ----------------------------------------------
# subset numeric variables only, exluding fishing
d2<-d1%>%
  dplyr::select(depth=Depth_m,
                sg100 =point_dist_Seagrass100, 
                river100=point_dist_river100,
                pr_inhab =pop_risk_dens_inhab,
                psi2=patch_compactness,
                blast10 =cumulative_blast_10,
                fishing_30lag=lag_all_30
                )%>%
  glimpse()
nm<- names(d2)

# calculate summary statistics
d3<- d2%>%
  pivot_longer(names_to = "Term",cols=c(depth:fishing_30lag))%>%
  group_by(Term)%>%
  summarize(
    n=n(),
    u_var=round(mean(value,na.rm=T),2),
    sd_var=round(sd(value,na.rm=T),2),
    sem_var=round((sd_var/sqrt(n)),2),
    min=round(min(value,na.rm=T),2),
    max=round(max(value,na.rm=T),2))%>%
  # mutate(ordr=c(1,4,3,6,5,2,7))%>%
  mutate(range=paste0(min," - ",max))%>%
  # arrange(ordr)%>%
  glimpse()
d3

# with multi year fishing values -----------------
write_csv(d3,"./doc/D_IndpVar_Pts_MeanSD_train_normalized_fishing.csv")


#update table with averaged sd of years fishing
t1<-d3%>%
  dplyr::select(Term,sd_var)%>%
  glimpse()
t1$sd_var[t1$Term=="fishing_30lag"]<-mean_sd_years
t1$sd_var[t1$Term=="blast10"]<-mean_sd_years_b

t1
t2<-t1%>%
  full_join(d0)%>%  
  glimpse()
t2


# --save to .csv so can import for probability info
write_csv(t2,"./doc/model_avg_odds_ratios_full2.csv")
