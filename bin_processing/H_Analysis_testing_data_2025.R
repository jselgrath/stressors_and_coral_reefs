# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(lattice)
library(boot)
library(sjPlot)
library(car)
library(pROC)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------

# ----------------------------------------------
# load final models
# ----------------------------------------------
load("./results_train/model_full.R") #full model
load("./results_train/model_no_landscape.R") #full model, no landscape variables
#         


# ---------------------------------------------
# load data and select non-correlated variables (see Correlation Viz)
# ---------------------------------------------
d0<-read_csv("./results_test/16_IndpVar_Pts_test_all.csv")%>%
  mutate(Depth_m=if_else(Depth_m<0,Depth_m,-.1),   # just in case
         Depth_m=Depth_m*-1)%>%   # made depth positive for easier interpretation
  mutate(Depth_m=if_else(Depth_m>15,15,Depth_m))%>%   # made depth narrower, as per main model
  mutate(MPA=as.factor(mpa_id),     # make factors
         ecological_zone=as.factor(ecological_zone),
         Reef_state=resilience_id
  )%>%
  filter(!is.na(Depth_m))%>% # one point
  glimpse()

d0$ecological_zone
d0$Depth_m

# check depth representation # ---------------------------------------------
range(d0$Depth_m)

# graph of depth
with(d0,xyplot(Reef_state~Depth_m|ecological_zone,type=c('g','p','l'),
               layout=c(4,1), index.cond = function(x,y)max(y)))

# check depth representation # ---------------------------------------------
max(d0$Depth_m, na.rm=T)
  
  # blast fishing in deep water - using to check for high blast fishing in shallow areas
  
d0%>%
    filter(Depth_m>12)%>%
    ggplot(aes(x,y,color=Depth_m,size=cumulative_blast_10))+geom_point()+geom_label(aes(label = point_id), nudge_y=0.2) #- one deep area is an atoll. fixed below
  
# remove NAs and make extreme depths shallower  - assuming spline errors
d1<-d0 %>%
    mutate(Depth_m=if_else(Depth_m>-15,Depth_m,-15))%>%#~8 deep outliers - probably misclassified in depth map. 
  glimpse()
    
d1<-data.frame(na.omit(d1))%>%# 27 NAs -  in population risk variables 
    glimpse()
  
d1%>%
    ggplot(aes(x,y,color=Reef_state))+geom_point()#+geom_label(aes(label = point_id), nudge_y=0.2)
  
  plot(d1$ecological_zone)

# calc percent
d1%>%
  group_by(Reef_state)%>%
  summarize(n=n())%>%
  glimpse()
#464 coral, 1019 - rubble = 31% coral in testing data

#############################
# centering variables based on mean
# from gelman and hill p 55
# https://www.r-bloggers.com/a-faster-scale-function/
# or substract by mean and divide by max (0-1)
##############################

# ---------------------------------------------
# center and scale function
# ---------------------------------------------
cs.<- function(x) scale(x,center=TRUE,scale=TRUE)



# ---------------------------------------------
# subset data, clean data names to match final model
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
d1$psi2<- -1*d1$psi # patch complexity

# ---------------------------------------------
# precompute interactions
# ---------------------------------------------
# precompute
d1$sg2<-d1$sg^2
d1$pri_f30<-d1$pr_inhab*d1$fishing_30lag
d1$pri_b10<-d1$pr_inhab*d1$blast10

glimpse(d1)

# set to same name as fxn
d2<-d1

# ----------------------------------------
# Re-Build model with new data
# ----------------------------------------
# predicted probabilities
d2$p_m_final<- round(predict(m_all2, newdata = d2, type = "response"),3)
qplot(d2$p_m_final) # historgram of predicted probabilities


d2$p_m_final_no_l<- round(predict(m_all3, newdata = d2, type = "response"),3)
qplot(d2$p_m_final_no_l)

# ----------------------------------------
#  -- Save predicted results to examine in GIS
# ----------------------------------------
write_csv(d2,"./results_test/m_final_test_data.csv")




# ----------------------------------------
# -- Checking Fit
# ----------------------------------------

## --- Model 1 (with landscape vars)
roc1 <- roc(d2$Reef_state, d2$p_m_final)
auc1 <- auc(roc1)
plot(roc1)

cm1 <- table(Predicted = d2$pred_class, Observed = d2$Reef_state)

TP1 <- cm1["1","1"]; TN1 <- cm1["0","0"]
FP1 <- cm1["1","0"]; FN1 <- cm1["0","1"]

prod_acc_pos1 <- TP1 / (TP1 + FN1)
prod_acc_neg1 <- TN1 / (TN1 + FP1)
user_acc_pos1 <- TP1 / (TP1 + FP1)
user_acc_neg1 <- TN1 / (TN1 + FN1)
total_acc1    <- (TP1 + TN1) / sum(cm1)
f1_pos1       <- 2 * (user_acc_pos1 * prod_acc_pos1) / (user_acc_pos1 + prod_acc_pos1)

metrics1 <- data.frame(
  Model  = "full",
  Metric = c("TP","TN","FP","FN",
             "Producer's Acc (Presence)","Producer's Acc (Absence)",
             "User's Acc (Presence)","User's Acc (Absence)",
             "Total Accuracy","F1 Score (Presence)","ROC AUC"),
  Value  = c(TP1, TN1, FP1, FN1,
             round(prod_acc_pos1,3), round(prod_acc_neg1,3),
             round(user_acc_pos1,3), round(user_acc_neg1,3),
             round(total_acc1,3), round(f1_pos1,3), round(auc1,3))
)

## --- Model 2 (no landscape vars)
roc2 <- roc(d2$Reef_state, d2$p_m_final_no_l)
auc2 <- auc(roc2)
plot(roc2)

# thresholding at 0.5 unless you want optimal threshold
d2$pred_class_no_l <- ifelse(d2$p_m_final_no_l > 0.5, 1, 0)

cm2 <- table(Predicted = d2$pred_class_no_l, Observed = d2$Reef_state)

TP2 <- cm2["1","1"]; TN2 <- cm2["0","0"]
FP2 <- cm2["1","0"]; FN2 <- cm2["0","1"]

prod_acc_pos2 <- TP2 / (TP2 + FN2)
prod_acc_neg2 <- TN2 / (TN2 + FP2)
user_acc_pos2 <- TP2 / (TP2 + FP2)
user_acc_neg2 <- TN2 / (TN2 + FN2)
total_acc2    <- (TP2 + TN2) / sum(cm2)
f1_pos2       <- 2 * (user_acc_pos2 * prod_acc_pos2) / (user_acc_pos2 + prod_acc_pos2)

metrics2 <- data.frame(
  Model  = "No landscape",
  Metric = c("TP","TN","FP","FN",
             "Producer's Acc (Presence)","Producer's Acc (Absence)",
             "User's Acc (Presence)","User's Acc (Absence)",
             "Total Accuracy","F1 Score (Presence)","ROC AUC"),
  Value  = c(TP2, TN2, FP2, FN2,
             round(prod_acc_pos2,3), round(prod_acc_neg2,3),
             round(user_acc_pos2,3), round(user_acc_neg2,3),
             round(total_acc2,3), round(f1_pos2,3), round(auc2,3))
)

## --- Combine both
metrics_tbl <- rbind(metrics1, metrics2)

metrics_tbl

# save table -------------
write_csv(metrics_tbl, "./doc/prediction_metrics.csv")


# ----------------------------------------
# -- graph roc plots
# ----------------------------------------

roc_df1 <- data.frame(
  FPR = 1 - roc1$specificities,
  TPR = roc1$sensitivities,
  Model = "Full Model\n(ROC AUC = 0.86)"
)

roc_df2 <- data.frame(
  FPR = 1 - roc2$specificities,
  TPR = roc2$sensitivities,
  Model = "No Landscape Variables\n(ROC AUC = 0.76)"
)

roc_df <- rbind(roc_df1, roc_df2)

source("./bin_processing/deets_2025.R")

# ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
#   geom_line(linewidth = 1.2) +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   theme_minimal(base_size = 14) +
#   labs(title = "ROC Curve Comparison", x = "False Positive Rate", y = "True Positive Rate")


# colors from viridis palette
cols <- sequential_hcl(2, palette = "Emrld")


# ROC plot
roc_plot <- 
  ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = cols) +
  theme_minimal(base_size = 14) +
  labs(
    title = "ROC Curve Comparison",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Model"
  ) 


# Print to screen
print(roc_plot)

# Export to file
ggsave("./doc/roc_comparison.png", roc_plot,
       width = 6, height = 4, dpi = 300)


