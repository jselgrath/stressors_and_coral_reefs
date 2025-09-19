# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs 
# --------------------------------------------

# goal: create Table 1 from model outputs
# Effects are standardized

# -------------------------------------
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(ggtext)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------

# load averaged model  ---------------
m1_avg <- readRDS("./results_train/model_full_avg.rds")
# m3_avg <- readRDS("./results_train/model_no_landscape_avg.rds")


# load final model (m_all2) for R2 values from full models ---------------
load("./results_train/model_full.R")
# load("./results_train/model_model_no_landscape.R")


# load data - fit to model (centered & scaled) ---------------------
d2<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>% # all
  mutate(MPA=as.factor(mpa_id),     # make factors - depth already positive
         ecological_zone=as.factor(ecological_zone),
         Reef_state=resilience_id )%>%
  glimpse()

# table to add to --
tab_export<-read_csv("./doc/model_avg_odds_ratios_full2.csv")%>%glimpse()

# -----------------------------------------------------------------

# -------------------------------------------
# --Add change in probability column to table --
# -------------------------------------------


# get averaged prediction ---------------------
# averaged fitted probabilities for each row in your dataset


# logit -> probability
logit2prob <- function(x) exp(x) / (1 + exp(x))

# Extract the original dataset used to fit the global model
model_data <- getData(m_all2)   # works for lme4/glmer fits


# get coefficient names, drop intercept
coef_terms <- setdiff(names(fixef(m_all2)), "(Intercept)")

# keep only those terms that match dataset column names
vars <- coef_terms[coef_terms %in% names(model_data)]





# --------------------------------------------------------------
# -- Function: average marginal effect for a variable----------------
marginal_effect <- function(var, model, data, per_sd = TRUE) {
  
  # Decide step size (per 1 SD or per 1 unit)
  step_for <- function(x) if (per_sd) sd(x, na.rm = TRUE) else 1
  
  data_up <- data
  
  # --- Case 1: main effect (column exists directly) ---
  if (var %in% names(data)) {
    step <- step_for(data[[var]])
    data_up[[var]] <- data_up[[var]] + step
  }
  
  # --- Case 2: squared term like "I(sg^2)" ---
  else if (grepl("^I\\(.+\\^2\\)$", var)) {
    base_var <- sub("^I\\((.+)\\^2\\)$", "\\1", var)
    if (!base_var %in% names(data)) return(NA)
    
    step <- step_for(data[[base_var]])
    data_up[[base_var]] <- data_up[[base_var]] + step
  }
  
  # --- Case 3: interaction term like "a:b" ---
  else if (grepl(":", var)) {
    parts <- strsplit(var, ":")[[1]]
    for (p in parts) {
      if (!p %in% names(data)) return(NA)
    }
    # step the first variable in the interaction
    step <- step_for(data[[parts[1]]])
    data_up[[parts[1]]] <- data_up[[parts[1]]] + step
  }
  
  # --- If not found, skip ---
  else {
    return(NA)
  }
  
  # Predictions
  p0 <- predict(model, newdata = data, type = "response", full = TRUE)
  p1 <- predict(model, newdata = data_up, type = "response", full = TRUE)
  
  mean(p1 - p0, na.rm = TRUE)
}


# apply fxn - skip interaction and polynomial terms that aren’t raw columns in your dataset.
delta_prob <- sapply(vars, marginal_effect,
                     model = m1_avg, data = model_data, per_sd = TRUE)

delta_df <- data.frame(
  Term = vars,
  DeltaProb = round(delta_prob * 100, 1)
)

# --------------------------------------------------------
# apply function to predictions
model_data <- getData(m_all2) # data from main model
coef_terms <- setdiff(names(fixef(m_all2)), "(Intercept)")

delta_prob <- sapply(coef_terms, marginal_effect,
                     model = m1_avg, data = model_data, per_sd = TRUE)

delta_df <- data.frame(
  Term = coef_terms,
  DeltaProb = round(delta_prob * 100, 2)  # % change in probability
)


# merge with results table
tab_with_marg <- tab_export %>%
  left_join(delta_df, by = "Term")%>%
  dplyr::select(Variable=Variables, Type = VarType, OddsRatio, CI, p_value, DeltaProb, sd_var, WeightSum, Nmodels)%>%
  glimpse()

tab_with_marg

# and for MPAs ------------------------------
d0 <- model_data; d0$MPA <- factor(600)
d1 <- model_data; d1$MPA <- factor(601)

p0 <- predict(m_all2, newdata = d0, type = "response")
p1 <- predict(m_all2, newdata = d1, type = "response") #, full = TRUE

# interpret MPA as the contrast between categories (inside vs outside).

mpa_dif<-mean(p1 - p0) * 100   # % point difference

# add this value to table
tab_with_marg$DeltaProb[tab_with_marg$Variable=="MPA (protected)"]<-mpa_dif
tab_with_marg


# --save to .csv so can import for probability info
write_csv(tab_with_marg,"./doc/model_avg_odds_ratios_full3.csv")


# --- Pretty Export with sjPlot ---
tab_df(tab_with_marg,
       title    = "Fixed Effects Odds Ratios (Averaged Model)",
       # subtitle = "Fixed effects averaged across ΔAIC ≤ 2 models",
       file     = "./doc/model_avg_odds_ratios_full3.doc")


# A +1 change in the scaled predictor corresponds to a +1 SD change in the raw data.

# The DeltaProb values are in percentage points.
# 
# They represent the average change in predicted probability of coral presence for a one–standard deviation (1-SD) increase in the predictor, holding everything else at observed values.
# 
# Example:
#   
#   Depth = 8.35 → a 1-SD increase in depth increased the probability of coral presence by ~8.4 percentage points on average.
# 
# Blast fishing = –5.87 → a 1-SD increase in blast fishing exposure decreased the probability of coral presence by ~5.9 percentage points.
# 
# Negative values = decreased probability; positive values = increased probability.
# 
# ⚠️ Note: For interaction terms (like Blast fishing × Population density risk), DeltaProb is left as NA in your table, because the effect depends on the level of the moderator. Those you’ve been handling separately with low/mean/high marginal effect plots.

# For probability scale (best for ecological interpretation):
# Compute marginal effects to say:
#   On average, a 1 m increase in depth increased the probability of live coral presence by ~X percentage points.


# notes about interactions etc: -------------

# You don’t have a column named I(sg^2), but you do have sg. So to estimate the effect: you perturb sg by +1 SD → then the model automatically updates the squared term. This tells you how probabilities change when seagrass distance increases, accounting for its quadratic effect.

# Interactions combine two variables. There’s no column called blast10:pr_inhab.To interpret them, you usually hold one variable constant (say, average pr_inhab), and perturb the other (blast10).Then predict probabilities and compute the difference. This tells you how the effect of blast10 depends on population density risk.











# or calc individualy (here, probabiliyt)
# ----------------------------------------------------------------------------------
# Average marginal effect for main effect (depth) ---------------
d_up <- d2
d_up$depth <- d_up$depth + sd(d2$depth)  # +1 SD

p0 <- predict(m_all2, newdata = d2, type = "response")
p1 <- predict(m_all2, newdata = d_up, type = "response")

depth_u<-mean(p1 - p0) # 0.08423834 - average % point change in coral probability for a 1 SD increase in depth
# This is the raw probability difference, averaged across rows of your dataset.
# It’s in proportion units (0.084 ≈ 8.4%).
# If you multiply by 100, it becomes 8.42%, which matches the table almost exactly.


# ----------------------------------------------------------------------------------
# Average marginal effect for main effect (blast10) ---------------
d_up <- d2
d_up$depth <- d_up$blast10 + sd(d2$blast10)  # +1 SD

p0 <- predict(m_all2, newdata = d2, type = "response")
p1 <- predict(m_all2, newdata = d_up, type = "response")

blast10_u<-mean(p1 - p0) #  0.08417629 - average % point change in coral probability for a 1 SD increase in blast fishing



# Odds ratio = exp(–0.512) ≈ 0.60 → odds of coral presence are 40% lower when blast fishing occurred (on average).

# On the probability scale, you computed the difference in mean probabilities when you flipped blast10. But because your model includes an interaction with pr_inhab (blast10:pr_inhab), the effect of blast10 depends on population density risk. At high pr_inhab, the positive interaction term can outweigh the negative main effect. If your dataset has many sites with high pr_inhab, the average effect of flipping blast10 in those cases can look positive, even though the “main effect” alone is negative.

# The coefficient is the main effect of blast10 when pr_inhab = 0 (i.e., centered). The marginal effect averages over all observed pr_inhab values, so the interaction dominates and flips the direction. “Blast fishing alone reduced odds of coral presence (OR = 0.60).” “However, the negative effect was offset or even reversed in areas with higher population density risk, due to a positive interaction.”




# for seagrass100 and sg100^2 ------------------------
d_up <- d2
d_up$sg100 <- d_up$sg100 + sd(d2$sg100)

p0 <- predict(m_all2, newdata = d2, type = "response")
p1 <- predict(m_all2, newdata = d_up, type = "response")

sg100_u<-mean(p1 - p0) # 0,05878703 - probability change associated with increasing seagrass distance (per 100 m), including its nonlinear effect


# Interpreting the OR If OR = 1.0 → no effect (odds unchanged).If OR > 1.0 → odds are higher (increased).If OR < 1.0 → odds are lower (decreased).

# Here, OR = 0.60.  That means the odds of coral presence under blast fishing are 0.60 × the odds without blast fishing.
# Percent change in odds=(OR−1)×100%      (0.60−1)×100%=−40% **** 40% decrease in odds ≠ a 40% decrease in probability.



# for fishing30:pop_density_risk -----------------------------------
# - hold pr_inhab at its mean and change blast10 from 0 → 1
# tells you the effect of blast fishing at the average population density
d0 <- d2
d0$blast10 <- 0

d1 <- d2
d1$blast10 <- 1

p0 <- predict(m_all2, newdata = d0, type = "response")
p1 <- predict(m_all2, newdata = d1, type = "response")

mean(p1 - p0) # -0.05804497

