# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
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


# load data ------------------
d2<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>%
  mutate(MPA=as.factor(mpa), 
         # Depth=Depth*-1,
         Ecological_zone=as.factor(ecological_zone)
  )%>%
  glimpse()

# load final model  ---------------
m1_avg <- readRDS("./results_train/model_full_avg.rds")
m3_avg <- readRDS("./results_train/model_no_landscape_avg.rds")

# load final model (m_all2) ---------------
load("./results_train/model_full.R")
load("./results_train/model_no_landscape.R")

# image("./results_train/mixedEf_final_all1_no_landscape.RData")


# -------------------------------
# -- FULL MODEL --
# -------------------------------
tab_model(m_all2) # need for R2 etc

# ---  variable type lookup ---
var_types <- c(
  "psi2"            = "Biophysical",
  "MPA601"          = "Socioeconomic",
  "sg"              = "Biophysical",
  "depth"           = "Biophysical",
  "I(sg^2)"         = "Biophysical",
  "blast10:pr_inhab"= "Socioeconomic",
  "fishing_30lag:pr_inhab"   = "Socioeconomic",
  "blast10"         = "Socioeconomic",
  "fishing_30lag"   = "Socioeconomic",
  "pr_inhab"        = "Socioeconomic",
  "river"           = "Biophysical"
)

# -- pretty names lookup  ---
pretty_names <- c(
  "psi2"            = "Patch Compactness",
  "MPA601"          = "MPA (protected)",
  "sg"              = "Distance to Seagrass",
  "depth"           = "Depth",
  "I(sg^2)"         = "Distance to Seagrass²",
  "blast10:pr_inhab"= "Blast fishing, 2000-2010 × Pop. density risk",
  "blast10"         = "Blast fishing, 2000-2010",
  "fishing_30lag"   = "Fishing legacy, 1980-2000",
  "fishing_30lag:pr_inhab"   = "Fishing legacy, 1980-2000 × Pop. density risk",
  "pr_inhab"        = "Population density risk",
  "river"           = "Distance to Rivers")


# -------------------------------
# summarize averaged model values
# -------------------------------

# --- Extract model averages ---
coef_table <- summary(m1_avg, full = TRUE)$coefmat.full
ci         <- confint(m1_avg, full = TRUE)

# --- Build base table on OR scale ---
tab_or <- data.frame(
  Term      = rownames(coef_table),
  OddsRatio = exp(coef_table[, "Estimate"]),
  CI_low    = exp(ci[, 1]),
  CI_high   = exp(ci[, 2]),
  p_value   = coef_table[, "Pr(>|z|)"],
  row.names = NULL,
  check.names = FALSE
)

# --- Add importance (SW) + Nmodels ---
# Sum of weights
sw_df <- sw(m1_avg) %>%
  enframe(name = "Term", value = "WeightSum")

# Nmodels (count of non-NA estimates per term)
inclusion_counts <- apply(m1_avg$coefArray[, "Estimate", ], 2, \(x) sum(!is.na(x)))
n_df <- enframe(inclusion_counts, name = "Term", value = "Nmodels")

# --- Lookup tables (pretty names + variable type) ---
pretty_lu <- enframe(pretty_names, name = "Term", value = "Parameters")
type_lu   <- enframe(var_types,    name = "Term", value = "VarType")

# --- Join everything together in one chain ---
tab_export <- tab_or %>%
  left_join(sw_df, by = "Term") %>%
  left_join(n_df,  by = "Term") %>%
  left_join(pretty_lu, by = "Term") %>%
  left_join(type_lu,   by = "Term") %>%
  mutate(
    Parameters = if_else(is.na(Parameters), Term, Parameters),
    OddsRatio  = round(OddsRatio, 2),
    CI_low     = round(CI_low, 2),
    CI_high    = round(CI_high, 2),
    WeightSum  = round(WeightSum, 2),
    CI         = paste0(CI_low, " - ", CI_high),
    p_value    = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE            ~ sprintf("%.3f", p_value)
    ),
    # sanitize labels for export
    Parameters = gsub("×", "x", Parameters),
    Parameters = gsub("²", "^2", Parameters)
  ) %>%
  select(Parameters, Type = VarType, OddsRatio, CI, p_value, WeightSum, Nmodels)%>%
  arrange(desc(OddsRatio))

# --- Export with sjPlot ---
tab_df(tab_export,
       title    = "Fixed Effects Odds Ratios (Averaged Model)",
       subtitle = "Fixed effects averaged across ΔAIC ≤ 2 models",
       file     = "./doc/model_avg_odds_ratios_full.doc")



# -------------------------------
# -- NO LANDSCAPE MODEL --
# -------------------------------


# ---  variable type lookup ---
var_types3 <- c(
  "MPA601"          = "Socioeconomic",
  "depth"           = "Biophysical",
  "blast10:pr_inhab"= "Socioeconomic",
  "fishing_30lag:pr_inhab"   = "Socioeconomic",
  "blast10"         = "Socioeconomic",
  "fishing_30lag"   = "Socioeconomic",
  "pr_inhab"        = "Socioeconomic"
)

# -- pretty names lookup  ---
pretty_names3 <- c(
  "MPA601"          = "MPA (protected)",
  "depth"           = "Depth",
  "blast10:pr_inhab"= "Blast fishing, 2000-2010 × Pop. density risk",
  "blast10"         = "Blast fishing, 2000-2010",
  "fishing_30lag"   = "Fishing legacy, 1980-2000",
  "fishing_30lag:pr_inhab"   = "Fishing legacy, 1980-2000 × Pop. density risk",
  "pr_inhab"        = "Population density risk")


# -------------------------------
# summarize averaged model values
# -------------------------------
tab_model(m_all3) # need for R2 etc

# --- Extract model averages ---
coef_table3 <- summary(m3_avg, full = TRUE)$coefmat.full
ci3         <- confint(m3_avg, full = TRUE)

# --- Build base table on OR scale ---
tab_or3 <- data.frame(
  Term      = rownames(coef_table3),
  OddsRatio = exp(coef_table3[, "Estimate"]),
  CI_low    = exp(ci3[, 1]),
  CI_high   = exp(ci3[, 2]),
  p_value   = coef_table3[, "Pr(>|z|)"],
  row.names = NULL,
  check.names = FALSE
)

# --- Add importance (SW) + Nmodels ---
# Sum of weights
sw_df3 <- sw(m3_avg) %>%
  enframe(name = "Term", value = "WeightSum")

# Nmodels (count of non-NA estimates per term)
inclusion_counts3 <- apply(m3_avg$coefArray[, "Estimate", ], 2, \(x) sum(!is.na(x)))
n_df3 <- enframe(inclusion_counts3, name = "Term", value = "Nmodels")

# --- Lookup tables (pretty names + variable type) ---
pretty_lu3 <- enframe(pretty_names3, name = "Term", value = "Parameters")
type_lu3   <- enframe(var_types3,    name = "Term", value = "VarType")

# --- Join everything together in one chain ---
tab_export3 <- tab_or3 %>%
  left_join(sw_df3, by = "Term") %>%
  left_join(n_df3,  by = "Term") %>%
  left_join(pretty_lu3, by = "Term") %>%
  left_join(type_lu3,   by = "Term") %>%
  mutate(
    Parameters = if_else(is.na(Parameters), Term, Parameters),
    OddsRatio  = round(OddsRatio, 2),
    CI_low     = round(CI_low, 2),
    CI_high    = round(CI_high, 2),
    WeightSum  = round(WeightSum, 2),
    CI         = paste0(CI_low, " - ", CI_high),
    p_value    = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE            ~ sprintf("%.3f", p_value)
    ),
    # sanitize labels for export
    Parameters = gsub("×", "x", Parameters),
    Parameters = gsub("²", "^2", Parameters)
  ) %>%
  select(Parameters, Type = VarType, OddsRatio, CI, p_value, WeightSum, Nmodels)%>%
  arrange(desc(OddsRatio))

# --- Export with sjPlot ---
tab_df(tab_export3,
       title    = "Fixed Effects Odds Ratios (Averaged Model - No Landscape Variables)",
       subtitle = "Fixed effects averaged across ΔAIC ≤ 2 models",
       file     = "./doc/model_avg_odds_ratios_no_landscape.doc")







# ------------------------
# -- other tables ---------
# -----------------------

# table showing both
tab_or_prob_all <- data.frame(
  Term        = rownames(coef_table),
  OddsRatio   = exp(coef_table[, "Estimate"]),
  CI_low_OR   = exp(ci[, 1]),
  CI_high_OR  = exp(ci[, 2]),
  Estimate    = coef_table[, "Estimate"],
  Probability = plogis(coef_table[, "Estimate"]), # not conditional, just logistic transform
  p_value     = coef_table[, "Pr(>|z|)"],
  row.names = NULL
)

tab_df(tab_or_prob_all,
       title = "Full Model Average (MuMIn, Odds Ratios & Probabilities)",
       digits = 3,
       file = "./doc/model_avg_details_full.doc")

# table showing both
tab_or_prob_all3 <- data.frame(
  Term        = rownames(coef_table3),
  OddsRatio   = exp(coef_table3[, "Estimate"]),
  CI_low_OR   = exp(ci3[, 1]),
  CI_high_OR  = exp(ci3[, 2]),
  Estimate    = coef_table3[, "Estimate"],
  Probability = plogis(coef_table3[, "Estimate"]), # not conditional, just logistic transform
  p_value     = coef_table3[, "Pr(>|z|)"],
  row.names = NULL
)

tab_df(tab_or_prob_all3,
       title = "Full Model Average (MuMIn, Odds Ratios & Probabilities)",
       digits = 3,
       file = "./doc/model_avg_details_no_landscape.doc")