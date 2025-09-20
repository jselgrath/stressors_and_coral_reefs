# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs 
# --------------------------------------------

# goal: create Figure 3 a & b from model outputs
# Effects are standardized
# -------------------------------------
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(ggtext)
library(dplyr)

# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------

# load averaged model  ---------------
m1_avg <- readRDS("./results_train/model_full_avg.rds")
m3_avg <- readRDS("./results_train/model_no_landscape_avg.rds")


# map colors - from berlin pallate in colorspace
color_map <- c(
  "Biophysical"    = "#688AFC",  # blue #1f78b4
  "Socioeconomic"  = "#3B4CC0",  # 
  "Not significant" = "grey70"   # neutral grey
)

source("./bin_processing/deets_2025.R")

# ----------------------------------
# -- FULL MODEL
# ----------------------------------

# --- Define variable type lookup ---
var_types <- c(
  "psi2"            = "Biophysical",
  "MPA601"          = "Socioeconomic",
  "sg100"              = "Biophysical",
  "depth"           = "Biophysical",
  "I(sg100^2)"         = "Biophysical",
  "blast10:pr_inhab"= "Socioeconomic",
  "fishing_30lag:pr_inhab"   = "Socioeconomic",
  "blast10"         = "Socioeconomic",
  "fishing_30lag"   = "Socioeconomic",
  "pr_inhab"        = "Socioeconomic",
  "river100"           = "Biophysical"
)

# --- Create a named lookup vector for pretty names ---
pretty_names <- c(
  "psi2"            = "Patch Compactness",
  "MPA601"          = "MPA (protected)",
  "sg100"              = "Distance to Seagrass",
  "depth"           = "Depth",
  "I(sg100^2)"         = "Distance to Seagrass^2",  # safer ASCII for export
  "blast10:pr_inhab"= "Blast fishing, 2000-2010 x Pop. density risk",
  "blast10"         = "Blast fishing, 2000-2010",
  "fishing_30lag"   = "Fishing legacy, 1980-2000",
  "fishing_30lag:pr_inhab"   = "Fishing legacy, 1980-2000 x Pop. density risk",
  "pr_inhab"        = "Population density risk",
  "river100"           = "Distance to Rivers"
)



# --- Build clean table from averaged model ---
coef_table <- summary(m1_avg)$coefmat.full
ci <- confint(m1_avg)

tab_ranked <- data.frame(
  Term     = rownames(coef_table),
  Estimate = coef_table[, "Estimate"],   # logit-scale coefficient
  CI_low   = ci[, 1],
  CI_high  = ci[, 2],
  p_value  = coef_table[, "Pr(>|z|)"],
  row.names = NULL,
  check.names = FALSE
) %>%
  filter(Term != "(Intercept)")%>% # not relevant for this graph
  # # Significance stars
  mutate(
  #   Stars   = case_when(
  #     p_value < 0.001 ~ "***",
  #     p_value < 0.01  ~ "**",
  #     p_value < 0.05  ~ "*",
  #     p_value < 0.1   ~ ".",
  #     TRUE            ~ ""
  #   ),
    SigFlag = ifelse(p_value < 0.05, 1, 0)
  ) %>%
  # Add pretty labels and types
  mutate(
    VarType      = dplyr::recode(Term, !!!var_types, .default = "Other"),
    Term_pretty  = dplyr::recode(Term, !!!pretty_names, .default = Term),
    Term_label   = paste0(Term_pretty),#, " ", Stars),
    AbsStrength  = abs(Estimate),
    VarType_sig  = ifelse(SigFlag == 1, VarType, "Not significant")
  ) %>%
  arrange(desc(AbsStrength))%>%
  mutate(
    VarType_sig = factor(
      VarType_sig,
      levels = c("Biophysical", "Socioeconomic", "Not significant") #rank levels for graph
    )
  )



# ---------------------------------
ggplot(tab_ranked,
       aes(x = reorder(Term_label, Estimate),
           y = Estimate,
           ymin = CI_low,
           ymax = CI_high,
           color = VarType_sig)) +
  ylim(-3.5,3.5)+
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_flip() +
  scale_color_manual(values = color_map, name = "Data Type") +
  labs(
    title = "",
    x = "",
    y = "Standardized Effect Size (95% CI)"
  ) +
  theme_deets(base_size = 11, legend = "none")
  

ggsave("./doc/fig3a_model_full.png", width = 8, height = 4, dpi = 300)





# ----------------------------------
# -- FULL MODEL
# ----------------------------------

# --- Define variable type lookup ---
var_types3 <- c(
  "MPA601"          = "Socioeconomic",
  "depth"           = "Biophysical",
  "blast10:pr_inhab"= "Socioeconomic",
  "fishing_30lag:pr_inhab"   = "Socioeconomic",
  "blast10"         = "Socioeconomic",
  "fishing_30lag"   = "Socioeconomic",
  "pr_inhab"        = "Socioeconomic"
)

# --- Create a named lookup vector for pretty names ---
pretty_names3 <- c(
  "MPA601"          = "MPA (protected)",
  "depth"           = "Depth",
  "blast10:pr_inhab"= "Blast fishing, 2000-2010 x Pop. density risk",
  "blast10"         = "Blast fishing, 2000-2010",
  "fishing_30lag"   = "Fishing legacy, 1980-2000",
  "fishing_30lag:pr_inhab"   = "Fishing legacy, 1980-2000 x Pop. density risk",
  "pr_inhab"        = "Population density risk"
)



# --- Build clean table from averaged model ---
coef_table3 <- summary(m3_avg)$coefmat.full
ci3 <- confint(m3_avg)

tab_ranked3 <- data.frame(
  Term     = rownames(coef_table3),
  Estimate = coef_table3[, "Estimate"],   # logit-scale coefficient
  CI_low   = ci3[, 1],
  CI_high  = ci3[, 2],
  p_value  = coef_table3[, "Pr(>|z|)"],
  row.names = NULL,
  check.names = FALSE
) %>%
  filter(Term != "(Intercept)")%>% # not relevant for this graph
  # Significance stars
  mutate(
  #   Stars   = case_when(
  #     p_value < 0.001 ~ "***",
  #     p_value < 0.01  ~ "**",
  #     p_value < 0.05  ~ "*",
  #     p_value < 0.1   ~ ".",
  #     TRUE            ~ ""
  #   ),
    SigFlag = ifelse(p_value < 0.05, 1, 0)
  ) %>%
  # Add pretty labels and types
  mutate(
    VarType      = dplyr::recode(Term, !!!var_types3, .default = "Other"),
    Term_pretty  = dplyr::recode(Term, !!!pretty_names3, .default = Term),
    Term_label   = paste0(Term_pretty),#, " ", Stars),
    AbsStrength  = abs(Estimate),
    VarType_sig  = ifelse(SigFlag == 1, VarType, "Not significant")
  ) %>%
  arrange(desc(AbsStrength))%>%
  mutate(
    VarType_sig = factor(
      VarType_sig,
      levels = c("Biophysical", "Socioeconomic", "Not significant") #rank levels for graph
    )
  )



# ---------------------------------
ggplot(tab_ranked3,
       aes(x = reorder(Term_label, Estimate),
           y = Estimate,
           ymin = CI_low,
           ymax = CI_high,
           color = VarType_sig)) +
  ylim(-3.5,3.5)+
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_flip() +
  scale_color_manual(values = color_map, name = "Data Type") +
  labs(
    # title = "Model Average Estimates (No Landscape) \nper 1 SD change in predictor (logit scale)",
    x = "",
    y = "Standardized Effect Size (95% CI)"
  ) +
  theme_deets(base_size = 11, legend = "none")

ggsave("./doc/fig3a_model_no_landscape.png", width = 8, height = 4, dpi = 300)


