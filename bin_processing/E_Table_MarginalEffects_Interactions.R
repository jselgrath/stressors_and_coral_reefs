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
# load("./results_train/model_n.R")


# load data - fit to model (centered & scaled) ---------------------
d2<-read_csv("./results_train/17_IndpVar_Pts_train_for_models_all.csv")%>% # all
  mutate(MPA=as.factor(mpa_id),     # make factors - depth already positive
         ecological_zone=as.factor(ecological_zone),
         Reef_state=resilience_id )%>%
  glimpse()



# interaction fxn -------------------------------------------

marginal_interaction <- function(interaction, model, data, probs = c(0.25, 0.5, 0.75)) {
  # split into component variables
  parts <- strsplit(interaction, ":")[[1]]
  var1 <- parts[1]   # the one we "bump"
  var2 <- parts[2]   # the moderator we vary
  
  if (!all(c(var1, var2) %in% names(data))) {
    stop("Variables not found in dataset")
  }
  
  # levels of moderator (low, mean, high by default)
  vals <- c(
    quantile(data[[var2]], probs[1], na.rm = TRUE),
    mean(data[[var2]], na.rm = TRUE),
    quantile(data[[var2]], probs[3], na.rm = TRUE)
  )
  names(vals) <- c("Low", "Mean", "High")
  
  # compute marginal effect of var1 at each value of var2
  effects <- sapply(vals, function(v) {
    d0 <- data; d0[[var1]] <- 0; d0[[var2]] <- v
    d1 <- data; d1[[var1]] <- 1; d1[[var2]] <- v
    
    p0 <- predict(model, newdata = d0, type = "response", full = TRUE)
    p1 <- predict(model, newdata = d1, type = "response", full = TRUE)
    
    mean(p1 - p0, na.rm = TRUE)
  })
  
  return(data.frame(
    Interaction = interaction,
    Moderator   = var2,
    Level       = names(effects),
    MarginalEff = round(effects * 100, 2)  # % change in probability
  ))
}

# -----------------------------------------------------------------


# marginal effects -- interaction terms  ----------------------------------
# get the dataset used to fit the global model
model_data <- getData(m_all2)

# compute marginal effects for blast10:pr_inhab
mi_bl_pr<-marginal_interaction("blast10:pr_inhab", m1_avg, model_data)


# for lag fishing
mi_fi_pr<-marginal_interaction("fishing_30lag:pr_inhab", m1_avg, model_data)

# save table of marginal effects for interactions ---------------
interactions<-tibble(rbind(mi_bl_pr,mi_fi_pr))%>%
  glimpse()
write_csv(interactions,"./doc/table_marginal_effects_interactions.csv")




# -- could extend to all interactions ------------------
coef_terms <- setdiff(names(fixef(m_all2)), "(Intercept)")
interaction_terms <- grep(":", coef_terms, value = TRUE)

interaction_results <- do.call(rbind, lapply(interaction_terms, marginal_interaction,
                                             model = m1_avg, data = model_data))

interaction_results
