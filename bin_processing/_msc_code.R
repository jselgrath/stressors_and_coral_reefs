# Misc code bits from analysis 




# -- probabilities - these depend on predictor values -----------------------------

# baseline probability (intercept only
# Baseline probability (when all predictors = 0)
intercept <- coef_table["(Intercept)", "Estimate"]
baseline_prob <- plogis(intercept)  # logistic transform
baseline_prob

# probabilities for a +1 change in a predictor x1:depend on scaling predictors
# depth ----
coef_x1 <- coef_table["depth", "Estimate"]
prob_x1 <- plogis(intercept + coef_x1)  # probability with x1 increased by 1
prob_x1 - baseline_prob                 # change in probability

# table showing both
tab_or_prob <- data.frame(
  Term        = rownames(coef_table),
  OddsRatio   = exp(coef_table[, "Estimate"]),
  CI_low_OR   = exp(ci[, 1]),
  CI_high_OR  = exp(ci[, 2]),
  Estimate    = coef_table[, "Estimate"],
  Probability = plogis(coef_table[, "Estimate"]), # not conditional, just logistic transform
  p_value     = coef_table[, "Pr(>|z|)"],
  row.names = NULL
)

tab_df(tab_or_prob,
       title = "Full Model Average (MuMIn, Odds Ratios & Probabilities)",
       digits = 3,
       file = "./doc/model_average_full_odds.doc")

# To get true predicted probabilities, you need to use predict() with new data.
# Because your ratio-scale predictors are centered and scaled (typically mean = 0, SD = 1):
# Each coefficient (estimate) from your logistic model is the change in log-odds of the outcome for a 1-SD increase in the predictor.
# Exponentiating the coefficient (exp(β)) gives the odds ratio for a 1-SD increase.
# This standardization makes the magnitudes of coefficients more directly comparable across predictors, since they’re all on the same scale.
# The sign of β tells you the direction:
# Positive → higher predictor values increase odds of the outcome.
# Negative → higher predictor values decrease odds.




# Ranked Effects from Full-Model Averaged Logistic Model -------------------

# # --- Extract results from  averaged model ---
# coef_table <- summary(m1_avg, full = TRUE)$coefmat.full
# ci <- confint(m1_avg, full = TRUE)
# 
# # Build a tidy data.frame with log-odds, odds ratios, and relative strength
# tab_ranked <- data.frame(
#   Term        = rownames(coef_table),
#   Estimate    = coef_table[, "Estimate"],            # log-odds
#   OddsRatio   = exp(coef_table[, "Estimate"]),       # odds ratio
#   SE          = coef_table[, "Std. Error"],
#   CI_low_OR   = exp(ci[, 1]),
#   CI_high_OR  = exp(ci[, 2]),
#   p_value     = coef_table[, "Pr(>|z|)"],
#   AbsStrength = abs(coef_table[, "Estimate"])        # effect size magnitude
# )
# 
# # Rank by absolute effect size (largest = strongest effect)
# tab_ranked <- tab_ranked %>%
#   arrange(desc(AbsStrength))
# 
# tab_ranked <- tab_ranked %>%
#   arrange(desc(log(OddsRatio)))
# 
# # Show as a clean sjPlot table
# tab_df(tab_ranked,
#        title = "Predictors Ranked by Relative Effect Strength (Full Model Average)",
#        digits = 3)


# Estimate = standardized log-odds (for 1 SD increase in predictor)
# OddsRatio = exponentiated effect (odds ratio)
# CI_low_OR / CI_high_OR = odds ratio confidence interval
# AbsStrength = absolute standardized coefficient, used for ranking
# p_value = model-averaged significance

# You can switch between “relative strength” metrics:
#   Abs(Estimate) → directly comparable since predictors are scaled
# |log(OddsRatio)| → essentially the same, but on the OR scale