# Load packages #### 
library(tidyverse) 
library(here) 
library(lme4)
library(ggeffects)
library(merTools)
library(sjPlot)
library(plotly)

# Load dataset #### 
pns19 <- read.csv("data/pns19.csv", stringsAsFactors = TRUE) |> 
  mutate(
    race = relevel(factor(race), ref = "white"),
    age = relevel(factor(age), ref = "young"),
    income = factor(income, levels = c("Ilow", "Imid", "Ihigh")),
    education = factor(education, levels = c("Elow", "Emid", "Ehigh"))
  )


# Estimate Logistic model (M1) -- NULL MODEL #### 
# Fit 
mA <- lme4::glmer(obesity ~ (1 |stratum), 
                  data = pns19, 
                  family = binomial)

# Save and Summary

summary(mA)
tab_model(mA, show.se = TRUE)

saveRDS(mA, "data/mA.rds")


# Predict (log-odds and intercept)
pns19 <- pns19 |> 
  dplyr::mutate(mAxbu = predict(mA, type = "response"),
         mAxb = predict(mA, type = "response", re.form = NA))

# Estimate Logistic model (M2) -- MAIN MODEL #### 
# Fit 
mB <- lme4::glmer(obesity ~ gender + race + age + income + education + (1 | stratum),
data = pns19,
family = binomial)

# Save and Summary
summary(mB)
tab_model(mB, show.se = TRUE)

saveRDS(mB, "data/mB.rds")

# Predict
pns19 <- pns19 |> 
  dplyr::mutate(mBmF = predict(mB, type = "response"),
         mBxb = predict(mB, type = "response", re.form = NA))

mB_predict <- merTools::predictInterval(mB,  level = 0.95, include.resid.var = FALSE) |> # log scale
  dplyr::rename_with(~ paste0("mB_", .x)) |>  
  dplyr::bind_cols(
       merTools::predictInterval(mB, level = 0.95, include.resid.var = FALSE, type = "probability") |> # fixed effects only
  dplyr::rename_with(~ paste0("mBprob_", .x)) |> 
  dplyr::mutate(id = row_number()) 
  )

mBu <- REsim(mB)

# Save new dataset with predictions
write.csv(pns19, 
  "data/pns19_maihda.csv", 
  row.names = FALSE)

# New dataset with predictions#### 
pns2 <- pns19  |> 
  dplyr::left_join(mB_predict, by = "id")  

# Save dataset with obesity means
write.csv(pns2, 
  "data/pns19_obesity-mean.csv", 
  row.names = FALSE)

# Collapse stratas with predictions
 strat_level <- pns2 |> 
  dplyr::group_by (gender,
            race,
            age,
            income,
            education,
            stratum,
            strata_n,
            mB_fit,
            mB_upr,
            mB_lwr, 
            mBprob_fit,
            mBprob_upr,
            mBprob_lwr,
            mBmF
  ) |> 
  dplyr::summarize(obesity_mean = mean(obesity)*100, .groups = "drop") |> 
  dplyr::mutate(rankprob = rank(mBprob_fit))

# Save dataset with obesity-strata level
write.csv(strat_level, 
  "data/pns19_stratum-obesity.csv", 
  row.names = FALSE)

# Calculate the Proportional Change in Variance (PCV)
# Variance matrices
pcv <- list(mA = mA, mB = mB) |> 
  purrr::map(~ purrr::pluck(dplyr::as_tibble(lme4::VarCorr(.)), 4)) |> 
    (\(x) ((x$mA - x$mB) / x$mA) * 100)()


# Calculate area under the receiver operating characteristic (ROC) curve 
auc <- c("mAxbu", "mAxb", "mBp_fit", "mBxb") |> 
  set_names() |> 
  map(~ Metrics::auc(pns2$obesity, pns2[[.]]))
