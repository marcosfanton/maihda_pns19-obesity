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
    age = relevel(factor(age), ref = "young"))


# Estimate Logistic model (M1) -- NULL MODEL #### 
# Fit 
mA <- lme4::glmer(obesity ~ (1 |stratum), 
                  data = pns19, 
                  family = binomial)

# Summary
summary(mA)
tab_model(mA, show.se = TRUE)

# Predict (log-odds and intercept)
pns19 <- pns19 |> 
  mutate(mAxbu = predict(mA, type = "response"),
         mAxb = predict(mA, type = "response", re.form = NA))

# Estimate Logistic model (M2) -- MAIN MODEL #### 
# Fit 
mB <- glmer(obesity ~ gender + race + age + income + education + (1 | stratum),
data = pns19,
family = binomial)

# Summary
summary(mB)
tab_model(mB, show.se = TRUE)

# Predict
pns19 <- pns19 |> 
  mutate(mBmF = predict(mB, type = "response"),
         mBxb = predict(mB, type = "response", re.form = NA))

mB_predict <- merTools::predictInterval(mB,  level = 0.95, include.resid.var = FALSE) |> # log scale
  dplyr::rename_with(~ paste0("mBp_", .x)) |>  
  dplyr::bind_cols(
       merTools::predictInterval(mB, level = 0.95, include.resid.var = FALSE) |> # fixed effects only
  dplyr::rename_with(~ paste0("mBprob_", .x)) |> 
  dplyr::mutate(id = row_number()) 
  )

m2u <- REsim(m2)

# New dataset with predictions#### 
pns2 <- pns19  |> 
  left_join(mB_predict, by = "id")  

# Collapse stratas with predictions
 strat_level <- pns2 |> 
  dplyr::group_by (gender,
            race,
            age,
            income,
            stratum,
            strata_n,
            mBp_fit,
            mBp_upr,
            mBp_lwr, 
            mBprob_fit,
            mBprob_upr,
            mBprob_lwr,
            mBmF
  ) |> 
  dplyr::summarize(obesity_mean = mean(obesity)*100, .groups = "drop") 

# Calculate the Proportional Change in Variance (PCV)
# Variance matrices
pcv <- list(mA = mA, mB = mB) |> 
  map(~ pluck(as_tibble(VarCorr(.)), 4)) |> 
    (\(x) ((x$mA - x$mB) / x$mA) * 100)()

# Calculate area under the receiver operating characteristic (ROC) curve 
auc <- c("mAxbu", "mAxb", "mBp_fit", "mBxb") |> 
  set_names() |> 
  map(~ auc(pns2$obesity, pns2[[.]]))
