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
m1 <- lme4::glmer(obesity ~ (1 |stratum), 
                  data = pns19, 
                  family = binomial)

# Summary
summary(m1)
tab_model(m1, show.se = TRUE)

# Predict (log-odds)
pns19$m1xbu <- stats::predict(m1, type = "response")

# Predict (only intercept)
pns19$m1xb <- predict(m1, type = "response", re.form = NA)

# Estimate Logistic model (M2) -- MAIN MODEL #### 
# Fit 
m2 <- glmer(obesity ~ gender + race + age + income + education + (1 | stratum),
data = pns19,
family = binomial)

# Summary
summary(m2)
tab_model(m2, show.se = TRUE)

# Predict
m2m <- merTools::predictInterval(m2,  level = 0.95, include.resid.var = FALSE)

m2m <- dplyr::mutate(m2m, id = row_number())

pns19$m2mF <- merTools::predict(m2, re.form = NA)

m2m_prob <- predictInterval(m2, level = 0.95, include.resid.var = FALSE,
type = "probability")

m2m_prob <- dplyr::mutate(m2m_prob, id = row_number())

pns19$m2xb <- predict(m2, re.form = NA, type = "response")

m2u <- REsim(m2)

# New dataset with predictions#### 
pns2 <- pns19  |> 
  left_join(m2m, by = "id") |> 
  rename(
    m1Bmfit = fit,
    m1Bmupr = upr,
    m1Bmlwr = lwr
  )  |> 
  left_join(m2Bm_prob, by = "id") |> 
  rename(
    m2Bmfit = fit,
    m2Bmupr = upr,
    m2Bmlwr = lwr
  ) %>%
  left_join(m2Bm, by = "id") |> 
  rename(
    m2BmfitL = fit,
    m2BmuprL = upr,
    m2BmlwrL = lwr
  )

# Collapse
stratum_level <- pns2 |> 
  dplyr::group_by (age,
            gender,
            race,
            income,
            stratum,
            strata_n,
            m1Am,
            m1Bmfit,
            m1Bmupr,
            m1Bmlwr,
            m2Bmfit,
            m2Bmupr,
            m2Bmlwr, 
            m2BmfitL,
            m2BmuprL,
            m2BmlwrL,
            m2BmF
  ) |> 
  dplyr::summarize(obesity, 
                   mean, 
                   .names = "mean_{col}",
            .groups = "drop")

stratum_level <- stratum_level |> 
  dplyr::mutate(mean_obesity = mean_obesity*100)  

# Ranking 
stratum_level <- stratum_level  |> 
  dplyr::mutate(rank = rank(m2mfit))