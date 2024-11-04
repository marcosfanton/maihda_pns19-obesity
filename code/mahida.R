# Load packages #### 
library(tidyverse) 
library(here) 
library(lme4)
library(ggeffects)
library(merTools)
library(sjPlot)
library(plotly)

# Load dataset #### 
pns19 <- read.csv("data/pns19.csv", stringsAsFactors = TRUE)

# Estimate Logistic model (M1) -- NULL MODEL #### 
# Fit 
m1 <- glmer(obesity ~ (1 |stratum), 
                  data=pns19, 
                  family=binomial)

# Summary
summary(m1)
tab_model(m1, show.se = TRUE)


