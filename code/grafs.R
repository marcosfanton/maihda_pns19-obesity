# Load packages #### 
library(tidyverse) 
library(here) 
library(scales)

# Load models and datasets ####
strata_level <- read.csv("data/pns19_stratum-obesity.csv", stringsAsFactors = TRUE) 


strata_level %>%
  mutate(rank2 = rank(mBp_fit)) %>%
  ggplot(aes(y = mBp_fit, x = rank2)) +
  geom_point() +
  geom_pointrange(aes(ymin = mBp_lwr, ymax = mBp_upr)) +
  ylab("Predicted Percent Diabetic, Model 2B") +
  xlab("Stratum Rank") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_bw()

# Rank the predicted stratum probabilities
stratum_level <- stratum_level %>%
  mutate(rank2=rank(m2Bmfit))

# convert probabilities to percentages
stratum_level$m2Bmfit <- stratum_level$m2Bmfit * 100
stratum_level$m2Bmupr <- stratum_level$m2Bmupr * 100
stratum_level$m2Bmlwr <- stratum_level$m2Bmlwr * 100

# Plot the caterpillar plot of the predicted stratum means
ggplot(stratum_level, aes(y=m2Bmfit, x=rank2)) +
  geom_point() +
  geom_pointrange(aes(ymin=m2Bmlwr, ymax=m2Bmupr)) +
  ylab("Predicted Percent Diabetic, Model 2B") +
  xlab("Stratum Rank") + 
  theme_bw()


# Generate list of 6 highest and 6 lowest predicted stratum means (for Table 4)
stratum_level <- stratum_level[order(stratum_level$rank2),]
head(stratum_level)
tail(stratum_level)

# Panel C

# create figure but on the log-odds scale
m2Bu <- REsim(model2B)
plotREsim(m2Bu)

# This is more difficult to do on the probability scale

# For this plot we follow a simulation-based approach to 
# calculating the limits of the approximate 95% confidence intervals of our 
# predictions. This involves simulating 1000 values for each predicted value.

# Approximate as the model assumes no sampling covariability between the 
# regression coefficients and the stratum random effect

# duplicate stratum data *1000 (creating a new dataframe, stratumsim)
stratumsim <- rbind(stratum_level, 
                    stratum_level[rep(1:nrow(stratum_level),999),])

# Generate the approximate standard error for the linear prediction on the
# logit scale. We do this based on the difference between one estimated
# confidence interval and the estimated fit, on the logit scale
stratumsim$m2Bmse <- (stratumsim$m2BmfitL - stratumsim$m2BmlwrL)/1.96

# specify initial value of the random-number seed (for replication purposes)
set.seed(354612)

# Generate the predicted stratum percentages based on the regression 
# coefficients and the predicted stratum random effect and factoring in 
# prediction uncertainty
stratumsim$m2Bpsim <- 100*invlogit(stratumsim$m2BmfitL + 
                                  rnorm(384000, mean=0, sd=stratumsim$m2Bmse))

# Generate the predicted stratum percentages ignoring the predicted stratum 
# effect
stratumsim$m2BpAsim <- 100*invlogit(stratumsim$m2BmF)

# Generate the difference in the predicted stratum percentages due to the 
# predicted stratum effect
stratumsim$m2BpBsim <- stratumsim$m2Bpsim - stratumsim$m2BpAsim

# sort the data by strata
stratumsim <- stratumsim[order(stratumsim$stratum),]

# collapse the data down to stratum level, generate mean and SE variables,
# then use these to generate rank and lower and upper limits of the approximate
# 95% confidence intervals of the difference in predicted stratum percentates 
# due to interaction variables.
stratumsim2 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(mean=mean(m2BpBsim), std=sd(m2BpBsim)) %>%
  mutate(rank=rank(mean)) %>%
  mutate(hi=(mean + 1.96*std)) %>%
  mutate(lo=(mean - 1.96*std))

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsim2, aes(x=rank, y=mean)) +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  xlab("Stratum Rank") +
  ylab("Difference in predicted percent diabetic due to interactions") +
  theme_bw()
