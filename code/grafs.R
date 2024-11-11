# Load packages #### 
library(tidyverse) 
library(here) 
library(scales)

# Load models and datasets ####
strat_level <- read.csv("data/pns19_stratum-obesity.csv", stringsAsFactors = TRUE) 

  
# Catterpilar plot (simple) ####  
strat_level |> 
  ggplot(aes(y = mBprob_fit, x = rankprob)) +
  geom_point() +
  geom_pointrange(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  ylab("Obesidade (%) Predita") +
  xlab("Estrato") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_bw()

# List fo 10 highest and lowest predicted stratum means
top10 <- bind_rows(
    strat_level |> 
      slice_max(rankprob, n = 10) |> 
      mutate(type = "top"),  # Top 10
    strat_level |> 
      slice_min(rankprob, n = 10) |> 
      mutate(type = "bottom") # Bottom 10
  )
