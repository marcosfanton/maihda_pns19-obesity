# Load packages #### 
library(tidyverse) 
library(here) 
library(scales)
library(MetBrewer)

# Load models and datasets ####
strat_level <- read.csv("data/pns19_stratum-obesity.csv", stringsAsFactors = TRUE) 
  
# Scatterplot (simple) ####  
strat_level |> 
  ggplot(aes(y = mBprob_fit, x = rankprob, color = stratum)) +
  geom_point() +
  geom_pointrange(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  ylab("Obesidade Predita (%)") +
  xlab("") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(~ gender + race + age, , scales = "free_x", space = "free_x") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 10) )

# List fo 10 highest and lowest predicted stratum means
top10 <- bind_rows(
    strat_level |> 
      slice_max(rankprob, n = 10) |> 
      mutate(type = "top"),  # Top 10
    strat_level |> 
      slice_min(rankprob, n = 10) |> 
      mutate(type = "bottom") # Bottom 10
  )

strat_level |> 
    dplyr::arrange(gender, race, age, income, education) |> 
      dplyr::mutate(stratum = factor(stratum, levels = unique(stratum))) |> 
        ggplot(aes(x = stratum, y = mBprob_fit, color = stratum)) +
          geom_point() +
          geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr), width = 0.1) +
            scale_color_manual(values = met.brewer("Peru1", 159)) +
    scale_y_continuous(labels = percent_format(scale = 100)) +
          theme_bw() +
    labs(y = "Obesidade Predita por Estrato Social", x = "") +
    theme(
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank(),
            legend.position = "none" )

fig1
