# Load packages #### 
library(tidyverse) 
library(here) 
library(scales)
library(MetBrewer)
library(showtext)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(cowplot)


# Load models and datasets ####
strat_level <- read.csv("data/pns19_stratum-obesity.csv", stringsAsFactors = TRUE) |> 
  dplyr::mutate(id = row_number())
  
# Fig.1 Scatterplot (sorted by obesity) ####  
font_add_google("Raleway", "raleway")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

fig1 <- strat_level |> 
  ggplot(aes(y = mBprob_fit, x = rankprob, color = stratum)) +
  geom_hline(yintercept = 0.2159, color = "black", 
             linetype = "dashed", alpha = 0.7,
            size = 0.5) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  ylab("Obesidade (%)") +
  xlab("Ranking de Estratos Sociais") +
  scale_y_continuous(labels = scales::label_percent(scale = 100, suffix = "")) +
    scale_color_manual(values = met.brewer("Peru1", 159)) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "raleway"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )

# Save fig1
ggsave(
  filename = "fig/fig1_epibr.png",
  bg = "white",
  width = 18,
  height = 7,
  dpi = 300,
  plot = fig1)


# Fig. 2 Scatterplot (sorted by social strata) ####  
strat_level |> 
    dplyr::arrange(gender, race, age, income, education) |> 
      dplyr::mutate(stratum = factor(stratum, levels = unique(stratum))) |> 
        ggplot(aes(x = stratum, y = mBprob_fit, color = stratum)) +
          geom_point() +
          geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
            scale_color_manual(values = met.brewer("Peru1", 159)) +
    scale_y_continuous(labels = percent_format(scale = 100)) +
          theme_bw() +
    labs(y = "Obesidade Predita por Estrato Social", x = "") +
    theme(
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank(),
            legend.position = "none",
            text = element_text(family = "raleway"))


# Fig. 3 Scatterplot (top-bottom 10)
# List fo 10 highest and lowest predicted stratum means
top10 <- bind_rows(
    strat_level |> 
      slice_max(rankprob, n = 10) |> 
      mutate(type = "top"),  # Top 10
    strat_level |> 
      slice_min(rankprob, n = 10) |> 
      mutate(type = "bottom") # Bottom 10
  )
# Plot
top10 |> 
  ggplot(aes(y = mBprob_fit, x = rankprob, color = stratum)) +
  geom_point(size = 1) +
    geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  ylab("Obesidade Predita (%)") +
  xlab("") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
    scale_color_manual(values = met.brewer("Peru1", 20)) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "raleway")) 




# Tables ####
# Table with models 
tab_model(mA, mB, 
  p.style = "stars",
  string.ci = "IC(95%)",
pred.labels = c("Intercepto",
"Gênero (Mulher)",
"Raça (Preta)",
"Raça (Pardo)",
"Idade (Tercil2)",
"Idade (Tercil3)",
"Renda (Tercil2)",
"Renda (Tercil3)",
"Educação (Tercil2)",
"Educação (Tercil3)"),
dv.labels = c("Modelo Vazio", "Modelo Ajustado"),
string.pred = "Coeficiente",
CSS = list(css.table = '+font-family: Raleway;'))





