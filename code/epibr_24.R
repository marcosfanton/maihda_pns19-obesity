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
library(gt)


# Load models and datasets ####
strat_level <- read.csv("data/pns19_stratum-obesity.csv", stringsAsFactors = TRUE) |> 
  dplyr::mutate(id = row_number())

# Config ####
#Font
font_add_google("Raleway", "raleway")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# Colors
colors <- met.brewer("Peru1", 159)

# Colors - Strata
strat_colors <- tibble(
  stratum = strat_level$stratum,
  color = colors
)

# Dataframe w/ colors
strat_level <- strat_level |> 
  left_join(strat_colors, by = "stratum")

# Fig.1 Scatterplot (sorted by obesity) ####  
fig1 <- strat_level |> 
  ggplot(aes(y = mBprob_fit, x = rankprob, color = color)) +
  geom_hline(yintercept = 0.2159, color = "black", 
             linetype = "dashed", alpha = 0.7,
            size = 0.5) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  labs(x = "Ranking de Estratos Sociais",
        y = "Obesidade Predita (%)") +
  scale_y_continuous(labels = scales::label_percent(scale = 100, suffix = "")) +
  scale_color_identity() + 
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "raleway", size = 24),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )

# Save fig1
ggsave(
  filename = "fig/fig1_epibr.png",
  bg = "white",
  width = 18,
  height = 7,
  dpi = 300,
  plot = fig1)

# Fig. 2 Scatterplot (top-bottom 10)
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
fig2 <- top10 |> 
  ggplot(aes(y = mBprob_fit, x = rankprob, color = color)) +
  geom_hline(yintercept = 0.2159, color = "black", 
             linetype = "dashed", alpha = 0.7,
            size = 0.5) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  labs(x = "Ranking de Estratos Sociais",
        y = "Obesidade Predita (%)") +
  scale_y_continuous(labels = scales::label_percent(scale = 100, suffix = "")) +
  scale_color_identity() + 
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "raleway", size = 24),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )

# Save fig2
ggsave(
  filename = "fig/fig2_epibr.png",
  bg = "white",
  width = 18,
  height = 7,
  dpi = 300,
  plot = fig2)

# Table with Top-Bottom 10
# Recode
top10 <- top10 |> 
  dplyr::mutate(stratum = recode(stratum,
    "womanblackmidIlowElow" = "Mulher-Preta-Idade(t2)-Renda(t1)-Educação(t1)",
    "womanbrownmidIlowElow" = "Mulher-Parda-Idade(t2)-Renda(t1)-Educação(t1)",
    "womanblackoldIlowElow" = "Mulher-Preta-Idade(t3)-Renda(t1)-Educação(t1)",
    "womanbrownmidImidElow" = "Mulher-Parda-Idade(t2)-Renda(t2)-Educação(t1)",
    "womanblackoldIhighEmid" = "Mulher-Preta-Idade(t3)-Renda(t3)-Educação(t2)",
    "womanblackmidImidElow" = "Mulher-Preta-Idade(t2)-Renda(t2)-Educação(t1)",
    "womanblackmidImidEhigh" = "Mulher-Preta-Idade(t2)-Renda(t2)-Educação(t3)",
    "manblackmidImidEmid" = "Homem-Preto-Idade(t2)-Renda(t2)-Educação(t2)",
    "womanwhiteyoungIhighEhigh" = "Mulher-Branca-Idade(t1)-Renda(t3)-Educação(t3)",
    "manbrownyoungIlowEmid" = "Homem-Pardo-Idade(t1)-Renda(t1)-Educação(t2)",
    "manbrownyoungIlowElow" = "Homem-Pardo-Idade(t1)-Renda(t1)-Educação(t1)",
    "womanwhiteyoungImidEhigh" = "Mulher-Branca-Idade(t1)-Renda(t2)-Educação(t3)",
    "womanbrownyoungIhighEhigh" = "Mulher-Parda-Idade(t1)-Renda(t3)-Educação(t3)",
    "womanwhitemidIhighEhigh" = "Mulher-Branca-Idade(t2)-Renda(t3)-Educação(t3)",
    "manwhiteyoungIhighEhigh" = "Homem-Branco-Idade(t1)-Renda(t3)-Educação(t3)",
    "manwhiteyoungIlowElow" = "Homem-Branco-Idade(t1)-Renda(t1)-Educação(t1)",
    "womanbrownyoungIlowEhigh" = "Mulher-Parda-Idade(t1)-Renda(t1)-Educação(t3)",
    "manwhiteyoungIlowEhigh" = "Homem-Branco-Idade(t1)-Renda(t1)-Educação(t3)",
    "womanblackmidIlowEmid" = "Mulher-Preta-Idade(t2)-Renda(t1)-Educação(t2)",
    "womanblackoldIlowEmid" = "Mulher-Preta-Idade(t3)-Renda(t1)-Educação(t2)",

  ),
              mBprob_lwr = mBprob_lwr*100,
              mBprob_upr = mBprob_upr*100
            ) 
# Table Top 10
tab_top10 <- top10 |> 
  filter(type == "top") |> 
  select(stratum, obesity_mean, mBprob_lwr, mBprob_upr) |> 
  gt() |> 
  cols_label(
    stratum = "Estrato",
    obesity_mean = "(%)",
    mBprob_lwr = "IC(95%)",
    mBprob_upr = ""
  ) |> 
  cols_unhide(columns = mBprob_upr) |>
  cols_merge(
    columns = c(mBprob_lwr, mBprob_upr),
    pattern = "({1},<< {2})>>",
  ) |> 
  tab_header(
    title = "Obesidade Predita por Estratos Sociais",
    subtitle = md("Top 10 Estratos com <u>Maior</u> Prevalência")
  ) |>  
  tab_spanner(
    label = "Obesidade",
    columns = c(obesity_mean, mBprob_lwr, mBprob_upr)
  ) |> 
  fmt_number(
    columns = c(obesity_mean, mBprob_lwr, mBprob_upr),
    decimals = 2
  ) |> 
  opt_table_font(font = "raleway") |> 
  opt_stylize(style = 1, color = 'gray') |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()
)
# Save table
gtsave(tab_top10, "fig/tab_top10.png")

# Table Bottom 10
tab_bottom10 <- top10 |> 
  filter(type == "bottom") |> 
  select(stratum, obesity_mean, mBprob_lwr, mBprob_upr) |> 
  gt() |> 
  cols_label(
    stratum = "Estrato",
    obesity_mean = "(%)",
    mBprob_lwr = "IC(95%)",
    mBprob_upr = ""
  ) |> 
  cols_unhide(columns = mBprob_upr) |>
  cols_merge(
    columns = c(mBprob_lwr, mBprob_upr),
    pattern = "({1},<< {2})>>",
  ) |> 
  tab_header(
    title = "Obesidade Predita por Estratos Sociais",
    subtitle = md("Top 10 Estratos com <u>Menor</u> Prevalência")
  ) |>  
  tab_spanner(
    label = "Obesidade",
    columns = c(obesity_mean, mBprob_lwr, mBprob_upr)
  ) |> 
  fmt_number(
    columns = c(obesity_mean, mBprob_lwr, mBprob_upr),
    decimals = 2
  ) |> 
  opt_table_font(font = "raleway") |> 
  opt_stylize(style = 1, color = 'gray') |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()
)
# Save table
gtsave(tab_bottom10, "fig/tab_bottom10.png")

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





