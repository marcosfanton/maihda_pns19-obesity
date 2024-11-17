# Load packages #### 
library(tidyverse) 
library(here) 
library(scales)
library(MetBrewer)
library(showtext)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
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
             linewidth = 0.5) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  labs(x = "Ranking de Estratos Sociais",
        y = "Prevalência de Obesidade Estimada (%)") +
  scale_y_continuous(labels = scales::label_percent(scale = 100, suffix = "")) +
  scale_color_identity() + 
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "raleway", size = 22),
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
             linewidth = 0.5) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  labs(x = "Ranking de Estratos Sociais",
        y = "Obesidade Estimada (%)") +
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
"womanblackmidIlowElow" = "Mulher-Preta-Idade(t2)-Renda(Baixa)-Educação(Baixa)",
"womanbrownmidIlowElow" = "Mulher-Parda-Idade(t2)-Renda(Baixa)-Educação(Baixa)",
"womanblackoldIlowElow" = "Mulher-Preta-Idade(t3)-Renda(Baixa)-Educação(Baixa)",
"womanbrownmidImidElow" = "Mulher-Parda-Idade(t2)-Renda(Média)-Educação(Baixa)",
"womanblackoldIhighEmid" = "Mulher-Preta-Idade(t3)-Renda(Alta)-Educação(Média)",
"womanblackmidImidElow" = "Mulher-Preta-Idade(t2)-Renda(Média)-Educação(Baixa)",
"womanblackmidImidEhigh" = "Mulher-Preta-Idade(t2)-Renda(Média)-Educação(Alta)",
"manblackmidImidEmid" = "Homem-Preto-Idade(t2)-Renda(Média)-Educação(Média)",
"womanwhiteyoungIhighEhigh" = "Mulher-Branca-Idade(t1)-Renda(Alta)-Educação(Alta)",
"manbrownyoungIlowEmid" = "Homem-Pardo-Idade(t1)-Renda(Baixa)-Educação(Média)",
"manbrownyoungIlowElow" = "Homem-Pardo-Idade(t1)-Renda(Baixa)-Educação(Baixa)",
"womanwhiteyoungImidEhigh" = "Mulher-Branca-Idade(t1)-Renda(Média)-Educação(Alta)",
"womanbrownyoungIhighEhigh" = "Mulher-Parda-Idade(t1)-Renda(Alta)-Educação(Alta)",
"womanwhitemidIhighEhigh" = "Mulher-Branca-Idade(t2)-Renda(Alta)-Educação(Alta)",
"manwhiteyoungIhighEhigh" = "Homem-Branco-Idade(t1)-Renda(Alta)-Educação(Alta)",
"manwhiteyoungIlowElow" = "Homem-Branco-Idade(t1)-Renda(Baixa)-Educação(Baixa)",
"womanbrownyoungIlowEhigh" = "Mulher-Parda-Idade(t1)-Renda(Baixa)-Educação(Alta)",
"manwhiteyoungIlowEhigh" = "Homem-Branco-Idade(t1)-Renda(Baixa)-Educação(Alta)",
"womanblackmidIlowEmid" = "Mulher-Preta-Idade(t2)-Renda(Baixa)-Educação(Média)",
"womanblackoldIlowEmid" = "Mulher-Preta-Idade(t3)-Renda(Baixa)-Educação(Média)"),
  mBprob_lwr = mBprob_lwr*100,
  mBprob_upr = mBprob_upr*100,
  mBprob_fit = mBprob_fit*100
) 

# Table Top 10
tab_top10 <- top10 |> 
  filter(type == "top") |> 
  select(stratum, mBprob_fit, mBprob_lwr, mBprob_upr) |> 
  gt() |> 
  cols_label(
    stratum = "Estrato",
    mBprob_fit = "(%)",
    mBprob_lwr = "IC(95%)",
    mBprob_upr = ""
  ) |> 
  cols_unhide(columns = mBprob_upr) |>
  cols_merge(
    columns = c(mBprob_lwr, mBprob_upr),
    pattern = "({1},<< {2})>>",
  ) |> 
  tab_header(
    title = "Obesidade Estimada por Estratos Sociais",
    subtitle = md("Top 10 Estratos com <u>Maior</u> Prevalência")
  ) |>  
  tab_spanner(
    label = "Obesidade",
    columns = c(mBprob_fit, mBprob_lwr, mBprob_upr)
  ) |> 
  fmt_number(
    columns = c(mBprob_fit, mBprob_lwr, mBprob_upr),
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
gtsave(tab_top10, "fig/tab3_top10_epibr.png")

# Table Bottom 10
tab_bottom10 <- top10 |> 
  filter(type == "bottom") |> 
  select(stratum, mBprob_fit, mBprob_lwr, mBprob_upr) |> 
  gt() |> 
  cols_label(
    stratum = "Estrato",
    mBprob_fit = "(%)",
    mBprob_lwr = "IC(95%)",
    mBprob_upr = ""
  ) |> 
  cols_unhide(columns = mBprob_upr) |>
  cols_merge(
    columns = c(mBprob_lwr, mBprob_upr),
    pattern = "({1},<< {2})>>",
  ) |> 
  tab_header(
    title = "Obesidade Estimada por Estratos Sociais",
    subtitle = md("Top 10 Estratos com <u>Menor</u> Prevalência")
  ) |>  
  tab_spanner(
    label = "Obesidade",
    columns = c(mBprob_fit, mBprob_lwr, mBprob_upr)
  ) |> 
  fmt_number(
    columns = c(mBprob_fit, mBprob_lwr, mBprob_upr),
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
gtsave(tab_bottom10, "fig/tab4_bottom10_epibr.png")

# Tables ####
# Table with models 
tab2 <- tab_model(mA, mB, 
  p.style = "stars",
  string.ci = "IC(95%)",
  show.reflvl = TRUE,
  prefix.labels = "none",
pred.labels = c("Intercepto",
"Gênero (Mulher)",
"Raça (Preta)",
"Raça (Pardo)",
"Idade (Tercil2)",
"Idade (Tercil3)",
"Renda (Média)",
"Renda (Alta)",
"Educação (Média)",
"Educação (Alta)"),
dv.labels = c("Modelo Nulo", "Modelo Aleatório"),
string.pred = "Coeficiente",
CSS = list(css.table = '+font-family: Raleway;'))



