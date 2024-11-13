# Load packages #### 
library(tidyverse) 
library(here) 
library(scales)
library(MetBrewer)
library(showtext)
library(gganimate)
library(gt)


# Load models and datasets ####
strat_level <- read.csv("data/pns19_stratum-obesity.csv", stringsAsFactors = TRUE) |> 
  dplyr::mutate(id = row_number())
  
# Fig.1 Scatterplot (sorted by obesity) ####  
font_add_google("Raleway", "raleway")
showtext_auto()

fig1 <- strat_level |> 
  ggplot(aes(y = mBprob_fit, x = rankprob, color = stratum)) +
  geom_point(size = 1) +
    geom_errorbar(aes(ymin = mBprob_lwr, ymax = mBprob_upr)) +
  ylab("Obesidade Predita (%)") +
  xlab("") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
    scale_color_manual(values = met.brewer("Peru1", 159)) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "raleway")) 

ggplotly(fig1)

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

# Tabela com os Top-Bottom 10
# Dictionary
dicionario <- c(
  "woman" = "Mulher",
  "man" = "Homem",
  "black" = "Preto",
  "brown" = "Pardo",
  "white" = "Branco",
  "Ihigh" = "Renda Alta",
  "Ilow" = "Renda Baixa",
  "Imid" = "Renda Média",
  "Ehigh" = "Educação Alta",
  "Elow" = "Educação Baixa",
  "Emid" = "Educação Média",
  "mid" = "IdadeIntermediária",
  "old" = "MaisVelho",
  "young" = "Jovem"
)
top10 %>%
  mutate(stratum = str_replace_all(stratum, dicionario)) %>%
  # Selecionar as colunas traduzidas para a tabela final
  select(stratum, obesity_mean) %>%
  # Criar a tabela com o pacote gt
  gt() %>%
  tab_header(
    title = "Tabela de Estratos e Média de Obesidade",
    subtitle = "Apresentando as médias de obesidade para cada estrato (em português)"
  ) %>%
  cols_label(
    stratum = "Estrato",
    obesity_mean = "Média de Obesidade (%)"
  ) %>%
  fmt_number(
    columns = obesity_mean,
    decimals = 1
  )
