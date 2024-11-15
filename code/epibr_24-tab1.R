# Load packages
library(tidyverse)
library(here)
library(gt)

# Dataset
table1_data <- tibble::tribble(
  ~Categoria,           ~n,     ~Distribuição_amostral,       ~Obesidade,
  "Idade",              NA,     NA,                           NA,
  "Tercil 1",           26278,  "36.55 (35.85-37.26)",        "16.13 (15.11-17.21)",
  "Tercil 2",           22964,  "31.94 (31.32-32.57)",        "25.16 (23.75-26.64)",
  "Tercil 3",           22654,  "31.51 (30.92-32.1)",         "24.28 (23.34-25.25)",
  "Gênero",             NA,     NA,                           NA,
  "Homem",              34467,  "47.94 (47.28-48.59)",        "19.81 (18.84-20.82)",
  "Mulher",             37429,  "52.06 (51.41-52.72)",        "23.22 (22.33-24.13)",
  "Raça",               NA,     NA,                           NA,
  "Branca",             30628,  "42.6 (41.82-43.39)",         "21.11 (20.22-22.26)",
  "Preta",              8505,   "11.83 (11.39-12.29)",        "23.96 (22.4-25.59)",
  "Parda",              32763,  "45.57 (44.84-46.3)",         "21.42 (20.52-22.34)",
  "Renda (SM)",        NA,     NA,                           NA,
  "Tercil 1 (≤1 SM)",   37738,  "52.49 (51.62-53.36)",        "21.24 (20.31-22.19)",
  "Tercil 2 (1 – 3 SM)",26350,  "36.65 (35.91-37.4)",         "22.58 (21.39-23.81)",
  "Tercil 3 (> 3 SM)",  7808,   "10.86 (10.25-11.49)",        "19.91 (18.47-21.44)",
  "Escolaridade",       NA,     NA,                           NA,
  "Tercil 1",           21245,  "29.55 (28.87-30.24)",        "23.48 (22.52-24.47)",
  "Tercil 2",           34503,  "47.99 (47.27-48.71)",        "21.41 (20.19-22.69)",
  "Tercil 3",           16148,  "22.46 (21.68-23.27)",        "19.46 (18.4-20.57)",
  "Total",              71896,  "100.00",                     "21.59 (20.80-22.40)"
)

# Table 1
tab1 <- table1_data |> 
  gt() |> 
  tab_header(
    title = md("**Distribuição das características sociais segundo a amostra analítica e prevalência de obesidade (IMC≥30kg/m²). Pesquisa Nacional de Saúde, 2019.**")
  ) |> 
  cols_label(
    Categoria = " ",
    n = "n",
    Distribuição_amostral = md("% (IC 95%)"),
    Obesidade = md("% (IC 95%)")
  ) |>  
  opt_table_font(font = "raleway") |> 
  opt_stylize(style = 1, color = 'gray') |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Categoria,
      rows = Categoria %in% c("Idade", "Gênero", "Raça", "Renda (SM)", "Escolaridade", "Total")
    )
  ) |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
  ) |> 
  sub_missing(
    rows = everything(),
    missing_text = ""
  ) |> 
  opt_vertical_padding(scale = 0.1) |> 
  tab_options(table.font.size = 28)

 # Save table
gtsave(tab1, "fig/tab1.png")
  
