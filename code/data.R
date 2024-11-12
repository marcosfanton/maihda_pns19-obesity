# Packages #### 
library(tidyverse) 
library(here) 
library(PNSIBGE) # Extraction microdata PNS (National Survey on Health - https://www.pns.icict.fiocruz.br/)

# Data Import + Recode####

# Selection of variables from PNS19 database
vars <- c(
  "P005", # pregnancy (2 = NO)
  "C006", # sex/gender
  "C008", # age 
  "C009", # race/color/ethnicity
  "P00102", # weight self-awareness ("Do you know your weight?" (1 = YES))
  "P00103", # self-reported weight
  "P00104", # final weight  
  "P00402", # height self-awareness ("Do you know your height?" (1 = YES))
  "P00403", # self-reported weight
  "P00404", # final height
  "VDF004", # Household income per capita based on minimum wage (MW in 2019: R$ 998,00)
  "VDD004A") # Highest level of education achieved (9 years system) 

# Import dataset with PNSIBGE
raw19 <- get_pns(year = 2019, 
  vars = vars, # variables of interest
  selected = TRUE, # keep only respondents of the individual questionnaire
  design = FALSE) # unstructured database (non-survey format)

# Save dataset
write.csv(raw19, # -- n: 90.846
  "data/raw/raw19.csv", 
  row.names = FALSE)

# Recode variables
pns19 <- raw19 |>
  dplyr::filter(C008 >= 18 & C008 <= 65)   |>  # filter for age 
  dplyr::filter(P005 != "Sim"| is.na(P005))  |>  # keep only non-pregnant respondents
  dplyr::mutate(
    age = factor( 
      cut(C008, c(breaks = quantile(C008, 
                                    probs = seq(0, 1, 1/3), # age by tercile  
                                    na.rm = TRUE)), 
          labels = c("young", "mid", "old"), # young (18-35), mid (36-49), old (50-65) 
          right = FALSE, 
          include.lowest = TRUE)), 
    gender = factor(dplyr::case_when( 
      C006 == "Homem" ~ "man",
      C006 == "Mulher" ~ "woman"),
      levels = c("man", "woman")), 
    race = factor(dplyr::case_when(
      C009 == "Branca" ~ "white",
      C009 == "Preta" ~ "black",
      C009 == "Parda" ~ "brown"),
      levels = c("white", "black", "brown")),
    income = factor(case_when(
      VDF004 %in% c("Até ¼ salário mínimo",
                    "Mais de ¼ até ½ salário mínimo",
                    "Mais de ½ até 1 salário mínimo") ~ "Ilow",   
      VDF004 %in% c("Mais de 1 até 2 salários mínimos",
                "Mais de 2 até 3 salários mínimos") ~ "Imid",
      VDF004 %in% c("Mais de 3 até 5 salários mínimos",
                "Mais de 5 salários mínimos") ~ "Ihigh"),
      levels = c("Ilow", "Imid", "Ihigh")), # MW: Ilow (0-1), Imid(>1-3), Ihigh(>3)
    education = factor(dplyr::case_when(
  VDD004A %in% c("Sem instrução", 
                 "Fundamental incompleto ou equivalente") ~ "Elow",
  VDD004A %in% c("Fundamental completo ou equivalente", 
                 "Médio incompleto ou equivalente",
                "Médio completo ou equivalente") ~ "Emid",
  VDD004A %in% c("Superior incompleto ou equivalente", "Superior completo") ~ "Ehigh"),
  levels = c("Elow", "Emid",  "Ehigh")), # Education: Elow (0 - incomplete elementary school), Imid (complete elementary school - complete high school), Ihigh (incomplete-complete higher education)
    height = dplyr::coalesce(P00403, P00404),
    weight = dplyr::coalesce(P00103, P00104),
    bmi = weight/((height/100)^2), # BMI
    obesity = dplyr::case_when(    # Obesity(1 = YES, 0 = NO)
      bmi >= 30 ~ 1,
      TRUE ~ 0))  |>  
  dplyr::filter(bmi > 15 & bmi <= 60) |> # filter implausible BMI measures
  dplyr::select(age, # filter variables of interest
                gender, 
                race, 
                income, 
                education, 
                weight, 
                height, 
                bmi, 
                obesity) #  n: 73000

# Filter NA's 
pns19 <- drop_na(pns19) # --> n: 71896  

# Generate stratum variable ####
# List with all combinations
strata_levels <- expand.grid(
  age = unique(pns19$age),
  gender = unique(pns19$gender),
  race = unique(pns19$race),
  income = unique(pns19$income),
  education = unique(pns19$education)
)
# Stratum Levels (all)
strata_levels <- strata_levels |> 
  dplyr::mutate(stratum = factor(paste(gender, race, age, income, education, sep = "")))

# Include stratum variable in PNS19 dataset
pns19 <- pns19 |> 
  dplyr::mutate(
    stratum = factor(paste(gender, race, age, income, education, sep = ""),
              levels = levels(strata_levels$stratum)),
    id = dplyr::row_number() 
  ) |> 
  dplyr::group_by(stratum) |> 
  dplyr::mutate(strata_n = n()) |> 
  ungroup()

# Calculate each stratum size 
strata_tabel <- pns19 |>  
summarise(strata_n = n(), .by = stratum) |> 
  complete(stratum, fill = list(strata_n = 0)) |> 
  dplyr::arrange(strata_n)

# Save processed dataset for analysis 
write.csv(pns19, 
  "data/pns19.csv", 
  row.names = FALSE)

# Save table with strata with each stratum size
write.csv(strata_tabel,
"data/strata_tabel.csv",
row.names = FALSE)

