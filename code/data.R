# Packages #### 
library(tidyverse) 
library(here) 
library(openxlsx) 
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

#




