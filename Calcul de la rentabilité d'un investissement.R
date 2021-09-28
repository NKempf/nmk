# Return : calcul de la rentabilité d'un investissement

library(PMwR)
library(quantstrat)
library(tidyverse)
library(openxlsx)

# Import
journal_db <- read.xlsx("input/NMK_db.xlsx",sheet = "Journal",startRow = 2,detectDates = T)


from ="2020-10-01"
to ="2021-09-28"
symbols = c("TTE") # TOTAL et Danone
currency('USD')
initEq=10000
adjustment <- TRUE # Toujours travailler en prix ajustés

getSymbols(symbols, from=from, to=to, 
           adjust=TRUE) # Danone n'est pas reconnu
TTE.adj <- TTE

getSymbols(symbols, from=from, to=to, 
           adjust=FALSE) # Danone n'est pas reconnu

Cl(TTE["2021-09-28"])

# Exemple : total
total <- journal_db %>% 
  filter(instrument == "Total") %>% 
  mutate(
    pru = (amount * price + fees) / amount, # prix de revient unitaire
  )
