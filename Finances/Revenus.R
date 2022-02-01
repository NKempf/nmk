#  Finances NK - Revenus
library(openxlsx)
library(tidyverse)
library(janitor)# Marges
library(lubridate)
library(hrbrthemes)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(quantmod)


# I. Import des données----
load(file = "database/Finances NK/finances_NK.RData")

# Portefeuille Boursier
portfolio <- read.xlsx("input/Finances/fin_26_01_2022.xlsx",sheet = "portfolio",startRow = 2,detectDates = TRUE) %>% 
  mutate(
    date = as.Date(date, format =  "%d/%m/%Y")
  )

# II. Statistiques revenus----

# Revenus hors virements interne ou avec SG
rev <- compte %>% 
  filter(sens_operation == "Revenu" & !raison_operation %in% c("Virement_Exterieur","Virement_Lise","Virement_interne","Epargne","Avion") & 
           !str_detect(LIBELLE,
                       paste(c("VIR Caution pret immo")
                             ,collapse = "|"), negate = FALSE) 
  ) %>% 
  mutate(type_revenu = case_when(
    raison_operation %in% c("Insee") ~ "Paye",
    raison_operation %in% c("Sante","Netflix","Eau","Aucune","Impots","Peage") | str_detect(LIBELLE,
                                                                                            paste(c("AVOIR","SEPA ORANGE","VIR SEPA OFF  NOTARIAL VILLENAVE CHAMBER")
                                                                                                  ,collapse = "|"), negate = FALSE) ~ "Remboursement",
    TRUE ~ "autre"),
    
  ) 

# Focus dividendes
divi <- portfolio %>% 
  filter(tolower(action) == "dividendes")


# IV.1. Revenus totaux
rev2 <- rev %>% 
  select(date,type_revenu,MONTANT,ponderation) %>% 
  bind_rows(
    divi %>% 
      select(date,MONTANT = total) %>% 
      mutate(type_revenu = "Dividendes",ponderation = 1)
  ) %>% 
  mutate(mois = floor_date(date, unit = "month")) %>% 
  group_by(mois,type_revenu) %>%
  summarise(MONTANT = sum(MONTANT * ponderation)) %>% 
  mutate(type_revenu = factor(type_revenu,levels = c("autre","Dividendes","Remboursement","Paye")))


# Graphique des revenus
rev.g1 <- ggplot(rev2, aes(fill=type_revenu, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity")+
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Revenus par mois NK"
  )
ggplotly(rev.g1)

# IV.2. le détail des revenus du mois
mois.filtre <- 10

revprinc <- rev %>% 
  select(-type_operation,-sens_operation,-ponderation) %>% 
  filter(month(date) == mois.filtre) %>% 
  arrange(desc(MONTANT)) %>% 
  adorn_totals("row") 

tab_rev <- datatable(revprinc,rownames = FALSE,colnames = c("Date","Compte","Raison","Montant (€)","Origine","Type"))

# IV.3. Suivi des dividendes
divi_g1 <- divi %>% 
  mutate(mois = floor_date(date, unit = "month")) %>% 
  group_by(mois) %>% 
  summarise(
    dividendes = sum(total)) %>% 
  mutate(
    dividendes_cumules = cumsum(dividendes)
  ) 

rev.g2 <- ggplot(divi_g1) + 
  geom_bar(aes(y=dividendes, x=mois), stat="identity") +
  geom_line(aes(y=dividendes_cumules, x=mois),colour = "#fed766") +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    # fill = "Type",
    title = "Dividendes par mois NK"
  )
ggplotly(rev.g2)
