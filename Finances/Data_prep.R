# TODO

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

# I. Patrimoine----
fin <- read.xlsx("input/fin_16_11_2021.xlsx",detectDates = T,sheet = "patrimoine") %>% 
  mutate(date2 = format(date, "%Y-%m"))

fin2 <- fin %>% 
  select(date,BFM,LDD,PEA,Linxea_ass_vie, Nalo_ass_vie, PER, CTO, Maison) %>% 
  mutate(Epargne.totale = BFM + LDD + PEA + Linxea_ass_vie + Nalo_ass_vie + PER + CTO + Maison) %>% 
  gather(compte,value,-date)

# Graph : évolution du patrimoine 
g1 <- ggplot(fin2, aes(x=date,group = compte, y=value,color = compte)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b",date_breaks = "1 month") +
  theme_ipsum() +
  theme(axis.text = element_text( 
    angle = 90, 
    face=3)
  )+
  # ggtitle("Patrimoine")+
  labs(
    x = " ", 
    y = "Montant (€)", 
    colour = "Comptes",
    title = "Patrimoine NK"
  )
g1

ggplotly(g1)

# Graph : Patrimoine Marius
fin_marius <- fin %>% 
  select(date,yomoni_Marius) %>% 
  mutate(Epargne.totale = yomoni_Marius) %>%
  gather(compte,value,-date)

g2 <- ggplot(fin_marius, aes(x=date,group = compte, y=value,color = compte)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b",date_breaks = "1 month") +
  theme_ipsum() +
  theme(axis.text = element_text( 
    angle = 90, 
    face=3)
  )+
  # ggtitle("Patrimoine")+
  labs(
    x = " ", 
    y = "Montant (€)", 
    colour = "Comptes",
    title = "Patrimoine de Marius"
  )
g2

ggplotly(g2)

# II. Dépenses exceptionnelles----
dep_excep <- read.xlsx("input/fin_16_11_2021.xlsx",sheet = "dep_excep",startRow = 2,detectDates = TRUE)

dep2 <- dep_excep %>% 
  filter(month(date) == 10) %>% 
    arrange(desc(montant)) %>% 
  slice(1:5) %>% 
  adorn_totals("row") 

datatable(dep2,rownames = FALSE)


# III. Relevés de comptes----

# Boursorama Perso

bourso_perso <- read.xlsx("input/Comptes/Bourso perso/compte_PERSO_BOURSO_00040384302_du_01-09-2020_au_21-11-2021.xlsx",
                       sheet = 1,startRow = 4,detectDates = TRUE) %>% 
  mutate(date = as.Date(DATE.OPERATION, format =  "%d/%m/%Y")) %>% 
  select(-DATE.VALEUR,-X6,-DEVISE,-DATE.OPERATION)

bourso_perso <- bourso_perso %>% 
  mutate(type_operation = case_when
         (str_detect(LIBELLE, "VIR", negate = FALSE) ~ "Virement",
           str_detect(LIBELLE, "Relev", negate = FALSE) ~ "Releve Visa premier",
           str_detect(LIBELLE, "PRLV", negate = FALSE) ~ "Prelevement",
           str_detect(LIBELLE, "RETRAIT ", negate = FALSE) ~ "Retrait",
           TRUE ~ "autre"
         ),
         sens_operation = ifelse(MONTANT >=0, "Revenu","Depense"),
         qui_operation = case_when(
           type_operation == "Virement" & str_detect(LIBELLE, "SEPA M. NICOLAS KEMPF", negate = FALSE) ~ "SG NK",
           type_operation == "Virement" & (str_detect(LIBELLE, "Alimentation PEA", negate = FALSE) | 
                                             str_detect(LIBELLE, "Virement de Monsieur Nicolas", negate = FALSE) |
                                             str_detect(LIBELLE, "Virement interne", negate = FALSE) |
                                             str_detect(LIBELLE, "Regularisation du compte", negate = FALSE)
                                           ) ~ "Interne Bourso",
           type_operation == "Releve Visa premier" ~ "Visa premier Bourso NK",
           type_operation == "Prelevement" & str_detect(LIBELLE, "Orange", negate = FALSE) ~ "Orange",
           type_operation == "Prelevement" & str_detect(LIBELLE, "GENERALI VIE SA", negate = FALSE) ~ "Nalo_ass_vie",
           type_operation == "Prelevement" & str_detect(LIBELLE, "SURAVENIR", negate = FALSE) ~ "Linxea_ass_vie",
           type_operation == "Virement" & str_detect(LIBELLE, "MGEFI", negate = FALSE) ~ "MGEFI",
           type_operation == "Virement" & str_detect(LIBELLE, "DRFIP ILE DE FRANCE ET DE PARIS", negate = FALSE) ~ "Insee",
           type_operation == "Prelevement" & str_detect(LIBELLE, "SPIRICA", negate = FALSE) ~ "Linxea_per",

           TRUE ~ "autre"
         ),
         regularite_operation = case_when(
           qui_operation %in% c("Visa premier Bourso NK","Orange","Nalo_ass_vie","Linxea_ass_vie","Linxea_per","Insee") ~ "Mensuelle",
           TRUE ~ "Exceptionnelle"
         )
  )


# Revenus hors virements interne ou avec SG
rev <- bourso_perso %>% 
  filter(sens_operation == "Revenu" & !qui_operation %in% c("SG NK","Interne Bourso")) %>% 
  select(date, everything()) %>% 
  group_by(month(date),regularite_operation) %>% 
  summarise(MONTANT = sum(MONTANT)) %>% 
  rename(mois = `month(date)`)


# Graphique des revenus
g3 <- ggplot(rev, aes(fill=regularite_operation, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity")

g3



bourso_perso %>% 
  filter(type_operation == "autre")






# Portefeuille Boursier----
portfolio <- read.xlsx("input/fin_16_11_2021.xlsx",sheet = "portfolio",startRow = 2,detectDates = TRUE)

# Synthèse globale
portofolio.synth <- portfolio %>% 
  group_by(actions) %>% 
  summarise(value = sum(total))

# Synthèse par compte
portofolio.synth2 <- portfolio %>% 
  group_by(compte,actions) %>% 
  summarise(value = sum(total))

# Synthèse du porte_feuille
table(portfolio$Entreprise)

portfolio %>% 
  filter(!Entreprise %in% c("Cash"))

# Screener Actions----
screener <- read.xlsx("input/fin_16_11_2021.xlsx",sheet = "screener",startRow = 2,detectDates = TRUE)








