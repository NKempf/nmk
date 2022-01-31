#  Finances NK - Depenses
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


# II. Finances NK----

# TODO
# Analyse synthétique des mouvements des comptes
# Ensuite, faire une analyse par type : revenus, dépenses, epargne, virement











depenses <- compte %>% 
  filter(sens_operation %in% c("Depense","Epargne") & !raison_operation %in% c("Virement_Exterieur","Virement_Lise","Virement_interne"
                                                                # ,"Epargne"
                                                                ,"Avion")
         # & 
         #   !str_detect(LIBELLE,
         #               paste(c("VIR Caution pret immo")
         #                     ,collapse = "|"), negate = FALSE) 
  )  %>% 
  mutate(type_depense = case_when(
    raison_operation %in% c("Ammeublement","Assurance","Assurance_pret","Bricolage","Eau","Electricite","Gaz","Jardin","Maison","Ménage","Pret_immo") ~ "Logement",
    raison_operation %in% c("Essence","Voiture") ~ "Voiture",
    raison_operation %in% c("Alimentation") ~ "Alimentation",
    raison_operation %in% c("Coiffeur","Habillement") ~ "Soin_personne",
    raison_operation %in% c("Frais bancaire") ~ "Banque",
    raison_operation %in% c("Hotel","Peage","Train","Transport") ~ "Voyage_transport",
    raison_operation %in% c("Impots") ~ "Impots",
    raison_operation %in% c("Linxea_ass_vie","Linxea_per","Nalo_ass_vie") ~ "Epargne",
    raison_operation %in% c("Loisir","Netflix","Orange","Moto","Aucune") ~ "Loisir",
    raison_operation %in% c("Restaurant") ~ "Restaurant",
    raison_operation %in% c("Sante") ~ "Sante",
    TRUE ~ "autre"
  ),
  
  regularite_depense = case_when(
    raison_operation %in% c("Assurance","Assurance_pret","Eau","Electricite","Gaz","Linxea_ass_vie","Linxea_per","Ménage",
                            "Nalo_ass_vie","Netflix","Orange","Pret_immo") ~ "Mensuel",
    TRUE ~ "Exceptionnelle"
  ),
  
  # Correction : virement ponctuel sur assurance vie
  regularite_depense = ifelse(
    raison_operation %in% c("Linxea_ass_vie","Linxea_per","Nalo_ass_vie") & abs(MONTANT) >= 1000,"Exceptionnelle", regularite_depense)
  
  )

dep2 <- depenses %>% 
  mutate(mois = floor_date(date, unit = "month")) %>% 
  group_by(mois,type_depense) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT * ponderation))
  )

# G1 : dépenses par mois sans les virements
nk.dep.g1 <- ggplot(dep2, aes(fill=type_depense, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses par mois NK"
  )
ggplotly(nk.dep.g1)

# G2 : détails des dépenses par mois
mois.filtre <- 11

nk.dep.details <- depenses %>% 
  select(-sens_operation,-ponderation,-type_operation) %>% 
  filter(month(date) == mois.filtre & abs(MONTANT) >= 100) %>% 
  arrange(MONTANT) %>% 
  adorn_totals("row") 

nk.dep.g2 <- datatable(nk.dep.details,rownames = FALSE,colnames = c("Date","Compte","Montant (€)","Raison","Origine","Type"))
nk.dep.g2


dep3 <- depenses %>% 
  filter(regularite_depense == "Mensuel") %>% 
  mutate(
    type_depense_reguliere = case_when(
      raison_operation %in% c("Assurance","Assurance_pret","Pret_immo","Ménage") ~ "Maison",
      raison_operation %in% c("Eau","Electricite","Gaz") ~ "Energie_Eau",
      raison_operation %in% c("Linxea_ass_vie","Linxea_per","Nalo_ass_vie") ~ "Epargne",
      raison_operation %in% c("Netflix","Orange") ~ "TIC",
      TRUE ~ "autre"
    ),
    mois = floor_date(date, unit = "month")
  ) %>% 
  group_by(mois,type_depense_reguliere) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT*ponderation))
  ) %>%
  mutate(pct = round(100*MONTANT / sum(MONTANT),2))

dep.g3 <- ggplot(dep3, aes(fill=type_depense_reguliere, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = pct)) +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses régulières par mois NK"
  )

ggplotly(dep.g3)

save(compte,fin2,file = "input/Comptes/synthese_fin.RData")

# Epargne

effort_epargne <- compte %>% 
  filter(raison_operation %in% c("Linxea_ass_vie","Linxea_per","Nalo_ass_vie")) %>% 
  group_by(mois,raison_operation) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT*ponderation))
  ) %>%
  mutate(pct = round(100*MONTANT / sum(MONTANT),2))

dep.g4 <- ggplot(effort_epargne, aes(fill=raison_operation, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = pct)) +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Effort d'épargne par mois NK"
  )

ggplotly(dep.g4)

