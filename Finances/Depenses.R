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
  filter(sens_operation %in% c("Depense") & !raison_operation %in% c("Virement_Exterieur","Virement_Lise","Virement_interne"
                                                                # ,"Epargne"
                                                                ,"Avion")
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

# Analyse des dépenses par mois----

# Depenses par mois
dep2 <- depenses %>% 
  mutate(
    mois = floor_date(date, unit = "month")
    ) %>% 
  group_by(mois,type_depense) %>% 
  summarise(
    nb_dep = n(),
    dep_moy = abs(weighted.mean(MONTANT,ponderation)),
    dep_som = abs(sum(MONTANT * ponderation))
  )

# Depenses moyenne par an
dep3 <- dep2 %>% 
select(mois,type_depense,dep_som) %>% 
  filter(mois >= "2021-05-01") %>% 
  mutate(an = year(mois)) %>% 
  group_by(an,type_depense) %>% 
  summarise(
    dep_moy = abs(mean(dep_som)),
    dep_som = abs(sum(dep_som))
  )


# G1 : dépenses par mois sans les virements
nk.dep.g1 <- ggplot(dep2, aes(fill=type_depense, y=dep_som, x=mois)) + 
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


# G3 : dépenses par an sans les virements
nk.dep.g3 <- ggplot(dep3, aes(fill=type_depense, y=dep_moy, x=an)) + 
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Année", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses moyenne par an NK"
  )
ggplotly(nk.dep.g3)


# Dépenses régulières - nk
dep31 <- depenses %>% 
  # filter(compte == "Bourso joint") %>% 
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
  group_by(mois,raison_operation) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT * ponderation))
  ) %>%
  mutate(pct = round(100*MONTANT / sum(MONTANT),2))

nk.dep.g4 <- ggplot(dep31, aes(fill=raison_operation, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = pct)) +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses régulières par mois NK"
  )
ggplotly(nk.dep.g4)


# Focus sur les dépenses du compte-joint----
dep4 <- depenses %>% 
  filter(compte == "Bourso joint") %>% 
  mutate(
    mois = floor_date(date, unit = "month")
  ) %>% 
  group_by(mois,type_depense) %>% 
  summarise(
    nb_dep = n(),
    dep_moy = abs(mean(MONTANT)),
    dep_som = abs(sum(MONTANT))
  )

# G1 : dépenses par mois sans les virements
joint.dep.g1 <- ggplot(dep4, aes(fill=type_depense, y=dep_som, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses par mois JOINT"
  )
ggplotly(joint.dep.g1)

# Depenses moyenne par an
dep5 <- dep4 %>% 
  select(mois,type_depense,dep_som) %>% 
  filter(mois >= "2021-05-01") %>% 
  mutate(an = year(mois)) %>% 
  group_by(an,type_depense) %>% 
  summarise(
    dep_moy = abs(mean(dep_som)),
    dep_som = abs(sum(dep_som))
  )

# G2 : dépenses par an sans les virements
joint.dep.g2 <- ggplot(dep5, aes(fill=type_depense, y=dep_moy, x=an)) + 
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Année", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses moyenne par an JOINT"
  )
ggplotly(joint.dep.g2)


# Dépenses régulières - compte joint
dep6 <- depenses %>% 
  filter(compte == "Bourso joint") %>% 
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
  group_by(mois,raison_operation) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT))
  ) %>%
  mutate(pct = round(100*MONTANT / sum(MONTANT),2))

joint.dep.g3 <- ggplot(dep6, aes(fill=raison_operation, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = pct)) +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses régulières par mois JOINT"
  )
ggplotly(joint.dep.g3)

# save(compte,fin2,file = "input/Comptes/synthese_fin.RData")

# Epargne
effort_epargne <- compte %>% 
  filter(raison_operation %in% c("Linxea_ass_vie","Linxea_per","Nalo_ass_vie")) %>% 
  group_by(mois,raison_operation) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT*ponderation))
  ) %>%
  mutate(pct = round(100*MONTANT / sum(MONTANT),2))

epargne.g1 <- ggplot(effort_epargne, aes(fill=raison_operation, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = pct)) +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Effort d'épargne par mois NK"
  )

ggplotly(epargne.g1)

