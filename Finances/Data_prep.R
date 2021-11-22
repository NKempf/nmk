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
         raison_operation = case_when(
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

# SG Perso
sg_perso <- read.xlsx("input/Comptes/SG Perso/compte_SG_Perso_23_05_21_au_21_11_21.xlsx",
                          sheet = 1,startRow = 3,detectDates = TRUE)  %>% 
  mutate(date = as.Date(`Date.de.l'opération`, format =  "%d/%m/%Y")) %>%
  select(date,LIBELLE=Libellé,detail=`Détail.de.l'écriture`,MONTANT=`Montant.de.l'opération`)

sg_perso <- sg_perso %>% 
  mutate(type_operation = case_when
         (str_detect(LIBELLE, "VIR", negate = FALSE) ~ "Virement",
           str_detect(LIBELLE, "Relev", negate = FALSE) ~ "Releve Visa premier",
           str_detect(LIBELLE, "PRLV", negate = FALSE)|
             str_detect(LIBELLE, "COTISATION", negate = FALSE)|
             str_detect(LIBELLE, "PRELEVEMENT", negate = FALSE)|
             str_detect(LIBELLE, "FRAIS", negate = FALSE) ~ "Prelevement",
           str_detect(LIBELLE, "RETRAIT", negate = FALSE) ~ "Retrait",
           str_detect(LIBELLE, "CHEQUE", negate = FALSE) ~ "Cheque",
           str_detect(LIBELLE, "CARTE", negate = FALSE) ~ "CB",
           TRUE ~ "autre"
         ),
         sens_operation = ifelse(MONTANT >=0, "Revenu","Depense"),
         # Raison de l'opération
         raison_operation = case_when(
           str_detect(detail, 
             paste(c("JAZZ","PIANO RENOUV","FRAIS PAIEMENT HORS ZONE")
             ,collapse = "|"),negate = FALSE) ~ "Frais bancaire",
           str_detect(detail, "SURAVENIR", negate = FALSE) ~ "Linxea_ass_vie",
           str_detect(detail, "DRFIP ILE DE FRANCE ET DE PARIS", negate = FALSE) ~ "Insee",
           str_detect(detail, 
                      paste(c("Keolis Bordeaux","L AUTO D ARES")
                        ,collapse = "|"), negate = FALSE) ~ "Transport",
           str_detect(detail, 
                      paste(c("CHEZ HORTENSE")
                        ,collapse = "|"), negate = FALSE) ~ "Restaurant",
           
           str_detect(detail, 
                      paste(c("VIR PERM")
                        ,collapse = "|"), negate = FALSE) ~ "Epargne loisir",
           
           str_detect(toupper(detail), 
                      paste(c("VIR EUROPEEN EMIS LOGITEL POUR: KEMPF NICOLAS")
                            ,collapse = "|"), negate = FALSE) ~ "Boursorama Perso",
           str_detect(detail, 
                      paste(c("VIR EUROPEEN EMIS LOGITEL POUR: INDIVISION KEMPF DEMOUGEOT")
                            ,collapse = "|"), negate = FALSE) ~ "SG Joint",
           

           TRUE ~ "autre"
         )
         
         
         
  )



# Revenus hors virements interne ou avec SG
rev <- bourso_perso %>% 
  filter(sens_operation == "Revenu" & !raison_operation %in% c("SG NK","Interne Bourso")) %>% 
  select(date, everything()) %>% 
  mutate(type_revenu = case_when(
    regularite_operation == "Mensuelle" & str_detect(LIBELLE, "DRFIP ILE DE FRANCE ET DE PARIS", negate = FALSE) ~ "Paye",
    TRUE ~ "Exceptionnel"
  )
  ) %>% 
group_by(month(date),type_revenu) %>%
summarise(MONTANT = sum(MONTANT)) %>%
rename(mois = `month(date)`)

# Graphique des revenus
g3 <- ggplot(rev, aes(fill=type_revenu, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity")+
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Revenus par mois NK"
  )

g3






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








