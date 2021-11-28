# Finances NK

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
  select(date,BFM,LDD,PEA,Linxea_ass_vie, Nalo_ass_vie, PER, CTO, Maison ,livret_bourso) %>% 
  mutate(Epargne.totale = BFM + LDD + PEA + Linxea_ass_vie + Nalo_ass_vie + PER + CTO + Maison + livret_bourso) %>% 
  gather(compte,value,-date)

# Graph : évolution du patrimoine 
g1 <- ggplot(fin2, aes(x=date,group = compte, y=value,color = compte)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b",date_breaks = "1 month") +
  # theme_ipsum() +
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
  gather(compte,value,-date) %>% 
  filter(date >= "2021-10-01")

g2 <- ggplot(fin_marius, aes(x=date,group = compte, y=value,color = compte)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b",date_breaks = "1 month") +
  # theme_ipsum() +
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

# Bourso perso (refonte)
bourso_perso <- read.xlsx("input/Comptes/Bourso perso/compte_PERSO_BOURSO_00040384302_du_01-09-2020_au_21-11-2021.xlsx",
                          sheet = 1,startRow = 4,detectDates = TRUE) %>% 
  mutate(date = as.Date(DATE.OPERATION, format =  "%d/%m/%Y"),
         compte = "Bourso perso"
  ) %>% 
  select(-DATE.VALEUR,-X6,-DEVISE,-DATE.OPERATION) %>% 
  select(date, compte,everything()) %>% 
  filter(!str_detect(LIBELLE, "Relev", negate = FALSE)) # Suppression des dépenses de CD premier car intégration dans la table globale

# Bourso Joint
bourso_joint <- read.xlsx("input/Comptes/Bourso Joint/compte_JOINT_BOURSO_00040482911_du_05-02-2021_au_21-11-2021.xlsx",
                          sheet = 1,startRow = 4,detectDates = TRUE) %>% 
  mutate(date = as.Date(DATE.OPERATION, format =  "%d/%m/%Y"),
         compte = "Bourso joint"
  ) %>% 
  select(-DATE.VALEUR,-X6,-DEVISE,-DATE.OPERATION) %>% 
  select(date, compte,everything())

# SG Perso
sg_perso <- read.xlsx("input/Comptes/SG Perso/compte_SG_Perso_23_05_21_au_21_11_21.xlsx",
                      sheet = 1,startRow = 3,detectDates = TRUE)  %>% 
  mutate(date = as.Date(`Date.de.l'opération`, format =  "%d/%m/%Y"),
         compte = "SG perso"
  ) %>%
  select(date,compte,LIBELLE=`Détail.de.l'écriture`,MONTANT=`Montant.de.l'opération`)

# SG joint
sg_joint <- read.xlsx("input/Comptes/SG joint/compte_SG_JOINT_23_05_21_au_21_11_21.xlsx",
                      sheet = 1,startRow = 3,detectDates = TRUE)  %>% 
  mutate(date = as.Date(`Date.de.l'opération`, format =  "%d/%m/%Y"),
         compte = "SG joint"
  ) %>%
  select(date,compte,LIBELLE=`Détail.de.l'écriture`,MONTANT=`Montant.de.l'opération`)

# Carte visa premier
visa_premier <- read.xlsx("input/Comptes/Bourso perso/carte_VISA_PREMIER_4979________0747_du_22-11-2020_au_21-11-2021.xlsx",
                          sheet = 1,startRow = 4,detectDates = TRUE) %>% 
  mutate(date = as.Date(DATE.OPERATION, format =  "%d/%m/%Y"),
         compte = "Visa premier"
  ) %>% 
  select(-X5,-DEVISE,-DATE.OPERATION) %>% 
  select(date, compte,everything()) 


# Synthèse des comptes
compte <- bourso_perso %>% 
  bind_rows(bourso_joint,sg_perso,sg_joint,visa_premier) %>% 
  mutate(ponderation = ifelse(
    compte %in% c("SG joint","Bourso joint"), 0.5,1)) %>% 
  arrange(date)


compte <- compte %>% 
  mutate(LIBELLE = str_replace_all(LIBELLE, "[[:punct:]]", " "), # Suppression des caractères spéciaux
    
    type_operation = case_when(str_detect(LIBELLE, 
                     paste(c("VIR")
                           ,collapse = "|"), negate = FALSE) ~ "Virement",
           str_detect(LIBELLE, 
                      paste(c("CARTE")
                            ,collapse = "|"), negate = FALSE) ~ "Cb",
           str_detect(LIBELLE, "Relev", negate = FALSE) ~ "Releve Visa premier",
           str_detect(LIBELLE, 
                      paste(c("PRLV","PRELEVEMENT","FRAIS","ECH PRET","COTISATION","Commission de Cr","AVOIR","ARRETE")
                            ,collapse = "|"), negate = FALSE) ~ "Prelevement",
           str_detect(LIBELLE, "RETRAIT ", negate = FALSE) ~ "Retrait",
           str_detect(LIBELLE, "CHEQUE", negate = FALSE) ~ "Cheque",
           TRUE ~ "autre"
         ),
         sens_operation = ifelse(MONTANT >0, "Revenu","Depense"),
         
         raison_operation = case_when(
           str_detect(LIBELLE,
                      paste(c("JAZZ","PIANO RENOUV","FRAIS PAIEMENT HORS ZONE","FRAIS CB VISA ULTIM","COTISATION ANNUELLE CARTE Visa Premier",
                              "FORFAITAIRE NOMBRE JOURS")
                            ,collapse = "|"),negate = FALSE) ~ "Frais bancaire",
           str_detect(LIBELLE,
                      paste(c("Keolis Bordeaux","KEOLIS TBM","KADOLIS","KEOLIS")
                            ,collapse = "|"), negate = FALSE) ~ "Transport",
           str_detect(LIBELLE,
                      paste(c("CHEZ HORTENSE","BURGER KING","MCDONALD S","YAKI YAKI","LE MANCICIDOR","L AQUITAINE RESTAUR","CHEZ LAURETTE",
                              "MC DONALD'S","MC DONALD S","LITALIEN","UBER    EATS","MC DO BEGLES","LE GET","AU CHAROLAIS","BRIME DE URZ","VENTS ET MAREES",
                              "LE CHIEN DE PAVLO","LE CAFE DU THEA","CHAI PASCAL","LES MILLE ET UNE NU","PIZZ LA FLOREN","LE LATINO","PRET A MANGER",
                              "VOISIN 11012","L OTENTIK","ROCHER MALENDUR","le petit gerard","LE JARDIN DE PA","GOUNES FOOD",
                              "DJAM S PIZZAS","FINGER MOOD","AQUIT RESTAU","KFC VILLENAVE","MISS FOOD","ATMOSPHER","KFC","LE LIBANAIS DE M",
                              "VIR INST SAVEURS DU BOIS DU ROC")
                            ,collapse = "|"), negate = FALSE) ~ "Restaurant",
           str_detect(LIBELLE,
                      paste(c("ASF","ESCOTA","APRR","COFIROUTE","AUTOROUTES")
                            ,collapse = "|"), negate = FALSE) ~ "Peage",
           str_detect(LIBELLE,
                      paste(c("ESSFLOREALY","CARREFSTATION","AUCHAN SUPER","AUCHAN CARBURANT","L AUTO D ARES","ITM STATION","CORA DL CARBURA","ABB BORDEAUX",
                              "TOTAL","CHIOCCHIO","SCP MADIOT DUSSA","STATION U")
                            ,collapse = "|"), negate = FALSE) ~ "Essence",
           str_detect(LIBELLE,
                      paste(c("CYRILLUS","LA HALLE","MAILLE SOUPLE","KIABI","ROUMEGOUX   GIL","MAGODIS","ROUMEGOUX   GIL","MURMURS","HERBES FAUVE","HEDERA",
                              "SUMUP   MIRREY","JACADI","SUMUP   UKA DISTRIBUTION","la source","AUBERT","BABY LUX","MICHEL S","CDISCOUNT 1 A   4",
                              "MADIOT DUSSAUGE","BONOBO","VERTBAUDET","PETIT BATEAU","BEBE 9","BAHIA 4","SP   BIBSWORLD CO","BEBE AU NATUREL",
                              "ATELIERROSEMOOD","MAMAN NATUR ELLE")
                            ,collapse = "|"), negate = FALSE) ~ "Habillement",
           str_detect(LIBELLE,
                      paste(c("DECATHLON","STEAM","CULTURA","HOT SPOT","Leetchi SA","COURSRA","PIP PRESSION","BASSIN LUMIERES",
                              "JN LAVAGE","PHOTOMATON","AMAZON PAYMENTS COMMERCE ELECTRONIQUE","AMAZON PAYMENTS","CHRISTIAN VINET",
                              "CINEMA JEAN EUS","L EMBARCADERE","SARL COMPTOIR DU","BACS GIRONDE ROY","K0307","LA TAVERNE DU MI","CT JJ BOSC",
                              "SC LA PLACE","SARL AXELLE","LA POSTE","PICWICTOYS","UNIVERSOTAKU FR","BK              2","MATY VAD",
                              "ANVAL","LE NOUVEAU COUCH","RODAME","CENTRE DE PLONG","KARUTRADE","SARL 3L","AMAZON EU SARL","RR LCS BAINS",
                              "CHARLE S","INDIGO","AMAZON DIGITAL","FNACMARKETPLACE","GOOGLE Google Storage","EBB","CAVE LES VIGNERON","ARGOS",
                              "Stokke FR","ADEN   ANAIS LIMI")
                            ,collapse = "|"), negate = FALSE) ~ "Loisir",
           
           str_detect(LIBELLE,
                      paste(c("LOCAMOTO","MOTO SCOOT","CF2R")
                            ,collapse = "|"), negate = FALSE) ~ "Moto",
           
           str_detect(LIBELLE,
                      paste(c("E.LECLERC","BOULANGERIE","CARREFOURMARKET","LE PAIN DE TRANCHOIR","FOURNIL ARS","LE PAIN DE TRANC",
                              "MAISON DES VIANDE","PICARD","LECLERC","BIOCOOP","SUPER U","TOQUE CUIVRE","ARCOUR","AU ROCHER D AVON","FOURNIL DE BEGLE",
                              "LIDL","CARREFOUR","MTPK SAINT JEAN","LES MILLE PAINS","U EXPRESS","INTERMARCHE","COTE BOULANGE","INTERMARCH","GEANT",
                              "SOCCO SAINTE ROSE","YVONNE LIFESTOR3","VILLENAVE BIO","SUMUP   CABINET","FABRIQUE DE BEG",
                              "KSM BOUCHERIE CH")
                            ,collapse = "|"), negate = FALSE) ~ "Alimentation",
           
           str_detect(LIBELLE,
                      paste(c("SALON AURELIE","KLARY S COIFFU")
                            ,collapse = "|"), negate = FALSE) ~ "Coiffeur",
           str_detect(LIBELLE,
                      paste(c("NETFLIX COM","Netfixte","JULIEN GLENISSON")
                            ,collapse = "|"), negate = FALSE) ~ "Netflix",
           str_detect(LIBELLE,
                      paste(c("PEPINIERES LANN","JARDILAND")
                            ,collapse = "|"), negate = FALSE) ~ "Jardin",
           
           str_detect(LIBELLE,
                      paste(c("CASTORAMA","LEROY MERLIN","VISSERIE SERVICE","PLASTOREX")
                            ,collapse = "|"), negate = FALSE) ~ "Bricolage",
           
           str_detect(LIBELLE,
                      paste(c("IKEA","ZODIO","BOUSCAT CEN CB 4223","BUREAU VALLEE","HABITAT")
                            ,collapse = "|"), negate = FALSE) ~ "Ammeublement",
           
           
           str_detect(LIBELLE,
                      paste(c("Orange","VIR SEPA ORANGE")
                            ,collapse = "|"), negate = FALSE) ~ "Orange",
           
           str_detect(LIBELLE,
                      paste(c("MINT ENERGIE","ELECTRICITE DE FRANCE")
                            ,collapse = "|"), negate = FALSE) ~ "Electricite",
           
           str_detect(LIBELLE,
                      paste(c("SUEZ EAU")
                            ,collapse = "|"), negate = FALSE) ~ "Eau",
           
           str_detect(LIBELLE,
                      paste(c("DRFIP ILE DE FRANCE")
                            ,collapse = "|"), negate = FALSE) ~ "Insee",
           
           str_detect(LIBELLE,
                      paste(c("GMF ASSURANCES")
                            ,collapse = "|"), negate = FALSE) ~ "Assurance",
           
           str_detect(LIBELLE,
                      paste(c("Commission de Cr dit Logement","NOTARIAL VILLENAVE CHAMBER","VIR Caution pret immo")
                            ,collapse = "|"), negate = FALSE) ~ "Maison",
           
           str_detect(LIBELLE,
                      paste(c("URSSAF RHONE ALPES","Severine","REBIERE SEVERINE")
                            ,collapse = "|"), negate = FALSE) ~ "Ménage",
           
           str_detect(LIBELLE, 
                      paste(c("SURAVENIR")
                            ,collapse = "|"), negate = FALSE) ~ "Linxea_ass_vie",
           
           str_detect(LIBELLE, 
                      paste(c("GENERALI")
                            ,collapse = "|"), negate = FALSE) ~ "Nalo_ass_vie",
           
           str_detect(LIBELLE, 
                      paste(c("FEU VERT","SIMPLAUTO","NORAUTO","STATIONNEMENTS","MTPK PESSAC","PARKING ST CHARL","AEROPOT BORDEAU",
                              "AEROPORT GPE","STAT  BORDEAUX","HORODATEURS AUT","MTPK CITE MONDIA2","VIR Remboursement essence")
                            ,collapse = "|"), negate = FALSE) ~ "Voiture",
           
           str_detect(LIBELLE, 
                      paste(c("ECH PRET")
                            ,collapse = "|"), negate = FALSE) ~ "Pret_immo",
           
           str_detect(LIBELLE,
                      paste(c("SHA","HOTEL VATEL")
                            ,collapse = "|"), negate = FALSE) ~ "Hotel",
           
           str_detect(LIBELLE,
                      paste(c("Avion Gwada","AIR FRANCE")
                            ,collapse = "|"), negate = FALSE) ~ "Avion",
           
           str_detect(LIBELLE,
                      paste(c("DR DEBORDE","MGEFI","DR PARRIEUS","C P A M  BORDEAUX","PHARMACIE BARRIER","GRANDE PHARMACIE","MAISON SANTE PRO",
                              "PHARMA RIVETTE","PHARMACIE GARCIA","PHARMACIE")
                            ,collapse = "|"), negate = FALSE) ~ "Sante",
           
           str_detect(LIBELLE,
                      paste(c("Gaz de Bordeaux")
                            ,collapse = "|"), negate = FALSE) ~ "Gaz",
           
           str_detect(LIBELLE,
                      paste(c("TWINSEO")
                            ,collapse = "|"), negate = FALSE) ~ "Assurance_pret",
           
           str_detect(LIBELLE,
                      paste(c("SNCF INTERNET")
                            ,collapse = "|"), negate = FALSE) ~ "Train",
           
           str_detect(LIBELLE,
                      paste(c("SPIRICA")
                            ,collapse = "|"), negate = FALSE) ~ "Linxea_per",
            
           str_detect(LIBELLE,
                      paste(c("DIRECTION GENERALE DES FINANCE")
                            ,collapse = "|"), negate = FALSE) ~ "Impots",
           
           str_detect(LIBELLE,
                      paste(c("Virement interne depuis BOURSORAMA","VIR Alimentation PEA","VIR PERM POUR","VIR Assurance vie Marius")
                            ,collapse = "|"), negate = FALSE) ~ "Epargne",
           
           str_detect(LIBELLE,
                      paste(c("VIR SEPA M  NICOLAS KEMPF","VIR Virement de Monsieur Nicolas KEMPF","VIR Virement de Monsieur Nicolas KEM",
                              "VIR Regularisation du compte","VIR EUROPEEN EMIS LOGITEL POUR  KEMPF","VIR INST KEMPF NICOLAS",
                              "M  NICOLAS KEMPF","VIR Virement interne","VIR EUROPEEN EMIS LOGITEL POUR  INDIVISION KEMPF")
                            ,collapse = "|"), negate = FALSE) ~ "Virement_interne",
           
           str_detect(LIBELLE,
                      paste(c("VIR SEPA MELLE DEMOUGEOT LISE","VIR INST DEMOUGEOT LISE","VIR EUROPEEN EMIS LOGITEL POUR  Lise DEMOUGEOT",
                              "VIR SEPA CCCE Lise ")
                            ,collapse = "|"), negate = FALSE) ~ "Virement_Lise",
           
           str_detect(LIBELLE,
                      paste(c("VIR EUROPEEN EMIS LOGITEL POUR  M  Julien GLENISSON","VIR SEPA MME MICHAUD DANIELE","VIR SEPA M OU MME DEMOUGEOT OLIVIER",
                              "VIR Virement de Madame Joelle DEMOUG")
                            ,collapse = "|"), negate = FALSE) ~ "Virement_Exterieur",
           
           str_detect(LIBELLE,
                      paste(c("RETRAIT DAB","CHEQUE")
                            ,collapse = "|"), negate = FALSE) ~ "Aucune",

           TRUE ~ "autre"
         )
  )


# IV. Revenus----
# Revenus hors virements interne ou avec SG
rev <- compte %>% 
  filter(sens_operation == "Revenu" & !raison_operation %in% c("Virement_Exterieur","Virement_Lise","Virement_interne","Epargne","Avion") & 
           !str_detect(LIBELLE,
                      paste(c("VIR Caution pret immo")
                            ,collapse = "|"), negate = FALSE) 
           ) %>% 
  mutate(type_revenu = case_when(
    raison_operation %in% c("Insee") ~ "Paye",
    raison_operation %in% c("Sante","Netflix","Eau","Aucune") | str_detect(LIBELLE,
               paste(c("AVOIR","SEPA ORANGE","VIR SEPA OFF  NOTARIAL VILLENAVE CHAMBER")
                     ,collapse = "|"), negate = FALSE) ~ "Remboursement",
    TRUE ~ "autre"),
    type_revenu = factor(type_revenu,levels = c("Autre","Remboursement","Paye"))
  ) 

rev2 <- rev %>% 
  group_by(month(date),type_revenu) %>%
  summarise(MONTANT = sum(MONTANT * ponderation)) %>%
  rename(mois = `month(date)`)

# Graphique des revenus
g3 <- ggplot(rev2, aes(fill=type_revenu, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity")+
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Revenus par mois NK"
  )

g3

# Les principaux revenus par mois

mois.filtre <- 10

revprinc <- rev %>% 
  select(-type_operation,-sens_operation) %>% 
  filter(month(date) == mois.filtre) %>% 
  arrange(desc(MONTANT)) %>% 
  adorn_totals("row") 

tab_rev <- datatable(revprinc,rownames = FALSE,colnames = c("Date","Compte","Raison","Montant (€)","Origine","Type"))


# V. Dépenses----
depenses <- compte %>% 
  filter(sens_operation == "Depense" & !raison_operation %in% c("Virement_Exterieur","Virement_Lise","Virement_interne","Epargne","Avion")
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
  group_by(month(date),type_depense) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT * ponderation))
  ) %>%
  rename(mois = `month(date)`) %>% 
  mutate(mois = factor(as.character(mois)))



# Graphique des revenus
g4 <- ggplot(dep2, aes(fill=type_depense, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses par mois NK"
  )

g4


dep3 <- depenses %>% 
  filter(regularite_depense == "Mensuel") %>% 
  mutate(
    type_depense_reguliere = case_when(
      raison_operation %in% c("Assurance","Assurance_pret","Pret_immo","Ménage") ~ "Maison",
      raison_operation %in% c("Eau","Electricite","Gaz") ~ "Energie_Eau",
      raison_operation %in% c("Linxea_ass_vie","Linxea_per","Nalo_ass_vie") ~ "Epargne",
      raison_operation %in% c("Netflix","Orange") ~ "TIC",
      TRUE ~ "autre"
    )
  ) %>% 
  group_by(month(date),type_depense_reguliere) %>% 
  summarise(
    MONTANT = abs(sum(MONTANT*ponderation))
  ) %>%
  rename(mois = `month(date)`) %>% 
  mutate(mois = factor(as.character(mois)),
         pct = round(100*MONTANT / sum(MONTANT),2))

g5 <- ggplot(dep3, aes(fill=type_depense_reguliere, y=MONTANT, x=mois)) + 
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = pct)) +
  labs(
    x = "Mois", 
    y = "Montant (€)", 
    fill = "Type",
    title = "Dépenses régulières par mois NK"
  )

ggplotly(g5)





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








