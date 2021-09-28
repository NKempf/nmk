# Test des packages Blotter et quanstrat

library(tidyverse)
library(quantstrat)
library(data.table)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(lattice)
library(pander)

source("Fonctions/functions.R")


# Paramétrage du portefeuille----

from ="2020-10-01"
to ="2021-09-28"
symbols = c("TTE", "BN.PA") # TOTAL et Danone
currency('USD')
initEq=10000
adjustment <- TRUE # Toujours travailler en prix ajustés

getSymbols(symbols, from=from, to=to, 
           adjust=TRUE) # Danone n'est pas reconnu
stock(symbols, currency="USD", multiplier=1)


# Initialisation du portefeuille----

portefeuille <- "PEA"

rm("account.PEA",pos=.blotter)
rm("portfolio.PEA",pos=.blotter)

initPortf(portefeuille, symbol=symbols)
initAcct(portefeuille, portfolios = portefeuille,
         initEq = initEq)

# Achat d'actions----

# A savoir : Boursorama prend 0,5 % de frais jusqu'à 3 330 € par ordre (28/09/2021)

# 1er ordre total le 13/01/2021
frais <- 20*29.7*0.5/100
addTxn(Portfolio = portefeuille, 
       Symbol = "TTE", 
       TxnDate = "2020-10-13", 
       TxnQty = 20,
       TxnPrice = 29.7,
       TxnFees = -frais)

# MAJ portefeuille et cash
updatePortf(Portfolio = portefeuille)
updateAcct(name = portefeuille)
updateEndEq(portefeuille)

# Affichage
chart.Posn(portefeuille, Symbol = "TTE")

# Statistisques
out <- perTradeStats(portefeuille, "TTE")
t(out)

# Controle des dépendances entre le cash et le portefeuille
checkBlotterUpdate(port.st = portefeuille,account.st = portefeuille)


