# Test du package xts

library(xts)
library(quantstrat)

from ="2020-10-01"
to ="2021-09-28"
symbols = c("TTE") # TOTAL et Danone
currency('EUR')
initEq=10000
adjustment <- TRUE # Toujours travailler en prix ajustés

getSymbols(symbols, from=from, to=to, 
           adjust=TRUE) 

# Filtre
TTE['2021-03']

TTE['/2020-10-10']

first(TTE,'2 month')

first(last(TTE,'2 week'),'3 days')

# Plot
plot(TTE[,1],major.ticks='months',minor.ticks=FALSE,main=NULL,col=3)

# New functions
periodicity(TTE)
endpoints(TTE,on='months')

# Change periodicity
to.period(TTE,'months')
periodicity(to.period(TTE,'months'))
to.monthly(TTE)

period.apply(TTE[,4],INDEX=endpoints(TTE,on = 'month'),FUN=max)
apply.monthly(TTE[,4],FUN=max)
period.max(TTE[,4], endpoints(TTE))



