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

library(tidyverse)
library(tidyquant)

require(quantmod)
# the easiest form of getting data is for yahoo finance where you know the appropriate symbols (Apple is "APPL")
getSymbols(Symbols = "AAPL", from="2010-01-01", to="2018-03-01", periodicity="monthly")
head(AAPL)
is.xts(AAPL)
plot(AAPL[, "AAPL.Adjusted"], main = "AAPL")
chartSeries(AAPL, TA=c(addVo(),addBBands(), addADX())) # Plot and add technical indicators
getSymbols(Symbols = c("GOOG","^GSPC"), from="2000-01-01", to="2018-03-01", periodicity="monthly") # now get Google and the S&P500
getSymbols('DTB3', src='FRED') # fred does not recognize from and to

stocks <- cbind("Apple"=AAPL[,"AAPL.Adjusted"],"Google"=GOOG[,"GOOG.Adjusted"],"SP500"=GSPC[,"GSPC.Adjusted"])
rf.daily <- DTB3["2010-01-01/2018-03-01"]
rf.monthly <- to.monthly(rf.daily)[,"rf.daily.Open"]
rf <- xts(coredata(rf.monthly),order.by =  as.Date(index(rf.monthly)))








tq_exchange_options()
tq_index_options()
tq_get_options()
nyse <- tq_exchange("NYSE")
nasdaq <- tq_exchange("NASDAQ")
sp500 <- tq_index("SP500")

stocks.selection <- sp500 %>% 
  inner_join(rbind(nyse,nasdaq) %>% select(symbol,last.sale.price,market.cap,ipo.year),by=c("symbol")) %>% # join datasets
  filter(ipo.year<2000&!is.na(market.cap)) %>% # filter years with ipo<2000 or ipo=NA
  arrange(desc(weight)) %>% # sort in descending order
  slice(1:10)

stocks <- cbind("Apple"=AAPL[,"AAPL.Adjusted"],"Google"=GOOG[,"GOOG.Adjusted"],"SP500"=GSPC[,"GSPC.Adjusted"])
rf.daily <- DTB3["2010-01-01/2018-03-01"]
rf.monthly <- to.monthly(rf.daily)[,"rf.daily.Open"]
rf <- xts(coredata(rf.monthly),order.by =  as.Date(index(rf.monthly)))

chartSeries(GSPC, TA=c(addMACD(fast=3, slow=12,signal=6,type=SMA)))
macd <- MACD(GSPC[,"GSPC.Adjusted"], nFast=3, nSlow=12,nSig=6,maType=SMA, percent = FALSE)
buy_sell_signal <- Lag(ifelse(macd$macd < macd$signal, -1, 1))
buy_sell_returns <- (ROC(GSPC[,"GSPC.Adjusted"])*buy_sell_signal)["2001-06-01/"]
portfolio <- exp(cumsum(buy_sell_returns)) # for nice plotting we assume that we invest one dollar and see hoiw much we have at the end of the observation period
plot(portfolio)

require(PerformanceAnalytics)
rets <- cbind(buy_sell_returns,ROC(GSPC[,"GSPC.Adjusted"]))
colnames(rets) <- c("investment","benchmark")
charts.PerformanceSummary(rets,colorset=rich6equal)
chart.Histogram(rets, main = "Risk Measures", methods = c("add.density", "add.normal","add.risk"),colorset=rich6equal)


tq_exchange(sp)
glimpse(SP500)
