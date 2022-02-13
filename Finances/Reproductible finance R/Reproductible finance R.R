# Reproductible finance with R

# Expérimentations de backtesting et portfolio management

library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)


# Get returns----

# I. Chargement des données
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2012-12-31", 
             to = "2017-12-31",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)

head(prices,3)

# Converting monthly prices
prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
head(prices_monthly,3)

# Calculating monthly returns
asset_returns_xts <- Return.calculate(prices_monthly, method = "log") %>% 
  na.omit()

# Visualisation

# En ligne
highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log returns") %>% 
  hc_add_series(asset_returns_xts[,symbols[1]], name = symbols[1]) %>% 
  hc_add_series(asset_returns_xts[,symbols[2]], name = symbols[2]) %>% 
  hc_add_series(asset_returns_xts[,symbols[3]], name = symbols[3]) %>% 
  hc_add_series(asset_returns_xts[,symbols[4]], name = symbols[4]) %>% 
  hc_add_series(asset_returns_xts[,symbols[5]], name = symbols[5]) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  # hc_navigator(enabled = F) %>% 
  hc_scrollbar(enabled = F) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)

# En barre
hc_hist <- hist(asset_returns_xts[,symbols[1]],
                breaks = 50,
                plot = FALSE)

hchart(hc_hist, color = "cornflowerblue") %>% 
  hc_title(text = paste(symbols[1],
                        "Log returns Distribution",
                        sep= " "
                        )) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = F)

# Fonction...

hc_hist_fun <- function(n=1,object,color){
  hc_hist <- hist(object[,symbols[n]],
                             breaks = 50,
                             plot = FALSE)
  
  hchart(hc_hist, color = "cornflowerblue") %>% 
    hc_title(text = paste(symbols[n],
                          "Log returns Distribution",
                          sep= " "
    )) %>% 
    hc_add_theme(hc_theme_flat()) %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_legend(enabled = F)
  
}

map(1:5,hc_hist_fun,asset_returns_xts,"blue")

# Building a portfolio----

# Poids des assets dans le portfolio

w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

tibble(w,symbols) %>% 
  summarise(total_weight = sum(w))

# Return of a multi-asset portfolio is equal to the sum of the weighted returns of each asset

portfolio_returns_byhand <- 
  (asset_returns_xts[,symbols[1]] * w[1]) +
  (asset_returns_xts[,symbols[2]] * w[2]) +
  (asset_returns_xts[,symbols[3]] * w[3]) +
  (asset_returns_xts[,symbols[4]] * w[4]) +
  (asset_returns_xts[,symbols[5]] * w[5]) 
names(portfolio_returns_byhand) <- "returns"


highchart(type = "stock") %>% 
  hc_title(text = "Portfolio Monthly Log returns") %>% 
  hc_add_series(portfolio_returns_byhand) %>% 
  # hc_add_series(asset_returns_xts[,symbols[2]], name = symbols[2]) %>% 
  # hc_add_series(asset_returns_xts[,symbols[3]], name = symbols[3]) %>% 
  # hc_add_series(asset_returns_xts[,symbols[4]], name = symbols[4]) %>% 
  # hc_add_series(asset_returns_xts[,symbols[5]], name = symbols[5]) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  # hc_navigator(enabled = F) %>% 
  hc_scrollbar(enabled = F) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)

# Return in xts world
portfolio_returns_xts_rebalanced_monthly <- Return.portfolio(asset_returns_xts,
                                                             weights = w,
                                                             rebalance_on = "months") %>% 
  `colnames<-`("returns")

head(portfolio_returns_xts_rebalanced_monthly,3)

highchart(type = "stock") %>% 
  hc_title(text = "Portfolio Monthly Log returns") %>% 
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns, name = "Rebalanced Monthly",
                color = "cornflowerblue"  
                ) %>% 
  # hc_add_series(asset_returns_xts[,symbols[2]], name = symbols[2]) %>% 
  # hc_add_series(asset_returns_xts[,symbols[3]], name = symbols[3]) %>% 
  # hc_add_series(asset_returns_xts[,symbols[4]], name = symbols[4]) %>% 
  # hc_add_series(asset_returns_xts[,symbols[5]], name = symbols[5]) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  # hc_navigator(enabled = F) %>% 
  hc_scrollbar(enabled = F) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)


hc_portfolio <- hist(portfolio_returns_xts_rebalanced_monthly$returns,breaks = 50,plot=FALSE)

hchart(hc_portfolio, color = "cornflowerblue",name = "Portfolio") %>% 
  hc_title(text = paste(symbols[1],
                        "Portfolio returns Distribution",
                        sep= " "
  )) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = F)
