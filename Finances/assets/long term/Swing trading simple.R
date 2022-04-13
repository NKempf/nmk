# Swing trading long terme--------------------------------------------------------------------------------------------

# Packages necessaires
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

source("finances/fonctions/nmk_fonctions.R")


# I. Import des données ---------------------------------------------------------------------------------------------
symbols <- c("SPY","TTE","RUI.PA")

# getSymbols(symbols, src = 'yahoo', 
#            from = "2012-12-31", 
#            to = Sys.Date(),
#            auto.assign = TRUE, 
#            warnings = FALSE)

prices <-
getSymbols(symbols, src = 'yahoo', 
           from = "2012-12-31", 
           to = Sys.Date(),
           auto.assign = TRUE, 
           warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)


highchart(type = "stock") %>% 
  hc_title(text = "Daily price Total") %>% 
  hc_add_series(Cl(TTE), name = "Total") %>% 
  hc_add_series(Cl(SPY), name = "S&P500") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_scrollbar(enabled = F) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)



# II. Analyse du profit--------------------------------------------------------------------------------------------- 

# To Monthly Returns in the xts World----
prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
head(prices_monthly,3)

asset_returns_xts <- Return.calculate(prices_monthly, method = "log") %>% 
  na.omit()

# To Monthly Returns in the tidyverse----
asset_returns_dplyr_byhand <- 
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>%
  # now remove the index because it got converted to row names
  remove_rownames() %>% 
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  spread(asset, returns) %>% 
  select(date, symbols) %>% 
  na.omit()

asset_returns_long <- 
  asset_returns_dplyr_byhand %>% 
  gather(asset, returns, -date)


# To Monthly Returns in the tidyquant World----
asset_returns_tq_builtin <- 
  prices %>%
  tk_tbl(preserve_index = TRUE, 
         rename_index = "date") %>%
  gather(asset, prices, -date) %>% 
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn, 
               period = "monthly", 
               type = "log") %>% 
  spread(asset, monthly.returns) %>% 
  select(date, symbols) %>% 
  na.omit()

# Visualisation avec XTS----
highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log returns") %>% 
  hc_add_series(asset_returns_xts[,symbols[1]], name = symbols[1]) %>% 
  hc_add_series(asset_returns_xts[,symbols[2]], name = symbols[2]) %>% 
  hc_add_series(asset_returns_xts[,symbols[3]], name = symbols[3]) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_scrollbar(enabled = F) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)

# Distribution des profils----
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

map(1:length(symbols),hc_hist_fun,asset_returns_xts,"blue")

# Profit d'un portefeuille----

w <- c(0.20,
       0.60,
       0.2)

tibble(w,symbols) %>% 
  summarise(total_weight = sum(w))


# Portfolio Returns in the xts World----
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
  hc_add_theme(hc_theme_flat()) %>% 
  hc_scrollbar(enabled = F) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)


# Portfolio Returns in the tidyverse----
portfolio_returns_dplyr_byhand <- 
  asset_returns_long %>%
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2]
                             # asset == symbols[3] ~ w[3],
                             # asset == symbols[4] ~ w[4],
                             # asset == symbols[5] ~ w[5]
                             ),
         weighted_returns = returns * weights) %>% 
  group_by(date) %>% 
  summarise(returns = sum(weighted_returns))

# Portfolio Returns in the tidyquant World----
portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")


# III. Gestion du risque--------------------------------------------------------------------------------------------- 

# Standard deviation by hands
covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix,5)
sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)

sd_matrix_algebra_percent <- 
  round(sd_matrix_algebra * 100, 2) %>% 
  `colnames<-`("standard deviation")

sd_matrix_algebra_percent

# Standard Deviation in the xts world----

portfolio_sd_xts_builtin <- 
  StdDev(asset_returns_xts, weights = w)

portfolio_sd_xts_builtin_percent <- 
  round(portfolio_sd_xts_builtin * 100, 2)

portfolio_sd_xts_builtin_percent


# Standard Devation in the tidyverse----
portfolio_sd_tidy_builtin_percent <-
  portfolio_returns_dplyr_byhand %>% 
  summarise(
    sd = sd(returns),
    sd_byhand = 
      sqrt(sum((returns - mean(returns))^2)/(nrow(.)-1))) %>% 
  mutate(dplyr = round(sd, 4) * 100,
         dplyr_byhand = round(sd_byhand, 4) * 100)  


# Standard Deviation in the tidyquant world----
portfolio_sd_tidyquant_builtin_percent <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_performance(Ra = returns, 
                 Rb = NULL, 
                 performance_fun = table.Stats) %>% 
  select(Stdev) %>% 
  mutate(tq_sd = round(Stdev, 4) * 100)


# Visualizing Standard Deviation----
portfolio_returns_dplyr_byhand %>%
  ggplot(aes(x = date, y = returns)) + 
  geom_point(color = "cornflowerblue") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  ggtitle("Scatterplot of Returns by Date") +
  theme(plot.title = element_text(hjust = 0.5))


sd_plot <- 
  sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot <- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red = 
           if_else(returns < (mean_plot - sd_plot), 
                   returns, as.numeric(NA)),
         hist_col_green = 
           if_else(returns > (mean_plot + sd_plot), 
                   returns, as.numeric(NA)),
         hist_col_blue = 
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))) %>% 
  
  ggplot(aes(x = date)) + 
  
  geom_point(aes(y = hist_col_red),
             color = "red") +
  
  geom_point(aes(y = hist_col_green),
             color = "green") +
  
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple", 
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot), 
             color = "purple", 
             linetype = "dotted") +
  labs(title = "Colored Scatter with Line", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

# Compare risk of our portfolio----
asset_returns_long %>%
  group_by(asset) %>% 
  summarize(sd = 100 * sd(returns)) %>% 
  add_row(asset = "Portfolio", 
          sd = portfolio_sd_tidy_builtin_percent$dplyr) %>% 
  ggplot(aes(x = asset, 
             y = sd, 
             colour = asset)) +
  geom_point() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(
    aes(x = "Portfolio", 
        y = 
          portfolio_sd_tidy_builtin_percent$dplyr + .2), 
    label = "Portfolio",
    color = "cornflowerblue")


# Expected Returns versus risk----
asset_returns_long %>% 
  group_by(asset) %>%
  summarise(expected_return = mean(returns),
            stand_dev = sd(returns)) %>% 
  add_row(asset = "Portfolio",
          stand_dev = 
            sd(portfolio_returns_tq_rebalanced_monthly$returns),
          expected_return = 
            mean(portfolio_returns_tq_rebalanced_monthly$returns)) %>% 
  
  ggplot(aes(x = stand_dev, 
             y = expected_return, 
             color = asset)) +
  geom_point(size = 2) +
  geom_text(
    aes(x = 
          sd(portfolio_returns_tq_rebalanced_monthly$returns) * 1.11, 
        y = 
          mean(portfolio_returns_tq_rebalanced_monthly$returns), 
        label = "Portfolio")) +
  ylab("expected return") +
  xlab("standard deviation") +
  ggtitle("Expected Monthly Returns v. Risk") +
  scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
  # The next line centers the title
  theme_update(plot.title = element_text(hjust = 0.5))


# Rolling Standard Deviation in the xts world----
window <- 24

port_rolling_sd_xts <- 
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = sd,
            width = window) %>% 
  # omit the 23 months for which there is no rolling 24
  # month standard deviation
  na.omit() %>% 
  `colnames<-`("rolling_sd")

tail(port_rolling_sd_xts, 3)


# Rolling Standard Devation with the tidyverse and tibbletime----
sd_roll_24 <- 
  rollify(sd, window = window)

port_rolling_sd_tidy_tibbletime <- 
  portfolio_returns_tq_rebalanced_monthly %>%
  as_tbl_time(index = date) %>% 
  mutate(sd = sd_roll_24(returns)) %>% 
  select(-returns) %>% 
  na.omit()

tail(port_rolling_sd_tidy_tibbletime, 3)

# Rolling Standard Deviation in the tidyquant world----
port_rolling_sd_tq <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>%
  select(date, rolling_sd) %>% 
  na.omit()
port_rolling_sd_tidy_tibbletime %>% 
  mutate(sd_tq = port_rolling_sd_tq$rolling_sd,
         sd_xts = round(port_rolling_sd_xts$rolling_sd, 4)) %>% 
  tail(3)


# Rolling sd for SPY----
spy_rolling_sd_xts <- 
  rollapply(asset_returns_xts$SPY,
            FUN = sd,
            width = window) %>% 
  # omit the 23 months for which there is no rolling 24
  # month standard deviation
  na.omit() %>% 
  `colnames<-`("rolling_sd") %>% 
  round(4)*100


# Visualizing Rolling Standard Deviation in the xts world----
port_rolling_sd_xts_hc <- 
  round(port_rolling_sd_xts, 4) * 100

highchart(type = "stock") %>% 
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc, 
                color = "cornflowerblue",name = "Portfolio") %>% 
  hc_add_series(spy_rolling_sd_xts, 
                color = "green",name = "SPY") %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"), 
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled= TRUE) %>% 
  hc_legend(enabled = TRUE)

# # Visualizing Rolling Standard Deviation in the tidyverse----
# port_rolling_sd_tq %>%
#   ggplot(aes(x = date)) + 
#   geom_line(aes(y = rolling_sd), color = "cornflowerblue") + 
#   scale_y_continuous(labels = scales::percent) +
#   scale_x_date(breaks = pretty_breaks(n = 8)) +
#   labs(title = "Rolling Standard Deviation", y = "") +
#   theme(plot.title = element_text(hjust = 0.5))







