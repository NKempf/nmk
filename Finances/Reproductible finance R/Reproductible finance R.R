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


# I. Get returns----

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

# To Monthly Returns using tibbletime----
# asset_returns_tbltime <- 
#   prices %>% 
#   to.monthly(indexAt = "lastof", 
#              OHLC = FALSE) %>%
#   tk_tbl(preserve_index = TRUE, 
#          rename_index = "date") %>%
#   # this is the the tibbletime function
#   tbl_time(index = "date") %>%
#   gather(asset, returns, -date) %>% 
#   group_by(asset) %>% 
#   tq_transmute(mutate_fun = periodReturn, 
#                type = "log") %>% 
#   spread(asset, monthly.returns) %>% 
#   select(date, symbols) %>% 
#   na.omit()

# Tidy Asset Returns----
asset_returns_long <- 
  asset_returns_dplyr_byhand %>% 
  gather(asset, returns, -date)

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

# II. Building a portfolio----

w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

tibble(w,symbols) %>% 
  summarise(total_weight = sum(w))

##  Portfolio Returns By-Hand----
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

# Portfolio Returns in the tidyverse----
portfolio_returns_dplyr_byhand <- 
  asset_returns_long %>%
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
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


# III. Risk management----


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
  labs(title = "Colored Scatter", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))


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

# Expected Retyrns verses risk----
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





