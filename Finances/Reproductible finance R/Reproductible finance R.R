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

# Visualizing Rolling Standard Deviation in the xts world----
port_rolling_sd_xts_hc <- 
  round(port_rolling_sd_xts, 4) * 100

highchart(type = "stock") %>% 
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc, 
                color = "cornflowerblue") %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"), 
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled= TRUE) %>% 
  hc_legend(enabled = TRUE)

# Visualizing Rolling Standard Deviation in the tidyverse----
port_rolling_sd_tq %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = rolling_sd), color = "cornflowerblue") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Standard Deviation", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

# Skewness in the xts world----
skew_xts <-  
  skewness(portfolio_returns_xts_rebalanced_monthly$returns)

skew_xts

# Skewness in the tidyverse----
skew_tidy <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  summarise(skew_builtin = skewness(returns),
            skew_byhand = 
              (sum((returns - mean(returns))^3)/length(returns))/
              ((sum((returns - mean(returns))^2)/length(returns)))^(3/2)) %>% 
  select(skew_builtin, skew_byhand)

skew_tidy %>% 
  mutate(xts = coredata(skew_xts)) %>% 
  mutate_all(funs(round(., 3))) 

# Visualizing Skewness----
portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) + 
  geom_histogram(alpha = .7, 
                 binwidth = .003, 
                 fill = "cornflowerblue", 
                 color = "cornflowerblue") +
  scale_x_continuous(breaks = 
                       pretty_breaks(n = 10))


portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red = 
           if_else(returns < (mean(returns) - 2*sd(returns)), 
                   returns, as.numeric(NA)),
         returns = 
           if_else(returns > (mean(returns) - 2*sd(returns)), 
                   returns, as.numeric(NA))) %>% 
  ggplot() + 
  geom_histogram(aes(x = hist_col_red),
                 alpha = .7, 
                 binwidth = .003, 
                 fill = "red", 
                 color = "red") +
  geom_histogram(aes(x = returns),
                 alpha = .7, 
                 binwidth = .003, 
                 fill = "cornflowerblue", 
                 color = "cornflowerblue") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("monthly returns")


portfolio_density_plot <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) +
  stat_density(geom = "line", 
               alpha = 1, 
               colour = "cornflowerblue")

portfolio_density_plot


shaded_area_data <- 
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x < 
           mean(portfolio_returns_tq_rebalanced_monthly$returns))

portfolio_density_plot_shaded <- 
  portfolio_density_plot + 
  geom_area(data = shaded_area_data, 
            aes(x = x, y = y), 
            fill="pink", 
            alpha = 0.5)

portfolio_density_plot_shaded

median <- 
  median(portfolio_returns_tq_rebalanced_monthly$returns)
mean <- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)
median_line_data <- 
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x <= median)
portfolio_density_plot_shaded +
  
  geom_segment(data = shaded_area_data, 
               aes(x = mean, 
                   y = 0, 
                   xend = mean, 
                   yend = density), 
               color = "red", 
               linetype = "dotted") +
  
  annotate(geom = "text", 
           x = mean, 
           y = 5, 
           label = "mean", 
           color = "red", 
           fontface = "plain", 
           angle = 90,
           alpha = .8, 
           vjust =  -1.75) +
  
  geom_segment(data = median_line_data, 
               aes(x = median, 
                   y = 0, 
                   xend = median, 
                   yend = density), 
               color = "black", 
               linetype = "dotted") +
  
  annotate(geom = "text", 
           x = median, 
           y = 5, 
           label = "median", 
           fontface = "plain", 
           angle = 90, 
           alpha = .8, 
           vjust =  1.75) +
  ggtitle("Density Plot Illustrating Skewness")

asset_returns_long %>% 
  # The following line group_by(asset) is not in the book! 
  # It was added after a tip from a very kind reader. I will post a full explanation of why it is needed and why it was missing to begin with. Mea culpa!
  group_by(asset) %>%
  summarize(skew_assets = skewness(returns)) %>% 
  add_row(asset = "Portfolio", 
          skew_assets = skew_tidy$skew_byhand)%>% 
  ggplot(aes(x = asset, 
             y = skew_assets, 
             colour = asset)) +
  geom_point() +
  geom_text(
    aes(x = "Portfolio", 
        y = 
          skew_tidy$skew_builtin + .04), 
    label = "Portfolio",
    color = "cornflowerblue") +
  # alternate geom_text()
  # Here's a way to label all the points
  # geom_text(aes(label = asset),
  #          nudge_y = .04)
  labs(y = "skewness")

# Rolling Skewness in the xts world----
window <- 24

rolling_skew_xts <- 
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = skewness,
            width = window) %>% 
  na.omit()


# Rolling Skewness in the tidyverse with tibbletime
skew_roll_24 <- 
  rollify(skewness, window = window)
roll_skew_tibbletime <- 
  portfolio_returns_tq_rebalanced_monthly %>%
  as_tbl_time(index = date) %>% 
  mutate(skew = skew_roll_24(returns)) %>% 
  select(-returns) %>% 
  na.omit()

# Rolling Skewness in the tidyquant world
rolling_skew_tq <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(select = returns, 
            mutate_fun = rollapply,
            width      = window,
            FUN        = skewness,
            col_rename = "tq") %>% 
  na.omit()
rolling_skew_tq %>%
  select(-returns) %>% 
  mutate(xts = coredata(rolling_skew_xts),
         tbltime = roll_skew_tibbletime$skew) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  tail(3)

# Visualizing Rolling Skewness----

highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month Skewness") %>%
  hc_add_series(rolling_skew_xts, 
                name = "Rolling skewness", 
                color = "cornflowerblue") %>%
  hc_yAxis(title = list(text = "skewness"),
           opposite = FALSE,
           max = 1, 
           min = -1)  %>% 
  hc_navigator(enabled = FALSE) %>%    
  hc_scrollbar(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)

rolling_skew_tq %>% 
  ggplot(aes(x = date, y = tq)) +
  geom_line(color = "cornflowerblue") +  
  ggtitle("Rolling  24-Month Skew ") +
  ylab(paste("Rolling ", window, " month skewness", 
             sep = " ")) + 
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = pretty_breaks(n = 8)) + 
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme_update(plot.title = element_text(hjust = 0.5))




