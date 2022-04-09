# Backtesting Schumann

library(tidyverse)
library (NMOF)
library (PMwR)
library(parallel)
library(quadprog)
library(zoo)
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE,
        warn = 2)
par_btest <- list(bty = "n",
                  las = 1,
                  mar = c(3, 3, 1, 1),
                  mgp = c(2, 0.5, 0),
                  tck = 0.01,
                  ps = 9)
col.market <- grey(0.3)
col.ew <- grey(0.1)








set.seed (2552551)


# I. la performance des stratégies en finance est très sensibles aux données en entrée
randomPriceSeries <-
  function ( length , vol = 0.01 , demean = FALSE ) {
    x <- cumprod (1 + rnorm ( length - 1 , sd = vol ) )
    scale1 ( c (1 , x ) , centre = demean , level = 100)
  }

x <- randomPriceSeries (250)
plot (x , type = "s" , xlab = "" , ylab = "x" , )
par_btest



do.call(par, par_btest)
x <- randomPriceSeries(250)
plot(x, type = "s", xlab = "", ylab = "x", )


m <- c(10, 30)
###
MA_crossover <- function(m, S) {
  m.fast <- MA(S, m[1], pad = NA)
  m.slow <- MA(S, m[2], pad = NA)
  
  crossover <- function() {
    if (m.fast[Time()] > m.slow[Time()])
      1
    else
      0
  }
  tail(btest(S, signal = crossover,
             b = 60, initial.cash = 100,
             convert.weights = TRUE)$wealth,
       n = 1)
}

MA_crossover(m, x)

x <- randomPriceSeries(250)
MA_crossover(m, x)

m <- c(10, 30)
initial_wealth <- 100
buyhold_profit <- final_profit <- numeric(100)

for (i in seq_along(final_profit)) {
  S <- randomPriceSeries(250)
  final_profit[i] <- MA_crossover(m, S) - initial_wealth
  buyhold_profit[i] <- S[length(S)] - S[1]
}

summary(final_profit - buyhold_profit)

do.call(par, par_btest)
plot(ecdf(final_profit - buyhold_profit),
     main = "",
     pch = NA,
     verticals = TRUE,
     xlab = paste("Final-profit difference",
                  "(crossover minus buy-and-hold)"))
abline(v = 0,
       h = ecdf(final_profit - buyhold_profit)(0))

do.call(par, par_btest)
library("MASS")
do.call(par, par_btest)
eqscplot(buyhold_profit, final_profit,
         main = "", pch = 19, cex = 0.6)
abline(v = 0, h = 0)

MA_crossover_optimized <- function(S) {
  fast <- 1:20
  slow <- 21:60
  
  crossover <- function() {
    if (m.fast[Time()] > m.slow[Time()])
      1
    else
      0
  }
  best <- -10000
  best.par <- c(0, 0)
  for (f in fast) {
    m.fast <- MA(S, f, pad = NA)
    for (s in slow) {
      m.slow <- MA(S, s, pad = NA)
      res <- btest(S, crossover, b = 60,
                   initial.cash = 100,
                   convert.weights = TRUE)
      if (tail(res$wealth,1) > best) {
        best <- tail(res$wealth,1)
        best.par <- c(f, s)
        best.wealth <- res$wealth
      }
    }
  }
  attr(best, "wealth") <- best.wealth
  attr(best, "parameters") <- best.par
  best
}

x <- randomPriceSeries(1000)
res <- MA_crossover_optimized(x)

res2 <- gridSearch(function(m, S)
  -MA_crossover(m, S),
  S = x,
  levels = list(1:20, 21:60))

x <- scale1(c(1, cumprod(1 + rnorm(999, sd = 0.01))),
            centre = TRUE, level = 100)
res <- MA_crossover_optimized(x)

do.call(par, par_btest)
X <- scale1(cbind(x, attr(res, "wealth")), level = 100)
plot(X[,1], type = "s", xlab = "", ylab = "",
     ylim = range(X, na.rm = TRUE))
lines(X[,2], type = "l", lwd = 1.5, col = grey(0.5))


x <- scale1(c(1, cumprod(1 + rnorm(999, sd = 0.01))),
            centre = TRUE, level = 100)
res <- MA_crossover_optimized(x)


do.call(par, par_btest)
X <- scale1(cbind(x, attr(res, "wealth")), level = 100)
plot(X[,1], type = "s", xlab = "", ylab = "",
     ylim = range(X, na.rm = TRUE))
lines(X[,2], type = "l", lwd = 1.5, col = grey(0.5))

buyhold_profit_opt <- final_profit_opt <- numeric(100)

for (i in seq_along(final_profit_opt)) {
  x <- randomPriceSeries(1000)
  res_gs <- gridSearch(function(m, S) -MA_crossover(m, S),
                       S = x,
                       levels = list(1:20, 21:60))
  final_profit_opt[i] <- -res_gs$minfun - initial_wealth
}

summary(final_profit_opt)


# Quelles données utiliser ?----
# at any point in time, the algorithm must only rely on data and information that actually was available

# II. Simple Backtest----

library(PMwR)

prices <- 101:110
prices
bt.results <- btest(prices, function() 1)


journal(bt.results)
position(bt.results)


trade_details <- function(bt.results, prices)
  data.frame(price    = prices,
             suggest  = bt.results$suggested.position,
             position = unname(bt.results$position),
             wealth   = bt.results$wealth,
             cash     = bt.results$cash)

trade_details(bt.results, prices)


signal <- function() {
  if (Close() <= 105)
    1
  else
    0
}

trade_details(btest(prices, signal), prices)

# Variante dplyr
btest(prices, signal) %>% 
  trade_details(prices)

<<<<<<< HEAD
# p 26
=======
# p 33
>>>>>>> b38f98fb5f6416eb6e5676d15dad3d817b2c1f4e
