# Test package PMwR

library(PMwR)
library(openxlsx)

# Import
journal_db <- read.xlsx("input/NMK_db.xlsx",sheet = "Journal",startRow = 2,detectDates = T)

# Test du journal
J <- journal(amount = c(1, 2, -2, 3))
J
journal()
position(J)


J <- journal(
  timestamp = journal_db$timestamp, 
  account = journal_db$account, 
  instrument = journal_db$instrument, 
  amount = journal_db$amount,
  price = journal_db$price, 
  fees = journal_db$fees, 
  total = journal_db$total, 
  note = journal_db$note)
J
class(J)

# Calcul des balances
position(J)
position(J, when = as.Date("2021-06-10"))
position(J, when = "all")

position(J, use.account = TRUE)

as.data.frame(position(J, use.account = TRUE))

# Aggregating and transforming journals
tapply(J,
       INDEX = format(J$timestamp, "%Y-%m"),
       FUN = function(x) sum(abs(x$amount)))

tapply(J,
       INDEX = list(format(J$timestamp, "%Y-%m"),
                    J$instrument),
       FUN = function(x) sum(abs(x$amount)))

# Calcul du profit / perte
pl(J)

# Return
simple_returns <- function(x)
  x[-1L]/x[-length(x)] - 1

head(DAX)

P <- head(DAX[[1]], n = 5)
P

simple_returns(P)

returns(P)
returns(cbind(P, P))
data.frame(price = P, returns = returns(P, pad = NA))

P[1] * cumprod(1 + returns(P, pad = 0))


library("zoo")
dax <- zoo(DAX[[1]], as.Date(row.names(DAX)))
rex <- zoo(REXP[[1]], as.Date(row.names(REXP)))
returns(dax, period = "month")

returns(dax, period = "ann")
returns(window(dax, end = as.Date("2014-1-31")),
        period = "ann")

prices <- c(100, 102, 104, 104, 104.5,
            2, 2.2, 2.4, 2.3,   2.5,
            3.5,   3, 3.1, 3.2,   3.1)

dim(prices) <- c(5, 3)
prices

returns(prices,
        weights = c(10, 50, 40)/100,
        rebalance.when = c(1, 4))

cf <- c(100, 100, -200)
t <- c(1, 4, 5)
x <- c(100, 101, 104, 203, 4)
div_adjust(x, t, div = -cf, backward = FALSE)



# Backtesting----

# https://gitlab.com/NMOF/NMOF2-Code/blob/master/15_Backtesting/R/Backtesting.R
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3374195

###################################################
### code chunk number 1: chapter-settings
###################################################
Sys.setenv(LANGUAGE = "en")
library("NMOF")
library("PMwR")
library("parallel")
library("quadprog")
library("zoo")
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


library("NMOF")
library("PMwR")
set.seed (2552551)

randomPriceSeries <-
  function ( length , vol = 0.01 , demean = FALSE ) {
    x <- cumprod (1 + rnorm ( length - 1 , sd = vol ) )
    scale1 ( c (1 , x ) , centre = demean , level = 100)
  }

do.call(par, par_btest)
x <- randomPriceSeries(250)
plot(x, type = "s", xlab = "", ylab = "x", )


# Simple trading strategy----
# 2 moving average m10 et m30
# Si m10 > m30 alors achat
#  Sinon vente
m <- c(10, 30)

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
# Return avec la série originale
MA_crossover(m, x)

# Return avec une autre série
x <- randomPriceSeries(250)
MA_crossover(m, x)

# On fait 100 essais
m <- c(10, 30)
initial_wealth <- 100
buyhold_profit <- final_profit <- numeric(100)

for (i in seq_along(final_profit)) {
  S <- randomPriceSeries(250)
  final_profit[i] <- MA_crossover(m, S) - initial_wealth
  buyhold_profit[i] <- S[length(S)] - S[1]
}
final_profit
buyhold_profit

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


###################################################
### code chunk number 25: optimized_crossover2
###################################################
do.call(par, par_btest)
X <- scale1(cbind(x, attr(res, "wealth")), level = 100)
plot(X[,1], type = "s", xlab = "", ylab = "",
     ylim = range(X, na.rm = TRUE))
lines(X[,2], type = "l", lwd = 1.5, col = grey(0.5))


###################################################
### code chunk number 26
###################################################
x <- scale1(c(1, cumprod(1 + rnorm(999, sd = 0.01))),
            centre = TRUE, level = 100)
res <- MA_crossover_optimized(x)


###################################################
### code chunk number 27: optimized_crossover3
###################################################
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


###################################################
### code chunk number 30: optimized-profits
###################################################
do.call(par, par_btest)
plot(ecdf(final_profit_opt - buyhold_profit_opt),
     verticals = TRUE, main = "", pch = NA,
     xlab = paste("Final-profit difference",
                  "(crossover minus buy-and-hold)"))
abline(v = 0,
       h = ecdf(final_profit_opt - buyhold_profit_opt)(0))






