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








