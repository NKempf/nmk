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
