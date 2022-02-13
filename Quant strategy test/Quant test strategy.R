# Quant strategy test

# https://www.r-bloggers.com/2019/05/backtest-trading-strategies-like-a-real-quant/

# 1. Stratégie simple : croisement des moyennes mobiles normales et exponentielles des 20 derniers jours----

library(quantmod)

getSymbols("^GSPC", from = "2000-01-01")
head(GSPC)
tail(GSPC)

chartSeries(GSPC, theme = chartTheme("white"), subset = "last 10 months", show.grid = TRUE)
addSMA(20)
addEMA(20)



# 2. Strategy d'achat vente avec l'indicateur de stress des marchés

# Indicateur mesurer le stress des marchés : https://www.chicagofed.org/publications/nfci/index

# Step 1: Load libraries and data
library(quantmod)
library(PerformanceAnalytics)
## 
## Attaching package: 'PerformanceAnalytics'
## The following object is masked from 'package:graphics':
## 
##     legend
getSymbols('NFCI', src = 'FRED',  from = '2000-01-01')
## [1] "NFCI"
NFCI <- na.omit(lag(NFCI)) # we can only act on the signal after release, i.e. the next day
getSymbols("^GSPC", from = '2000-01-01')
## [1] "^GSPC"
data <- na.omit(merge(NFCI, GSPC)) # merge before (!) calculating returns)
data$GSPC <- na.omit(ROC(Cl(GSPC))) # calculate returns of closing prices
# Step 2: Create your indicator
data$sig <- ifelse(data$NFCI < 1, 1, 0)
data$sig <- na.locf(data$sig)
# Step 3: Use indicator to create equity curve
perf <- na.omit(merge(data$sig * data$GSPC, data$GSPC))
colnames(perf) <- c("Stress-based strategy", "SP500")
# Step 4: Evaluate strategy performance
table.DownsideRisk(perf)
##                               Stress-based strategy   SP500
## Semi Deviation                               0.0075  0.0087
## Gain Deviation                               0.0071  0.0085
## Loss Deviation                               0.0079  0.0095
## Downside Deviation (MAR=210%)                0.0125  0.0135
## Downside Deviation (Rf=0%)                   0.0074  0.0087
## Downside Deviation (0%)                      0.0074  0.0087
## Maximum Drawdown                             0.5243  0.6433
## Historical VaR (95%)                        -0.0173 -0.0188
## Historical ES (95%)                         -0.0250 -0.0293
## Modified VaR (95%)                          -0.0166 -0.0182
## Modified ES (95%)                           -0.0268 -0.0311
table.Stats(perf)
##                 Stress-based strategy     SP500
## Observations                4858.0000 4858.0000
## NAs                            0.0000    0.0000
## Minimum                       -0.0690   -0.0947
## Quartile 1                    -0.0042   -0.0048
## Median                         0.0003    0.0005
## Arithmetic Mean                0.0002    0.0002
## Geometric Mean                 0.0002    0.0001
## Quartile 3                     0.0053    0.0057
## Maximum                        0.0557    0.1096
## SE Mean                        0.0001    0.0002
## LCL Mean (0.95)               -0.0001   -0.0002
## UCL Mean (0.95)                0.0005    0.0005
## Variance                       0.0001    0.0001
## Stdev                          0.0103    0.0120
## Skewness                      -0.1881   -0.2144
## Kurtosis                       3.4430    8.5837
charts.PerformanceSummary(perf)





