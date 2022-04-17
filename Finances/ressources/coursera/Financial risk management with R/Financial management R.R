# RNGkind(sample.kind=”Rounding”)
library(magrittr)
library(tidyverse)
library(moments)
library(MASS)
library(quantmod)
library(metRology)
library(rugarch) 

# Exercice 2--------------------------------------------------------------------

wilsh<-getSymbols("WILL5000IND",src="FRED",auto.assign=FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
head(wilsh,3)
tail(wilsh,3) 


load("finances/ressources/coursera/Financial risk management with R/qjzU8SH6Ro-81PEh-maPtg_0c46ff6057ab4ab2a4482e18632530f1_FRED_gold.rda")

head(gold)
tail(gold)

# Exercice 3--------------------------------------------------------------------
logret <- diff(log(wilsh))[-1]
head(logret,3) 
round(head(logret,3),6) 

ret <- exp(logret) - 1
round(head(ret,3),6) 

# les résultats du log return et de discrete return sont proches

logret <- diff(log(gold))[-1]
head(logret,3) 
round(head(logret,3),6) 

ret <- exp(logret) - 1
round(head(ret,3),6) 
round(tail(ret,3),6) 

# Exercice 4--------------------------------------------------------------------
logret.w <- apply.weekly(logret,sum)
logret.m <- apply.monthly(logret,sum)
logret.q <- apply.quarterly(logret,sum)
logret.y <- apply.yearly(logret,sum)

ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1 

logret.w %>% 
  na.omit() %>% 
  round(6) %>% 
  head(3)
  
# Quiz 1------------------------------------------------------------------------

usjp <- getSymbols("DEXJPUS",src="FRED",auto.assign=FALSE) %>% 
na.omit()
usjp <- usjp["1979-12-31/2017-12-31"]
usjp$DEXJPUS <- 1 / usjp$DEXJPUS
# names(usjp) <- "TR"
head(usjp,3)
tail(usjp,3) 

logret <- diff(log(usjp)) %>% na.omit() 
head(logret,3)

logret.m <- apply.monthly(logret,sum)
ret.m <- exp(logret.m)-1
ret.m %>% 
  round(6) %>% 
  head(3)

logret.q <- apply.quarterly(logret,sum)
logret.q %>% 
  round(6) %>% 
  tail(3)

logret.y <- apply.yearly(logret,sum)
ret.y <- exp(logret.y)-1
ret.y %>% 
  round(6) %>% 
  tail(3)

# Exercice 5--------------------------------------------------------------------
logret <- diff(log(gold)) %>% na.omit() 
options("scipen"=100, "digits"=4)
round( mean(logret), 8)
round( sd(logret),6 ) 


# Exercice 6--------------------------------------------------------------------
mu <- mean(logret)
sig <- sd(logret) 

var <- qnorm(0.05,mu,sig) 

HFvar <- 1000 * ( exp(var)-1 ) # in millions of dollars

var %>% 
  round(6)

HFvar %>% 
  round(1)

# Exercice 7--------------------------------------------------------------------

es <- mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05 
HFvar <- 1000 * ( exp(es)-1 )

es %>% 
  round(6)

HFvar %>% 
  round(1)


# Exercice 8--------------------------------------------------------------------

# Simulation method 1:
set.seed(123789)
rvec <- rnorm(100000,mu,sig) 
VaR <- quantile(rvec,0.05) 
ES <- mean(rvec[rvec<VaR]) 

VaR %>% 
  round(6)
ES %>% 
  round(6)

# Simulation method 2: 
set.seed(123789)
rvec <- sample(as.vector(logret),100000,replace=TRUE)

VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])

VaR %>% 
  round(6)
ES %>% 
  round(6)

# Exercice 9--------------------------------------------------------------------
# Skewness
rvec <- as.vector(logret)
round(skewness(rvec),2) # negative asymétrie => left-tail

logret.wilsh <- diff(log(wilsh))[-1]
rvec.wilsh <- as.vector(logret.wilsh)
round(skewness(rvec.wilsh),2) 


# Kurtosis
round(kurtosis(rvec),2) # courbe des log return => leptokurtique

# Jarque-Bera test of normality
jarque.test(rvec) # Non-normal

# Exercice 10-------------------------------------------------------------------
# Scaled student t distribution
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6) 

t.fit$estimate[2] %>% round(6)


# Simulated scaled t distribution
alpha <- 0.05
set.seed(123789) 
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6) 


# Exercice 11-------------------------------------------------------------------
# Simulation Method 1

alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 

# Simulation Method 2
alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])

# Simulation Method 3
alpha <- 0.05
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 

# Exercice 12-------------------------------------------------------------------
acf(logret) 
acf( abs(logret) ) 

# GARCH model with scaled student error
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])


round(fit.garch@fit$coef,6)

save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c( "logret", "s", "z" ) 

acf(save1$z)
acf(abs(save1$z))

# Exercice 13-------------------------------------------------------------------
set.seed(123789) #set seed value
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp") 

rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])

set.seed(123789) #set seed value
gold2 <- gold["1979-12-31/2008-09-15"]
logret <- diff(log(gold2))[-1]
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp") 

rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])


set.seed(123789) #set seed value
gold2 <- gold["1979-12-31/1987-10-19"]
logret <- diff(log(gold2))[-1]
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp") 

rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])



