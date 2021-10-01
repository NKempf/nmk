# Test du package tidyquant


library(recipes)
library(plyr)
library(tidyverse)
library(tidyquant)

tq_exchange_options()
tq_index_options()
tq_get_options()
nyse <- tq_exchange("NYSE")
nasdaq <- tq_exchange("NASDAQ")
sp500 <- tq_index("SP500")

glimpse(sp500)

stocks.selection <- sp500 %>% 
  inner_join(rbind(nyse,nasdaq) %>% select(symbol,last.sale.price,market.cap,ipo.year),by=c("symbol")) %>% # join datasets
  filter(ipo.year<2000&!is.na(market.cap)) %>% # filter years with ipo<2000 or ipo=NA
  arrange(desc(weight)) %>% # sort in descending order
  slice(1:10)

stocks.prices <- stocks.selection$symbol %>% 
  tq_get(get  = "stock.prices",from = "2000-01-01",to = "2017-12-31") %>%
  group_by(symbol)
index.prices <- "^GSPC" %>% 
  tq_get(get  = "stock.prices",from = "2000-01-01",to = "2017-12-31") 
stocks.prices %>% slice(1:2) # show the first two entries of each group


stocks.dividends <- stocks.selection$symbol %>% 
  tq_get(get  = "dividends",from = "2000-01-01",to = "2017-12-31") %>%
  group_by(symbol)
stocks.splits <- stocks.selection$symbol %>% 
  tq_get(get  = "splits",from = "2000-01-01",to = "2017-12-31") %>%
  group_by(symbol)

stocks.ratios <- stocks.selection$symbol %>% 
  tq_get(get  = "key.ratios",from = "2000-01-01",to = "2017-12-31") %>%
  group_by(symbol)

stocks.ratios %>% filter(section=="Growth") %>% unnest() %>% 
  filter(sub.section=="EPS %",category=="Year over Year") %>% 
  ggplot(aes(x=date,y=value,color=symbol)) + geom_line(lwd=1.1) +
  labs(title="Year over Year EPS in %", x="",y="") +
  theme_tq() + scale_color_tq()


library(Quandl)
mydata = Quandl("FRED/GDP")


Quandl.api_key("MmU3DwK8xy6VJtsAjqQ-")

Quandl.search(query = "Oil", database_code = "NSE", per_page = 3)
quandl.aapl <- c("WIKI/AAPL") %>%
  tq_get(get          = "quandl",
         from         = "2000-01-01",
         to           = "2017-12-31",
         column_index = 11, # numeric column number (e.g. 1)
         collapse     = "daily",  # can be “none”, “daily”, “weekly”, “monthly”, “quarterly”, “annual”
         transform    = "none")    # for summarizing data: “none”, “diff”, “rdiff”, “cumul”, “normalize”

av_api_key("67L4KQSVHW4SCT2L")

alpha.aapl <- c("AAPL") %>%
  tq_get(get          = "alphavantager",
         av_fun="TIME_SERIES_DAILY_ADJUSTED") # for daily data
alpha.aapl.id <- c("AAPL") %>%
  tq_get(get          = "alphavantager",
         av_fun="TIME_SERIES_INTRADAY",  # for intraday data
         interval="5min") # 5 minute intervals

library(FFdownload)
library(timetk)

tempf <- tempfile(fileext = ".RData")
inputlist <- c("F-F_Research_Data_Factors")
FFdownload(output_file = tempf, inputlist=inputlist, exclude_daily = TRUE, download = TRUE, download_only=FALSE)
load(tempf)


Fig <- exp(cumsum(FFdata$`x_F-F_Research_Data_Factors`$monthly$Temp2["1960-01-01/",c("Mkt.RF","SMB","HML")]/100))
plotFF <- plot(fig[,"Mkt.RF"],main="FF 3 Factors",major.ticks = "years",format.labels="%Y",col="black",lwd=2,lty=1,cex=0.8)
plotFF <- lines(fig[,"SMB"],on=NA,main="Size",col="darkgreen",lwd=2,lty=1,ylim=c(0,5),cex=0.8)
plotFF <- lines(fig[,"HML"],on=NA,main="Value",col="darkred",lwd=2,lty=1,ylim=c(0,15),cex=0.8)
plotFF

factors <- FFdata$`x_F-F_Research_Data_Factors`$monthly$Temp2 %>% 
  tk_tbl(rename_index="date") %>% # make tibble
  mutate(date=as.Date(date, frac=1)) %>% # make proper month-end date format
  gather(key=FFvar,value = price,-date) # gather into tidy format
factors %>% group_by(FFvar) %>% slice(1:2)

ir <- tq_get(c("TB1YR","TB3MS"), get = "economic.data") %>%
  group_by(symbol)


stocks.prices.monthly <- stocks.prices %>% 
  tq_transmute(select = c(adjusted,volume), # which column to choose
               mutate_fun = to.monthly,     # funtion: make monthly series
               indexAt = "lastof") %>%      # ‘yearmon’, ‘yearqtr’, ‘firstof’, ‘lastof’, ‘startof’, or ‘endof’
  ungroup() %>% mutate(date=as.yearmon(date)) 
stocks.returns <- stocks.prices %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,   # create monthly  returns
               period="monthly", 
               type="arithmetic") %>% 
  ungroup() %>% mutate(date=as.yearmon(date)) 
index.returns <- index.prices %>% 
  tq_transmute(select = adjusted,mutate_fun = periodReturn, 
               period="monthly", type="arithmetic") %>% 
  mutate(date=as.yearmon(date))
index.returns$sp500 <- index.returns$monthly.returns
factors.returns <- factors %>% mutate(price=price/100) %>%  # already is monthly
  mutate(date=as.yearmon(date)) %>% 
  spread(key = FFvar,value = price)
stocks.prices.monthly %>% ungroup() %>% slice(1:5) # show first 5 entries

stocks.final <- stocks.prices.monthly %>% 
  left_join(stocks.returns) %>% 
  left_join(index.returns %>% select(-monthly.returns)) %>% 
  left_join(factors.returns)

stocks.final %>% group_by(symbol) %>%
  tq_mutate(select     = adjusted, 
            mutate_fun = MACD, 
            col_rename = c("MACD", "Signal")) %>%
  select(symbol,date,adjusted,MACD,Signal) %>%
  tail() # show last part of the dataset

save(stocks.final,file="stocks.RData")


regr_fun <- function(data,formula) {
  coef(lm(formula, data = timetk::tk_tbl(data, silent = TRUE)))
}

regr_fun <- function(data) { coef(lm(fb.returns ~ xlk.returns, data = timetk::tk_tbl(data, silent = TRUE))) }


class(index.returns)









