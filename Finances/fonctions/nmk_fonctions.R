# NMK Fonctions----



# STANDARD DEVIATION / RISK MEASURE----------------------------------------------------------------------------
sd_glissant <- function(xts_2d,window = 24){
  rollapply(xts_2d,
            FUN = sd,
            width = window) %>% 
    # omit the 23 months for which there is no rolling 24
    # month standard deviation
    na.omit() %>% 
    `colnames<-`("rolling_sd") %>% 
    round(4)*100
}



rolling_skew_xts <- function(xts,window = 24)
  rollapply(xts,
            FUN = skewness,
            width = window) %>% 
  na.omit()



