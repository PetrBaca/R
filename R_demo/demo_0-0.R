# test test
# test test test
rm(list = ls())

library('lubridate')
library('stringr')

library('xts')
library('Quandl')

source('/Users/Admin/Documents/_DEV/R/FUNCTIONS/FAPPLY.R')


## analysis begins
dat_raw <- Quandl("CHRIS/ICE_B1", type = "xts")

dat <- fapply_aggregate(dat_raw)
plot(dat[,"Settle"])

summary(dat)

sessionInfo()






