
# clear all
# rm(list = ls())

## =-=-=-=-=-=-=-=-=-=-= ##
## LIBRARIES
## =-=-=-=-=-=-=-=-=-=-= ##

library('lubridate')
library('stringr')

library('xts')
library('Quandl')

## =-=-=-=-=-=-=-=-=-=-= ##
## FUNCTIONS
## =-=-=-=-=-=-=-=-=-=-= ##

# period.apply - for example agggregate to monthly average - for fapply
fapply_aggregate <- function(x, per = "month", f = mean, eop = F, ...) {
  if(!require("xts")) {install.packages("xts"); library("xts")}
  if(!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
  
  ep <- endpoints(x, paste0(per, "s"))
  res <- period.apply(x, ep, f, na.rm = T)
  
  index(res) <- floor_date(index(res), per)
  if(eop == T) index(res) <- ceiling_date(index(res) + 1, per) - 1
  
  return(res)
}

# auto.arima
fapply_auto.arima <- function(x, h = 12, xreg = NULL, xreg_fcst = NULL, level_fcst = c(50, 75, 90, 95), ret = "point", ...) {
  
  # auto arima point forecasts
  # IF interval is returned, other fapply functions won't work !!!
  # extends original series with forecasts and returns result
  # if external regressors are to be supplied, it must be done as follows
  #     - it must be list of vectors or matrices whose names are the same as the names of the variables in 'x'
  #       to be forecasted (with particular regressors)!
  
  if(!require("xts")) {install.packages("xts"); library("xts")}
  if(!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
  if(!require("forecast")) {install.packages("forecast"); library("forecast")}
  
  per <- round(365/as.numeric(median(diff(index(x)))), 0) # get perioditity
  st <- as.numeric(unlist(strsplit(x = as.character(start(x)), split = "-")))[1:2]
  
  ind_fcst <- NULL
  if(per == 1) ind_fcst <- years(1:h)
  if(per == 4) ind_fcst <- months(1:h * 3)
  if(per == 12) ind_fcst <- months(1:h)
  if(per == 52) ind_fcst <- weeks(1:h)
  if(per == 365) ind_fcst <- days(1:h)
  if(is.null(ind_fcst)) stop("Time series is of unknown periodicity!")
  
  # variable for results
  # point results
  ind_fcst <- c(index(x), last(index(x)) %m+% ind_fcst)
  res_point <- xts(matrix(NA, nr = nrow(x) + h, nc = ncol(x)), ind_fcst)
  colnames(res_point) <- colnames(x)
  
  # interval results
  res_int <- list()
  for(i in 1:ncol(x)) {
    res_int[[i]] <- xts(matrix(NA, nr = nrow(x) + h, nc = 2 * length(level_fcst) + 1), ind_fcst)
    colnames(res_int[[i]]) <- c("mean", paste0("L_", level_fcst, "pct"), paste0("U_", level_fcst, "pct"))
  } 
  names(res_int) <- colnames(x)
  
  # models
  mods <- list()
  
  for(i in 1:ncol(x)) {
    cat("Estimating ", i, " of ", ncol(x))
    x_ts <- x[,i] # get i-th series from the group
    x_ts <- na.contiguous(x_ts) # remove NA's
    ind <- index(x_ts) # get index of the series after removal of NA's
    
    x_ts <- ts(coredata(x_ts), start = st, freq = per)[,1]
    xr <- xreg[[colnames(x)[i]]]
    xr_f <- xreg_fcst[[colnames(x)[i]]]
    
    # xreg conditional
    if(is.null(xr)) {
      mod <- auto.arima(x = x_ts, ...)
      fcst <- forecast(object = mod, h = h, level = level_fcst)
    } else {
      mod <- auto.arima(x = x_ts, xreg = xr, ...)
      fcst <- forecast(object = mod, h = h, xreg = xr_f, level = level_fcst)
    }
    
    coredata(res_point[,i]) <- c(coredata(x[,i]), coredata(fcst$mean))
    
    coredata(res_int[[i]]) <- cbind(c(coredata(x[,i]), coredata(fcst$mean)),
                                    rbind(matrix(rep(x[,i], length(level_fcst)), nc = length(level_fcst)), fcst$lower),
                                    rbind(matrix(rep(x[,i], length(level_fcst)), nc = length(level_fcst)), fcst$upper))
    mods[[i]] <- rbind(mod$coef,
                       round(sqrt(diag(mod$var.coef)),4))
    cat(" DONE!\n")
  }
  names(mods) <- colnames(x)
  
  res <- NULL
  if(ret == "point") res <- res_point
  if(ret == "interval") {
    res <- res_int
    warning("Function 'fapply_auto.arima' returns interval forecasts. You cannot process its results with other 'fapply' functions!")
  }
  
  attr(res, "models") <- mods
  attr(res, "hist_end") <- last(index(x))
  
  return(res)  
  
}

## =-=-=-=-=-=-=-=-=-=-= ##
## ANALYSIS
## =-=-=-=-=-=-=-=-=-=-= ##

## analysis begins
dat_raw <- Quandl("CHRIS/ICE_B1", type = "xts")

dat <- fapply_aggregate(dat_raw)
plot(dat[,"Settle"])

summary(dat)








