
#### Load packages and libraries ####
source("CCIDPackages.R")

rm(list=ls(all=TRUE))
graphics.off()

load('CCID.Rdata')

# Create a list of daily means, temps_avg, and print this list
price_daily = list()
ret_daily = list()
ret_daily_xts = list()
abs_ret_daily = list()
abs_ret_daily_xts = list()
volume_daily = list()
volume_daily_xts = list()
ep = endpoints(CC_price_xts, on = "days")
for (i in c(1:(length(ep)-1))) {
  price_daily[[i]] = CC_price_xts[index(CC_price_xts)[(ep[i]+1):(ep[i+1])], ]
  volume_daily[[i]] = CC_volume_xts[index(CC_volume_xts)[(ep[i]+1):(ep[i+1])], ]
  volume_daily_xts[[i]] = volume_daily[[i]] 
  volume_daily[[i]] = as.data.frame(volume_daily[[i]])
  ret_daily[[i]] = as.data.frame(apply(price_daily[[i]], MARGIN = 2, function(x) diff(log(as.numeric(x)))))                                                   
  ret_daily_xts[[i]] = ret_daily[[i]] 
  abs_ret_daily[[i]] = abs(ret_daily[[i]])
  abs_ret_daily_xts[[i]] = abs_ret_daily[[i]] 
  colnames(ret_daily[[i]])     = coins
  colnames(volume_daily[[i]])     = coins
  colnames(abs_ret_daily[[i]])     = coins
  rownames(ret_daily_xts[[i]])  = index(price_daily[[i]])[-1]
  rownames(abs_ret_daily_xts[[i]]) = index(price_daily[[i]])[-1]
  rownames(volume_daily_xts[[i]]) = index(price_daily[[i]])[-1]
  abs_ret_daily_xts[[i]]  =     xts(abs_ret_daily_xts[[i]], 
                                    order.by = as.POSIXlt(rownames(abs_ret_daily_xts[[i]])), 
                                    format = "%d.%m.%y %H:%M")
  ret_daily_xts[[i]]  =     xts(ret_daily_xts[[i]], 
                                order.by = as.POSIXlt(rownames(ret_daily_xts[[i]])), 
                                format = "%d.%m.%y %H:%M")
  volume_daily_xts[[i]]  =     xts(volume_daily_xts[[i]], 
                                   order.by = as.POSIXlt(rownames(volume_daily_xts[[i]])), 
                                   format = "%d.%m.%y %H:%M")
  
}

ret_daily_mean     = aaply(laply(ret_daily_xts, as.matrix), c(2, 3), mean)
abs_ret_daily_mean = aaply(laply(abs_ret_daily_xts, as.matrix), c(2, 3), mean)
colnames(ret_daily_mean)     = coins
colnames(abs_ret_daily_mean) = coins
plot(zoo::zoo(abs_ret_daily_mean))
#### GAM for volatility and trading volume ####
DT = list()
DT_vol = list()
gam_cc = list()
gam_vol_cc = list()
for (coin in coins){
  #  coin = coins[1]
  i = which(coins==coin)

  DT[[coin]]  = sapply(abs_ret_daily, "[", i)
  for (j in 1:length(abs_ret_daily_xts)) {
    DT[[coin]][[j]] = xts(DT[[coin]][[j]], order.by =  index(abs_ret_daily_xts[[j]]))
  }
  DT[[coin]] = do.call(rbind, lapply(DT[[coin]], function(x) x))
  colnames(DT[[coin]]) = "Return"
  DT[[coin]] = as.data.frame(DT[[coin]])
  DT[[coin]]$date_time = rownames(DT[[coin]] )
  DT[[coin]]$date = as.character(as.Date(DT[[coin]]$date_time))
  DT[[coin]]$time = format(as.POSIXct(DT[[coin]]$date_time), format = "%H:%M:%S")
  DT[[coin]]$time_num = as.numeric(as.factor(DT[[coin]]$time))
  DT[[coin]]$week = weekdays(as.Date(DT[[coin]]$date,'%Y-%m-%d'))
  DT[[coin]]$week_num = wday(ymd(DT[[coin]]$date)-1)
  n_date = unique(DT[[coin]]$date)
  n_weekdays = unique(DT[[coin]]$week)
  period = 24*12-1 #12 per 5 min per hour - 1return
  data_r = as.data.frame(DT[[coin]][(DT[[coin]]$date %in% n_date[1:62]),])
  N = nrow(data_r) # number of observations in the train set
  window = 62 # number of days in the train set
  
  matrix_gam = data.table(Return = data_r[[1]],
                          Daily = rep(1:period, window),
                          Weekly = data_r$week_num)
  gam_1 = gam(Return ~  s(Weekly, bs = "ps", k = 7),
              data = matrix_gam,
              family = gaussian)
  gam_cc[[coin]]$Weekly = gam_1
  gam_3 = gam(Return ~  s(Daily, bs = "cr", k = period) +
                s(Weekly, bs = "ps", k = 7),
              data = matrix_gam,
              family = gaussian)
  pdfname4 = paste0("Gam_weekly", coin, ".pdf")
  pdf(file = pdfname4)
  plot(gam_1, shade = TRUE,  xlab = "Time", ylab = "Abs. returns")
  
  dev.off()
  gam_cc[[coin]]$Daily = gam(Return ~ s(time_num, bs = "cr", k = 287),
                             data = DT[[coin]],
                             family = gaussian)
  pdfname4 = paste0("Gam_", coin, ".pdf")
  pdf(file = pdfname4)
  plot(gam_cc[[coin]]$Daily, shade = TRUE, xlab = "Time", ylab = "Abs. returns", main = coin,  xaxt='n')
  axis(1, seq(1,287, 24), format(as.POSIXlt(rownames( DT[[coin]])[seq(1,287, 24)]), "%H:%M"), las = 2 )
  
  dev.off()
  #Gam Trading volume
  DT_vol[[coin]]  = sapply(volume_daily, "[", i)
  for (j in 1:length(volume_daily_xts)) {
    DT_vol[[coin]][[j]] = xts(DT_vol[[coin]][[j]], order.by =  index(volume_daily_xts[[j]]))
  }
  DT_vol[[coin]] = do.call(rbind, lapply(DT_vol[[coin]], function(x) x))
  colnames(DT_vol[[coin]]) = "Volume"
  DT_vol[[coin]] = as.data.frame(DT_vol[[coin]])
  DT_vol[[coin]]$date_time = rownames(DT_vol[[coin]] )
  DT_vol[[coin]]$date = as.character(as.Date(DT_vol[[coin]]$date_time))
  DT_vol[[coin]]$time = format(as.POSIXct(DT_vol[[coin]]$date_time), format = "%H:%M:%S")
  DT_vol[[coin]]$time_num = as.numeric(as.factor(DT_vol[[coin]]$time))
  DT_vol[[coin]]$week = weekdays(as.Date(DT_vol[[coin]]$date,'%Y-%m-%d'))
  DT_vol[[coin]]$week_num = wday(ymd(DT_vol[[coin]]$date)-1)
  data_r_vol = as.data.frame(DT_vol[[coin]][(DT_vol[[coin]]$date %in% n_date[1:62]),])
  matrix_gam_vol = data.table(Volume = data_r_vol[[1]],
                              Daily = rep(1:288, window),
                              Weekly = data_r_vol$week_num)
  
  gam_2 = gam(Volume ~  s(Weekly, bs = "ps", k = 7),
              data = matrix_gam_vol,
              family = gaussian)
  gam_vol_cc[[coin]]$Weekly = gam_2
  pdfname5 = paste0("Gam_weekly_vol", coin, ".pdf")
  pdf(file = pdfname5)
  plot(gam_2, shade = TRUE,  xlab = "Time", ylab = "Absolute returns (vola)")
  
  dev.off()
  gam_vol_cc[[coin]]$Daily = gam(Volume ~ s(Daily, bs = "cr", k = 288),
                                 data = matrix_gam_vol,
                                 family = gaussian)
  pdfname6 = paste0("Gam_vol", coin, ".pdf")
  pdf(file = pdfname6)
  plot(gam_vol_cc[[coin]]$Daily, shade = TRUE, xlab = "Time", ylab = "Trading volume", main = coin,  xaxt='n')
  axis(1, seq(1,288, 24), format(as.POSIXlt(rownames(DT_vol[[coin]])[seq(1,288, 24)]), "%H:%M"), las = 2 )
  dev.off()
  gam_4 = gam(Volume ~  s(Daily, bs = "cr", k = 288)+
                s(Weekly, bs = "ps", k = 7),
              data = matrix_gam_vol,
              family = gaussian)
  
  
  pdfname7 = paste0("Gam_3D_abs_ret", coin, ".pdf")
  pdf(file = pdfname7)
  vis.gam(gam_3, n.grid = 50, theta = 35, phi = 32, zlab = "",
          ticktype = "detailed", color = "topo", main = "Volatility",  xaxt='n')
  axis(1, seq(1,288, 24), format(as.POSIXlt(rownames( DT_vol[[coin]])[seq(1,288, 24)]), "%H:%M"), las = 2 )
  dev.off()
  pdfname8 = paste0("Gam_3D_volume", coin, ".pdf")
  pdf(file = pdfname8)
  vis.gam(gam_4, n.grid = 50, theta = 35, phi = 32, zlab = "", 
          ticktype = "detailed",  color = "topo", main = "Trading Volume")#axes=FALSE,
  axis(1, seq(1,288, 24), format(as.POSIXlt(rownames( DT_vol[[coin]])[seq(1,288, 24)]), "%H:%M"), las = 2 )
  dev.off()
  
}
