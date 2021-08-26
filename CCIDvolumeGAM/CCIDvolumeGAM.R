
#### Load packages and libraries ####
source("CCIDPackages.R")

rm(list=ls(all=TRUE))
graphics.off()

load('CCID.Rdata')

# Create a list of daily means, temps_avg, and print this list
#CC_returns_mat = as.data.frame(CC_returns)
#CC_returns_xts = xts::as.xts(CC_returns_mat, order.by = TIME_DATE[-1])
#CC_abs_returns_mat = as.data.frame(abs(CC_returns_mat))
#CC_abs_returns_xts = xts::as.xts(CC_abs_returns_mat, order.by = TIME_DATE[-1])
CC_price_mat = as.data.frame(CC_price)
CC_price_xts = xts::as.xts(CC_price_mat, order.by = TIME_DATE)
CC_volume_mat = as.data.frame(CC_volume)
CC_volume_mat = as.data.frame(CC_volume_mat, order.by = TIME_DATE)
CC_volume_xts = xts::as.xts(CC_volume_mat, order.by = TIME_DATE)
#n = nrow(CC_returns_mat)
#result = matrix(1, ncol = 7, nrow = ncol(CC_returns_mat), dimnames = list(coins, c("rho(ret):", 
#                                                                  "rho n(ret^2):", "rho(|ret|):", "S:", "K:", "JB:", "JB p-value:")))
#rownames(result) = coins

price_daily = list()
#ret_daily = list()
#ret_daily_xts = list()
#abs_ret_daily = list()
#abs_ret_daily_xts = list()
volume_daily = list()
volume_daily_xts = list()
ep = endpoints(CC_volume_xts, on = "days")
for (i in c(1:(length(ep)-1))) {
  price_daily[[i]] = CC_price_xts[index(CC_price_xts)[(ep[i]+1):(ep[i+1])], ]
  volume_daily[[i]] = CC_volume_xts[index(CC_volume_xts)[(ep[i]+1):(ep[i+1])], ]
  volume_daily_xts[[i]] = volume_daily[[i]] 
  volume_daily[[i]] = as.data.frame(volume_daily[[i]])
  colnames(volume_daily[[i]])     = coins
  rownames(volume_daily_xts[[i]]) = index(price_daily[[i]])[-1]
 
}

#### GAM for trading volume ####
DT_vol = list()
gam_vol_cc = list()
for (coin in coins){
  
  i = which(coins==coin)
  
 
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
  n_date = unique(DT_vol[[coin]]$date)
  n_weekdays = unique(DT_vol[[coin]]$week)
  data_r_vol = as.data.frame(DT_vol[[coin]][(DT_vol[[coin]]$date %in% n_date[1:62]),])
  N = nrow(data_r_vol) # number of observations in the train set
  window = 62 # number of days in the train set
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
  
  pdfname8 = paste0("Gam_vol_weekly_daily2D", coin, ".pdf")
  pdf(file = pdfname8)
  plot(gam_4, shade = TRUE, xlab = "Time", ylab = "Trading volume", main = coin,  xaxt='n')
  axis(1, seq(1,288, 24), format(as.POSIXlt(rownames(DT_vol[[coin]])[seq(1,288, 24)]), "%H:%M"), las = 2 )
  dev.off()

  pdfname9 = paste0("Gam_3D_volume", coin, ".pdf")
  pdf(file = pdfname9)
  vis.gam(gam_4, n.grid = 50, theta = 35, phi = 32, zlab = "", 
          ticktype = "detailed",  color = "topo", main = "Trading Volume")#axes=FALSE,
  axis(1, seq(1,288, 24), format(as.POSIXlt(rownames( DT_vol[[coin]])[seq(1,288, 24)]), "%H:%M"), las = 2 )
  dev.off()
  
}
