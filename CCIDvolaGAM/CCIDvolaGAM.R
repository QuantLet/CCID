
#### Load packages and libraries ####
source("CCIDPackages.R")

rm(list=ls(all=TRUE))
graphics.off()

load('CCID.Rdata')

# Create a list of daily means, temps_avg, and print this list
CC_returns_mat = as.data.frame(CC_returns)
CC_returns_xts = xts::as.xts(CC_returns_mat, order.by = TIME_DATE[-1])
CC_abs_returns_mat = as.data.frame(abs(CC_returns_mat))
CC_abs_returns_xts = xts::as.xts(CC_abs_returns_mat, order.by = TIME_DATE[-1])
CC_price_mat = as.data.frame(CC_price)
CC_price_xts = xts::as.xts(CC_price_mat, order.by = TIME_DATE)
n = nrow(CC_returns_mat)
result = matrix(1, ncol = 7, nrow = ncol(CC_returns_mat), dimnames = list(coins, c("rho(ret):", 
                                                                                   "rho n(ret^2):", "rho(|ret|):", "S:", "K:", "JB:", "JB p-value:")))
rownames(result) = coins

price_daily = list()
ret_daily = list()
ret_daily_xts = list()
abs_ret_daily = list()
abs_ret_daily_xts = list()
ep = endpoints(CC_price_xts, on = "days")
for (i in c(1:(length(ep)-1))) {
  price_daily[[i]] = CC_price_xts[index(CC_price_xts)[(ep[i]+1):(ep[i+1])], ]
  ret_daily[[i]] = as.data.frame(apply(price_daily[[i]], MARGIN = 2, function(x) diff(log(as.numeric(x)))))                                                   
  ret_daily_xts[[i]] = ret_daily[[i]] 
  abs_ret_daily[[i]] = abs(ret_daily[[i]])
  abs_ret_daily_xts[[i]] = abs_ret_daily[[i]] 
  colnames(ret_daily[[i]])     = coins
  colnames(abs_ret_daily[[i]])     = coins
  rownames(ret_daily_xts[[i]])  = index(price_daily[[i]])[-1]
  rownames(abs_ret_daily_xts[[i]]) = index(price_daily[[i]])[-1]
  abs_ret_daily_xts[[i]]  =     xts(abs_ret_daily_xts[[i]], 
                                    order.by = as.POSIXlt(rownames(abs_ret_daily_xts[[i]])), 
                                    format = "%d.%m.%y %H:%M")
  ret_daily_xts[[i]]  =     xts(ret_daily_xts[[i]], 
                                order.by = as.POSIXlt(rownames(ret_daily_xts[[i]])), 
                                format = "%d.%m.%y %H:%M")
  
}

ret_daily_mean     = aaply(laply(ret_daily_xts, as.matrix), c(2, 3), mean)
abs_ret_daily_mean = aaply(laply(abs_ret_daily_xts, as.matrix), c(2, 3), mean)
colnames(ret_daily_mean)     = coins
colnames(abs_ret_daily_mean) = coins
plot(zoo::zoo(abs_ret_daily_mean))#, ylim = c(0, 0.0046))
#### GAM for volatility ####
DT = list()
gam_cc = list()


for (coin in coins){
  #  coin = coins[1]
  i = which(coins==coin)

  DT[[coin]]  = sapply(abs_ret_daily, "[", i)
  for (j in 1:length(abs_ret_daily_xts)) {
    DT[[coin]][[j]] = xts(DT[[coin]][[j]], order.by =  index(abs_ret_daily_xts[[j]]))
  }
  DT[[coin]] = do.call(rbind, lapply(DT[[coin]], function(x) x))
  colnames(DT[[coin]]) = "Abs_return"
  DT[[coin]] = as.data.frame(DT[[coin]])
  DT[[coin]]$date_time = rownames(DT[[coin]] )
  DT[[coin]]$date = as.character(as.Date(DT[[coin]]$date_time))
  DT[[coin]]$time = format(as.POSIXct(DT[[coin]]$date_time), format = "%H:%M:%S")
  DT[[coin]]$time_num = as.numeric(as.factor(DT[[coin]]$time))
  DT[[coin]]$week = weekdays(as.Date(DT[[coin]]$date,'%Y-%m-%d'))
  DT[[coin]]$week_num = wday(ymd(DT[[coin]]$date)-1)
  n_date = unique(DT[[coin]]$date)
  n_weekdays = unique(DT[[coin]]$week)
  period = 24*12-1 #12 per 5 min per hour - 1return = 287
  data_r = as.data.frame(DT[[coin]][(DT[[coin]]$date %in% n_date[1:62]),])
  N = nrow(data_r) # number of observations in the train set
  window = 62 # number of days in the train set
  
  matrix_gam = data.table(Abs_return = data_r[[1]],
                          Daily = rep(1:period, window),
                          Weekly = data_r$week_num)
  gam_1 = gam(Abs_return ~  s(Weekly, bs = "ps", k = 7),
              data = matrix_gam,
              family = gaussian)
  gam_cc[[coin]]$Weekly = gam_1
  gam_3 = gam(Abs_return ~  s(Daily, bs = "cr", k = period) +
                s(Weekly, bs = "ps", k = 7),
              data = matrix_gam,
              family = gaussian)
  pdfname4 = paste0("Gam_weekly", coin, ".pdf")
  pdf(file = pdfname4)
  plot(gam_1, shade = TRUE,  xlab = "Time", ylab = "Abs. returns (vola)")#OK
  
  dev.off()
  pdfname5 = paste0("Gam_weekly_daily2D", coin, ".pdf")
  pdf(file = pdfname5)
  plot(gam_3, shade = TRUE,  xlab = "Time", ylab = "Abs. returns (vola)")#OK
  dev.off()
  
  gam_cc[[coin]]$Daily = gam(Abs_return ~ s(time_num, bs = "cr", k = 287),
                             data = DT[[coin]],
                             family = gaussian)
  pdfname4 = paste0("Gam_", coin, ".pdf")
  pdf(file = pdfname4)
  plot(gam_cc[[coin]]$Daily, shade = TRUE, xlab = "Time", ylab = "Abs. returns (vola)", main = coin,  xaxt='n')# OK
  axis(1, seq(1,287, 24), format(as.POSIXlt(rownames( DT[[coin]])[seq(1,287, 24)]), "%H:%M"), las = 2 )
  
  dev.off()
  
  #3D plots
  pdfname7 = paste0("Gam_3D_abs_ret", coin, ".pdf")
  pdf(file = pdfname7)
  vis.gam(gam_3, n.grid = 50, theta = 35, phi = 32, zlab = "",
          ticktype = "detailed", color = "topo", main = "Volatility",  xaxt='n')
  axis(1, seq(1,288, 24), format(as.POSIXlt(rownames( DT[[coin]])[seq(1,288, 24)]), "%H:%M"), las = 2 )
  dev.off()
}
