####5Set working directory
#setwd("~")
#### Load packages and libraries ####
source("CCIDPackages.R")

rm(list=ls(all=TRUE))
graphics.off()

load('CCID.Rdata')


#### Descriptive statistics and JB statistics of 5 min returns ####

msg1 = "This program calculates the first order auto correlation of returns, squared returns and absolute returns and skewness, kurtosis and the Bera Jarque statistic for intraday CC returns"
print(msg1)

CC_returns_mat = as.data.frame(CC_returns)
CC_returns_xts = xts::as.xts(CC_returns_mat, order.by = TIME_DATE[-1])
CC_abs_returns_mat = as.data.frame(abs(CC_returns_mat))
CC_abs_returns_xts = xts::as.xts(CC_abs_returns_mat, order.by = TIME_DATE[-1])
CC_price_mat = as.data.frame(CC_price)
CC_price_xts = xts::as.xts(CC_price_mat, order.by = TIME_DATE)
CC_volume_mat = as.data.frame(CC_volume)
CC_volume_mat = as.data.frame(CC_volume_mat, order.by = TIME_DATE)
CC_volume_xts = xts::as.xts(CC_volume_mat, order.by = TIME_DATE)
n = nrow(CC_returns_mat)
result = matrix(1, ncol = 7, nrow = ncol(CC_returns_mat), dimnames = list(coins, c("rho(ret):", 
                                                                                   "rho n(ret^2):", "rho(|ret|):", "S:", "K:", "JB:", "JB p-value:")))
rownames(result) = coins
for (i in 1:ncol(CC_returns_mat)) {
  # start calculation
  ret1 = CC_returns_mat[, i]
  
  skew = skewness(ret1)
  kurt = kurtosis(ret1)
  ret1 = as.matrix(ret1)
  ret2 = matrix(c(ret1^2))
  ret3 = matrix(c(abs(ret1)))
  n = nrow(ret1)
  rho1 = cor(ret1[2:n], ret1[1:(n - 1)])
  rho2 = cor(ret2[2:n], ret2[1:(n - 1)])
  rho3 = cor(ret3[2:n], ret3[1:(n - 1)])
  jb = jarque.bera.test(ret1)
  
  # end calculation
  result[i, 1] = rho1
  result[i, 2] = rho2
  result[i, 3] = rho3
  result[i, 4] = skew[1]
  result[i, 5] = kurt[1]
  result[i, 6] = jb[[1]]
  result[i, 7] = jb[[3]]
}

# Save Output Descriptive statistics


print(x = xtable(result, caption = "ACF and JB statistics, 5min intervals"), file = 'ACF_table_5min.tex',
      include.rownames = F, booktabs = T, floating = F)
cols = rainbow(length(coins), s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
