####5Set working directory
#setwd("~")
#### Load packages and libraries ####
source("CCIDPackages.R")

rm(list=ls(all=TRUE))
graphics.off()

load('CCID.Rdata')



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

cols = rainbow(length(coins), s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)


# 5min vola (absolute returns) plots
for (i in 1:length(coins)){

  pdfname5 =  paste0("Vola_",gsub(" ", "", coins[i], fixed = TRUE),".pdf")
  pdf(file = pdfname5)
  plot(zoo::zoo(CC_abs_returns_xts[, i]), col = cols[i], ylab = coins[i])
  dev.off()
}
dev.off()

