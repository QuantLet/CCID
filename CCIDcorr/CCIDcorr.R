####5Set working directory
#setwd("~")
#### Load packages and libraries ####
source("CCIDPackages.R")

rm(list=ls(all=TRUE))
graphics.off()

load('CCID.Rdata')
#### Correlation of returns####
#Split to negative and positive
CC_returns_negative = list()
CC_returns_positive = list()
CC_returns_mat = as.data.frame(CC_returns)
CC_returns_xts = xts::as.xts(CC_returns_mat, order.by = TIME_DATE[-1])
for(coin in coins){
  CC_returns_negative[[coin]] = CC_returns_xts[CC_returns_xts[, which(coins =='BTC')] < 0,  which(coins == coin)]
  CC_returns_positive[[coin]] =  CC_returns_xts[CC_returns_xts[, which(coins =='BTC')]  > 0,  which(coins == coin)]
}

CC_ret_neg_mat = as.matrix(do.call(merge,CC_returns_negative))
colnames(CC_ret_neg_mat) = coins
CC_ret_pos_mat = as.matrix(do.call(merge,CC_returns_positive))
colnames(CC_ret_pos_mat) = coins
cor(CC_ret_neg_mat)
cor(CC_ret_pos_mat)

#Save latex table with correlations
print(x = xtable(cor(CC_ret_neg_mat), caption = "Correlation, when market goes down"), file = 'Correlation, when market goes down.tex',
      include.rownames = F, booktabs = T, floating = F)

print(x = xtable(cor(CC_ret_pos_mat), caption = "Correlation, when market goes up"), file = 'Correlation, when market goes up.tex',
      include.rownames = F, booktabs = T, floating = F)

print(x = xtable(corr.test(CC_returns_mat)$r, capxtion = "Correlation with significance"), file = 'Correlation for the entire dataset.tex',
      include.rownames = F, booktabs = T, floating = F)
print(x = xtable(corr.test(CC_returns_mat)$p, caption = "P-valur of significance"), file = 'Significance.tex',
      include.rownames = F, booktabs = T, floating = F)