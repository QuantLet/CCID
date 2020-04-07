# Install and load packages
libraries = c("PortfolioAnalytics", "PerformanceAnalytics", "zoo",
              "plotly", "plyr", "RiskPortfolios", "devtools", "PMwR","Jmisc",
               "FRAPO",  "R.utils", "xts", "fastcluster", "fPortfolio",
              "dplyr", "shiny", "anytime", "ivmte", "highfrequency", "fBasics", 
              "tseries", "xtable", "grDevices", "mgcv", "tidyr","Hmisc", "feather",
              "data.table", "mgcv", "car", "ggplot2", "grid", "animation", "tidyverse", 
              "lubridate", "psych")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
