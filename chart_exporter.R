source("helper.R")
library(PerformanceAnalytics)
chart.Correlation(return_data)
for (i in 2:num_of_tickers) {
  print(chart.RollingCorrelation(Ra = return_data[,1],Rb = return_data[,i]) )
}