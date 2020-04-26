library(quantmod)
library(ggplot2)
library(reshape2)
library(ggfortify)
library(gtable)
library(grid)
library(gridExtra)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(xts)
library(timetk)
theme_set(theme_classic())

#Collect Data from Yahoo
collect_data <- function(tickers,start_date){
  adj_close_data <- c()
  return_data <- c()
  for (ticker in tickers) {
    ticker_close <- Ad(getSymbols(ticker,src = "yahoo",auto.assign = FALSE,from=start_date))
    returns <- dailyReturn(ticker_close,type = "log")
    if (ticker == "VIXY") {
      ticker_close <- ticker_close/100
    }
    adj_close_data <- cbind(adj_close_data,ticker_close)
    return_data <- cbind(return_data,returns)
  }
  adj_close_data <- as.data.frame(na.omit(adj_close_data))
  return_data <- return_data[-1]
  colnames(adj_close_data)<-tickers
  colnames(return_data)<-tickers
  result <- list("close_data" = adj_close_data,"return_data" = return_data)
  return(result)
}

#Plot Price Path for all tickers
plot_ad_close <- function(close_data,to_local=FALSE) {
  adj_close_plot <- autoplot(as.xts(close_data),facets = FALSE)
  adj_close_plot <- adj_close_plot+xlab("Date")+ylab("Value")+ggtitle("Adjust Close Price")
  if (to_local) {
    ggsave("plots/adj_close_plots.jpg",plot = adj_close_plot,width =30.86 ,height =18.76 )
  }
  return(adj_close_plot)
}

#Plot Return Paths for all tickers
plot_return_paths <- function(return_data,to_local=FALSE) {
  return_plot <- autoplot(as.xts(return_data),facets = FALSE)
  return_plot <- return_plot+xlab("Date")+ylab("Value")+ggtitle("Returns")
  if (to_local) {
    ggsave("plots/return_plots.jpg",plot = return_plot,width =30.86 ,height =18.76 )
  }
  return(return_plot)
}

#Plot And Save Distributions
plot_return_distributions <- function(return_data,column,to_local = F){
  ticker <- colnames(return_data)[column]
  distribution_plot <- ggplot(return_data,aes(x=return_data[,column]))+
    geom_vline(aes(xintercept=mean(return_data[,column])),colour = "red",linetype="dashed")+
    geom_histogram(binwidth = 0.00125,colour = "blue")+
    geom_density(alpha=0.05, fill="#FF6666") +
    labs(title = ticker)
  if (to_local) {
    ggsave(paste("plots/",ticker,"_distribution.jpg",sep = ''),distribution_plot)
  }
  return(distribution_plot)
}

Calculate_VaR <- function(return_data,i,method){
  Value_at_Risk <- round(VaR(return_data[,i],method,p=0.95)[1],digits = 4)
  # VaR(return_data[,i],methods = methods)
  paste("Methods:",method,",Asset:",colnames(return_data)[i],",Daily VaR:",Value_at_Risk)
}

#Construct Portfolio
construct_portfolio <- function(return_data,short=TRUE,risk_measure){
  portf <- portfolio.spec(colnames(return_data))
  portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
  portf <- add.objective(portf, type="risk", name=risk_measure)
  if (short) {
    portf <- add.constraint(portf, type="box", min=-0.499, max=1.501)
  }
  else{
    portf <- add.constraint(portf, type="box", min=-0, max=1)
  }
  return(portf)
}

#Load csv to xts
load_csv_xts <- function(path){
  df <- read.csv(path)
  len <- length(colnames(df))
  df <- df[,2:len]
  df$date = as.Date(df$date)
  df <- xts::xts(df[,-1], order.by = df$date)
}









# 
# relative_performance_to_SPY <- function(return_data = return_data,i) {
#   chart.RelativePerformance(Ra = return_data[,1],Rb = return_data[,i])
# }
# 
# draw_downs <- function(return_data = return_data,i){chart.Drawdown(return_data[,i])}
#   




















