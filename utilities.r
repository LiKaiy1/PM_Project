library(quantmod)
library(WindR)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ggfortify)
library(ggplot2)
library(rvest)
library(dplyr)
library(roll)
library(timetk)
theme_set(theme_classic())

#get S&P500 Companies
scrap_index_components <- function(index) {
  if (index == 'GSPC') {
    sp_500 <- data.frame(read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
                           html_node("table.wikitable") %>%
                           html_table())
    sp_500 <- data.frame(
      Symbol = sp_500$Symbol,
      Security = sp_500$Security,
      Sector = sp_500$GICS.Sector
    )
    return(sp_500)
  }
  else{
    csi300 <- read_html("https://en.wikipedia.org/wiki/CSI_300_Index") %>%
      html_nodes("table.wikitable") 
    csi300 <- html_table(csi300[2])[[1]]
    return(csi300)
  }
}


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

# Load csv to xts
load_csv_xts<-function(path){
  df <- read.csv(path)
  len <- length(colnames(df))
  df <- df[,2:len]
  df$date = as.Date(df$date)
  df <- xts::xts(df[,-1],order.by = df$date)
}


#Collect Monthly Returns
collect_monthly <- function(tickers,start_date){
  adj_close_data <- c()
  return_data <- c()
  for (ticker in tickers) {
    ticker_close <- Ad(getSymbols(ticker,src = "yahoo",auto.assign = FALSE,from=start_date))
    returns <- monthlyReturn(ticker_close,type = "log")
    return_data <- cbind(return_data,returns)
  }
  return_data <- return_data[-1]
  colnames(return_data)<-tickers
  return(return_data)
}

#Rolling Regression
rolling_regress <- function(macro_research,i,to_local = F) {
  x <- matrix(macro_research[,14:16],ncol = 3,nrow = nrow(macro_research))
  y <- matrix(macro_research[,i],ncol = 1,nrow = nrow(macro_research))
  # rolling regressions with exponential decay
  weights <- 0.9 ^ (100:1)
  result <- roll_lm(x, y, 100, weights)
  rolling_coffecients <- data.frame(date=index(macro_research),result$coefficients)
  rolling_coffecients <- xts(rolling_coffecients[,2:5],order.by = rolling_coffecients[,1])
  rolling_coffecients <- na.omit(rolling_coffecients)
  colnames(rolling_coffecients) <- c("Intercept","OAS","TED","TreasurySpread")
  # r_squares <- data.frame(date=index(macro_research),result$r.squared)
  # r_squares <- xts(r_squares[,2],order.by = r_squares[,1])
  # r_squares <- na.omit(r_squares)
  factor_plot <- autoplot(rolling_coffecients,facets = F)+ggtitle(colnames(macro_research)[i])
  if (to_local) {
    ggsave(paste("plot/",colnames(macro_research)[i],".jpg",sep = ''),factor_plot,height = 7,width = 12)
  }
  return(rolling_coffecients)
}
#Rolling Regression R square
rolling_regress_r_square <- function(macro_research,i,to_local = F) {
  x <- matrix(macro_research[,14:16],ncol = 3,nrow = nrow(macro_research))
  y <- matrix(macro_research[,i],ncol = 1,nrow = nrow(macro_research))
  # rolling regressions with exponential decay
  weights <- 0.9 ^ (100:1)
  result <- roll_lm(x, y, 100, weights)
  # rolling_coffecients <- data.frame(date=index(macro_research),result$coefficients)
  # rolling_coffecients <- xts(rolling_coffecients[,2:5],order.by = rolling_coffecients[,1])
  # rolling_coffecients <- na.omit(rolling_coffecients)
  # colnames(rolling_coffecients) <- c("Intercept","OAS","TED","TreasurySpread")
  r_squares <- data.frame(date=index(macro_research),result$r.squared)
  r_squares <- xts(r_squares[,2],order.by = r_squares[,1])
  r_squares <- na.omit(r_squares)
  factor_plot <- autoplot(r_squares,facets = F)+ggtitle(colnames(macro_research)[i])
  if (to_local) {
    ggsave(paste("plot/",colnames(macro_research)[i],"_r_square.jpg",sep = ''),factor_plot,height = 7,width = 12)
  }
  return(r_squares)
}
#Generate random portfolios
generate_rondom_portfolios <- function(mean_returns,cov_mat) {
  n <- length(colnames(cov_mat))
  wts <- runif(n)
  wts <- wts/sum(wts)
  port_returns <- (sum(wts*mean_returns)+1)^252-1
  port_risk <- sqrt(t(wts)%*%(cov_mat%*%wts))
  Sharpe <- (port_returns/port_risk)[1]
  return(list(wts,port_risk,port_returns,Sharpe))
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

# Create cumulative return function
cum.rtn <- function(clean.xts.obj, g = TRUE){
  x <- clean.xts.obj
  if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
  return(y)
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

#Generate the view
generate_view <- function(research_data,to_local = F) {
  view <- c()
  for (i in 1:length(colnames(research_data))) {
    print(colnames(research_data)[i])
    if (colnames(research_data)[i] == "TLT" | colnames(research_data)[i] == "LQD") {
      # print(quantile(research_data[,i],0.75))
      view <- c(view,as.numeric(quantile(research_data[,i],0.75)))
    }
    else{
      # print(quantile(research_data[,i],0.5)) 
      view <- c(view,as.numeric(quantile(research_data[,i],0.25)))
    }
  }
  view <- data.frame(Assets = colnames(research_data),view = view)
  if (to_local) {
    write.csv(view,"data/view.csv")
  }
  return(view)
}



gg.charts.PerformanceSummary <- function(rtn.obj, geometric = TRUE, main = "", plot = TRUE)
{
  
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj
  }
  
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
    y
  }
  
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
    y
  }
  
  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj,geometric)
  {
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(x,geometric)
    tmp$rtn <- x
    tmp$dd <- dd.xts(x,geometric)
    colnames(tmp) <- c("Index","Return","Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if(main == ""){
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }
    
    gg.xts <- ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
      facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
      geom_line(data = subset(df, variable == "Index")) +
      geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
      geom_line(data = subset(df, variable == "Drawdown")) +
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      ggtitle(title.string) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      ylab("") +
      xlab("")
    
  }
  else
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main == ""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
    } else {
      title.string <- main
    }
    
    if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
    
    gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +
      
      # panel layout
      facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin =
                   , labeller = label_value) + # label_value is default
      
      # display points for Index and Drawdown, but not for Return
      # geom_point(data = subset(df, variable == c("Index","Drawdown"))
      #            , aes(colour = factor(asset)), size = 1.2, show.legend = TRUE) +
      
      # manually select shape of geom_point
      # scale_shape_manual(values = c(1,2,3)) +
      
      # line colours for the Index
      geom_line(data = subset(df, variable == "Index"), aes(colour = factor(asset)), show.legend = T) +
      
      # bar colours for the Return
      geom_bar(data = subset(df,variable == "Return"), stat = "identity"
               , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +
      
      # line colours for the Drawdown
      geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # horizontal ticks
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      
      # main y-axis title
      ylab("") +
      
      # main x-axis title
      xlab("") +
      
      # main chart title
      ggtitle(title.string)
    
  }
  
  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot == TRUE){
    plot(gg.xts)
  } else {}
  
}


plot_backtest <- function(return_set,to_local){
  # advanced charts.PerforanceSummary based on ggplot
  
  
  # display chart
  # print(gg.charts.PerformanceSummary(return_set, geometric = TRUE))
  
  if (to_local) {
    ggsave("plot/backtest_result.jpg",height = 8,width = 12)
  }
  
  
}


