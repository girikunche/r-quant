library(quantmod)
library(PerformanceAnalytics)

#Start Date
dt<-"2010-04-01"
tilldate<-"2020-03-01"

#tickers<-c("FB","AAPL","AMZN","NFLX")
tickers<-c("VEA","EEM","VWO","TIP","SHV","IGOV","GSG","DJP","VNQ","EFA")


#Assign PortfolioProces to Null
portfolioPrices<-NULL

#Get data from Yahoo Finance
for (ticker in tickers){
  
  #Combine all columens from getSymbols, add to portfolioPrices
  #Take the adjusted closing price - Column 6
  portfolioPrices<-cbind(portfolioPrices,getSymbols.yahoo(ticker,from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6])
}

#check if there is any missing data
colSums(is.na(portfolioPrices))
#if output is zero for each ticker,  that means no missing data

#calculate Returns for portfolio. Remove NA's using na.omit
#ROC funtion calculates daily changes for individual columns
portfolioReturns<-na.omit(Return.calculate(portfolioPrices))

#Basis Stats for each ticket
ReturnStats<-table.Stats(portfolioReturns)


