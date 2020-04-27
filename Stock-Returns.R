library(quantmod)
library(PerformanceAnalytics)

#Start Date
dt<-"2010-04-01"

#End Date
tilldate<-"2020-03-01"

#List of Stocks or ETF's for Analysis
tickers<-c("VEA","EEM","VWO","TIP","SHV","IGOV","GSG","DJP","VNQ","EFA")


#Assign PortfolioProces to Null
portfolioPrices<-NULL

#Get data from Yahoo Finance, Ticker by Ticker
for (ticker in tickers){
  
  #Combine all columns from getSymbols, add to portfolioPrices
  #Take the adjusted closing price - Column 6
  portfolioPrices<-cbind(portfolioPrices,getSymbols.yahoo(ticker,from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6])
}

#check if there is any missing data
colSums(is.na(portfolioPrices))
#if output is zero for each ticker,  that means no missing data

#calculate Returns for portfolio. Remove NA's using na.omit
#ROC funtion calculates daily changes for individual columns
portfolioReturns<-na.omit(Return.calculate(portfolioPrices))

#Basic Stats for each ticker
ReturnStats<-table.Stats(portfolioReturns)

#Plot chart of Correlation between securities
#Distribution of each variable is shown on the diagonal
#Top of diagonal has the value of coorelation plus the significance level as stars
chart.Correlation(portfolioReturns)




