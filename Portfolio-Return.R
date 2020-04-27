library(quantmod)
library(PerformanceAnalytics)

#Start Date
dt<-"2010-04-01"
tilldate<-"2020-03-01"

#Add tickers
#tickers<-c("FB","AAPL","AMZN","NFLX")
tickers<-c("VEA","EEM","VWO","TIP","SHV","IGOV","GSG","DJP","VNQ","EFA")

#Risk free return

#Assign Weights
weights<-c(.25,0.25,0.25,0.25)

#Assign PortfolioProces to Null
portfolioPrices<-NULL

#Get data from Yahoo Finance
for (ticker in tickers){
  
  #Combine all columens from getSymbols, add to portfolioPrices
  #Take the closing proces- Column 4
  portfolioPrices<-cbind(portfolioPrices,getSymbols.yahoo(ticker,from=dt,to=tilldate,periodicity='daily',auto.assign=FALSE)[,4])
  
}

#check if there is any missing data
colSums(is.na(portfolioPrices))
#if output is zero for each ticker,  that means no missing data

#calculate Returns for portfolio. Remove NA's using na.omit
#ROC funtion calculates daily changes for individual columns
portfolioReturns<-na.omit(ROC(portfolioPrices))


#create a benchmark to compare tickers. Compare with S&P 500 - ^GSPC
benchmarkPrices<-getSymbols.yahoo('^GSPC',from=dt,to=tilldate,periodicity='daily',auto.assign=FALSE)[,4]

#check if there is any missing data
colSums(is.na(benchmarkPrices))
#if output is zero for each ticker,  that means no missing data

#calculate Returns for S&P 500. Remove NA's using na.omit
benchmarkReturns<-na.omit(ROC(benchmarkPrices))

#Calculate portfolio values
#Aggregate all the returns and use the weights defined above

portfolioReturn<-Return.portfolio(portfolioReturns)

#Calculate Beta
#Risk free rate is 0.035 and there are 252 trading days
CAPM.beta(portfolioReturn,benchmarkReturns,0.03/2494)

#Calculate Jensen's Alpha
CAPM.jensenAlpha(portfolioReturn,benchmarkReturns,0.03/2494)

#Sharpe Ratio
SharpeRatio(portfolioReturn,0.03/2494)

#Annualized Returns
table.AnnualizedReturns(portfolioReturn)

#Calendar Returns
table.CalendarReturns(portfolioReturn)

