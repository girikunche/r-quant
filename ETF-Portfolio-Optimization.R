library(quantmod)
library(PerformanceAnalytics)

#Initialize Portfolio Object
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

#Start Date
dt<-"2010-04-01"
#End Date
tilldate<-"2020-03-02"

#List of Stocks or ETF's for Analysis
tickers<-c("VEA","EEM","VWO","TIP","SHV","IGOV","GSG","DJP","VNQ","EFA")

#13 Week US Bonds Ticker to get the Risk Free Rate
treasury<-("^IRX")

#Assign Weights to each security
weights<-c("0.1","0.1","0.1","0.1","0.1","0.1","0.1","0.1","0.1","0.1")

#Initilize portfolio to NULL
portfolioPrices<-NULL

#Get data from Yahoo Finance, Ticker by Ticker

for (ticker in tickers){
  #Combine all columns from getSymbols, add to portfolioPrices
  #Take the adjusted closing price - Column 6
  portfolioPrices<-cbind(portfolioPrices,getSymbols.yahoo(ticker,from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6])
}

#Get data from Yahoo Finance, for IRX

irx<-getSymbols.yahoo(treasury,from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6]

#check if there is any missing data
colSums(is.na(portfolioPrices))

#if output is zero for each ticker,that means no missing data
#calculate Returns for portfolio. Remove NA's using na.omit
#ROC funtion calculates daily changes for individual columns

portfolioReturns<-na.omit(Return.calculate(portfolioPrices))

#Basic Stats for each ticker
ReturnStats<-table.Stats(portfolioReturns)

#Print Basis Stats
print(ReturnStats)



#Plot chart of Correlation between securities
#Distribution of each variable is shown on the diagonal
#Top of diagonal has the value of coorelation plus the significance level as stars
chart.Correlation(portfolioReturns)

#create a benchmark to compare tickers. Compare with S&P 500 - ^GSPC
benchmarkPrices<-getSymbols.yahoo('^GSPC',from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6]

#check if there is any missing data
colSums(is.na(benchmarkPrices))

#if output is zero for each ticker,  that means no missing data
#calculate Returns for S&P 500. Remove NA's using na.omit

benchmarkReturns<-na.omit(Return.calculate(benchmarkPrices))

#Calculate beta's for each security against benchmark, considering risk free rate Rf
beta_measure<-CAPM.beta(portfolioReturns,benchmarkReturns,Rf=irx/1200)
#Display Beta's
print(beta_measure)

#Print Regression Chart
#Takes A Set Of Returns And Relates Them To A Market Benchmark In A Scatterplot
chart.Regression(portfolioReturns,benchmarkReturns,main="Return of Assets vs Return of Benchmark",Rf=irx,fit="conditional",legend.loc= "bottomright")


