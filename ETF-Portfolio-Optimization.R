library(quantmod)
library(PerformanceAnalytics)

#Initialize Portfolio Object
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

#Start Date
dt<-"2010-04-01"
#End Date
tilldate<-"2020-03-01"

#List of Stocks or ETF's for Analysis
tickers<-c("VEA","EEM","VWO","TIP","SHV","IGOV","GSG","DJP","VNQ","EFA")

#Assign Weights
weights<-c("0.1","0.1","0.1","0.1","0.1","0.1","0.1","0.1","0.1","0.1")

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

xy<-Return.portfolio(portfolioReturns,weights=NULL,geometric = TRUE)

#create a benchmark to compare tickers. Compare with S&P 500 - ^GSPC
benchmarkPrices<-getSymbols.yahoo('^GSPC',from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6]

#check if there is any missing data
colSums(is.na(benchmarkPrices))
#if output is zero for each ticker,  that means no missing data
#calculate Returns for S&P 500. Remove NA's using na.omit
benchmarkReturns<-na.omit(Return.calculate(benchmarkPrices))

#Basic Stats for each ticker
ReturnStats<-table.Stats(portfolioReturns)


#Calculate Beta
#Risk free rate is 0.035 and there are 118 trading months
CAPM.beta(xy,benchmarkReturns,0.01)

#Calculate Jensen's Alpha
CAPM.jensenAlpha(portfolioReturns,benchmarkReturns,0.01)

#Sharpe Ratio
SharpeRatio(portfolioReturns,0.01)

#Annualized Returns
table.AnnualizedReturns(portfolioReturns)

#Calendar Returns
table.CalendarReturns(portfolioReturns)











#Assign names of the securities to fund.names
fund.names<-tickers

#Create a portfolio Object
pspec<-portfolio.spec(assets = fund.names)
print.default(pspec)

#Add constraint to the Portfolio Object

#Use add.contraint
#Sum of Weight Constraint

pspec<-add.constraint(portfolio=pspec, type="weight_sum",min_sum=1,max_sum=1)

#Add Box constraint
pspec<-add.constraint(portfolio=pspec, type="box",min=0.0,max=1.0)

#Add objective
#Add portfolio return objective
pspec <-add.objective(portfolio=pspec,type='return',name='mean')
pspec<- add.objective(portfolio=pspec,type="risk",name="var")



#Solver

opt_maxret <- optimize.portfolio(R=portfolioReturns, portfolio=pspec,optimize_method="ROI",trace=TRUE)
print(opt_maxret)

opt_minvar <- optimize.portfolio(R=portfolioReturns, portfolio=pspec,optimize_method="ROI",trace=TRUE)
print(opt_minvar)


