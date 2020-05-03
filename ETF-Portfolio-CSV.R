#Script to Read Security data from CSV file and calculate Beta
library(xts)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

#Initialize Portfolio Object
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

#Start Date
dt<-"2010-04-01"
#End Date
tilldate<-"2020-04-01"

#13 Week US Bonds Ticker to get the Risk Free Rate
treasury<-("^IRX")

read_prices<-read.csv("ETF.csv",header=TRUE,stringsAsFactors = FALSE)
#Correct Columns
names(read_prices)<-c("Date","VEA","EEM","VWO","TIP","SHV","IGOV","GSG","DJP","VNQ","EFA")

#Create a single xts object using one column and the index
#Merge with the other columns

sec_xts = xts(read_prices$VEA, order.by=as.Date(read_prices$Date, format="%m/%d/%Y"))
sec_prices<-merge(sec_xts,read_prices$EEM,read_prices$VWO,read_prices$TIP,read_prices$SHV,read_prices$IGOV,read_prices$GSG,read_prices$DJP,read_prices$VNQ,read_prices$EFA)
colnames(sec_prices)=c("VEA","EEM","VWO","TIP","SHV","IGOV","GSG","DJP","VNQ","EFA")


#if output is zero for each ticker,that means no missing data
#calculate Returns for portfolio. Remove NA's using na.omit
sec_returns<-na.omit(Return.calculate(sec_prices))

#Print Basic Statistics for each security
sec_returns_stats<-subset(t(table.Stats(sec_returns)),select=c("Arithmetic Mean","Geometric Mean","Median","Variance","Stdev"))
print(sec_returns_stats,digits=2)


#Covariance between ETF's
sec_covariance<-cov(sec_returns)
print(sec_covariance,digits=2)

#Correlation between ETF's
sec_correlation<-cor(sec_returns)
print(sec_correlation,digits=2)

#Plot chart of Correlation between securities
#Distribution of each variable is shown on the diagonal
#Top of diagonal has the value of coorelation plus the significance level as stars
chart.Correlation(sec_returns)

#create a benchmark to compare tickers. Compare with S&P 500 - ^GSPC
benchmarkPrices<-getSymbols.yahoo('^GSPC',from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6]

#create a benchmark to compare tickers. Compare with Dow Jones Index
djia<-getSymbols.yahoo("^DJI",from=dt,to=tilldate,periodicity='monthly',auto.assign=FALSE)[,6]

#check if there is any missing data
colSums(is.na(benchmarkPrices))
colSums(is.na(djia))
#if output is zero for each ticker,  that means no missing data
#calculate Returns for S&P 500. Remove NA's using na.omit
benchmarkReturns<-na.omit(Return.calculate(benchmarkPrices))
djia_returns<-na.omit(Return.calculate(djia))

#Get data from Yahoo Finance, for IRX
irx<-getSymbols.yahoo(treasury,from="2010-05-01",to="2020-04-01",periodicity='monthly',auto.assign=FALSE)[,6]

#Calculate beta's for each security against benchmark, considering risk free rate Rf
beta_measure_vs_sp500<-CAPM.beta(sec_returns,benchmarkReturns,Rf=irx/1190)
beta_measure_vs_djia<-CAPM.beta(sec_returns,djia_returns,Rf=irx/1190)

#Display Beta's
print(beta_measure_vs_sp500,digits=2)
print(beta_measure_vs_djia,digits=2)

#Print Regression Chart
#Takes A Set Of Returns And Relates Them To A Market Benchmark In A Scatterplot
chart.Regression(sec_returns,benchmarkReturns,main="Assets Return vs Benchmark Return",Rf=irx,fit="conditional",
                 legend.loc= "bottomright")

#Initialize Portfolio Objects
portf<-portfolio.spec(colnames(sec_returns))
#Add portfolio Constraints
portf<-add.constraint(portf,type="weight_sum",min_sum=1 , max_sum=1)
#Add allocation Constraints
portf<-add.constraint(portf,type="box",min=0 , max=1)
#Add Objectives - Maximize return
portf<-add.objective(portf,type="return",name="mean")
#Add Objectives - Minimize Risk
portf<-add.objective(portf,type="risk",name="StdDev")

#Optimize using ROI method
optPort <- optimize.portfolio(sec_returns, portf, optimize_method = "ROI",trace=TRUE)

#Generate the efficient frontier for a mean-variance portfolio
#First argument is asset returns. Second argument is portfolio
meanvar.ef<-create.EfficientFrontier(R=sec_returns,portfolio=portf,type="mean-StdDev")

#Risk and return metrics along the efficient frontier
summary(meanvar.ef, digits=2)
meanvar.ef$frontier

#Plot Efficient Frontier of the portfolio, rf=0
chart.EfficientFrontier(meanvar.ef, match.col="StdDev",type="b", 
                        rf=0,main="Efficient Frontier",element.color="blue")



