
Nepse_MA<-read.csv(file.choose())
head(Nepse_MA)
tail(Nepse_MA)
class(Nepse_MA)

#Run/Install the required libraries

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(timetk)
library(dplyr)

# 1st step

#Lets plot this data in a chart

# But, first we need to change our data from
# a data frame to xts object. To do that, we need an extra 
# line of code

Nepse_MA1<-xts(Nepse_MA[,-1],order.by=as.POSIXct(Nepse_MA$Date))
head(Nepse_MA1)

# Now, here is the code for chart

barChart(Nepse_MA1,theme=chartTheme("black"))

# 2nd step

# Lets now add moving averages in our chart. We shall go with
# 20 and 50 Day SMA

sma50_nepse<-SMA(Nepse_MA1$CLOSE, n=50)
sma200_nepse<-SMA(Nepse_MA1$CLOSE,n=200)
lineChart(Nepse_MA1, theme=chartTheme("black"))
addSMA(n=50,col="purple")
addSMA(n=200,col="orange")
legend("left", col = c("green","purple","orange"),
       legend=c("Nepse_MA1","SMA50","SMA200"),
       lty = 1,bty = "n",
       text.col = "white",cex=0.8)

#3rd Step


# Now, its time for us to add trading signals: BUY and SELL
# So, what we are trying to do here is we are gonna buy when
# our 20 Day MA crosses the 50 Day MA and goes upward and 
# similarly we shall be selling when the 50 day MA crosses the
# 20 day MA and goes downward

#SMA 20 Day Crossover signal


sma50_nepse_ts<-Lag(
  ifelse(Lag(Cl(Nepse_MA1))<Lag(sma50_nepse) & 
           Cl(Nepse_MA1)>sma50_nepse,1,
         ifelse(Lag(Cl(Nepse_MA1))>Lag(sma50_nepse)&
                  Cl(Nepse_MA1)< sma50_nepse,-1,0)))
sma50_nepse_ts[is.na(sma50_nepse_ts)]<-0

#SMA 50 Day Cross over signal

sma200_nepse_ts<-Lag(
  ifelse(Lag(Cl(Nepse_MA1))< Lag(sma200_nepse) &
           Cl(Nepse_MA1)>sma200_nepse,1,
         ifelse(Lag(Cl(Nepse_MA1))>Lag(sma200_nepse)&
                  Cl(Nepse_MA1)< sma200_nepse,-1,0)))
sma200_nepse_ts[is.na(sma200_nepse_ts)]<-0

#SMA 20 and SMA 50 Crossover signal

sma_nepse_ts<-Lag(ifelse(Lag(sma50_nepse)< Lag(sma200_nepse)&
                           sma50_nepse>sma200_nepse,1,
                         ifelse(Lag(sma50_nepse)>Lag(sma200_nepse)&
                                  sma50_nepse<sma200_nepse,-1,0)))
sma_nepse_ts[is.na(sma_nepse_ts)]<-0


# In the above code, we are passing on specific conditions and 
# if the condition satisfies a 'BUY' signal our created trading 
# signal will turn to 1 which means a buy. If the condition 
# satisfies a 'SELL' signal our created trading signal will 
# turn to -1 which means a sell. If it doesn't satisfies any of 
# the mentioned conditions, then it will turn to 0 which means 
# nothing


# 4th step

# Now, we shall create trading strategies using previously 
# created trading signals

# we are going to create conditions to check our holding 
# position in that stock (i.e., whether we hold, bought or sold
# the stock). The trading strategy we are about to code will 
# return 1 if we hold the stock or returns 0 if we don't own 
# the stock.

sma_Nepse_strat<- ifelse(sma_nepse_ts>1,0,1)
for (i in 1 :length(Cl(Nepse_MA1))){
  sma_Nepse_strat[i]<-ifelse(sma_nepse_ts[i]==1,1,ifelse(
    sma_nepse_ts[i]==-1,0,sma_Nepse_strat[i,-1]))
}
sma_Nepse_strat[is.na(sma_Nepse_strat)]<-1
sma_Nepse_stratcomp<-cbind(sma50_nepse,sma200_nepse,
                           sma_nepse_ts, sma_Nepse_strat)
colnames(sma_Nepse_stratcomp)<-c("SMA(50)","SMA(200)",
                                 "SMA SIGNAL","SMA POSITION")
View(sma_Nepse_stratcomp)

# 5th step: Conducting our backtest

# we will first calculate SMA strategy daily returns and
# comapare that returns to a Buy and hold strategy 

ret_Nepse<-diff(log(Cl(Nepse_MA1)))

benchmark_Nepse<-ret_Nepse

# Now lets conduct our backtest

sma_Nepse_ret<-ret_Nepse*sma_Nepse_strat
sma_Nepse_comp<-cbind(sma_Nepse_ret,benchmark_Nepse)
colnames(sma_Nepse_comp)<-c("SMA Crossover","Nepse Benchmark")
charts.PerformanceSummary(sma_Nepse_comp,
                          main="Nepse VS SMA 50/200 crossover Performance")
sma_Nepse_comp_table<-table.AnnualizedReturns(sma_Nepse_comp)

View(sma_Nepse_comp_table)


getwd()
setwd("C://Users//user//Desktop")

write.csv(sma_Nepse_comp_table,file="SMA vs NEPSE.csv")
