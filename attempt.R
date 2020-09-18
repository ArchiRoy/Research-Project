##loading the required libraries
require(quantmod)
require(smooth)
require(Mcomp)
require(zoo)
require(ggplot2)
require(dbplyr)
require(tidyquant)
require(Quandl)
require(TTR)
require(rlist)
require(na.tools)
require(e1071)

##Selecting a data set to work with from Quandl
##This is the Microsoft stock prices for the year 2016

Quandl.api_key("ih_xHB2_Cn-b6obEP6Ww")
msft_data <- Quandl.datatable("WIKI/PRICES" ,
                              qopts.columns=c("date", "close"),
                              ticker=c("MSFT"),
                              date.gte=c("2016-01-01"),
                              date.lte=c("2016-12-31"))

library(readr) 
msft_data <- read_csv("msft_data.csv") 
View(msft_data)

##Visualizing the Data
r1<- ggplot(data = msft_data, aes(x = date, y = close))+geom_line()+ggtitle("Mircosoft Stock Price Data 2016-2017")
r1
b<- as.data.frame(BBands(msft_data$close,n=20,sd=2))
lower_band<- b$dn
moving_average_line<-b$mavg
upper_band<- b$up
r1+geom_line(aes(y=lower_band),color="red")+geom_line(aes(y=upper_band),color="red")+geom_line(aes(y=moving_average_line),color="blue")
#+geom_hline(yintercept=F_sigma_p,color="green",linetype="dotted")

price<- msft_data$close
price

##Prespecifying the values 

N=252 #Total number of data points
delta=(1/356) #The annualizing factor
n=20 #The window frame used
prob=0.15 #The usual quantile we take


##Calculating the return series
r<-0
for (i in 2:252)
{
  r[i]=(price[i]-price[i-1])/price[i-1]
}
r 

##Calculating the drift series
m<- 0
for (k in 21:252)
{
  m[k]=0
  for (i in 0:19)
  {
    m[k]=m[k]+(r[(k-i)]/20)
  }
}
m  ##This is basically the simple moving average with the pre fixed window size of the return series
meu<- 0
for (i in 1:231)
{
  meu[i]=m[i+21] 
}
meu ##Getting rid of the NA values
drift=(meu/delta) ##Anuualization
drift      ##the drift series


##Calculating the volatility series
s<-0
for (k in 21:252)
{
  s[k]=0
  for(i in 0:19)
  {
    s[k]=s[k]+((r[k-i]^2)/19)
  }
  s[k]=s[k]-((20/19)*(m[k]^2))
}
s=sqrt(s)
s
s1 <- 0
for (i in 1:231)
{
  s1[i]=s[i+21] 
}
s1 #Getting rid of the NA values
volatility_series<- s1/(sqrt(delta))
volatility_series ##the volatility series


##The pth percentile of the volatility series, which we shall use throughout the analysis
F_sigma_p=quantile(volatility_series,prob)
F_sigma_p

#Let's visulaize the volatility series
plot(volatility_series)
abline(h=F_sigma_p,col="green")



list1<- which(volatility_series>F_sigma_p)
list1
list2<- which(volatility_series<=F_sigma_p)
list2

p<- 0
q<- 0
for (i in 1:length(volatility_series))
{
  p[1]=0
  q[1]=min(list1)
  p[i+1]=min(list2[list2>=q[i]])
  q[i+1]=min(list1[list1>=p[i]])
}
p
q

##Sorting out the finite values in c and d
p[!is.finite(p)] <- NA
p<- p[!is.na(p)]
p<- unique(p)
p

q[!is.finite(q)] <- NA
q<- q[!is.na(q)]
q<- unique(q)
q

##Computing the pth squeeze durations
d<-q-p
d

##Computing the value of the test statistic
t<- c(mean(d),sd(d),skewness(d),kurtosis(d))
t

meu_hat=mean(drift)
sigma_hat=mean(volatility_series)
meu_hat
sigma_hat