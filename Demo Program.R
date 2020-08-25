Demo_Data<- as.data.frame(Book1)
##taking the first 100 months of data

acp<- Demo_Data$`Adjusted Closing Price`
acp
acp <- ts(acp)
##declaring the adjusted closing price as a time series object

rtrn <- Demo_Data$Return
rtrn
##declaring the return series

library(ggfortify)
autoplot(acp)
##plotting the time series data

m<- 0

n=20
N=100

for (k in 21:100)
{
  m[k]=0
for (i in 0:20)
{
  m[k]=m[k]+(rtrn[(k-i)]/20)
}
}
m
length(m)
 
meu <- 0

for (i in 1:79)
{
 meu[i]=m[i+21] 
}
meu

delta=(1/12)
delta
##because we have monthly data for 100 months

drift=(meu/delta)
drift
##the drift series

s <- 0

for (k in 21:100)
{
  s[k]=0
  for(i in 0:20)
  {
    s[k]=s[k]+(((rtrn[k-i])-(m[k]))^2)
  }
}
s

s=sqrt(s/(19))
s

s1 <- 0
for (i in 1:79)
{
  s1[i]=s[i+21] 
}
s1

volatility=(s1/sqrt(delta))
volatility
##the volatility series

p=0.15
##the pre specified value of p

F_sigma_p=quantile(volatility,p)
F_sigma_p

s1= which(volatility>F_sigma_p)
s1

s2= which(volatility<=F_sigma_p)
s2

a <- 0
b <- 0

for (i in 0:100)
{
  a[0]<- 0
  b[0]<-1
  for (j in 1:100)
  {
    a[j]=min(s1[s1>=b[j-1]])
    b[j]=min(s2[s2>=a[j]])
  }
}

a
b
warnings()
##this code shows a warning saying "no non- missing argument to min;returning Inf", I am unable to figure out what is wrong with this piece of code



d=b-a

mean=mean(d)
sd=sd(d)

cm3=sum((d-mean(d))^3)/(3)
cm3

cm4=sum((d-mean(d))^4)/(3)
cm4

skewness=(cm3/(sd^3))
kurtosis=(cm4/(sd^4))
skewness
kurtosis

L=length(d)

##discriminating statistics
Test<- c(mean,sd,skewness,kurtosis)
Test  ##this is the test statistic based on which we will carry out the inference

##We now have to find the sampling distribution of T under binary regime switching model hypothesis with the help of Monte Carlo Method


##since there was something wrong with the code computing d I have computed d, T1 and T2 manually
Test1<- Test[c(1,2)]
Test1
##for the sake of simplicity we shall take the mean and variance only for the mean and variance as test statistic

