##testing whether the data comes from a GBM model with regime switching subclass
##the p-admissible class is the sigleton mean(drift)=0.111795 and mean(volatility)=0.061741


##we shall generate from 10 surrogate samples from the relation X(t(i+1))=X(t(i))exp(W(t)) where Wt~N(0.011795,0.003812)
y<- rnorm(100,mean=0.011795,sd=0.061741)
y

x <- 0
for (i in 1:100)
{
  x[1]=1.958333
  for(j in 2:100)
  {
    x[j]=x[(j-1)]*(exp(y[j]))
  }
}
x
##x is the genearted data series under GBM hypothesis
##every time we run the program we will get different data sets

r<- 0

for(k in 1:100)
{
 r[k+1]=((x[k+1]-x[k]))/(x[k]) 
}

r
##the return series under GBM hypothesis

m<- 0

n=20
N=100

for (k in 21:100)
{
  m[k]=0
  for (i in 0:20)
  {
    m[k]=m[k]+(r[(k-i)]/20)
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

drift=(meu/delta)
drift
mean(drift)

s <- 0

for (k in 21:100)
{
  s[k]=0
  for(i in 0:20)
  {
    s[k]=s[k]+(((r[k-i])-(m[k]))^2)
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
##volatility seires for the generated series under GBM hypothesis
mean(volatility)
((mean(volatility))^2)*0.5

p=0.15

F_sigma_p=quantile(volatility,p)
F_sigma_p

s1= which(volatility>F_sigma_p)
s1

s2= which(volatility<=F_sigma_p)
s2

sort(volatility,decreasing = FALSE)
a <- 0
b <- 0

for (i in 0:100)
{
  a[0]=0
  b[0]=1
  for (j in 1:100)
  {
    a[j]=min(s1[s1>=b[j-1]])
    b[j]=min(s2[s2>=a[j]])
  }
}

a
b
##this piece of code shows the same warning

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
Test_star=c(mean,sd,skewness,kurtosis)
Test_star

Test1GBM=Test_star[1]
Test1GBM

Test2GBM=Test_star[2]
Test2GBM

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data
##running the same code 10 times we will get 10 values of Test_star1

##drawing the bivariate boxplot of drift and volatility representations from surroagate data

##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 10 element vector
T1GBMvect=c() ##recording 10 values of T1
T2GBMvect=c() ##recording 10 values of T2

install.packages("asbio")
library(asbio)
bv.boxplot(T1GBMvect, T2GBMvect, D = 7, xlab = "T1", ylab="T2", main="Boxplot of (T1,T2) under GBM regime switch hypothesis",pch = 21)

##adding the observed point in the two dimensional boxplot

points(Test1[1],Test1[2],pch=24,col="blue")

##this would add the observed values in the boxplot denoted by a filled triangle point-up point

##we can do primary comparison seeing the graph

##now we shall do the hypothesis tesing

x1<- 0
y1<- 0
for (i in 1:10)
{
  if (T1GBMvect[i]>=Test1[1])
    y1=1
  else
    y1=0
  x1=x1+y1
}
x1
gB1=min(c(x1,10-x1))/10  ##this would be a two-element vector

alpha_theta1=2*min(gB)##defining the surrogate data test statistic
##alpha_theta1 would be a one- dimensional scalar

x2<- 0
y2<- 0
for (i in 1:10)
{
  if (T2GBMvect[i]>=Test1[2])
    y2=1
  else
    y2=0
  x2=x2+y2
}
x2
gB2=min(c(x2,10-x2))/10  ##this would be a two-element vector

alpha_theta2=2*min(gB2)

alpha_test=min(c(alpha_theta1,alpha_theta2))

##so our test statistic would be
alpha_test

##note that here the p-admissible class is a singleton so this would be taken as our test statistic

##we shall reject our null hypothesis H0: "the data in hand comes from a GBM model with our chosen p-admissible class" if the value of alpha_theta is too small

##carrying out the one sided t-test for checking if alpha_theta is too small

t.test(alpha_theta, mu = 0, alternative = "greater")


##observe the p value to accept or reject the null hypothesis

##if the null hypothesis is rejected we shall proceed to check wheather the data comes from MMGBM or SMMGBM hypothesis
