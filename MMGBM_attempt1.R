##testing whether the data comes from a MMGBM model with regime switching subclass

##The A class (chosen subclass of the Cp class) is now a one-parameter family of models {meu1=meu2=-0.223053,sigma1=0.167725,sigma2=0.271171,lambda1=5.6667lambda2}

##We choose {meu1=meu2=-0.223053,sigma1=0.167725,sigma2=0.271171,lambda1???{5,10,15},lambda2???{28.3335,56.667,85.0005}}

#Doubt: How do we take an educated guess of lambda1 (Here we have taken the values of lambda considered in the paper)

##generating the surrogate samples

##Take lambda= 10

##generating the regime-switch

library(e1071)

x_series<- 0
for(i in 1:252)
  {
  x_series[1]=1
    if(x_series[i]==2)
    {
    x_series[i+1]=x_series[i]-(((-1)^x_series[i])*rbinom(1,1,0.027397))
    }
    if(x_series[i]==1)
    {
    x_series[i+1]=x_series[i]-(((-1)^x_series[i])*rbinom(1,1,0.155252))
    }
 }
x_series

mean(x_series)
sd(x_series)

##generating the actual markov modulated series

y1mmgbm<- rnorm(252,mean=0.003133,sd=0.423708)
y1mmgbm

dmmgbm <- 0
for (i in 1:252)
{
  dmmgbm[1]=62.14
  for(j in 2:252)
  {
    dmmgbm[j]=dmmgbm[(j-1)]*(exp(y1mmgbm[j]))
  }
}
dmmgbm
##dmmgbm is the genearted data series under MMGBM hypothesis
##every time we run the program we will get different data sets

rmmgbm<- 0
for(k in 2:252)
{
  rmmgbm[k]=((dmmgbm[k]-dmmgbm[k-1]))/(dmmgbm[k]) 
}
rmmgbm      ##the return series under GBM hypothesis

mmmgbm<- 0
for (k in 21:252)
{
  mmmgbm[k]=0
  for (i in 0:19)
  {
    mmmgbm[k]=mmmgbm[k]+(rmmgbm[(k-i)]/20)
  }
}
mmmgbm


meummgbm <- 0
for (i in 1:231)
{
  meummgbm[i]=mmmgbm[i+21] 
}
meummgbm

delta=(1/365)
delta

driftmmgbm=(meummgbm/delta)
driftmmgbm
mean(driftmmgbm)

smmgbm<-0
for (k in 21:252)
{
  smmgbm[k]=0
  for(i in 0:19)
  {
    smmgbm[k]=smmgbm[k]+((rmmgbm[k-i]^2)/19)
  }
  smmgbm[k]=smmgbm[k]-((20/19)*(mmmgbm[k]^2))
}
smmgbm=sqrt(smmgbm)
smmgbm
s1mmgbm<- 0
for (i in 1:231)
{
  s1mmgbm[i]=smmgbm[i+21] 
}
s1mmgbm

volatilitymmgbm=(s1mmgbm/sqrt(delta))
volatilitymmgbm
##volatility seires for the generated series under GBM hypothesis

p=0.15

F_sigma_p_mmgbm=quantile(volatilitymmgbm,p)
F_sigma_p_mmgbm

list1mmgbm= which(volatilitymmgbm>F_sigma_p_mmgbm)
list1mmgbm

list2mmgbm= which(volatilitymmgbm<=F_sigma_p_mmgbm)
list2mmgbm

ammgbm <- 0
bmmgbm <- 0
for (i in 1:length(volatilitymmgbm))
{
  ammgbm[1]=0
  bmmgbm[1]=min(list1mmgbm)
  ammgbm[i+1]=min(list2mmgbm[list2mmgbm>=bmmgbm[i]])
  bmmgbm[i+1]=min(list1mmgbm[list1mmgbm>=ammgbm[i]])
}
ammgbm
bmmgbm

##Sorting out the finite values in c and d
ammgbm[!is.finite(ammgbm)] <- NA
ammgbm<- ammgbm[!is.na(ammgbm)]
ammgbm<- unique(ammgbm)
ammgbm

bmmgbm[!is.finite(bmmgbm)] <- NA
bmmgbm<- bmmgbm[!is.na(bmmgbm)]
bmmgbm<- unique(bmmgbm)
bmmgbm


distmmgbm=bmmgbm-ammgbm
distmmgbm


##discriminating statistics
Test_starmmgbm=c(mean(distmmgbm),sd(distmmgbm),skewness(distmmgbm),kurtosis(distmmgbm))
Test_starmmgbm

Test1MMGBM=Test_starmmgbm[1]
Test1MMGBM

Test2MMGBM=Test_starmmgbm[2]
Test2MMGBM

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data

##running the same code 21 times we will get 21 values of Test_starmmgbm







##Take lambda= 5

##generating the regime-switch

library(e1071)

x_series2<- 0
for(i in 1:252)
{
  x_series2[1]=1
  if(x_series2[i]==2)
  {
    x_series2[i+1]=x_series2[i]-(((-1)^x_series2[i])*rbinom(1,1,0.0136699))
  }
  if(x_series2[i]==1)
  {
    x_series2[i+1]=x_series2[i]-(((-1)^x_series2[i])*rbinom(1,1,0.077630))
  }
}
x_series2

mean(x_series2)
sd(x_series2)

##generating the actual markov modulated series

y2mmgbm<- rnorm(252,mean=0.002801649,sd=0.2043353)
y2mmgbm

dmmgbm2 <- 0
for (i in 1:252)
{
  dmmgbm2[1]=62.14
  for(j in 2:252)
  {
    dmmgbm2[j]=dmmgbm2[(j-1)]*(exp(y2mmgbm[j]))
  }
}
dmmgbm2
##dmmgbm is the genearted data series under MMGBM hypothesis
##every time we run the program we will get different data sets

rmmgbm2<- 0
for(k in 2:252)
{
  rmmgbm2[k]=((dmmgbm2[k]-dmmgbm2[k-1]))/(dmmgbm2[k]) 
}
rmmgbm2      ##the return series under GBM hypothesis

mmmgbm2<- 0
for (k in 21:252)
{
  mmmgbm2[k]=0
  for (i in 0:19)
  {
    mmmgbm2[k]=mmmgbm2[k]+(rmmgbm2[(k-i)]/20)
  }
}
mmmgbm2


meummgbm2 <- 0
for (i in 1:231)
{
  meummgbm2[i]=mmmgbm2[i+21] 
}
meummgbm2

delta=(1/365)
delta

driftmmgbm2=(meummgbm2/delta)
driftmmgbm2
mean(driftmmgbm2)

smmgbm2<-0
for (k in 21:252)
{
  smmgbm2[k]=0
  for(i in 0:19)
  {
    smmgbm2[k]=smmgbm2[k]+((rmmgbm2[k-i]^2)/19)
  }
  smmgbm2[k]=smmgbm2[k]-((20/19)*(mmmgbm2[k]^2))
}
smmgbm2=sqrt(smmgbm2)
smmgbm2
s1mmgbm2<- 0
for (i in 1:231)
{
  s1mmgbm2[i]=smmgbm2[i+21] 
}
s1mmgbm2

volatilitymmgbm2=(s1mmgbm2/sqrt(delta))
volatilitymmgbm2
##volatility seires for the generated series under GBM hypothesis

p=0.15

F_sigma_p_mmgbm2=quantile(volatilitymmgbm2,p)
F_sigma_p_mmgbm2

list1mmgbm2= which(volatilitymmgbm2>F_sigma_p_mmgbm2)
list1mmgbm2

list2mmgbm2= which(volatilitymmgbm2<=F_sigma_p_mmgbm2)
list2mmgbm2

ammgbm2 <- 0
bmmgbm2 <- 0
for (i in 1:length(volatilitymmgbm2))
{
  ammgbm2[1]=0
  bmmgbm2[1]=min(list1mmgbm2)
  ammgbm2[i+1]=min(list2mmgbm2[list2mmgbm2>=bmmgbm2[i]])
  bmmgbm2[i+1]=min(list1mmgbm2[list1mmgbm2>=ammgbm2[i]])
}
ammgbm2
bmmgbm2

##Sorting out the finite values in c and d
ammgbm2[!is.finite(ammgbm2)] <- NA
ammgbm2<- ammgbm2[!is.na(ammgbm2)]
ammgbm2<- unique(ammgbm2)
ammgbm2

bmmgbm2[!is.finite(bmmgbm2)] <- NA
bmmgbm2<- bmmgbm2[!is.na(bmmgbm2)]
bmmgbm2<- unique(bmmgbm2)
bmmgbm2


distmmgbm2=bmmgbm2-ammgbm2
distmmgbm2


##discriminating statistics
Test_starmmgbm2=c(mean(distmmgbm2),sd(distmmgbm2),skewness(distmmgbm2),kurtosis(distmmgbm2))
Test_starmmgbm2

Test1MMGBM2=Test_starmmgbm2[1]
Test1MMGBM2

Test2MMGBM2=Test_starmmgbm2[2]
Test2MMGBM2

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data

##running the same code 21 times we will get 21 values of Test_starmmgbm







##Take lambda= 15

##generating the regime-switch

library(e1071)

x_series3<- 0
for(i in 1:252)
{
  x_series3[1]=1
  if(x_series3[i]==2)
  {
    x_series3[i+1]=x_series3[i]-(((-1)^x_series3[i])*rbinom(1,1,0.041096))
  }
  if(x_series3[i]==1)
  {
    x_series3[i+1]=x_series3[i]-(((-1)^x_series3[i])*rbinom(1,1,0.232878))
  }
}
x_series3

mean(x_series3)
sd(x_series3)

##generating the actual markov modulated series

y3mmgbm<- rnorm(252,mean=0.002927556,sd=0.328547)
y3mmgbm

dmmgbm3 <- 0
for (i in 1:252)
{
  dmmgbm3[1]=62.14
  for(j in 2:252)
  {
    dmmgbm3[j]=dmmgbm3[(j-1)]*(exp(y3mmgbm[j]))
  }
}
dmmgbm3
##dmmgbm is the genearted data series under MMGBM hypothesis
##every time we run the program we will get different data sets

rmmgbm3<- 0
for(k in 2:252)
{
  rmmgbm3[k]=((dmmgbm3[k]-dmmgbm3[k-1]))/(dmmgbm3[k]) 
}
rmmgbm3      ##the return series under GBM hypothesis

mmmgbm3<- 0
for (k in 21:252)
{
  mmmgbm3[k]=0
  for (i in 0:19)
  {
    mmmgbm3[k]=mmmgbm3[k]+(rmmgbm3[(k-i)]/20)
  }
}
mmmgbm3


meummgbm3 <- 0
for (i in 1:231)
{
  meummgbm3[i]=mmmgbm3[i+21] 
}
meummgbm3

delta=(1/365)
delta

driftmmgbm3=(meummgbm3/delta)
driftmmgbm3
mean(driftmmgbm3)

smmgbm3<-0
for (k in 21:252)
{
  smmgbm3[k]=0
  for(i in 0:19)
  {
    smmgbm3[k]=smmgbm3[k]+((rmmgbm3[k-i]^2)/19)
  }
  smmgbm3[k]=smmgbm3[k]-((20/19)*(mmmgbm3[k]^2))
}
smmgbm3=sqrt(smmgbm3)
smmgbm3
s1mmgbm3<- 0
for (i in 1:231)
{
  s1mmgbm3[i]=smmgbm3[i+21] 
}
s1mmgbm3

volatilitymmgbm3=(s1mmgbm3/sqrt(delta))
volatilitymmgbm3
##volatility seires for the generated series under GBM hypothesis

p=0.15

F_sigma_p_mmgbm3=quantile(volatilitymmgbm3,p)
F_sigma_p_mmgbm3

list1mmgbm3= which(volatilitymmgbm3>F_sigma_p_mmgbm3)
list1mmgbm3

list2mmgbm3= which(volatilitymmgbm3<=F_sigma_p_mmgbm3)
list2mmgbm3

ammgbm3 <- 0
bmmgbm3 <- 0
for (i in 1:length(volatilitymmgbm3))
{
  ammgbm3[1]=0
  bmmgbm3[1]=min(list1mmgbm3)
  ammgbm3[i+1]=min(list2mmgbm3[list2mmgbm3>=bmmgbm3[i]])
  bmmgbm3[i+1]=min(list1mmgbm3[list1mmgbm3>=ammgbm3[i]])
}
ammgbm3
bmmgbm3

##Sorting out the finite values in c and d
ammgbm3[!is.finite(ammgbm3)] <- NA
ammgbm3<- ammgbm3[!is.na(ammgbm3)]
ammgbm3<- unique(ammgbm3)
ammgbm3

bmmgbm3[!is.finite(bmmgbm3)] <- NA
bmmgbm3<- bmmgbm3[!is.na(bmmgbm3)]
bmmgbm3<- unique(bmmgbm3)
bmmgbm3


distmmgbm3=bmmgbm3-ammgbm3
distmmgbm3


##discriminating statistics
Test_starmmgbm3=c(mean(distmmgbm3),sd(distmmgbm3),skewness(distmmgbm3),kurtosis(distmmgbm3))
Test_starmmgbm3

Test1MMGBM3=Test_starmmgbm3[1]
Test1MMGBM2

Test2MMGBM3=Test_starmmgbm3[2]
Test2MMGBM3

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data

##running the same code 21 times we will get 21 values of Test_starmmgbm






