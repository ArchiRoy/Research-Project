##testing whether the data comes from a GBM model with regime switching subclass
##the p-admissible class is the sigleton mean(drift)=-0.2230525 and mean(volatility)=0.2556539


##we shall generate from 10 surrogate samples from the relation X(t(i+1))=X(t(i))exp(W(t)) where Wt~N(-0.0007183481,0.06535893)
y1<- rnorm(252,mean=-0.0007183481,sd=0.2556539)
y1

dgbm <- 0
for (i in 1:252)
{
  dgbm[1]=62.14
  for(j in 2:252)
  {
    dgbm[j]=dgbm[(j-1)]*(exp(y1[j]))
  }
}
dgbm
##dgbm is the genearted data series under GBM hypothesis
##every time we run the program we will get different data sets

rgbm<- 0
for(k in 2:252)
{
  rgbm[k]=((dgbm[k]-dgbm[k-1]))/(dgbm[k]) 
}
rgbm      ##the return series under GBM hypothesis

mgbm<- 0
for (k in 21:252)
{
  mgbm[k]=0
  for (i in 0:19)
  {
    mgbm[k]=mgbm[k]+(rgbm[(k-i)]/20)
  }
}
mgbm


meugbm <- 0
for (i in 1:231)
{
  meugbm[i]=mgbm[i+21] 
}
meugbm

delta=(1/365)
delta

driftgbm=(meugbm/delta)
driftgbm
mean(driftgbm)

sgbm<-0
for (k in 21:252)
{
  sgbm[k]=0
  for(i in 0:19)
  {
    sgbm[k]=sgbm[k]+((rgbm[k-i]^2)/19)
  }
  sgbm[k]=sgbm[k]-((20/19)*(mgbm[k]^2))
}
sgbm=sqrt(sgbm)
sgbm
s1gbm<- 0
for (i in 1:231)
{
  s1gbm[i]=sgbm[i+21] 
}
s1gbm

volatilitygbm=(s1gbm/sqrt(delta))
volatilitygbm
##volatility seires for the generated series under GBM hypothesis

p=0.15

F_sigma_p_gbm=quantile(volatilitygbm,p)
F_sigma_p_gbm

list1gbm= which(volatilitygbm>F_sigma_p_gbm)
list1gbm

list2gbm= which(volatilitygbm<=F_sigma_p_gbm)
list2gbm

agbm <- 0
bgbm <- 0
for (i in 1:length(volatilitygbm))
{
  agbm[1]=0
  bgbm[1]=min(list1gbm)
  agbm[i+1]=min(list2gbm[list2gbm>=bgbm[i]])
  bgbm[i+1]=min(list1gbm[list1gbm>=agbm[i]])
}
agbm
bgbm

##Sorting out the finite values in c and d
agbm[!is.finite(agbm)] <- NA
agbm<- agbm[!is.na(agbm)]
agbm<- unique(agbm)
agbm

bgbm[!is.finite(bgbm)] <- NA
bgbm<- bgbm[!is.na(bgbm)]
bgbm<- unique(bgbm)
bgbm


distgbm=bgbm-agbm
distgbm


##discriminating statistics
Test_stargbm=c(mean(distgbm),sd(distgbm),skewness(distgbm),kurtosis(distgbm))
Test_stargbm

Test1GBM=Test_stargbm[1]
Test1GBM

Test2GBM=Test_stargbm[2]
Test2GBM

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data

##running the same code 21 times we will get 21 values of Test_stargbm

