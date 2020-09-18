library(pracma)
library(gsl)
library(purrr)
library(e1071)

PFunc<- function(k,lambda,y)
{
  ((lambda^k)*(y^(k-1))*exp(-lambda*y))/ (gamma(k)-(gamma_inc((lambda*y),k)))
}


k=2  ##how to take an educated guess about values of k
lambda1<- c(28.3335,56.6667,85.0005)
lambda2<- c(5,10,15)

##For lambda2=5 ie for lambda1=28.3335

pro<- 0
x_s<- 0
y_s<- 0
for(i in 1:252)
{
  y_s[1]=0.5 #How do we initialize y
  x_s[1]=1
  if(x_s[i]==1)
  {
    pro[i]=PFunc(2,28.3335,y_s[i])
    y_s[i+1]=(y_s[i]+(i*delta))*(1-(delta*rbinom(1,1,pro[i])))
    x_s[i+1]=x_s[i]+(((-1)^x_s[i])*(delta*rbinom(1,1,pro[i])))
  }
  if(x_s[i]==2)
  {
    pro[i]=PFunc(2,5,y_s[i])
    y_s[i+1]=(y_s[i]+(i*delta))*(1-(delta*rbinom(1,1,pro[i])))
    x_s[i+1]=x_s[i]+(((-1)^x_s[i])*(delta*rbinom(1,1,pro[i])))
  }
}
delta=(1/365)

sample(1:252,1,replace = TRUE)
r1<- rbinom(252,1,0.0246)
r1
x_s<- r1+1
x_s
mean(x_s)
sd(x_s)

(mean(x_s)-(0.5*sd(x_s)*sd(x_s)))*delta

y_smm<- rnorm(252,mean=0.002772992,sd=0.1527587)
y_smm

dsm<- 0
for (i in 1:252)
{
  dsm[1]=62.14
  for(j in 2:252)
  {
    dsm[j]=dsm[(j-1)]*(exp(y_smm[j]))
  }
}
dsm
##dsm is the genearted data series under MMGBM hypothesis
##every time we run the program we will get different data sets

rsm<- 0
for(k in 2:252)
{
  rsm[k]=((dsm[k]-dsm[k-1]))/(dsm[k]) 
}
rsm      ##the return series under GBM hypothesis

msm<- 0
for (k in 21:252)
{
  msm[k]=0
  for (i in 0:19)
  {
    msm[k]=msm[k]+(rsm[(k-i)]/20)
  }
}
msm


meusm <- 0
for (i in 1:231)
{
  mmeusm[i]=msm[i+21] 
}
meusm

delta=(1/365)
delta

driftsm=(meusm/delta)
driftsm
mean(driftsm)

ssm<-0
for (k in 21:252)
{
  ssm[k]=0
  for(i in 0:19)
  {
    ssm[k]=ssm[k]+((rsm[k-i]^2)/19)
  }
  ssm[k]=ssm[k]-((20/19)*(msm[k]^2))
}
ssm=sqrt(ssm)
ssm
s1sm<- 0
for (i in 1:231)
{
  s1sm[i]=ssm[i+21] 
}
s1sm

volatilitysm=(s1sm/sqrt(delta))
volatilitysm
##volatility seires for the generated series under GBM hypothesis

p=0.15

F_sigma_p_sm=quantile(volatilitysm,p)
F_sigma_p_sm

list1sm= which(volatilitysm>F_sigma_p_sm)
list1sm

list2sm= which(volatilitysm<=F_sigma_p_sm)
list2sm

asm <- 0
bsm <- 0
for (i in 1:length(volatilitysm))
{
  asm[1]=0
  bsm[1]=min(list1sm)
  asm[i+1]=min(list2sm[list2sm>=bsm[i]])
  bsm[i+1]=min(list1sm[list1sm>=asm[i]])
}
asm
bsm

##Sorting out the finite values in c and d
asm[!is.finite(asm)] <- NA
asm<- asm[!is.na(asm)]
asm<- unique(asm)
asm

bsm[!is.finite(bsm)] <- NA
bsm<- bsm[!is.na(bsm)]
bsm<- unique(bsm)
bsm


distsm=bsm-asm
distsm


##discriminating statistics
Test_starsm=c(mean(distsm),sd(distsm),skewness(distsm),kurtosis(distsm))
Test_starsm

Test1SMMGBM=Test_starsm[1]
Test1SMMGBM

Test2SMMGBM=Test_starsm[2]
Test2SMMGBM

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data

##running the same code 21 times we will get 21 values of Test_starmmgbm







##For lambda2=10 ie for lambda1=56.6667

pro2<- 0
x_s2<- 0
y_s2<- 0
for(i in 1:252)
{
  y_s2[1]=0.5 #How do we initialize y
  x_s2[1]=1
  if(x_s2[i]==1)
  {
    pro2[i]=PFunc(2,28.3335,y_s[i])
    y_s2[i+1]=(y_s2[i]+(i*delta))*(1-(delta*rbinom(1,1,pro2[i])))
    x_s2[i+1]=x_s2[i]+(((-1)^x_s2[i])*(delta*rbinom(1,1,pro2[i])))
  }
  if(x_s[i]==2)
  {
    pro[i]=PFunc(2,5,y_s[i])
    y_s2[i+1]=(y_s2[i]+(i*delta))*(1-(delta*rbinom(1,1,pro2[i])))
    x_s2[i+1]=x_s2[i]+(((-1)^x_s2[i])*(delta*rbinom(1,1,pro2[i])))
  }
}
delta=(1/365)

sample(1:252,1,replace = TRUE)
r2<- rbinom(252,1,0.567)
r2
x_s2<- r2+1
x_s2
mean(x_s2)
sd(x_s2)

(mean(x_s2)-(0.5*sd(x_s2)*sd(x_s2)))*delta

y_smm2<- rnorm(252,mean=0.003922211,sd=0.4978929)
y_smm2

dsm2<- 0
for (i in 1:252)
{
  dsm2[1]=62.14
  for(j in 2:252)
  {
    dsm2[j]=dsm2[(j-1)]*(exp(y_smm2[j]))
  }
}
dsm2
##dsm is the genearted data series under MMGBM hypothesis
##every time we run the program we will get different data sets

rsm2<- 0
for(k in 2:252)
{
  rsm2[k]=((dsm2[k]-dsm2[k-1]))/(dsm2[k]) 
}
rsm2     ##the return series under SMMGBM hypothesis

msm2<- 0
for (k in 21:252)
{
  msm2[k]=0
  for (i in 0:19)
  {
    msm2[k]=msm2[k]+(rsm2[(k-i)]/20)
  }
}
msm2


meusm2 <- 0
for (i in 1:231)
{
  meusm2[i]=msm2[i+21] 
}
meusm2

delta=(1/365)
delta

driftsm2=(meusm2/delta)
driftsm2
mean(driftsm2)

ssm2<-0
for (k in 21:252)
{
  ssm2[k]=0
  for(i in 0:19)
  {
    ssm2[k]=ssm2[k]+((rsm2[k-i]^2)/19)
  }
  ssm2[k]=ssm2[k]-((20/19)*(msm2[k]^2))
}
ssm2=sqrt(ssm2)
ssm2
s1sm2<- 0
for (i in 1:231)
{
  s1sm2[i]=ssm2[i+21] 
}
s1sm2

volatilitysm2=(s1sm2/sqrt(delta))
volatilitysm2
##volatility seires for the generated series under SMMGBM hypothesis

p=0.15

F_sigma_p_sm2=quantile(volatilitysm2,p)
F_sigma_p_sm2

list1sm2= which(volatilitysm2>F_sigma_p_sm2)
list1sm2

list2sm2= which(volatilitysm2<=F_sigma_p_sm2)
list2sm2

asm2 <- 0
bsm2 <- 0
for (i in 1:length(volatilitysm))
{
  asm2[1]=0
  bsm2[1]=min(list1sm2)
  asm2[i+1]=min(list2sm2[list2sm2>=bsm2[i]])
  bsm2[i+1]=min(list1sm2[list1sm2>=asm2[i]])
}
asm2
bsm2

##Sorting out the finite values in c and d
asm2[!is.finite(asm2)] <- NA
asm2<- asm2[!is.na(asm2)]
asm2<- unique(asm2)
asm2

bsm2[!is.finite(bsm2)] <- NA
bsm2<- bsm2[!is.na(bsm2)]
bsm2<- unique(bsm2)
bsm2


distsm2=bsm2-asm2
distsm2


##discriminating statistics
Test_starsm2=c(mean(distsm2),sd(distsm2),skewness(distsm2),kurtosis(distsm2))
Test_starsm2

Test1SMMGBM2=Test_starsm2[1]
Test1SMMGBM2

Test2SMMGBM2=Test_starsm2[2]
Test2SMMGBM2

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data

##running the same code 21 times we will get 21 values of Test_starmmgbm







##For lambda2=15 ie for lambda1=85.0005

pro3<- 0
x_s3<- 0
y_s3<- 0
for(i in 1:252)
{
  y_s3[1]=0.5 #How do we initialize y
  x_s3[1]=1
  if(x_s3[i]==1)
  {
    pro3[i]=PFunc(2,28.3335,y_s3[i])
    y_s3[i+1]=(y_s3[i]+(i*delta))*(1-(delta*rbinom(1,1,pro3[i])))
    x_s3[i+1]=x_s3[i]+(((-1)^x_s3[i])*(delta*rbinom(1,1,pro3[i])))
  }
  if(x_s3[i]==2)
  {
    pro3[i]=PFunc(2,5,y_s[i])
    y_s3[i+1]=(y_s3[i]+(i*delta))*(1-(delta*rbinom(1,1,pro3[i])))
    x_s3[i+1]=x_s3[i]+(((-1)^x_s3[i])*(delta*rbinom(1,1,pro3[i])))
  }
}
delta=(1/365)

sample(1:252,1,replace = TRUE)
r3<- rbinom(252,1,0.7540)
r3
x_s3<- r3+1
x_s3
mean(x_s3)
sd(x_s3)

(mean(x_s3)-(0.5*sd(x_s3)*sd(x_s3)))*delta

y_smm3<- rnorm(252,mean=0.004563936,sd=0.4291849)
y_smm3

dsm3<- 0
for (i in 1:252)
{
  dsm3[1]=62.14
  for(j in 2:252)
  {
    dsm3[j]=dsm3[(j-1)]*(exp(y_smm3[j]))
  }
}
dsm3
##dsm is the genearted data series under MMGBM hypothesis
##every time we run the program we will get different data sets

rsm3<- 0
for(k in 2:252)
{
  rsm3[k]=((dsm3[k]-dsm3[k-1]))/(dsm3[k]) 
}
rsm3      ##the return series under GBM hypothesis

msm3<- 0
for (k in 21:252)
{
  msm3[k]=0
  for (i in 0:19)
  {
    msm3[k]=msm3[k]+(rsm3[(k-i)]/20)
  }
}
msm3


meusm3 <- 0
for (i in 1:231)
{
  mmeusm3[i]=msm3[i+21] 
}
meusm3

delta=(1/365)
delta

driftsm3=(meusm3/delta)
driftsm3
mean(driftsm3)

ssm3<-0
for (k in 21:252)
{
  ssm3[k]=0
  for(i in 0:19)
  {
    ssm3[k]=ssm3[k]+((rsm3[k-i]^2)/19)
  }
  ssm3[k]=ssm3[k]-((20/19)*(msm3[k]^2))
}
ssm3=sqrt(ssm3)
ssm3
s1sm3<- 0
for (i in 1:231)
{
  s1sm3[i]=ssm3[i+21] 
}
s1sm3

volatilitysm3=(s1sm3/sqrt(delta))
volatilitysm3
##volatility seires for the generated series under GBM hypothesis

p=0.15

F_sigma_p_sm3=quantile(volatilitysm3,p)
F_sigma_p_sm3

list1sm3= which(volatilitysm3>F_sigma_p_sm3)
list1sm3

list2sm3= which(volatilitysm3<=F_sigma_p_sm3)
list2sm3

asm3 <- 0
bsm3 <- 0
for (i in 1:length(volatilitysm3))
{
  asm3[1]=0
  bsm3[1]=min(list1sm3)
  asm3[i+1]=min(list2sm3[list2sm3>=bsm3[i]])
  bsm3[i+1]=min(list1sm3[list1sm3>=asm[i]])
}
asm3
bsm3

##Sorting out the finite values in c and d
asm3[!is.finite(asm3)] <- NA
asm3<- asm[!is.na(asm3)]
asm3<- unique(asm3)
asm3

bsm3[!is.finite(bsm3)] <- NA
bsm3<- bsm3[!is.na(bsm3)]
bsm3<- unique(bsm3)
bsm3


distsm3=bsm3-asm3
distsm3


##discriminating statistics
Test_starsm3=c(mean(distsm3),sd(distsm3),skewness(distsm3),kurtosis(distsm3))
Test_starsm3

Test1SMMGBM3=Test_starsm3[1]
Test1SMMGBM3

Test2SMMGBM3=Test_starsm3[2]
Test2SMMGBM3

##these would be one-dimensioanl scalars

##thus we get the value for one surrogate data

##running the same code 21 times we will get 21 values of Test_starmmgbm







