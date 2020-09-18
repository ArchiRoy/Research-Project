##For lambda2=5

##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 21 element vector
T1SMMGBMvect1=c(13.888889,16,19,16,14,14.142857,14.5,13.272727,14.5,13.181818,12.916667,13.6,14,16.5, 14.5,14.5,13.6,14.5,16,14.5,13.111111) ##recording 21 values of T1
T2SMMGBMvect1=c(14.31406,13.34664,19.128709,15.899152,15.215362,15.113009,15.154748,14.02718,15.154748,11.940009,13.028901,15.23278,13.607887, 8.544004,14.6291,13.295018,13.864376,15.976143,15.01996,15.529144,12.571208) ##recording 21 values of T2

length(T1SMMGBMvect1)
length(T2SMMGBMvect1)
SMMGBM<- data.frame(T1SMMGBMvect1,T2SMMGBMvect1)
SMMGBM

##visualizing the sampling distribution of the statistics T1 and T2 from the simulted data
par(xpd=TRUE)
boxplot(T1SMMGBMvect1,main="Sampling Distribution of T1 in under SMMGBM hypothesis for lambda=5",sub="The red point is the empirical value of T1 obtained from the data in hand")
points(12,col="red")

par(xpd=TRUE)
boxplot(T2SMMGBMvect1,main="Sampling distribution of T2 under SMMGBM hypothesis for lambda=5",sub="The red point is the empirical value of T2 obtained from the data in hand")
points(12.767145,col="red")


##Both the boxplots provide us with some visual evidence in favour of the null hypothesis that the data in hand comes from a SMMGBM under the appropriate Cp class

##we can do primary comparison seeing the graph

##The graph shows that the point may be a potential outlier

##now we shall do the hypothesis tesing

##The null hypothesis is Ho:The data in hand comes from a Cp class of GBM where Cp class here is the sigleton (-0.2230525,0.2556539)

z1smm1<-0
for (i in 1:21)
{
  if(t[1]>=T1SMMGBMvect1[i])
    z1smm1[i]=0
  if(t[1]<T1SMMGBMvect1[i])
    z1smm1[i]=1
}
z1smm1
sum(z1smm1)
zsm1<- min(sum(z1smm1),21-sum(z1smm1))/21
zsm1

z2smm1<- 0
for(i in 1:21)
{
  if(t[2]>=T2SMMGBMvect1[i])
    z2smm1[i]=0
  if(t[2]<T2SMMGBMvect1[i])
    z2smm1[i]=1
}
z2smm1
sum(z2smm1)
wsm1<- min(sum(z2smm1),(20-sum(z2smm1)))/21
wsm1

alpha_value_SMMGBM1<- max(zsm1,wsm1)*2
alpha_value_SMMGBM1
conf_coeff_smmgbm1<- 100*(1-alpha_value_SMMGBM1)
conf_coeff_smmgbm1




##For lambda=10

##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 21 element vector
T1SMMGBMvect2=c(13.11111,12.8,15.142857,17.2,17.2,14,16,14.333333,16,13.833333,17.2,15.142857,13.272727,14.5,13.6,15.142857,9,13.6,12,12.142857,12.456352) ##recording 21 values of T1
T2SMMGBMvect2=c(12.571208,12.167948,14.049087,17.758866, 14.816638,13.570714,17.536677,14.163332,17.047270,13.920034,16.723095,17.244045,12.053821,15.209881,15.103376,13.532165,10.424330,12.836273,15.6205,12.734262,12.453271) ##recording 21 values of T2

length(T1SMMGBMvect2)
length(T2SMMGBMvect2)
SMMGBM2<- data.frame(T1SMMGBMvect2,T2SMMGBMvect2)
SMMGBM2

##visualizing the sampling distribution of the statistics T1 and T2 from the simulted data
par(xpd=TRUE)
boxplot(T1SMMGBMvect2,main="Sampling Distribution of T1 in under SMMGBM hypothesis for lambda=10",sub="The red point is the empirical value of T1 obtained from the data in hand")
points(12,col="red")

par(xpd=TRUE)
boxplot(T2SMMGBMvect2,main="Sampling distribution of T2 under SMMGBM hypothesis for lambda=10",sub="The red point is the empirical value of T2 obtained from the data in hand")
points(12.767145,col="red")


##Both the boxplots provide us with some visual evidence in favour of the null hypothesis that the data in hand comes from a SMMGBM under the appropriate Cp class

##we can do primary comparison seeing the graph

##The graph shows that the point is in the range

##now we shall do the hypothesis tesing

##The null hypothesis is Ho:The data in hand comes from a Cp class of GBM where Cp class here is the sigleton (-0.2230525,0.2556539)

z1smm2<-0
for (i in 1:21)
{
  if(t[1]>=T1SMMGBMvect2[i])
    z1smm2[i]=0
  if(t[1]<T1SMMGBMvect2[i])
    z1smm2[i]=1
}
z1smm2
sum(z1smm2)
zsm2<- min(sum(z1smm2),21-sum(z1smm2))/21
zsm2

z2smm2<- 0
for(i in 1:21)
{
  if(t[2]>=T2SMMGBMvect2[i])
    z2smm2[i]=0
  if(t[2]<T2SMMGBMvect2[i])
    z2smm2[i]=1
}
z2smm2
sum(z2smm2)
wsm2<- min(sum(z2smm2),(20-sum(z2smm2)))/21
wsm2

alpha_value_SMMGBM2<- max(zsm2,wsm2)*2
alpha_value_SMMGBM1
conf_coeff_smmgbm2<- 100*(1-alpha_value_SMMGBM2)
conf_coeff_smmgbm2




##For lambda=15

##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 21 element vector
T1SMMGBMvect3=c(13.920034,18.666667,14.75,17,19,13.5,12.25,16,19.25,16.4,10.7,14,10.6,13.5,14.6667,10.1,16.5,11.5,17.25,17.5,13.333333) ##recording 21 values of T1
T2SMMGBMvect3=c(16.5,17.094599,16.849576,17.778175,11.31371,10.7071068,11.258306, 17.071068,16.291529,15.813777,11.888562,14.242641,11.577621,13.535534,13.511885,10.316228,18.485281,14.403282,16.849574,16.350853,12.516611) ##recording 21 values of T2

length(T1SMMGBMvect3)
length(T2SMMGBMvect3)
SMMGBM3<- data.frame(T1SMMGBMvect3,T2SMMGBMvect3)
SMMGBM3

##visualizing the sampling distribution of the statistics T1 and T2 from the simulted data
par(xpd=TRUE)
boxplot(T1SMMGBMvect3,main="Sampling Distribution of T1 in under SMMGBM hypothesis for lambda=15",sub="The red point is the empirical value of T1 obtained from the data in hand")
points(12,col="red")

par(xpd=TRUE)
boxplot(T2SMMGBMvect3,main="Sampling distribution of T2 under SMMGBM hypothesis for lambda=15",sub="The red point is the empirical value of T2 obtained from the data in hand")
points(12.767145,col="red")


##Both the boxplots provide us with some visual evidence in favour of the null hypothesis that the data in hand comes from a SMMGBM under the appropriate Cp class


##we can do primary comparison seeing the graph

##The graph shows that the point is in the range

##now we shall do the hypothesis tesing

##The null hypothesis is Ho:The data in hand comes from a Cp class of GBM where Cp class here is the sigleton (-0.2230525,0.2556539)

z1smm3<-0
for (i in 1:21)
{
  if(t[1]>=T1SMMGBMvect3[i])
    z1smm3[i]=0
  if(t[1]<T1SMMGBMvect3[i])
    z1smm3[i]=1
}
z1smm3
sum(z1smm3)
zsm3<- min(sum(z1smm3),21-sum(z1smm3))/21
zsm3

z2smm3<- 0
for(i in 1:21)
{
  if(t[2]>=T2SMMGBMvect3[i])
    z2smm3[i]=0
  if(t[2]<T2SMMGBMvect3[i])
    z2smm3[i]=1
}
z2smm3
sum(z2smm3)
wsm3<- min(sum(z2smm3),(20-sum(z2smm3)))/21
wsm3

alpha_value_SMMGBM3<- max(zsm3,wsm3)*2
alpha_value_SMMGBM3
conf_coeff_smmgbm3<- 100*(1-alpha_value_SMMGBM3)
conf_coeff_smmgbm3

##Final testing for SMMGBM

alpha_value_SMMGBM<- c(0.0952381,0.2380952,0.1904762)
conf_coeff_smmgbm<- min((1-alpha_value_SMMGBM))
conf_coeff_smmgbm



t.test(alpha_value_SMMGBM,mu=0,alternative = "greater",level=conf_coeff_smmgbm)

Hence we aceept the null hypothesis that the data comes from Cp class of SMMGBM


##Visual comparison using the boxplot
library(asbio)
bv.boxplot(T1SMMGBMvect1,T2SMMGBMvect1,xlab = "T1",ylab="T2")
points(12.7,12,col="red")
title(main="Joint distribution of(T1,T2) under SMMGBM hypothesis",sub="lambda=5")

bv.boxplot(T1SMMGBMvect2,T2SMMGBMvect2,xlab = "T1",ylab="T2")
points(12.7,12,col="red")
title(main="Joint distribution of(T1,T2) under SMMGBM hypothesis",sub="lambda=10")

bv.boxplot(T1SMMGBMvect3,T2SMMGBMvect3,xlab = "T1",ylab="T2")
points(12.7,12,col="red")
title(main="Joint distribution of(T1,T2) under SMMGBM hypothesis",sub="lambda=15")

##We may conclude that the data comes from SMMGBM model with binary regime switch setup since in most of the cases the red point falls inside the inner hinge

