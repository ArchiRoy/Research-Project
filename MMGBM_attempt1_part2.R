##For lambda1=10

##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 21 element vector
T1MMGBMvect1=c(3.6, 4.5, 7.2, 8, 9,  4.714286, 3.272727, 6, 6, 4.5, 3.272727, 7.2, 3, 3.272727, 5.142857, 4.5, 5.142857, 2.769231, 5.142857, 5.142857, 4.54) ##recording 21 values of T1
T2MMGBMvect1=c(5.316641, 5.879747,9.121403, 12.12436, 8.041559, 4.644505, 4.776838, 6.811755, 5.51362, 4.375255, 2.148996, 6.418723, 3.133398, 3.165151, 9.668309, 2.77746, 6.36209, 1.964427, 4.561746, 5.047394, 6.770122) ##recording 21 values of T2

length(T1MMGBMvect1)
length(T2MMGBMvect1)
MMGBM<- data.frame(T1MMGBMvect1,T2MMGBMvect1)
MMGBM

##visualizing the sampling distribution of the statistics T1 and T2 from the simulted data
par(xpd=TRUE)
boxplot(T1MMGBMvect1,main="Sampling Distribution of T1 in under MMGBM hypothesis for lambda=10",sub="The red point is the empirical value of T1 obtained from the data in hand")
points(12,col="red")

par(xpd=TRUE)
boxplot(T2MMGBMvect1,main="Sampling distribution of T2 under MMGBM hypothesis for lambda=10",sub="The red point is the empirical value of T2 obtained from the data in hand")
points(12.767145,col="red")


##Both the boxplots provide us with enough visual evidence in favour of the null hypothesis that the data in hand comes from a simple GBM under the appropriate Cp class

##we can do primary comparison seeing the graph

##The graph shows that the point may be a potential outlier

##now we shall do the hypothesis tesing

##The null hypothesis is Ho:The data in hand comes from a Cp class of GBM where Cp class here is the sigleton (-0.2230525,0.2556539)

t<- c(mean(d),sd(d),skewness(d),kurtosis(d))
t
t[2]

z1mm1<-0
for (i in 1:21)
{
  if(t[1]>=T1MMGBMvect1[i])
    z1mm1[i]=0
  if(t[1]<T1MMGBMvect1[i])
    z1mm1[i]=1
}
z1mm1
sum(z1mm1)
zmm1<- min(sum(z1mm1),21-sum(z1mm1))/21
zmm1

z2mm1<- 0
for(i in 1:21)
{
  if(t[2]>=T2MMGBMvect1[i])
    z2mm1[i]=0
  if(t[2]<T2MMGBMvect1[i])
    z2mm1[i]=1
}
z2mm1
sum(z2mm1)
wmm1<- min(sum(z2mm1),(20-sum(z2mm1)))/21
wmm1

alpha_value_MMGBM1<- max(zmm1,wmm1)*2
alpha_value_MMGBM1
conf_coeff_mmgbm1<- 100*(1-alpha_value_MMGBM1)
conf_coeff_mmgbm1


##For lambda1=5

##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 21 element vector
T1MMGBMvect2=c(6,3.6,4.5, 6, 3.6, 3, 3.6, 12, 6, 6,4.5, 5.142857, 6,7.2,4,5.142857,3, 6, 4.5,6,4.5) ##recording 21 values of T1
T2MMGBMvect2=c(6.164414,4.765618, 4.070802, 3.63318,5.561774,2.558409,2.54733,11,7.375636, 7.668116, 5.976143,3.023716, 4.049691,4.266146, 5.361903,7.174691,4.285281,6.78233,4.503967,6.033241,3.625308) ##recording 21 values of T2

length(T1MMGBMvect2)
length(T2MMGBMvect2)
MMGBM2<- data.frame(T1MMGBMvect2,T2MMGBMvect2)
MMGBM2

##visualizing the sampling distribution of the statistics T1 and T2 from the simulted data
par(xpd=TRUE)
boxplot(T1MMGBMvect2,main="Sampling Distribution of T1 in under MMGBM hypothesis for lambda=5",sub="The red point is the empirical value of T1 obtained from the data in hand")
points(12,col="red")

par(xpd=TRUE)
boxplot(T2MMGBMvect2,main="Sampling distribution of T2 under MMGBM hypothesis for lambda=5",sub="The red point is the empirical value of T2 obtained from the data in hand")
points(12.67145,col="red")


##Both the boxplots provide us with enough visual evidence against the null hypothesis that the data in hand comes from a simple GBM under the appropriate Cp class

##we can do primary comparison seeing the graph

##The graph shows that the point may be a potential outlier

##now we shall do the hypothesis tesing

##The null hypothesis is Ho:The data in hand comes from a Cp class of GBM where Cp class here is the sigleton (-0.2230525,0.2556539)

t<- c(mean(d),sd(d),skewness(d),kurtosis(d))
t
t[2]

z1mm2<-0
for (i in 1:21)
{
  if(t[1]>=T1MMGBMvect2[i])
    z1mm2[i]=0
  if(t[1]<T1MMGBMvect2[i])
    z1mm2[i]=1
}
z1mm2
sum(z1mm2)
zmm2<- min(sum(z1mm2),21-sum(z1mm2))/21
zmm2

z2mm2<- 0
for(i in 1:21)
{
  if(t[2]>=T2MMGBMvect2[i])
    z2mm2[i]=0
  if(t[2]<T2MMGBMvect2[i])
    z2mm2[i]=1
}
z2mm2
sum(z2mm2)
wmm2<- min(sum(z2mm2),(20-sum(z2mm2)))/21
wmm2

alpha_value_MMGBM2<- max(zmm2,wmm2)*2
alpha_value_MMGBM2
conf_coeff_mmgbm2<- 100*(1-alpha_value_MMGBM2)
conf_coeff_mmgbm2




##For lambda1=15

##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 21 element vector
T1MMGBMvect3=c(3,  5.666667,3, 6.0013, 2,8.9077,2.545455,3.0087,3,8.906,4.4,4.5,3,5,3,6,4.375,3,3,3,3) ##recording 21 values of T1
T2MMGBMvect3=c(6.148945,  4.501851, 2.327699,2.872281,1.732051,1.809068,10.14889,11.69188,4.636809,4.09878,4.806246,6.335839,4.516636,6.379655,4.059087, 5.490251,2.774244,4.966555,8.445906,11.46008,8.434623) ##recording 21 values of T2

length(T1MMGBMvect3)
length(T2MMGBMvect3)
MMGBM3<- data.frame(T1MMGBMvect3,T2MMGBMvect3)
MMGBM3

##visualizing the sampling distribution of the statistics T1 and T2 from the simulted data
par(xpd=TRUE)
boxplot(T1MMGBMvect3,main="Sampling Distribution of T1 in under MMGBM hypothesis for lambda=15",sub="The red point is the empirical value of T1 obtained from the data in hand")
points(12,col="red")

par(xpd=TRUE)
boxplot(T2MMGBMvect3,main="Sampling distribution of T2 under MMGBM hypothesis for lambda=15",sub="The red point is the empirical value of T2 obtained from the data in hand")
points(12.7671453,col="red")


##Both the boxplots provide us with enough visual evidence against the null hypothesis that the data in hand comes from a simple GBM under the appropriate Cp class

##we can do primary comparison seeing the graph

##The graph shows that the point may be a potential outlier

##now we shall do the hypothesis tesing

##The null hypothesis is Ho:The data in hand comes from a Cp class of GBM where Cp class here is the sigleton (-0.2230525,0.2556539)

t<- c(mean(d),sd(d),skewness(d),kurtosis(d))
t
t[2]

z1mm3<-0
for (i in 1:21)
{
  if(t[1]>=T1MMGBMvect3[i])
    z1mm3[i]=0
  if(t[1]<T1MMGBMvect3[i])
    z1mm3[i]=1
}
z1mm3
sum(z1mm3)
zmm3<- min(sum(z1mm3),21-sum(z1mm3))/21
zmm3

z2mm3<- 0
for(i in 1:21)
{
  if(t[2]>=T2MMGBMvect3[i])
    z2mm3[i]=0
  if(t[2]<T2MMGBMvect3[i])
    z2mm3[i]=1
}
z2mm3
sum(z2mm3)
wmm3<- min(sum(z2mm3),(20-sum(z2mm3)))/21
wmm3

alpha_value_MMGBM3<- max(zmm3,wmm3)*2
alpha_value_MMGBM3
conf_coeff_mmgbm3<- 100*(1-alpha_value_MMGBM3)
conf_coeff_mmgbm3


##Final Testing

alpha_r<- c(alpha_value_MMGBM1,alpha_value_MMGBM2,alpha_value_MMGBM3)
level_of_significance<- min(alpha_value_MMGBM1,alpha_value_MMGBM2,alpha_value_MMGBM3)
t.test(alpha_r,mu=0,alternative = "greater",conf.level = level_of_significance)


##p-value is 0.002113 which is less than our level of significance 90% so we reject the null hypothesis that the data comes from a Cp class of MMGBM models

##Comparing with bivariate boxplots

library(asbio)
bv.boxplot(T1MMGBMvect1,T2MMGBMvect1,xlab = "T1",ylab="T2")
points(12.7,12,col="red")
title(main="Joint distribution of(T1,T2) under MMGBM hypothesis",sub="lambda=10")

bv.boxplot(T1MMGBMvect2,T2MMGBMvect2,xlab = "T1",ylab="T2")
points(12.7,12,col="red")
title(main="Joint distribution of(T1,T2) under MMGBM hypothesis",sub="lambda=5")

bv.boxplot(T1MMGBMvect3,T2MMGBMvect3,xlab = "T1",ylab="T2")
points(12.7,12,col="red")
title(main="Joint distribution of(T1,T2) under MMGBM hypothesis",sub="lambda=15")

