##recording the value of the Test1GBM and Test2GBM running the code 10 times and getting a 21 element vector
T1GBMvect=c(3.272727, 7.2, 12, 7.2, 5.142857, 3.6, 2, 5.142857, 4, 5.142857, 6, 5.142857, 5.2, 4.5, 6, 5.142857, 4.5, 6, 5.142857, 5.142857, 6) ##recording 10 values of T1
T2GBMvect=c(2.935674, 10.61603, 14.93318, 7.463243, 5.580579, 5.501515,1.414214 , 5.580579, 5.17204, 4.634241, 7.42967, 5.580579, 4.207137,3.116775, 5.291503, 6.41427, 4.780914, 3.162278, 6.542899, 5.398412, 6.663332) ##recording 10 values of T2

GBM<- data.frame(T1GBMvect,T2GBMvect)
GBM

##visualizing the sampling distribution of the statistics T1 and T2 from the simulted data

boxplot(T1GBMvect,main="Sampling Distribution of T1 in under GBM hypothesis",sub="The red point is the empirical value of T1 obtained from the data in hand")
points(12,col="red")

boxplot(T2GBMvect,main="Sampling distribution of T2 under GBM hypothesis",sub="The red point is the empirical value of T2 obtained from the data in hand")
points(12.7671453,col="red")


##Both the boxplots provide us with enough visual evidence against the null hypothesis that the data in hand comes from a simple GBM under the appropriate Cp class

##we can do primary comparison seeing the graph

##The graph shows that the point may be a potential outlier

##now we shall do the hypothesis tesing

##The null hypothesis is Ho:The data in hand comes from a Cp class of GBM where Cp class here is the sigleton (-0.2230525,0.2556539)

t<- c(mean(d),sd(d),skewness(d),kurtosis(d))
t
t[2]

z1<-0
for (i in 1:21)
{
  if(t[1]>=T1GBMvect[i])
    z1[i]=0
  if(t[1]<T1GBMvect[i])
    z1[i]=1
}
z1
sum(z1)
z<- min(sum(z1),21-sum(z1))/21
z

z2<- 0
for(i in 1:21)
{
  if(t[2]>=T2GBMvect[i])
    z2[i]=0
  if(t[2]<T2GBMvect[i])
    z2[i]=1
}
z2
sum(z2)
w<- min(sum(z2),(20-sum(z2)))/21
w

alpha_value_GBM<- max(z,w)*2
alpha_value_GBM
conf_coeff_gbm<- 100*(1-alpha_value_GBM)
conf_coeff_gbm

##note that here the p-admissible class is a singleton so this would be taken as our chosen level of significance

##We shall reject the aforementioned null hypothesis at 90.47619% level, if the value of alpha_value_GBM is reasonably small


##This reduces to comparing at 90% level of significance if the values 0.09 and 0 are significantly different? Since we have only one value of alpha_value_GBM


##Visual comparison using bivariate boxplot
library(asbio)
bv.boxplot(T1GBMvect,T2GBMvect,xlab = "T1",ylab="T2")
points(12,12.7,col="red")
title(main="Joint Distribution of (T1,T2) under GBM hypothesis")

##Since the red point falls way far from the outer hinge it is surely an outlier