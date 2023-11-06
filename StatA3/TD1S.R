rm(list=ls())
install.packages("plot3D")
install.packages("rgl")
install.packages("plot3Drgl")
setwd("D:/R/datasets")
library(tidyverse)
library(matlib)
library(plot3D)
library(rgl)
library(plot3Drgl)

#----------------------------------------------------------------------------------------------------------------------
#Ex 1
n=4
p=1/2
x=c(-1,-0.001,0,0.999,1,1.999,2,2.999,3,3.999,4,4.5)
y=pbinom(x,n,p)
xq=c(0.0625,0.3125,0.6875,0.9375,1)
yq=qbinom(xq,n,p)
#sans package
plot(x,y,type="l",main="FdR d'une loi binomiale n=4 p=1/2")
plot(xq,yq,cex=0.7)
#avec ggplot
data.frame(x,y)%>%
  ggplot(aes(x,y))+geom_line(col="black")+
  theme_classic()+
  labs(title="FdR d'une loi binomiale n=4 p=1/2")

data.frame(xq,yq)%>%
  ggplot(aes(xq,yq))+geom_point(col="black")+
  theme_classic()+
  labs(title="Fonction des quantiles d'une loi binomiale n=4 p=1/2",x="x",y="y")+
  scale_x_continuous(breaks = seq(0,1,0.0625),limits=c(0,1))

#N=93414
#test unilatéral H0 p=3/4 vs H1 p>3/4
n=100
p=0.75
Pval83=1-pbinom(82,n,p)#83-1
binom.test(x=83,n,p,alternative="greater")
Pval79=1-pbinom(78,n,p)#79-1
binom.test(x=79,n=100,p=0.75,alternative="greater")
#test bilatéral H0 p=1/2 vs H1 p!=1/2
n=100
p=0.5
PvalBilat59=2*(1-pbinom(58,100,0.5))#59-1
binom.test(x=59,n,p,alternative = "two.sided")
PvalBilat61=2*(1-pbinom(60,100,0.5))#61-1
binom.test(x=61,n,p,alternative = "two.sided")



#------------------------------------------------------------------------------------------------------------------------------------
#Ex 2
x=c(-1,0.659,0.66,1.049,1.05,1.919,1.92,3.509,3.51,4)
y=c(0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1)
real=c(0.66,1.05,1.92,3.51)
mu=mean(real)
lambda=1/mu
sigmaSq=sd(real)
sigmaSqNC=sigmaSq*3/4
plot(x,y,type="l",main="FdR empirique")

df=data.frame(x,y)
colors=c("Empirique"="black","Normale"="red","Exponentielle"="blue")
df%>%
  ggplot()+
  geom_line(aes(x,y,color="Empirique"))+
  stat_function(aes(x,color="Normale"),fun=function(x) pnorm(x,mean=mu,sd=sigmaSq))+
  stat_function(aes(x,color="Exponentielle"),fun=function(x) pexp(x,rate=lambda))+
  theme_classic()+
  labs(title="FdR de la loi empirique et comparaison aux FdR usuelles",color="Legend")+
  scale_color_manual(values=colors)

#Kolmogorov-Smirnov avec loi normales
K1=max(0.25-pnorm(0.66,mean=mu,sd=sigmaSq),pnorm(0.66,mean=mu,sd=sigmaSq))
K2=max(0.5-pnorm(1.05,mean=mu,sd=sigmaSq),pnorm(1.05,mean=mu,sd=sigmaSq)-0.25)
K3=max(0.75-pnorm(1.92,mean=mu,sd=sigmaSq),pnorm(1.92,mean=mu,sd=sigmaSq)-0.5)
K4=max(1-pnorm(3.51,mean=mu,sd=sigmaSq),pnorm(3.51,mean=mu,sd=sigmaSq)-0.75)
KS=max(K1,K2,K3,K4)
ks.test(real,"pnorm",mu,sigmaSq)
#Kolmogorov-Smirnov avec loi exponentielles
K1=max(0.25-pexp(0.66,rate=lambda),pexp(0.66,rate=lambda))
K2=max(0.5-pexp(1.05,rate=lambda),pexp(1.05,rate=lambda)-0.25)
K3=max(0.75-pexp(1.92,rate=lambda),pexp(1.92,rate=lambda)-0.5)
K4=max(1-pexp(3.51,rate=lambda),pexp(3.51,rate=lambda)-0.75)
KS=max(K1,K2,K3,K4)
ks.test(real,"pexp",lambda)
#---------------------------------------------------------------------------------------------------------------------------------
#Exo à part
lot=c(392,396,386,389,388,387,403,397,401,391,400,402,394,406,406,400)
m=mean(lot)
s=sd(lot)


