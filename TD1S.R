rm(list=ls())
setwd("D:/R/datasets")
library(tidyverse)
library(matlib)
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
  ggplot(aes(x,y))+geom_area(fill="white",col="black")+
  theme_classic()+
  labs(title="FdR d'une loi binomiale n=4 p=1/2")

data.frame(xq,yq)%>%
  ggplot(aes(xq,yq))+geom_point(col="black")+
  theme_classic()+
  labs(title="Fonction des quantiles d'une loi binomiale n=4 p=1/2",x="x",y="y")+
  scale_x_continuous(breaks = seq(0,1,0.0625),limits=c(0,1))

#N=93414
n=100
p=0.75
Pval83=1-pbinom(83,n,p)
Pval79=1-pbinom(79,n,p)

x1=c(rep(1,79),rep(0,21))
t.test(x1,mu=0.75,alternative="two.sided")
x2=c(rep(1,83),rep(0,17))
t.test(x2,mu=0.75,alternative="two.sided")

#------------------------------------------------------------------------------------------------------------------------------------
#Ex 2
x=c(-1,0.659,0.66,1.049,1.05,1.919,1.92,3.509,3.51,4)
y=c(0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1)
plot(x,y,type="l",main="FdR empirique")

data.frame(x,y)%>%
  ggplot(aes(x,y))+geom_area(fill="white",col="black")+
  theme_classic()+
  labs(title="FdR d'une loi binomiale n=4 p=1/2")