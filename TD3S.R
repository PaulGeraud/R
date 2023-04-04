rm(list=ls())
install.packages("plot3D")
install.packages("rgl")
install.packages("plot3Drgl")
install.packages("readr")
setwd("D:/R/datasets")
library(tidyverse)
library(matlib)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(readr) #library used to import the file

#partie 1 analytique sur un jeu de donnée réduit
circ=c(36,42,33,39,43,34,37,41,27,30) #x
ht=c(18.25,19.75,16.5,18.25,19.5,16.25,17.25,19,16.25,17.5) #y

mx=mean(circ)
my=mean(ht)

vx=var(circ)*9/10
vy=var(ht)*9/10
cxy=cov(circ,ht)*9/10

B1=cxy/vx

B0=my-B1*mx

data2=data.frame(circ,ht)
plot(data2)

reg=lm(ht~circ) #5
summary(reg)

htpred=B1*circ+B0
res=(htpred-ht)^2
scr=sum(res)
sct=vy*10
sce=sum((htpred-my)^2)
R2=sce/sct

xpred=c(26,35,39)
ypred=B1*xpred+B0

#partie 2

#import eucalyptus
data <- read_delim("https://regression-avec-r.github.io/donnees/eucalyptus.txt", delim = ";")
data=data%>%
  select(numero,ht,circ)
