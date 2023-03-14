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
#------------------------------------------------------------------------------------------------------------------------------------------
#Ex1
mu0=6
muE=7
n=20
sdE=2.4 #écart type non corrigé
#partie1 (entreprise)
qt(0.95,df=n-1) #quantile de la loi de Student . df= degree of freedom
pval=pt(1.81,df=n-1,lower.tail = FALSE) #probas de la loi de STudent
#partie2 (association)
qt(0.05,df=n-1)
pt(-1.81,df=n-1)
