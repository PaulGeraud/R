rm(list=ls())#libere l'espace memoire des variables précedemment utilisées
setwd("D:/R/datasets")
library(ggforce)
library(tidyverse)
library(plot3D)
library(rayshader)
library(rgl)
library(plot3Drgl)
library(matlib)
library(mvtnorm)

f=function(x) exp(-3*x)
I=1/3*(1-exp(-3))
N=10000


N=2^seq(1,16,0.25)

IappMC=rep(0,length(N))
k=1

for (i in N){
  a=0
  b=1
  X=runif(i)
  IappMC[k]=mean(f(X))
  k=k+1
}

ErreurMC=abs(IappMC-I)

IappRect=rep(0,length(N))
k=1

for (i in N) #on applique la méthode des rectangles pour différentes valeurs de N , pour tracer un graphe
{
  a=0
  b=1
  X=seq(a,b,length.out = i+1 ) 
  X=X[2:length(X)] #slicing pour enlever le 1er rectangle qui est en dehors de l'intervalle
  IappRect[k]=mean(f(X))#largeur =1/i , longueur = f(X). La formule de la méthode des rectangles coincide ici avec la moyenne(sum 1/i *F(X))
  k=k+1
}
ErreurRect=abs(IappRect-I)

plot(log(N),log(ErreurRect),col="blue")
points(log(N),log(ErreurMC),col="red")
ErrRect.lm=lm(log(ErreurRect)~log(N))
print(ErrRect.lm$coefficients)

#partie 3--------------------------------------------------------------------------------
lambda=1
J=pi/2
N=10000
X=rexp(N,lambda)
h=function(x,L)
{
  return(exp(L*x)/(L*(1+x^2)))
}
mean(h(X,lambda))
#résultats cohérents, mais la convergence est vraiment lente

#vitesse de convergence
lambda=0.01
N=2^seq(1,20,0.1)
Err=rep(0,length(N))
k=1
for(n in N)
{
  X=rexp(n,lambda)
  Err[k]=abs(mean(h(X,lambda))-J)
  k=k+1
}
regression=lm(log(Err)~log(N))
plot(log(N),log(Err))
abline(regression,col="red")
regression
#la pente est très faible, pente de -0.1
#diminuer la valeur de lamba augmente la vitesse de convergence (0.01 / 0.09 sont de bonnes valeurs)
#question 5
Lambdas=2^seq(-10,1,0.1)
Err=rep(0,length(Lambdas))
k=1
for(L in Lambdas)
{
  X=rexp(n,L)
  Err[k]=abs(mean(h(X,L))-J)
  k=k+1
}
plot(log(Lambdas),log(Err))
#On minimise l'erreur avec un lambda entre exp(-3) et exp(-2)
