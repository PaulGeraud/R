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
#Ex1
g=function(x){return(exp(x))}
n=10000
X=runif(n)
Estimation=mean(g(X))
Estimation
I=exp(1)-1
#valeur cohérente
#var théorique et estimation
(-exp(2)+4*exp(1)-3)/2
var(g(X))

#Question 7: création de Sn
X=runif(n)
Y=1-X
EstimationAnti=mean((g(X)+g(Y))/2)
EstimationAnti
#la variance, donc la vitesse de convergence a été réduite
#-----------------------------------------------------------------------------------------
#Ex2
rm(list=ls())
#à l'ordre 1
I=exp(1)-1
g=function(x){return(exp(x))}
n=10000
b=1.5
P=function(x){return(1+x)}
X=runif(n)
Tn=mean(g(X)-P(X)+b)
var(g(X)-P(X)+b)
-exp(2)/2 +3*exp(1) -53/12 #valeur théorique
#à l'ordre 3
n=10000
b=41/24
P=function(x){return(1+x+x^2/2+x^3/6)}
X=runif(n)
Tn=mean(g(X)-P(X)+b)
var(g(X)-P(X)+b)
0.00018 #valeur théorique approchée
#combinaison variable contrôle +antithétiques
n=10000
b=41/24
P=function(x){return(1+x+x^2/2+x^3/6)}
X=runif(n)
T=((g(X)-P(X)+b)+(g(1-X)-P(1-X)+b))/2
mean(T)
var(T) #la variance a bien diminuée
#question 9 Meilleur résultat que pour le dl à l'ordre 1. Résultat logique: dl à l'ordre 1 =fct affine. On a pris ici la fonction affine qui minimise la variance
n=10000
X=runif(n)
a=18-6*exp(1)
T=g(X)-a*X+a/2
mean(T)
#question 10
n=100
A=runif(n)
B=g(A)
plot(A,B)
regression=lm(B~A)
abline(regression,col='red')
regression
a=regression$coefficients[2] #valeur du coeff directeur
b=regression$coefficients[1]# valeur du coeff à l'origine
var(T)
#question 11
a=regression$coefficients[2] #valeur du coeff directeur
n=10000
X=runif(n)
a=18-6*exp(1)
T=g(X)-a*X+a/2 #les b s'annulent pour enlever le biais
mean(T)
var(T)
