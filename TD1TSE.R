rm(list=ls())#libere l'espace memoire des variables précedemment utilisées
setwd("D:/R/datasets")
install.packages(devtools)
library(ggforce)
library(tidyverse)
library(plot3D)
library(rayshader)
library(rgl)
library(plot3Drgl)
library(matlib)
library(mvtnorm)
devtools::install_github("RamiKrispin/TSstudio")
#install.packages("TSstudio")
library(TSstudio) #Manipuler des séries temporelles
install.packages("hrbrthemes")
library(hrbrthemes)

#getting familiar with time series
tserie=ts(rnorm(100),start=c(2000,1),frequency = 4)
print(tserie)
#substetting time serie
subt=window(tserie,start=c(2000,1),end=c(2012,1))
print(subt)
#2D time serie
ts.1 <- c (799 ,1174.8 ,865.1 ,1334.6 ,635.4 ,918.5 ,685.5 ,998.6 ,784.2 ,985 ,882.8 ,1071)
ts.2 <- c (655 ,1306.9 ,1323.4 ,1172.2 ,562.2 ,824 ,822.4 ,1265.5 ,799.6 ,1105.6 ,1106.7 ,1337.8)
matrice <- matrix (c(ts.1, ts.2) , nrow = 12) #concaténation en 2D avec 12 lignes. pas très intuitif, on utilisera plutôt un dataframe
#manière alternative d'écrire une time serie trimestrielle
ts1 <- ts (101:106 , freq =4, start =2012.75)
#la fonction lag permet de décaler une série. Ici, avance de 1 trimestre
ts1L=stats::lag(ts1,-1)
print(ts1L)

#renvoit les valeurs des différentes time series là ou les deux ont des valeurs
ts.intersect (ts1 , stats::lag (ts1 , -1))
#renvoit les valeurs des différentes time series là ou au moins une série à des valeurs (met des NaN pour le reste)
ts.union (ts1 , stats::lag (ts1 , -1))
#meme resultat que avec l'union
cbind (ts1 , stats::lag(ts1 , -1))
#Dates
#format classique YYYY-MM-DD ou YYYY/MM/DD
date1=as.Date("2002/03/19")
#On peut rentrer dans un autre format en spécifiant ce dernier
date2=as.Date("19/03/2002",format="%d/%m/%Y")#%Y avec Y en UPCASe, d et m en lowercase
setequal(date1,date2) #vérif que ça donne les mêmes résultats
#Extraire le nombre de jours entre deux dates
period <- as.Date (c("2007-06-22", "2004-02-13")) #marche aussi avec des vecteurs
# number of days between 6/22/07 and 2/13/04
days <- period[1] - period[2]
print(days)

data(USgas) #récupère une série temporelle
ts_info(USgas) #donne des infos sur la série temporelle sélectionnée
ts_plot(USgas) #fait un plot interactif (bien moche)
#avec des légendes et titres
ts_plot(USgas,
          title="US_Monthly_Natural_Gas_Consumption ",
          Xtitle="Time",
          Ytitle = "Billion_Cubic_Feet")
#en rajoutant un slider
ts_plot(USgas,
        title="US_Monthly_Natural_Gas_Consumption ",
        Xtitle="Time",
        Ytitle = "Billion_Cubic_Feet",
        slider=TRUE)
#avec des marqueurs
ts_plot(USgas,
        title="US_Monthly_Natural_Gas_Consumption ",
        Xtitle="Time",
        Ytitle = "Billion_Cubic_Feet",
        line.mode="lines+markers")

data("Coffee_Prices")
ts_info(Coffee_Prices)
ts_plot(Coffee_Prices,
        type="multiple")

#Exercices
obs=rnorm(1000,0,1.7)
#1-a
u=ts(obs,start=0,frequency =1)
x11()
ts_plot(u,title="time serie of 1000 white noise")
#autocorrelation
acf(u,plot=TRUE)
#=1 at time 0  which is logic, correlated with itself. After it, correlations are really small, in adequation with white noise definition
#à part la première, quasiement toutes les autocorrélations sont statistiquement égales à 0
#1-b
#création de la marche aléatoire
y=rep(3,1000) #le 1er paramètre est la seed
for (i in 2:1000)
{
  y[i]=y[i-1]+obs[i]
}

#Autre méthode avec cumsum (somme la valeur précedante pour la valeur actuelle), surement meilleure
y2=obs
y2[1]=3 #seed
y2=cumsum(y2)
#plots
y=ts(y,start=0,freq=1)
ts_plot(y,title="time serie of a random walk with 1000 steps")
#estimation de la densité avec la méthode du Kernel
#conversion en df
y_df=data.frame(Date=time(y),Val=as.vector(y))
view(y_df)
y_df%>%
  ggplot(aes(x=Val))+geom_density(col='blue')+theme_classic()+
  labs(title="Estimated density of a random walk")
# la densité a à peu près la forme d'une courbe en cloche mais ce n'est pas une gaussienne pour autant (il y a des lepto curtic)
#ces bosses pour les rendements représentent les crash 
acf(y,plot=TRUE)
#on a une autocorrelation forte mais décroissante. Ceci est une conséquence du fait que phi=1, donc processus non stationnaire

#LjumbBox test
#voir la feuille, m puissance du test, n nombres d'observations =1000 et k le lag

#Exercice 2
#importation
CAC40=read.csv("cac40.csv", header = TRUE, sep = ";", quote = "\"", dec = ".",
                     fill = TRUE, comment.char = "")
#visualisation
view(CAC40) #La 1ere colonne sont les dates, il faut trouver un bon format
print(sapply(CAC40,class)) #types des variables
#plot 3rd col
CAC40%>%
  ggplot(aes(x=Name,y="HERMES.INTL."))+geom_line(col='red')+
  ylim(250,300)
#question 3 il faut faire les opérations sur les RENDEMENTS PAS LES PRIX

#question 5 on met les rendements au carré. On devrait observer des cluster