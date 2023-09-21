rm(list=ls())#libere l'espace memoire des variables pr�cedemment utilis�es
#setwd("D:/R/datasets")
install.packages("plotly")
install.packages("hrbrthemes")
install.packages("TSstudio")
install.packages("gridExtra")
#install.packages(devtools)
#library(ggforce)

#library(plot3D)
#library(rayshader)
#library(rgl)
#library(plot3Drgl)
#library(matlib)
#library(mvtnorm)
#devtools::install_github("RamiKrispin/TSstudio")
library(plotly)
library(TSstudio) #Manipuler des s�ries temporelles
library(tidyverse)
library(gridExtra)
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
matrice <- matrix (c(ts.1, ts.2) , nrow = 12) #concat�nation en 2D avec 12 lignes. pas tr�s intuitif, on utilisera plut�t un dataframe
#mani�re alternative d'�crire une time serie trimestrielle
ts1 <- ts (101:106 , freq =4, start =2012.75)
#la fonction lag permet de d�caler une s�rie. Ici, avance de 1 trimestre
ts1L=stats::lag(ts1,-1)
print(ts1L)

#renvoit les valeurs des diff�rentes time series l� ou les deux ont des valeurs
ts.intersect (ts1 , stats::lag (ts1 , -1))
#renvoit les valeurs des diff�rentes time series l� ou au moins une s�rie � des valeurs (met des NaN pour le reste)
ts.union (ts1 , stats::lag (ts1 , -1))
#meme resultat que avec l'union
cbind (ts1 , stats::lag(ts1 , -1))
#Dates
#format classique YYYY-MM-DD ou YYYY/MM/DD
date1=as.Date("2002/03/19")
#On peut rentrer dans un autre format en sp�cifiant ce dernier
date2=as.Date("19/03/2002",format="%d/%m/%Y")#%Y avec Y en UPCASe, d et m en lowercase
setequal(date1,date2) #v�rif que �a donne les m�mes r�sultats
#Extraire le nombre de jours entre deux dates
period <- as.Date (c("2007-06-22", "2004-02-13")) #marche aussi avec des vecteurs
# number of days between 6/22/07 and 2/13/04
days <- period[1] - period[2]
print(days)

data(USgas) #r�cup�re une s�rie temporelle
ts_info(USgas) #donne des infos sur la s�rie temporelle s�lectionn�e
ts_plot(USgas) #fait un plot interactif (bien moche)
#avec des l�gendes et titres
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
acf(u,plot=TRUE, lag.max=50)
#=1 at time 0  which is logic, correlated with itself. After it, correlations are really small, in adequation with white noise definition
#� part la premi�re, quasiement toutes les autocorr�lations sont statistiquement �gales � 0
#1-b
#cr�ation de la marche al�atoire

#Cumsum (somme la valeur pr�cedante pour la valeur actuelle), surement meilleure
y=rnorm(1000,0,1.7)
y[1]=3 #seed
y=cumsum(y)
#plots
y=ts(y,start=0,freq=1)
ts_plot(y,title="time serie of a random walk with 1000 steps")
#estimation de la densit� avec la m�thode du Kernel
#conversion en df
y_df=data.frame(Date=time(y),Val=as.vector(y))
view(y_df)
y_df%>%
  ggplot(aes(x=Val))+geom_density(col='blue')+
  labs(title="Estimated density of a random walk")
# la densit� a � peu pr�s la forme d'une courbe en cloche mais ce n'est pas une gaussienne pour autant (il y a des lepto curtic)
#ces bosses pour les rendements repr�sentent les crash 
acf(y,plot=TRUE)
#on a une autocorrelation forte mais d�croissante. Ceci est une cons�quence du fait que phi=1, donc processus non stationnaire

#LjumbBox test
#voir la feuille, m puissance du test, n nombres d'observations =1000 et k le lag

#Exercice 2
#importation
CAC40=read.csv("cac40.csv", header = TRUE, sep = ";",
               fill = TRUE)%>%
  mutate(Name=as.Date(Name,origin='1900/01/01'))%>% 
  rename(DATE=Name,HERMES=`HERMES.INTL.`)%>%
  select(DATE,HERMES,SANOFI)%>%
  mutate(DR_HERMES=(HERMES-lag(HERMES))/lag(HERMES))%>%
  mutate(DR_SANOFI=(SANOFI-lag(SANOFI))/lag(SANOFI))%>%
  mutate(DR_HERMES_SQ=DR_HERMES^2)%>%
  mutate(DR_SANOFI_SQ=DR_SANOFI^2)
  
#visualisation
view(CAC40) #La 1ere colonne sont les dates, il faut trouver un bon format
print(sapply(CAC40,class)) #types des variables
#plot HERMES

#Cours de Hermes et Sanofi
p1=CAC40%>%
  ggplot(aes(x=DATE,y=HERMES))+geom_line(col='red')
ggplotly(p1)

p2=CAC40%>%
  ggplot(aes(x=DATE,y=SANOFI))+geom_line(col='blue')
ggplotly(p2)
#Densité des rendements de Hermes et Sanofi
p3=CAC40%>%
  ggplot(aes(x=DR_HERMES))+geom_density(col='red')
ggplotly(p3)

p4=CAC40%>%
  ggplot(aes(x=DR_SANOFI))+geom_density(col='blue')
ggplotly(p4)
#Quantile plot
aH=sd(CAC40$DR_HERMES,na.rm=TRUE)
bH=mean(CAC40$DR_HERMES,na.rm=TRUE)
p5=CAC40%>%
  ggplot(aes(sample=DR_HERMES))+stat_qq(col='red')+
  geom_abline(intercept=bH,slope=aH,col='black')

qqnorm(CAC40$DR_HERMES,col='red')
qqline(CAC40$DR_HERMES)

aS=sd(CAC40$DR_SANOFI,na.rm=TRUE)
bS=mean(CAC40$DR_SANOFI,na.rm=TRUE)
p6=CAC40%>%
  ggplot(aes(sample=DR_SANOFI))+stat_qq(col='blue')+
  geom_abline(intercept=bS,slope=aS,col='black')

qqnorm(CAC40$SANOFI,col='blue')
qqline(CAC40$SANOFI)

x11()
grid.arrange(p3,p4,p5,p6,nrow=2,ncol=2)


x11()
p1
x11()
p2
x11()
p3
x11()
p4



#question 5 on met les rendements au carr�. On devrait observer des cluster
