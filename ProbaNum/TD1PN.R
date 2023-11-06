rm(list=ls())#libere l'espace memoire des variables précedemment utilisées
setwd("D:/R/datasets")
install.packages("tidyverse") #Installation de la librairie tidyverse, on utilise  notamment les packages dplyr et ggplot2
install.packages("ggforce")
install.packages("plot3D")
install.packages("rayshader")
install.packages("rgl")
install.packages("plot3Drgl")
install.packages("matlib")
install.packages("mvtnorm")
library(ggforce)
library(tidyverse)
library(plot3D)
library(rayshader)
library(rgl)
library(plot3Drgl)
library(matlib)
library(mvtnorm)

#Ex 1
drawMax=function(b,n,k) #b borne sup de la loi unif, n nombre d'échantillons, k nombre de tirages/ échantillon
{
  A=rep(0,n)
  for(i in seq(1,n))
  {
    A[i]=max(runif(k,0,b))
  }
  return (A)
}
IC=function(esp,sd,n,alpha) #retourne l'intervalle de confiance sous l'hypothèse du TCL avec moyenne et variance connu pour un échantillon de taille n au risque alpha
{
  A=seq(1,2)
  z=-qnorm((alpha)/2)
  A[0]=esp-z*(sd/sqrt(n))
  A[1]=esp+z*(sd/sqrt(n))
  return (A)
}

MySample=drawMax(2,1000,100)
moy=mean(MySample)
MySd=sd(MySample)
IntConf=IC(moy,MySd,1000,0.01)
#Correction
B=matrix(runif(100*1000,0,2),nrow=100,ncol=1000) #création d'une matrice de 100 lignes et 1000 colonnes Permet de stocker tous les tirages dans une matrice
T=apply(B,2,max)#Applique la fonction max sur chaque colonne, pour le faire sur chaque ligne, remplacez le 2 par 1
b=2
n=100
N=1000
esp=n*b/(n+1)
vari=n*b^2/((n+2)*(n+1)^2)
z=qnorm(1-0.01/2)
IF=c(esp-z*sqrt(vari)/sqrt(N),esp+z*sqrt(vari)/sqrt(N)) #intervalle de fluctuation
#test de l'IF
Tbar=rep(0,1000)
for(i in 1:1000)
{
  Tbar[i]=mean(apply(matrix(runif(n*N,0,2),nrow=n,ncol=N),2,max))
}

sum(Tbar<IF[1])+sum(Tbar>IF[2]) #Permet de compter le nombre de valeurs en dehors de l'IF
#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Ex 2
N=100
Sn2=rep(0,100)
Sn2Cor=rep(0,100)
for(n in seq(100,10000,100))
{
  X=rnorm(n)
  Sn2[n/100]=var(X)*(n-1)/n
  Y=rnorm(n)
  Sn2Cor[n/100]=var(Y)
}
#correction + sous forme de lignes
plot(seq(100,10000,100),Sn2,type="l",col="red")
lines(seq(100,10000,100),Sn2Cor,col="blue")
abline(a=1,b=0,col="black") #fait une ligne droite parallèle à l'axe des abscisses
#ma version sous forme de nuage de points
plot(seq(1:100),rep(1,100),col="black",type="l",ylim=c(0.95,1.05),xlab="n*100",ylab="")
points(seq(1:100),Sn2,col="red",cex=0.5)
points(seq(1:100),Sn2Cor,col="blue",cex=0.7)

#c'est mieux de faire 2 tirages différents un sera estimé avec Sn2, l'autre avec Sn2Cor
n=100


#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#Ex 3
#estimation de pi
N=1000#number of points
x=runif(N,-1,1)
y=runif(N,-1,1)
df<-data.frame(x,y)%>%
  mutate(norm=x^2+y^2)%>%
  mutate(couleur=ifelse(x^2+y^2<1,"red","blue")) #tentative un peu flopesque
df%>% #avec packages
  ggplot(aes(x,y))+geom_circle(aes(x0 = 0,y0=0,r=1))+geom_point(aes(colour=cut(norm,c(0,1,Inf))))+scale_color_manual(name="norm",values=c("(0,1]"="red",
                                                                                                                                          "(1,Inf]"="blue"),
                                                                                                                     labels=c("Inside circle","Outside circle"))+
  theme_classic()
df%>% #version plus simple mais les couleurs sont pas bonnes
  ggplot(aes(x,y))+geom_circle(aes(x0 = 0,y0=0,r=1))+geom_point(aes(colour=ifelse(x^2+y^2<1,"red","blue")))+
  theme_classic()

#sans package
t=seq(-pi,pi,0.01)
plot(cos(t),sin(t),type="l")

couleur=ifelse(x^2+y^2<1,"red","blue")
points(x,y,col=couleur,pch=20)



couleur=ifelse(x^2+y^2<1,"red","blue")


InCircle=sum(df$norm<1)
#OutCircle=N-InCircle
PiMC=4*InCircle/N
#tracé des erreurs, on va utiliser une échelle logarithmique de base 2 par
I=2^seq(1,20,0.1) #intéressant comme manière de déclarer
Err=rep(0,length(I))
k=0
for(n in I)
{
  X=runif(n,-1,1)
  Y=runif(n,-1,1)
  Est=4*mean(X^2+Y^2<=1) # le mean fait l'équivalent du sum/n
  Err[k]=abs(Est-pi)
  k=k+1
}
plot(I,Err)
plot(log(I),log(Err))

Err.lm=lm(log(Err)~log(I)) #ça marche pas
print(Err.lm$coefficients)
abline(Err.lm,col="red")
#log(Err)=0.5*log(n) où n ets le nombre de points
#Err 1/racine(n)
#Vitesse de convergence en 1/sqrt(n) (c'est pas incroyable)


#estimation hypersphère dimension 5
N=10^6 #nombre de points N^5 tirages à réaliser
a=runif(N,-1,1)
b=runif(N,-1,1)
c=runif(N,-1,1)
d=runif(N,-1,1)
e=runif(N,-1,1)
df2<-data.frame(a,b,c,d,e)%>%
  mutate(norm=a^2+b^2+c^2+d^2+e^2)
InSphere5=sum(df2$norm<1)
V5MC=32*InSphere5/N #valeur théorique 5.26379
#Avec une matrice
T=runif(5*N,-1,1)
A=matrix(T,nrow=N,ncol=5) #chaque ligne représente un point de R^5
D=apply(A^2,1,sum) #A^2 fait que chaque valeur est mise au carré
V5MC=2^5*mean(D<=1)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#Ex 4
#Un estimateur de l'intégrale est 4x²y méthode de Monte Carlo
N=10000
x=runif(N,0,2)
y=runif(N,0,2)
df3<-data.frame(x,y)%>%
  mutate(T=4*x^2*y)
Est=mean(df3$T) #valeur réelle
#méthode de rejet
N=10000
x=runif(N,0,2)
y=runif(N,0,2)
z=runif(N,0,8)
Est2=32*mean(z<=x^2*y)
#graphes (non demandé)
x=seq(0,2,length.out=100)
y=seq(0,2,length.out=100)
myf=function(x,y)
{
  z=y*x^2
  return(z)
}
#z2=matrix(outer(x,y,myf),nrow=100,ncol=100)
#image(z2)
z=outer(x,y,myf) #stock in a matrix val of z for (x,y) in [0;2]²
colnames(z)=y  #change name of each column
rownames(z)=x  #change name of each row
df4=as.data.frame(z)%>% #convert the matrix into a data frame
  rownames_to_column(var="x")%>% #give a significant name to the column
  gather(y,value,-x)%>%
  mutate(y=as.numeric(y), 
         x=as.numeric(x),
         value_range = cut(value, 20))#ligne intéressante

#2D
df4%>%
  ggplot(aes(x=x,y=y,fill=value_range))+
  geom_raster()+
  scale_fill_manual(values=colorRampPalette(c("blue","green","red"))(20))+
  theme_classic()+
  labs(title="2D Graph of z=x^2y function on [0:2]x[0:2]")

#3D
persp3D(x,y,z,main="3D Graph of z=x^2y function on [0:2]x[0:2]",colvar=z,
        zlab="value of function",clab=c("Values","Numbers"),
        ticktype="detailed",nticks=1
)
plotrgl(lighting = TRUE, smooth = TRUE,fill=z)
rglwidget()

#other stuff-------------------------------------------------------------------------------------------------------------------

#base graph
persp3D(x,y,z)
#add some legend
persp3D(x,y,z,main="3D Graph of z=x²y function on [0:2]² ",zlab="image")
#Change view angle. Theta control left right movements, phi up/down
persp3D(x,y,z,main="3D Graph of z=x²y function on [0:2]² ",zlab="image"
        ,theta=45,phi=45)
#Scale shafts
persp3D(x,y,z,main="3D Graph of z=x²y function on [0:2]² ",zlab="image"
        ,theta=45,phi=45,
        color="purple",shade=0.4)
persp3D(x,y,z,main="3D Graph of z=x²y function on [0:2]² ",zlab="image"
        #turn off box
        ,theta=45,phi=45,
        box=F)
#turn on Axes (doesnt work ;(  )
persp3D(x,y,z,main="3D Graph of z=x²y function on [0:2]² ",zlab="image"
        ,theta=45,phi=45,
        box=T,axes=T,nticks=10,ticktype='detailed')
#create a color palette
my.color<-colorRampPalette(c("blue","red"))
mycolors<-my.color(100)
nrow.z<-dim(z)[1]
ncol.z<-dim(z)[2]
zfacet<-z[-1,-1]+z[-1,-ncol.z]+z[-nrow.z,-1]+z[-nrow.z,-ncol.z]
facet.color<-cut(zfacet,100)
persp3D(x,y,z,main="3D Graph of z=x²y function on [0:2]² ",zlab="image"
        ,theta=45,phi=45,
        col=mycolors[facet.color],shade=0.9)
#rgl package
View(iris)
with(iris,plot3d(x,y,z),type="s")
persp3d(x,y,z,type="p",col=mycolors[facet.color])

#funny
with(quakes, 
     scatter3D(x=long, y=lat, z=-depth, colvar=mag, pch=16, cex=1.5, 
               xlab="longitude", ylab="latitude", zlab="depth, km", 
               clab=c("Richter", "Magnitude"), main="Earthquakes off Fiji", 
               ticktype="detailed", theta=10, d=2, 
               colkey=list(length=0.5, width=0.5, cex.clab=2))
)
plotrgl(lighting = TRUE, smooth = TRUE, cex=2)
rglwidget()

persp3D(x,y,z,main="3D Graph of z=x²y function on [0:2]² ",zlab="image"
        ,theta=45,phi=45,
        box=T,axes=T,nticks=10,ticktype='detailed')

persp3D(x,y,z,main="3D Graph of z=x^2y function on [0:2]x[0:2]",colvar=z,
        zlab="value of function",clab=c("Values","Numbers"),
        ticktype="detailed",nticks=1
)
plotrgl(lighting = TRUE, smooth = TRUE)

#plot a gaussian vector


x=seq(-5,5,length.out=100)
y=seq(-5,5,length.out=100)

moy=c(0,0) #vecteur d'espérance
sig=matrix(c(4,0,0,4),nrow=2) #matrice de covariance. Elle doit être positive,inversible et à déterminant positif
z=matrix(0,100,100)
for(i in 1:100)
{
  for(j in 1:100)
  {
    z[j,i]=dmvnorm(c(x[j],y[i]),moy,sig,checkSymmetry = TRUE) #LA SYNTAXE
  }
}
persp3D(x,x,z,xlab="x",ylab="y",zlab="",box=T,axes=T,ticktype="detailed")
plotrgl(lighting = TRUE, smooth = TRUE,fill=z)

rglwidget()







Isig=inv(sig)
GVect=function(x,y)
{
  a=c(x[0]-moy[0],y[0]-moy[1])
  z=(1/sqrt(2*pi))^2*1/sqrt(sig)*exp(-(a%*%Isig%*%t(a))/2)
  return (z)
}
VG=Vectorize(GVect)






