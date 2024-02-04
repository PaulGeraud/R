#install.packages("fdrtool")
library(tidyverse)
library(ggforce)
library(gridExtra)
library(plotly)
#library(fdrtool)
#Exercice 1 , Box Muller
set.seed(1898)
Box_Muller=function(n)
{
  U_1=runif(n,0,1)
  U_2=runif(n,0,1)
  R=-2*log(U_1) #simule loi exponentielle
  Theta=2*pi*U_2 #simule unif sur (0,2pi)
  return(data.frame(N_1=sqrt(R)*cos(Theta),N_2=sqrt(R)*sin(Theta)))
}

n=1000000 #Nombres de tirages
N=Box_Muller(n)

N%>%
  ggplot(aes(x=N_1,y=N_2))+geom_point()+
  labs(title="Résultat en 2D")

#diagnostics que ce sont bien des lois normales
p1=N%>%
    ggplot(aes(x=N_1))+geom_histogram(fill="lightblue",col="black",bins = sqrt(n))+
  labs(title="Histogramme de la 1ere loi normale marginale")
p2=N%>%
    ggplot(aes(x=N_2))+geom_histogram(fill="pink",col="black",bins= sqrt(n))+
  labs(title="Histogramme de la 2eme loi normale marginale")

#Les QQPlots semblent être gourmants en mémoire
p3=N%>%
  ggplot(aes(sample=N_1))+stat_qq(col="lightblue")+
  geom_abline(intercept=0,slope=1)+
  labs(title="QQplot de la 1ere loi normale marginale")
p4=N%>%
  ggplot(aes(sample=N_2))+stat_qq(col="pink")+
  geom_abline(intercept=0,slope=1)+
  labs(title="QQplot de la 2eme loi normale marginale")
x11()
grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
grid.arrange(p1,p2,nrow=2)
ks.test(N$N_1,"pnorm")
ks.test(N$N_2,"pnorm")

#Box Muller modifié
Box_Muller_Mod=function(n)
{
  #Compte le nombres d'itérations
  nb_iter=n
  U_1=runif(n,-1,1)
  U_2=runif(n,-1,1)
  
  to_reject = U_1^2 + U_2^2 > 1
  while(any(to_reject))
  {
    #on régénère les points en dehors du cercle
    nb_iter = nb_iter + sum(to_reject)
    U_1[to_reject] = runif(sum(to_reject), -1, 1)
    U_2[to_reject] = runif(sum(to_reject), -1, 1)
    to_reject = U_1^2 + U_2^2 > 1
  }
  #proba d'être en dehors du cercle unité environ 27%. On s'attend donc à environ à 1.27n tirages
  #cat("Nombres d'itérations nécessaires: ",nb_iter,"\n")
  R=-2*log(U_1^2+U_2^2)
  return(data.frame(X=sqrt(R)*U_1/sqrt(U_1^2+U_2^2),
                    Y=sqrt(R)*U_2/sqrt(U_1^2+U_2^2)))
}
N=Box_Muller_Mod(n)

#diagnostics que ce sont bien des lois normales
p1=N%>%
  ggplot(aes(x=X))+geom_histogram(fill="lightblue",col="black",bins = sqrt(n))+
  labs(title="Histogramme de la 1ere loi normale marginale")
p2=N%>%
  ggplot(aes(x=Y))+geom_histogram(fill="pink",col="black",bins= sqrt(n))+
  labs(title="Histogramme de la 2eme loi normale marginale")

x11()
grid.arrange(p1,p2,nrow=2)

#Comparaison temps d'exécution entre Box Muller et modified Box Muller
measure_time=function(func, repetitions,n) 
{
  times = numeric(repetitions)
  for (i in 1:repetitions) 
  {
    time = system.time(func(n))[3]
    times[i] = time
  }
  return(mean(times))
}

time_BM = measure_time(Box_Muller,50,n)
time_BMM = measure_time(Box_Muller_Mod,50,n)
# Afficher les résultats
print(paste("Temps d'exécution de Box Muller: ", round(time_BM,5), "secondes")) #Environ 0.1615 secondes
print(paste("Temps d'exécution de Box Muller Modifié: ", round(time_BMM,5), "secondes")) #Environ 0.2565 secondes
#La deuxième méthode est moins performante
round(time_BMM/time_BM,5) #rapport de 1.5


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Exercice 2

#Partie I : loi demi normale

n=10^6
M=sqrt(2/pi)*exp(1/2)

#check que M est bien la bonne valeur
ToOpt=function(x)
{
  return (-abs(DensityAbsNorm(x)/dexp(x,1)))
}
val=optim(par=3,fn=ToOpt,method='Brent',lower=0,upper=100)$par
cat("M=",-val$value) #OK!

p=1/M
q=1-p
#gives number of fails
test=rnbinom(n,n,prob=p)
mean(test)
n*q/p

DensityAbsNorm=function(x)
{
    val=ifelse(x>=0,sqrt(2/pi)*exp(-x^2/2),0)
  return (val)
}

AcceptanceRejectionHalfNorm=function(n,M)
{
  #Compte le nombres d'itérations
  nb_iter=n
  U=runif(n,0,1)
  Y=rexp(n,rate=1)
  #Y=-ln(runif(n,0,1)) #inverse method
  to_reject = U>DensityAbsNorm(Y)/(M*dexp(Y))
  while(any(to_reject))
  {
    #on régénère les points en dehors du cercle
    nb_iter = nb_iter + sum(to_reject)
    U[to_reject] = runif(sum(to_reject),0, 1)
    Y[to_reject] = rexp(sum(to_reject),rate=1)
    #Y[to_reject]=-ln(runif(sum(to_reject),0,1))
    to_reject = U>DensityAbsNorm(Y)/(M*dexp(Y))
  }
  #proba d'être en dehors du cercle unité environ 27%. On s'attend donc à environ à 1.27n tirages
  cat("Nombres d'itérations nécessaires: ",nb_iter,"\n")
  return(data.frame(X=Y,U=U))
}

#environ identiques
X=AcceptanceRejectionHalfNorm(n,M)
n/p

#plots
df=data.frame(x=seq(0,5,length.out=n),X=X$X,U=X$U)%>%
  mutate(ref=DensityAbsNorm(x))


#réponse attendue
x11()
df%>%
  ggplot(aes(x=X))+geom_histogram(fill="lightblue",col="black",bins = sqrt(n))+
  labs(title="Histogram of the distribution generated with acceptance/rejection method")

#test de K.S. il faut load fdrtool
ks.test(X$X,"phalfnorm")

#Partie II: Générer une loi normale

AcceptanceRejectionfNorm=function(n,M)
{
  #on génère d'abord une loi demi-normale
  Xd=AcceptanceRejectionHalfNorm(n,M)$X
  #générer n lois de bernouilli(1/2)
  #X=ifelse(pbinom(n,p=1/2,size=1)==0,-Xd,Xd)
  X=ifelse(runif(n,0,1)<0.5,-Xd,Xd) #inversion theorem
  return (data.frame(X=X,x=seq(0,5,length.out=n)))
}

N3=AcceptanceRejectionfNorm(n,M)

a=data.frame(X=N3)

x11()
N3%>%
  ggplot(aes(x=X))+geom_histogram(fill="lightblue",col="black",bins = sqrt(n))+
  labs(title="Histogram ")

ks.test(N3$X,"pnorm")

#Comparaison

times = numeric(50)
for (i in 1:50) 
{
    time = system.time(AcceptanceRejectionfNorm(n,M))[3]
    times[i] = time
}
time_AccRej=mean(times)

print(paste("Temps d'exécution de AcceptanceRejNormale: ", round(time_AccRej,5), "secondes")) #Environ 1.126 secondes

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Autres graphs, graphs colorés de l'acceptance rejection method
n=10^4
#template


#Ex 1

Box_Muller_Mod_Graph=function(n)
{
  U_1=runif(n,-1,1)
  U_2=runif(n,-1,1)
  norm = sqrt(U_1^2 + U_2^2)
  return(data.frame(x=U_1,y=U_2,norm=norm))
}

df=Box_Muller_Mod_Graph(n)
x11()
df%>%
  ggplot(aes(x,y))+geom_circle(aes(x0 = 0,y0=0,r=1))+geom_point(aes(colour=cut(norm,c(0,1,Inf))))+scale_color_manual(name="norm",values=c("(0,1]"="red",
                                                                                                                                          "(1,Inf]"="blue"),
                                                                                                                     labels=c("Inside circle","Outside circle"))+
  theme_classic()

#estimation de l'aire du disque
InDisk=sum(df$norm<1)
#OutCircle=N-InCircle
DiskArea=InDisk/n


#Ex 2


AcceptanceRejectionHalfNormGraph=function(n,M)
{
  U=runif(n,0,1)
  Y=rexp(n,rate=1)
  #Y=-ln(runif(n,0,1)) #inverse method
  to_reject = U>DensityAbsNorm(Y)/(M*dexp(Y))
  return(data.frame(X=Y,U=U,to_reject=to_reject))
}

df2=AcceptanceRejectionHalfNormGraph(n,M)

x11()
df2%>%
  ggplot(aes(x=X,y=U,col=to_reject))+geom_point()+
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue"))+
  labs(title="Illustration of the acceptance-rejection sampling method")


x=seq(0,9,length.out=10000)
f=DensityAbsNorm(x)
g=dexp(x)
plot(x,f,type="l",ylim=c(0,1))
lines(x,g*M,col="blue")
