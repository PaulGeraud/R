#init
library(tidyverse)
library(ggforce)
library(gridExtra)
set.seed(41564)
N=1000
BBG=rnorm(N)

#Ex1 AR Process

Y_ar=rep(0,N)
for (i in 3:N)
{
  Y_ar[i]=0.6*Y_ar[i-1]+0.2*Y_ar[i-2]+BBG[i]
}

#plot de la série
p1=data.frame(Date=c(1:N),Valeur=Y_ar)%>%
  ggplot(aes(x=Date,y=Valeur))+geom_line(col='black')+
  labs(title="Evolution d'un AR(2)")
p1
#autocrrels
par(mfrow = c(3, 1))
acf(Y_ar,type="covariance", lag.max=24,
    main="Fonction d'autocovariance de l'AR")
acf(Y_ar,type="correlation", lag.max=24,
    main="Fonction d'autocorrélation de l'AR")
acf(Y_ar,type="partial", lag.max=24,
    main="Fonction d'autocorrélation partielle de l'AR")
#LjungBox
Box.test(Y_ar, lag = 24, type = "Ljung-Box")

Roots=polyroot(c(1,-0.6,-0.2)) #! order c+bx+ax^2

data.frame(Re=Re(Roots),Im=Im(Roots))%>%
  ggplot(aes(x=Re,y=Im))+geom_point(col='red',size=3)+geom_circle(aes(x0 = 0,y0=0,r=1),col="black")+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
  xlim(c(-4.5,4.5))+ylim(c(-4.5,4.5))+
  labs(title="Graphe des racines du polynôme caractéristique par rapport au cercle unité")

#roots outside the unit circle=>staionnarity

#Ex2 MA Process
Y_am=rep(0,N)
Y_am[1]=BBG[1]
Y_am[2]=BBG[2]
for (i in 3:N)
{
  Y_am[i]=BBG[i]+0.5*BBG[i-1]+0.3*BBG[i-2]
}

#test avec arima.sim()
arimaTest=arima.sim(n=N,model=list(order=c(0,0,2), ma= c(0.5, 0.3)),innov=BBG)

#plot de la série
p2=data.frame(Date=c(1:N),Valeur=Y_am)%>%
  ggplot(aes(x=Date,y=Valeur))+geom_line(col='black')+
  labs(title="Evolution d'un AM(2)")

p3=data.frame(Date=c(1:N),Sim=arimaTest)%>%
  ggplot(aes(x=Date,y=Sim))+geom_line(col='blue')+
  labs(title="Evolution d'un AM(2) généré par arima.sim()")

grid.arrange(p2,p3,nrow=2,ncol=1)


#autocrrels
par(mfrow = c(3, 1))
acf(Y_am,type="covariance", lag.max=24,
    main="Fonction d'autocovariance de l'AM")
acf(Y_am,type="correlation", lag.max=24,
    main="Fonction d'autocorrélation de l'AM")
acf(Y_am,type="partial", lag.max=24,
    main="Fonction d'autocorrélation partielle de l'AM")

Box.test(Y_am, lag = 24, type = "Ljung-Box")
#résultats surprenants avec graphe autocorrel, ressemble à celui d'un MA(3)

Roots_AM=polyroot(c(1,0.5,0.3)) #! order c+bx+ax^2

data.frame(Re=Re(Roots_AM),Im=Im(Roots_AM))%>%
  ggplot(aes(x=Re,y=Im))+geom_point(col='red',size=3)+geom_circle(aes(x0 = 0,y0=0,r=1),col="black")+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
  labs(title="Graphe des racines du polynôme caractéristique par rapport au cercle unité")
#Racines en dehors du cercle unité, processus inversible

#Bonus. Imagine at time =500, we have a shock, u_t=5, what happens ?

BBGS=BBG
BBGS[500]=5
Y_arS=rep(0,N)
for (i in 3:N)
{
  Y_arS[i]=0.6*Y_arS[i-1]+0.2*Y_arS[i-2]+BBGS[i]
}

Y_amS=rep(0,N)
Y_amS[1]=BBGS[1]
Y_amS[2]=BBGS[2]
for (i in 3:N)
{
  Y_amS[i]=BBGS[i]+0.5*BBGS[i-1]+0.3*BBGS[i-2]
}

p4=data.frame(Date=c(1:N),Valeur=Y_arS)%>%
  ggplot(aes(x=Date,y=Valeur))+geom_line(col='black')+
  labs(title="Evolution d'un AR(2) perturbé à t=500")


p5=data.frame(Date=c(1:N),Valeur=Y_amS)%>%
  ggplot(aes(x=Date,y=Valeur))+geom_line(col='black')+
  labs(title="Evolution d'un AM(2) pertubé à t=500")


grid.arrange(p1,p2,p4,p5,nrow=2,ncol=2)

#on peut voir que le choc s'estompe très rapidement dans le temps


#Ex 3
#génération
Y_arma = arima.sim(model=list(order=c(2,0,2), ma=c(1.1, -0.28), ar=c(0.6,-0.25)), n=1000,innov=BBG)
#plot
p7=data.frame(Date=c(1:N),Valeur=Y_arma)%>%
  ggplot(aes(x=Date,y=Valeur))+geom_line(col='black')+
  labs(title="Evolution d'un ARMA(2,2)")
p7

par(mfrow = c(3, 1))
acf(Y_arma,type="covariance", lag.max=24,
    main="Fonction d'autocovariance de l'ARMA")
acf(Y_arma,type="correlation", lag.max=24,
    main="Fonction d'autocorrélation de l'ARMA")
acf(Y_arma,type="partial", lag.max=24,
    main="Fonction d'autocorrélation partielle de l'ARMA")

Box.test(Y_am, lag = 24, type = "Ljung-Box")
#Inversibilité et stationnarité du processsus
Roots_AM=polyroot(c(1,1.1,-0.28))
Roots_AR=polyroot(c(1,-0.6,0.25))

data.frame(ReAM=Re(Roots_AM),ImAM=Im(Roots_AM),ReAR=Re(Roots_AR),ImAR=Im(Roots_AR))%>%
  ggplot()+geom_point(aes(x=ReAM,y=ImAM,color="AM"),size=3)+geom_point(aes(x=ReAR,y=ImAR,color="AR"),size=3)+
  geom_circle(aes(x0 = 0,y0=0,r=1),col="black")+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
  xlim(c(-2,2))+ylim(c(-2,2))+
  scale_color_manual(values = c("AM" = "red", "AR" = "blue"), 
                     labels = c("AM" = "Racines polynôme AM", "AR" = "Racines polynôme AR")) +
  labs(title = "Graphe des racines du polynôme caractéristique par rapport au cercle unité") +
  guides(color = guide_legend(title = "Légende"))
  #Racines AR en dehors du cercle unité=>stationnaire
  #Une des racines AM dans le cercle unité=>Non invertible