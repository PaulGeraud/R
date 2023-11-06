#manipulation listes et vecteurs
A=c(1,3,7,9)
1:10
seq(1,10)
seq(1,10,0.5)
seq(1,10,length.out=15)
rep(0,10)
rep(c(1,2),10)
matrix(c(1,2,3,4,5,6,7,8,9),nrow=3)
matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,byrow=TRUE)
#dataframe
df=data.frame(r1=c(0,1,3,5),r2=3:6,r3=rep(c(2,3),2))
df
df[1,2]
df$r2[1]
df[1,]
df[,1]
df$r1
df[c(1,2),]
df[c(1,2),c(1,3)]
df$r2[df$r1>2]

#définir une fonction

f=function(x)
{
  return(x^2)
}
f(c(1,2,3))

#fonctions liées au proba
#fct de répartition
punif(2,0,3) #P(X<2)
pnorm(1,0,1)
pbinom(3,10,0.5)
#inverse fct rep
qnorm(0.975,0,1)
#densite
dnorm(1,0,1)
#generer echantillon aléatoire
runif(1000,0,1)
hist(runif(1000,0,1))
T=rnorm(10000,0,1)
hist(T)
hist(rchisq(1000,5))
#representation graphique
X=seq(-3,3,length.out=100)
Y=f(X)
plot(X,Y)
plot(X,Y,type="l")
length(X)
plot(f,xlim=c(-3,3))
#superposer courbes
plot(X,Y,type="l")
lines(X,X^3,col="red")
points(X,X^4,col="blue")
plot.new()
plot(X,Y,type="l",col=15)
par(new=TRUE)
plot(X,X^3,type="l",col="blue",axes=FALSE,xlab="",ylab="")

#integrale par la méthode des rectangles

rectangle=function(f,a,b,n)
{
  h=(b-a)/n
  val=0
  for(i in seq(a,b-h,h))
  {
    val=val+f(i+h)*h
  }
  return (val)
}
rectangle(f,0,2,100)

#tracer un graphique de l'erreur en fonction de n
n=10000
N=1:n
ErrR=rep(0,n)
for(i in N)
{
  ErrR=abs(rectangle(f,0,1,i)-1/3)
}
plot(ErrR,type="l",xlab="n",ylab="% d'erreur")
plot(log(N),log)
