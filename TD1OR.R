library(tidyverse)
library(plot3D)
library(rgl)
library(plot3Drgl)

f=function(x,y)
{
  return (x*y/((1+x^2)*(1+y^2)))
}

x=seq(-10,10,length.out=500)
y=seq(-10,10,length.out=500)
z=outer(x,y,f) #stock in a matrix val of z for (x,y) in [0;2]?
colnames(z)=y  #change name of each column
rownames(z)=x 
persp3D(x,y,z,
        clab=c("Values","Numbers"),
        ticktype="detailed",
        main= "Graphe de la fonction de l'exo 6 sur [-10;10]x[-10:10]",
        zlab="Values"
        )
plotrgl(lighting = TRUE, smooth = TRUE,fill=z)
rglwidget()