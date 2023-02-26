#initialize

install.packages("tidyverse") #Installation de la librairie tidyverse, on utilise  notamment les packages dplyr et ggplot2
install.packages("ggforce")
install.packages("plot3D")
install.packages("rgl")
install.packages("plot3Drgl")
install.packages("matlib")
install.packages("mvtnorm")
install.packages("cli")
install.packages("devtools")
install.packages("ambient")
install.packages("magick")
devtools::install_github("tylermorganwall/rayshader")
installed.packages()
install.packages("rayshader")
update.packages("rayshader")
remove.packages()
#-------------------------------------------------------------------------------------------
rm(list=ls())#libere l'espace memoire des variables pr?cedemment utilis?es
setwd("D:/R/datasets")
library(tidyverse)
library(devtools)
library(cli)
library(rayshader)
library(plot3D)
library(ggforce)
library(rgl)
library(rgl)
library(matlib)
library(mvtnorm)
library(ambient)
library(magick)

g1=mtcars%>%
  ggplot(aes(y=mpg,x=disp,color=cyl))+
  geom_point(size=2)+
  scale_color_continuous(limits=c(0,8))+
  ggtitle("mtcars: Displacement vs msg vs #of cylinders")+
  theme(title=element_text(size=8),
        text=element_text(size=12))
g1%>%
  plot_gg(height = 3,width = 3.5,multicore = TRUE,pointcontract = 0.7,soliddepth=-200)
render_camera(zoom=1,theta=-30,phi=30)
render_snapshot(clear=FALSE)





loadzip = tempfile() 
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(shadowmap=ray_shade(elmat), max_darken=0.5) %>%
  add_shadow(ambient_shade(elmat),0)%>%
  plot_3d(elmat,zscale=10,fov=0,theta=135,zoom=0.75,phi=45,windowsize=c(1000,800))
Sys.sleep(0.2)
render_camera(fov=0,theta=60,zoom=0.75,phi=45)
render_scalebar(limits=c(0,5,10),label_unit = "km",position="W",y=50,
                scale_length = c(0.33,1))
render_compass(position = "E")
render_snapshot(clear=TRUE)

#Make my PC BURN
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 60, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "W", y=50,
                scale_length = c(0.33,1))

render_compass(position = "E")
Sys.sleep(0.2)
render_highquality(samples=200, scale_text_size = 24,clear=TRUE)

#clouds
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "lightblue") %>%
  add_shadow(cloud_shade(elmat, zscale = 10, start_altitude = 500, end_altitude = 1000,), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800),
          background="darkred")
render_camera(theta = 20, phi=40,zoom= 0.64, fov= 56 )

render_clouds(elmat, zscale = 10, start_altitude = 800, end_altitude = 1000, attenuation_coef = 2, clear_clouds = T)
render_snapshot(clear=FALSE)









# create a list of all installed packages
ip <- as.data.frame(installed.packages())
head(ip)
# if you use MRO, make sure that no packages in this library will be removed
ip <- subset(ip, !grepl("MRO", ip$LibPath))
# we don't want to remove base or recommended packages either\
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# determine the library where the packages are installed
path.lib <- unique(ip$LibPath)
# create a vector with all the names of the packages you want to remove
pkgs.to.remove <- ip[,1]
head(pkgs.to.remove)
# remove the packages
sapply(pkgs.to.remove, remove.packages, lib = path.lib)