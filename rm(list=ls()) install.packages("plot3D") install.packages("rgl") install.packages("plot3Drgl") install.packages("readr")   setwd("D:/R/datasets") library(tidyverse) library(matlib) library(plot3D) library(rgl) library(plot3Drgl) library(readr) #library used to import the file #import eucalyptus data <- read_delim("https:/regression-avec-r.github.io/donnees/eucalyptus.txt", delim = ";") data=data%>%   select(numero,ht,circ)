rm(list=ls())
install.packages("plot3D")
install.packages("rgl")
install.packages("plot3Drgl")
install.packages("readr")


setwd("D:/R/datasets")
library(tidyverse)
library(matlib)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(readr) #library used to import the file
#import eucalyptus
data <- read_delim("https://regression-avec-r.github.io/donnees/eucalyptus.txt", delim = ";")
data=data%>%
  select(numero,ht,circ)
