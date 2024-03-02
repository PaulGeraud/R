library(tidyverse)
library(data.table)
setwd("D:/R/Code/Life/DECES France 1970")




#Importation & Merge
Main=fread("deces-2000.csv",sep=";")
#Main$lieunaiss <- as.character(Main$lieunaiss)
for(i in 1971:2022)
{
  cat("Annee actuelle ",i,"\n")
  Add=read.csv(paste0("deces-",i,".csv"),sep=";")%>%
    select(sexe,datenaiss,datedeces)
#  Add$lieunaiss <- as.character(Add$lieunaiss)
  Main=bind_rows(Main,Add)
}

#Exportation en csv
write.csv(Main,"Ensemble-deces.csv")