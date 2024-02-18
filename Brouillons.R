data=read.csv("Ensemble-deces.csv",sep=",")
data=data%>%
  select(-X)%>%
  mutate(datenaiss=as.numeric(substr(datenaiss,1,4)),datedeces=as.numeric(substr(datedeces,1,4)),duree_vie=datedeces-datenaiss,
         sexe=ifelse(sexe==1,"H","F"))%>%
  drop_na()%>%
  filter(duree_vie>=0&duree_vie<=120&datedeces>=1970)


Resume_age=data%>%
  group_by(sexe)%>%
  summarise(sexe=first(sexe),AgeMoy=mean(duree_vie,na.rm=TRUE))

  
#Debugage
test=data%>%
  filter(duree_vie>110)
View(test)
summary(data$duree_vie)

ggplot(data, aes(x = duree_vie)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Répartition du nombre de personnes par âge",
       x = "Âge",
       y = "Nombre de personnes") +
  theme_minimal()

ggplot(data, aes(x = duree_vie)) +
  geom_density(fill="blue",alpha=0.7) +
  labs(title = "Répartition du nombre de personnes par âge",
       x = "Âge",
       y = "densité") +
  theme_minimal()

EvolAge=data%>%
  arrange(datedeces)%>%
  group_by(datedeces,sexe)%>%
  summarise(annee=last(datedeces),sexe=last(sexe),
            AgeMoy=mean(duree_vie))%>%
  ungroup()

x11()
EvolAge%>%
  ggplot(aes(x=annee,y=AgeMoy,col=sexe))+geom_line()+geom_point(size=2,shape=4)
  labs(title="Evolution de l'âge moyen au cours du temps")