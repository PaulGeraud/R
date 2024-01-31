#librairies
library(tidyverse)
library(lubridate)
#importer les données
premium=read.csv("premium.csv",sep=",")
claims=read.csv("claims.csv",sep=",")

View(premium)
View(claims)
#On regarde le format
sapply(premium,typeof)
sapply(claims,typeof)

#les dates sont mal gérés . D'autre oart dans le dataset premium, il y a des lignes doublons
premium=premium%>%
  mutate(event_month=ymd(event_month),subscription_date=ymd(subscription_date),subscription_cancelled_date=ymd(subscription_cancelled_date),
         actual_age=pet_age_at_subscription+floor(as.numeric(difftime(dmy("31/12/2022"),subscription_date , units = "days"))/365),
         fourchette=cut(actual_age, breaks = c(0,1,3,5,7, 8), labels = c("0-1","2-3","4-5","6-7","8+"), include.lowest = TRUE))%>%
  distinct()

claims=claims%>%
  mutate(act_date_month=ymd(act_date_month),claim_creation_month=ymd(claim_creation_month),claim_close_month=ymd(claim_close_month))%>%
  distinct() #pas nécessaire mais pour la forme

View(claims%>%filter(claim_type %in% c('ACCIDENT','MALADIE'))%>%arrange(pet_id))

#généralités
#Dataset premium
#aperçu de tous les clients
sort(unique(premium$pet_id)) #on peut voir que c'est pas continu: il y a des trous
nbr_clients=length(unique(premium$pet_id))
nbr_primes_clients=table(premium$pet_id)
nbr_primes_moy=mean(nbr_primes_clients) #Sur 2022, un client paye en moyenne 6.66 "primes"

#Exemple garantie complete
TestP=premium%>%
  filter(pet_id=="p627")
TestS=claims%>%
  filter(pet_id=="p627")

#Exemple n'a pas souscrit à de la prévention et donc aucun sinistre en prevention
TestP=premium%>%
  filter(pet_id=="p1181")
TestS=claims%>%
  filter(pet_id=="p116")

#Exempe; client qui coute super cher
TestP=premium%>%
  filter(pet_id=="p1013")
TestS=claims%>%
  filter(pet_id=="p1013")


#Exempe; client qui coute que dalle
TestP=premium%>%
  filter(pet_id=="p1041")
TestS=claims%>%
  filter(pet_id=="p1041")

#Calcul de taux de rétention

#nombre de clients qui ont souscrits avant le 01/2022
CS=nrow(premium%>%
          filter(year(subscription_date)<2022)%>%distinct(pet_id))
#nombre de clients qui ont souscrits avant le 01/2022 et qui sont partis avant le 31/12/2022
CL=nrow(premium%>%
          filter(year(subscription_date)<2022,subscription_cancelled_date<dmy("31/12/2022"))%>%
          distinct(pet_id))

#résultat
tx_ret=100*(CS-CL)/CS
cat("Le taux de rétention sur la période 2022 est de", round(tx_ret, digits = 2),"%")

#ratios S/P

#S/P global
#argent que reçoit l'assureur
P=sum(premium$total_hthc)
#sinistres que l'assureur à du payer
S=sum(claims$claims_reimbursed)
SPg=100*S/P
cat("Le ratio S/P global sur la période 2022 est de",round(SPg, digits = 2),"%")
#segmentation sur les espèces

unique(premium$pet_type) #différentes espèces

#chats
premiumCat=premium%>%
  filter(pet_type=="cat")
ListCat=unique(premiumCat$pet_id)
claimsCat=claims%>%
  filter(pet_id %in% ListCat)
SPcat=100*sum(claimsCat$claims_reimbursed)/sum(premiumCat$total_hthc)

#chiens
premiumDog=premium%>%
  filter(pet_type=="dog")
ListDog=unique(premiumDog$pet_id)
claimsDog=claims%>%
  filter(pet_id %in% ListDog)
SPdog=100*sum(claimsDog$claims_reimbursed)/sum(premiumDog$total_hthc)

#affichage
cat("Le ratio S/P pour les chats sur la période 2022 est de",round(SPcat, digits = 2),"%\n")
cat("Le ratio S/P pour les chiens sur la période 2022 est de",round(SPdog, digits = 2),"%\n")

#segmentation sur les races 
#on fait la somme des primes pour chaque race
ByRacePremium=premium%>%
  group_by(pet_race)%>%
  arrange(pet_race,pet_id)%>%
  summarise(pet_race=first(pet_race),prime_hthc=sum(total_hthc))%>%
  ungroup()

#Pour claims, comme la race n'est pas dans le fichier, il faut faire une jointure
SimplePremium=premium%>%
  select(pet_id,pet_race)%>%
  distinct(pet_id,pet_race)

ClaimsWithRace=merge(claims,SimplePremium,by='pet_id')

#somme des sinistres pour chaque race
ByRaceClaims=ClaimsWithRace%>%
  group_by(pet_race)%>%
  arrange(pet_race,pet_id)%>%
  summarise(pet_race=first(pet_race),claims_reimbursed=sum(claims_reimbursed))%>%
  ungroup()

#dernier merge pour obtenir le montant cumulé primes & sinistres, puis calculer le S/P par race
MergedRace=merge(ByRaceClaims,ByRacePremium,by="pet_race")%>%
  mutate(SPrace=100*claims_reimbursed/prime_hthc)

#affichage des différents ratios S/P
print("Ratio S/P sur la période 2022 pour chaque race:\n")
for (i in 1:nrow(MergedRace))
{
  cat(MergedRace$pet_race[i],": ",round(MergedRace$SPrace[i], digits = 2),"%\n")
}

#ratios S/P pour une fourchette d'age
ByAgePremium=premium%>%
  group_by(fourchette)%>%
  arrange(fourchette,pet_id)%>%
  summarise(fourchette=first(fourchette),prime_hthc=sum(total_hthc))%>%
  ungroup()

#Pour claims, comme la fourchette d'age n'est pas dans le fichier, il faut faire une jointure
SimplePremium=premium%>%
  select(pet_id,fourchette)%>%
  distinct(pet_id,fourchette)

ClaimsWithAge=merge(claims,SimplePremium,by='pet_id')

#somme des sinistres pour chaque fourchette d'age
ByAgeClaims=ClaimsWithAge%>%
  group_by(fourchette)%>%
  arrange(fourchette,pet_id)%>%
  summarise(fourchette=first(fourchette),claims_reimbursed=sum(claims_reimbursed))%>%
  ungroup()

#dernier merge pour obtenir le montant cumulé primes & sinistres, puis calculer le S/P par fourchette d'âge
MergedAge=merge(ByAgeClaims,ByAgePremium,by="fourchette")%>%
  mutate(SPage=100*claims_reimbursed/prime_hthc)

#affichage des différents ratios S/P
print("Ratio S/P sur la période 2022 pour trois fourchette d'âge:\n")
for (i in 1:nrow(MergedAge))
{
  cat(as.character(MergedAge$fourchette[i])," ans : ",round(MergedAge$SPage[i], digits = 2),"%\n")
}

#Généralités

#Que les primes qui ont souscrits à de la prévoyance
premiumPrev=premium%>%
  select(pet_id,pet_type,pet_race,prevention_hthc,prevention_limit)%>%
  filter(!is.na(prevention_hthc))

#Que les claims de type prévoyance
claimsPrev=claims%>%
  filter(claim_type=="PREVENTION")%>%
  select(pet_id,claims_reimbursed)
#ratio S/P global de la garantie prévention

SPgPrev=100*sum(claimsPrev$claims_reimbursed)/sum(premiumPrev$prevention_hthc)
cat("Le ratio S/P global pour la garantie prévoyance est de",round(SPgPrev, digits = 2),"%")

#Montant moyen attendu pour un assuré qui a souscrit 12 mois
PrimeAnnuelleMoy=12*mean(premiumPrev$prevention_hthc)
#En pratique, combien de primes versent en moyenne les clients ?
premiumPrevClient=premiumPrev%>%
  arrange(pet_id)%>%
  group_by(pet_id)%>%
  summarise(pet_id=first(pet_id),prime_prev=first(prevention_hthc),nbr_prime_verses=n())%>%
  ungroup()


VraiePrimeAnnuelleMoy=mean(premiumPrevClient$nbr_prime_verses)*mean(premiumPrevClient$prime_prev)

#On s'intérèsse à la valeur moyenne de la sinistralité / client
claimsPrevClient=claimsPrev%>%
  arrange(pet_id)%>%
  group_by(pet_id)%>%
  summarise(pet_id=first(pet_id),sinistralite_tot=sum(claims_reimbursed))%>%
  ungroup()

summary(claimsPrevClient$sinistralite_tot)
#50% des clients atteignent le plafond. En admettant que chaque assuré verse 12 primes (ce qui n'est pas le cas, ils en versent en général moins), le montant moyen de la sinistralité
#>montant moyen de primes. Au global, la garantie ne semble pas rentable et elle rogne sur les bénéfs générés par la partie principale