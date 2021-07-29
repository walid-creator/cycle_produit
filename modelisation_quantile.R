rm(list=ls())
setwd("~/dcma")

#libraries
library(readODS)
library(readxl)
library("FactoMineR")
library("factoextra")
library(ggplot2)
library(dplyr)
library(tidyverse)#graphes
library(quantreg)
?rq
#juste des essaies
#importation des données
newdataset_synthese <- read_ods("newdataset_synthese.ods")
viande1<-data.frame(newdataset_synthese[which(newdataset_synthese$groupe_aliment=='viandes, œufs, poissons'),])
#groupe11 <- read_ods("groupe11.ods")
#on centre la variable cible 
#groupe11["score_unique"]<-scale(groupe11["score_unique"])



#clustering

set.seed(123)
viande1[,c(2,4,5,6)]
X1 <- fastDummies::dummy_cols(viande1[,c(2,4,5,6)])
X<-as.matrix(X1[,-c(1,2,3,4)])
acm<-MCA(viande1[,c(2,4,5,6)])
cah1 <- HCPC(acm, graph = T ,nb.par=5,nb.clust = 7)
?HCPC
set.seed(123)
classe<-cah1$data.clust
table(classe$clust)
#les axes qui représentent le plus chaque cluster
cah$desc.axes
print(table(classe$clust))
#les ind les plus représentatifs de chaque cluster
x<-cah$desc.ind$para# le min des distances de chaque indiv du centre  de chaque cluster
cah$desc.ind$dist# le max des distances de chaque indiv du centre des autres clusters
#les var quanti représentant le plus chaque cluster
cah$desc.var


groupe1 <-  data.frame(classe[which(classe$clust=='1'), ],viande1[which(classe$clust=='1'), ]$sgroupe_aliment,viande1[which(classe$clust=='1'), ]$produit)
groupe2 <-  data.frame(classe[which(classe$clust=='2'), ],viande1[which(classe$clust=='2'), ]$sgroupe_aliment,viande1[which(classe$clust=='2'), ]$produit)
groupe3 <-  data.frame(classe[which(classe$clust=='3'), ],viande1[which(classe$clust=='3'), ]$sgroupe_aliment,viande1[which(classe$clust=='3'), ]$produit)
groupe4 <-  data.frame(classe[which(classe$clust=='4'), ],viande1[which(classe$clust=='4'), ]$sgroupe_aliment,viande1[which(classe$clust=='4'), ]$produit)
groupe5 <-  data.frame(classe[which(classe$clust=='5'), ],viande1[which(classe$clust=='5'), ]$sgroupe_aliment,viande1[which(classe$clust=='5'), ]$produit)
groupe6 <-  data.frame(classe[which(classe$clust=='6'), ],viande1[which(classe$clust=='6'), ]$sgroupe_aliment,viande1[which(classe$clust=='6'), ]$produit)
groupe7 <-  data.frame(classe[which(classe$clust=='7'), ],viande1[which(classe$clust=='7'), ]$sgroupe_aliment,viande1[which(classe$clust=='7'), ]$produit)





#régression linéaire
#rqessai <- lm(log(score_unique) ~ groupe_aliment + livraison + emballage + preparation,data=groupe11 )
#rqessai <- lm(scale(score_unique) ~ groupe_aliment + livraison + emballage + preparation,data=groupe11 )
#summary(rqessai)

#régression quantile
rqfit1 <- rq(score_unique ~ groupe_aliment + livraison + emballage + preparation  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=newdataset_synthese)
rqfit1$method
#metho: méthode de calculs des zeros: 
#puisque la fonction quantile n'est pas diff en zero on ne peut pas calculer les zeros par 
#la méthode de newton, il y'a alors 2 méthodes des plus connus:
#méthode du point intérieur(fn)(n grand) et méthode correspondant au simplexe(n<1000)(br)
#on choisit de ne pas centrer ou loger la variable cible car les résultats sont moins
#bons en terme du R2




?rq
#dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")
summary(rqfit1,se = "boot")
#ca correspond aux méthodes de calculs de la variance:
#si modèle de translation simple se=iid 
#sinon d'autres méthodes plus adaptées ker ou boot(boostrap) pour les méthodes à vérifier

rqfit2 <- rq(axe2 ~ groupe_aliment + livraison + emballage + preparation  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=newdataset_synthese)
summary(rqfit2,se = "boot")


rqfit3 <- rq(axe3 ~ groupe_aliment + livraison + emballage + preparation  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=newdataset_synthese)
summary(rqfit3,se = "boot")


rqfit4 <- rq(axe4 ~ groupe_aliment + livraison + emballage + preparation  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=newdataset_synthese)
summary(rqfit4,se = "boot")

rqfit5 <- rq(axe5 ~ groupe_aliment + livraison + emballage + preparation  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=newdataset_synthese)
summary(rqfit5,se = "boot")
?rq
#calcul des résidus
a<-data.frame(rqfit1$residuals)
#calcul du R2
b<-0
c<-0
length(groupe11$score_unique)
for (i in 1:length(groupe11$score_unique)) {b<- b+a$tau..0.90[i]^2
c<-c + (groupe11$score_unique[i]-mean(groupe11$score_unique))^2
e<-b/c}
R2 <- 1-e

ggplot(data = viande1,
       aes(groupe_aliment,score_unique)) + 
       geom_boxplot()




plot(note_qualite ~ groupe_aliment, data = dataset_synthese1, pch = 16)
abline(lm(note_qualite ~ groupe_aliment, data = dataset_synthese1), col = "red", lty = 2)
abline(rq(note_qualite ~ groupe_aliment, data = dataset_synthese1), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)
?summary.rq

rqfit2 <- rq(axe2 ~ groupe_aliment + livraison + emballage + preparation  ,tau=c(.05, .25, .5, .75, .95), data=newdataset_synthese)
#dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")
summary(rqfit2,se = "boot")
