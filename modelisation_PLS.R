rm(list=ls())
setwd("~/dcma")
#install.packages('readODS')
#install.packages("tidyverse")
#install.packages(c("FactoMineR", "factoextra"))

#libraries
library(readODS)
library(readxl)
library("FactoMineR")
library("factoextra")
library(ggplot2)
library(dplyr)
library(tidyverse)





#données étapes
##émission des gaz à effet de serre pour chaque étape du cycle de vie
dataset_par_etape <- read_ods("data.ods",skip=3,sheet="Detail etape",col_names=TRUE)
dataset_par_etapeV1 <- dataset_par_etape
dataset_synthese <- read_ods("data.ods",skip=1,sheet="Synthese",col_names=TRUE)
dataset_synthese1 <- dataset_synthese[,c(-1,-2,-6,-7,-8)]
colnames(dataset_synthese1)<-c("groupe_aliment","sgroupe_aliment","produit","livraison","emballage","preparation","note_qualite","score_unique", "CO2","Appauvrissement","Rayonnement","Formation","Particules","Acidifaction","EutrophisationT","EutrophisationTEau","EutrophisationMarine","UtilisationSol","Ecotoxicité","EpuisementEAu","EpuisementEnergie","EpuisementMinéraux")


#sélection des  viandes
viande1<-data.frame(dataset_synthese1[which(dataset_synthese1$groupe_aliment=='viandes, œufs, poissons'),])


##représentation graphique



colnames(dataset_synthese1)
ggplot(data=dataset_synthese1,
       aes (groupe_aliment,CO2)) +
  geom_boxplot ()


#types de viande et leurs modalités
levels(as.factor(viande1$sgroupe_aliment))
levels(as.factor(viande1$preparation))
viandes_cuites<-data.frame(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),])
levels(as.factor(viandes_cuites$produit))
poissons_cuits<-data.frame(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),])
levels(as.factor(poissons_cuits$produit))

# nlp pour détecter les différentes viandes cuites et poisson cuits
library(text2vec)
library(data.table)
library(magrittr)

prep_fun = tolower
tok_fun = word_tokenizer
it_train = itoken(viandes_cuites$produit, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)

vocab["taux"] <- vocab["doc_count"]*100/length(viandes_cuites$produit)
vocab1 <- vocab[c(101,97,96,94,89,74,66,60,56,51,30,20,17,11,10,9),c(1,4)]
colnames(vocab1)[1] <- "Viande"
sum(vocab1["taux"])

vocab1 <- data.frame(vocab1)
vocab1$taux <- as.numeric(vocab1$taux)
vocab1[1,2] <- vocab1[1,2] + vocab1[7,2] 
vocab1 <- t(vocab1[-7,])


#pour les poissons cuits
it_train = itoken(poissons_cuits$produit, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)

vocab["taux(%)"] <- vocab["doc_count"]*100/length(poissons_cuits$produit)
vocab1 <- vocab[c(1,5,6,13,15,21,24,28,31,35,36,37,41,43,45,46,47,50,58,59,62,63,64,68,70,71,72,74,75),c(1,4)]

colnames(vocab1)[1] <- "Poisson"
sum(vocab1["taux(%)"])

vocab1 <- t(vocab1)




#pas de valeurs manquantes
sum(is.na(dataset_par_etape))
colMeans(is.na(dataset_par_etape))
head(dataset_par_etape)

#ajout de la colonne étape
colnames(dataset_par_etape)[8:13]<- "score_unique"
colnames(dataset_par_etape)[15:20]<- "CO2"
colnames(dataset_par_etape)[22:27]<- "Appauvrissement"
colnames(dataset_par_etape)[29:34]<- "Rayonnement"
colnames(dataset_par_etape)[36:41]<- "Fphotochimique"
colnames(dataset_par_etape)[43:48]<- "Particules"
colnames(dataset_par_etape)[50:55]<- "Acification"
colnames(dataset_par_etape)[57:62]<- "EutrophisationT"
colnames(dataset_par_etape)[64:69]<- "EutrophisationEauD"
colnames(dataset_par_etape)[71:76]<- "EutrophisationM"
colnames(dataset_par_etape)[78:83]<- "UtilisationSol"
colnames(dataset_par_etape)[85:90]<- "Ecotoxicité"
colnames(dataset_par_etape)[92:97]<- "EpuisementEau"
colnames(dataset_par_etape)[99:104]<- "Eenergétique"
colnames(dataset_par_etape)[106:111]<- "EMineraux"
#ajout de la colonne étape
etape <- rbind(rep("agriculture",2480), rep("transformation",2480),rep("emballage",2480),rep("transport",2480),rep("distribution",2480),rep("consommation",2480))

a<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,c(8,15,22,29,36,43,50,57,63,71,78,85,92,99,106)]),rep("agriculture",2480))
colnames(a) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","CO2","Appauvrissement","Rayonnement","Fphotochimique","Particules","Acidification","EutrophisationT","EutrophisationEauD","EutrophisationM","UtilisationSol","Ecotoxicité","EpuisementEau","Eenergétique","EMineraux", "etape")
b<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,c(9,16,23,30,37,44,51,58,64,72,79,86,93,100,107)]),rep("transformation",2480))
colnames(b) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","CO2","Appauvrissement","Rayonnement","Fphotochimique","Particules","Acidification","EutrophisationT","EutrophisationEauD","EutrophisationM","UtilisationSol","Ecotoxicité","EpuisementEau","Eenergétique","EMineraux", "etape")
c<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,c(10,17,24,31,38,45,52,59,65,73,80,87,94,101,108)]),rep("emballage",2480))
colnames(c) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","CO2","Appauvrissement","Rayonnement","Fphotochimique","Particules","Acidification","EutrophisationT","EutrophisationEauD","EutrophisationM","UtilisationSol","Ecotoxicité","EpuisementEau","Eenergétique","EMineraux", "etape")
d<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,c(11,18,25,32,39,46,53,60,66,74,81,88,95,102,109)]),rep("transport",2480))
colnames(d) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","CO2","Appauvrissement","Rayonnement","Fphotochimique","Particules","Acidification","EutrophisationT","EutrophisationEauD","EutrophisationM","UtilisationSol","Ecotoxicité","EpuisementEau","Eenergétique","EMineraux", "etape")
e<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,c(12,19,26,33,40,47,54,61,67,75,82,89,96,103,110)]),rep("distribution",2480))
colnames(e) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","CO2","Appauvrissement","Rayonnement","Fphotochimique","Particules","Acidification","EutrophisationT","EutrophisationEauD","EutrophisationM","UtilisationSol","Ecotoxicité","EpuisementEau","Eenergétique","EMineraux", "etape")
f<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,c(13,20,27,34,41,48,55,62,68,76,83,90,97,104,111)]),rep("consommation",2480))
colnames(f) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","CO2","Appauvrissement","Rayonnement","Fphotochimique","Particules","Acidification","EutrophisationT","EutrophisationEauD","EutrophisationM","UtilisationSol","Ecotoxicité","EpuisementEau","Eenergétique","EMineraux", "etape")

newdataset_par_etape <- rbind(a,b,c,d,e,f)
dataset_par_etapeV1<- newdataset_par_etape
levels(as.factor(dataset_par_etapeV1$etape))

#centrer et réduire la variable cible
newdataset_par_etape["CO2"] <- scale(newdataset_par_etape$CO2)
newdataset_par_etape["Appauvrissement"] <- scale(newdataset_par_etape$Appauvrissement)
newdataset_par_etape["Rayonnement"] <- scale(newdataset_par_etape$Rayonnement)
newdataset_par_etape["Fphotochimique"] <- scale(newdataset_par_etape$Fphotochimique)
newdataset_par_etape["Particules"] <- scale(newdataset_par_etape$Particules)
newdataset_par_etape["Acidification"] <- scale(newdataset_par_etape$Acidification)
newdataset_par_etape["EutrophisationT"] <- scale(newdataset_par_etape$EutrophisationT)
newdataset_par_etape["EutrophisationEauD"] <- scale(newdataset_par_etape$EutrophisationEauD)
newdataset_par_etape["EutrophisationM"] <- scale(newdataset_par_etape$EutrophisationM)
newdataset_par_etape["UtilisationSol"] <- scale(newdataset_par_etape$UtilisationSol)
newdataset_par_etape["Ecotoxicité"] <- scale(newdataset_par_etape$Ecotoxicité)
newdataset_par_etape["EpuisementEau"] <- scale(newdataset_par_etape$EpuisementEau)
newdataset_par_etape["Eenergétique"] <- scale(newdataset_par_etape$Eenergétique)
newdataset_par_etape["EMineraux"] <- scale(newdataset_par_etape$EMineraux)

#export et import #mai trop long donc oublier
#write_ods(newdataset_par_etape, "newdataset_par_etape.ods")
#newdataset_par_etape <- read_ods("newdataset_par_etape.ods")
dataset_par_etapeV1<- newdataset_par_etape

#régression pls 
#install.packages("pls")
library(pls)
Y<-as.matrix(newdataset_par_etape[,c(6:19)])
X<-newdataset_par_etape[,c(1,20)]
#install.packages('fastDummies')
#transformation en indicatrice
X1 <- fastDummies::dummy_cols(X)
X<-as.matrix(X1[,-c(1,2)])
modele.pls <- plsr(Y ~ X,ncomp=4,method = "simpls", validation="CV") 

summary(modele)
RMSEP(modele.pls)
plot(RMSEP(modele.pls))
R2(modele.pls)
modele.pls$coefficients

#coef
coeff_reg_PLS_complet<-data.frame(modele.pls$coefficients
)
colnames(coeff_reg_PLS_complet)[1] <- "CO2(comp1)"
colnames(coeff_reg_PLS_complet)[2] <- "Ozone(comp1)"
colnames(coeff_reg_PLS_complet)[3] <- "Rayonnement(comp1)"
colnames(coeff_reg_PLS_complet)[4] <- "Fphotcohimique(comp1)"
colnames(coeff_reg_PLS_complet)[5] <- "Particules(comp1)"
colnames(coeff_reg_PLS_complet)[6] <- "Acidification(comp1)"
colnames(coeff_reg_PLS_complet)[7] <- "EutroT(comp1)"
colnames(coeff_reg_PLS_complet)[8] <- "EutroE(comp1)"
colnames(coeff_reg_PLS_complet)[9] <- "EutroM(comp1)"
colnames(coeff_reg_PLS_complet)[10] <- "UtilSol(comp1)"
colnames(coeff_reg_PLS_complet)[11] <- "Ecotox(comp1)"
colnames(coeff_reg_PLS_complet)[12] <- "EpuisEau(comp1)"
colnames(coeff_reg_PLS_complet)[13] <- "EpuisEner(comp1)"
colnames(coeff_reg_PLS_complet)[14] <- "EpuisMineraux(comp1)"

colnames(coeff_reg_PLS_complet)[15] <- "CO2(comp2)"
colnames(coeff_reg_PLS_complet)[16] <- "Ozone(comp2)"
colnames(coeff_reg_PLS_complet)[17] <- "Rayonnement(comp2)"
colnames(coeff_reg_PLS_complet)[18] <- "Fphotcohimique(comp2)"
colnames(coeff_reg_PLS_complet)[19] <- "Particules(comp2)"
colnames(coeff_reg_PLS_complet)[20] <- "Acidification(comp2)"
colnames(coeff_reg_PLS_complet)[21] <- "EutroT(comp2)"
colnames(coeff_reg_PLS_complet)[22] <- "EutroE(comp2)"
colnames(coeff_reg_PLS_complet)[23] <- "EutroM(comp2)"
colnames(coeff_reg_PLS_complet)[24] <- "UtilSol(comp2)"
colnames(coeff_reg_PLS_complet)[25] <- "Ecotox(comp2)"
colnames(coeff_reg_PLS_complet)[26] <- "EpuisEau(comp2)"
colnames(coeff_reg_PLS_complet)[27] <- "EpuisEner(comp2)"
colnames(coeff_reg_PLS_complet)[28] <- "EpuisMineraux"

colnames(coeff_reg_PLS_complet)[29] <- "CO2(comp3)"
colnames(coeff_reg_PLS_complet)[30] <- "Ozone(comp3)"
colnames(coeff_reg_PLS_complet)[31] <- "Rayonnement(comp3)"
colnames(coeff_reg_PLS_complet)[32] <- "Fphotcohimique(comp3)"
colnames(coeff_reg_PLS_complet)[33] <- "Particules(comp3)"
colnames(coeff_reg_PLS_complet)[34] <- "Acidification(comp3)"
colnames(coeff_reg_PLS_complet)[35] <- "EutroT(comp3)"
colnames(coeff_reg_PLS_complet)[36] <- "EutroE(comp3)"
colnames(coeff_reg_PLS_complet)[37] <- "EutroM(comp3)"
colnames(coeff_reg_PLS_complet)[38] <- "UtilSol(comp3)"
colnames(coeff_reg_PLS_complet)[39] <- "Ecotox(comp3)"
colnames(coeff_reg_PLS_complet)[40] <- "EpuisEau(comp3)"
colnames(coeff_reg_PLS_complet)[41] <- "EpuisEner(comp3)"
colnames(coeff_reg_PLS_complet)[42] <- "EpuisMineraux(comp3)"

colnames(coeff_reg_PLS_complet)[43] <- "CO2(comp4)"
colnames(coeff_reg_PLS_complet)[44] <- "Ozone(comp4)"
colnames(coeff_reg_PLS_complet)[45] <- "Rayonnement(comp4)"
colnames(coeff_reg_PLS_complet)[46] <- "Fphotcohimique(comp4)"
colnames(coeff_reg_PLS_complet)[47] <- "Particules(comp4)"
colnames(coeff_reg_PLS_complet)[48] <- "Acidification(comp4)"
colnames(coeff_reg_PLS_complet)[49] <- "EutroT(comp4)"
colnames(coeff_reg_PLS_complet)[50] <- "EutroE(comp4)"
colnames(coeff_reg_PLS_complet)[51] <- "EutroM(comp4)"
colnames(coeff_reg_PLS_complet)[52] <- "UtilSol(comp4)"
colnames(coeff_reg_PLS_complet)[53] <- "Ecotox(comp4)"
colnames(coeff_reg_PLS_complet)[54] <- "EpuisEau(comp4)"
colnames(coeff_reg_PLS_complet)[55] <- "EpuisEner(comp4)"
colnames(coeff_reg_PLS_complet)[56] <- "EpuisMineraux(comp4)"


modele.pls$Yscores
#pour avoir le pourcentage de variance expliqué
summary(modele.pls)
modele.pls$coefficients
#calcul de la dispersion à la moyenne pour chaque variable et chaque commposante
#matrices des R2
#avec des données non centrées
#L=list("CO2","Particules","Acidification","EutrophisationT","EutrophisationM","UtilisationSol","Ecotoxicité")
#summary(Y)
#valeur<-c(0,0.32561,0.3738,0.3745,0.2640,0.2997,0.28432)
#col<-list("moyenne","dispersion","R2")
#Ymoydisp<-data.frame(matrix(0,3,7,dimnames =list(col,L)))
#Ymoydisp[1,]<-valeur
#for (j in 1:7) { 
#  Ymoydisp[2,j] <- sum((Y[,j]-Ymoydisp[1,j])^2)
#}
#for (j in 1:7) { Ymoydisp[3,j] <- 1 - PRESS[j,7]/Ymoydisp[2,j]
#  }



# on prend au début 16 composantes et on choisit après 7 composantes après 
#minimisation du MSE


#poid de X 
loadings(modele.pls)

?loadings

#poids de Y
Yloadings(modele.pls)

#variance expliqué de chaque composante
explvar(modele.pls)

#score
modele.pls$



#coord des ind sur chaque composante pls
modele.pls$model


summary(modele.pls)
print(coef(modele.pls))


#analyse qualitative


CO2_impact <-data.frame(aggregate(dataset_par_etapeV1$CO2, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
CO2_impact ["ratio"] <- CO2_impact['x']/sum(CO2_impact['x'])
colnames(CO2_impact)[2]<-"quantité_émission"

couche_impact <-data.frame(aggregate(dataset_par_etapeV1$Appauvrissement, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
couche_impact ["ratio"] <- couche_impact['x']/sum(couche_impact['x'])
colnames(couche_impact)[2]<-"quantité_émission"

RAY_impact <-data.frame(aggregate(dataset_par_etapeV1$Rayonnement, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
RAY_impact ["ratio"] <- RAY_impact['x']/sum(RAY_impact['x'])
colnames(RAY_impact)[2]<-"quantité_émission"

photo_impact <-data.frame(aggregate(dataset_par_etapeV1$Fphotochimique, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
photo_impact ["ratio"] <- photo_impact['x']/sum(photo_impact['x'])
colnames(photo_impact)[2]<-"quantité_émission"



particule_impact <- data.frame(aggregate(dataset_par_etapeV1$Particules, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
particule_impact ["ratio"] <- particule_impact['x']/sum(particule_impact['x'])
colnames(particule_impact)[2]<-"quantité_émission"

acid_i <- data.frame(aggregate(dataset_par_etapeV1$Acidification, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
acid_i ["ratio"] <- acid_i['x']/sum(acid_i['x'])
colnames(acid_i)[2]<-"quantité_émission"

Eutrot_T <- data.frame(aggregate(dataset_par_etapeV1$EutrophisationT, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
Eutrot_T ["ratio"] <- Eutrot_T['x']/sum(Eutrot_T['x'])
colnames(Eutrot_T)[2]<-"quantité_émission"

Eutrot_E <- data.frame(aggregate(dataset_par_etapeV1$EutrophisationEauD, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
Eutrot_E ["ratio"] <- Eutrot_E['x']/sum(Eutrot_E['x'])
colnames(Eutrot_E)[2]<-"quantité_émission"


EutrotM <- data.frame(aggregate(dataset_par_etapeV1$EutrophisationM, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
EutrotM ["ratio"] <- EutrotM['x']/sum(EutrotM['x'])
colnames(EutrotM)[2]<-"quantité_émission"



util_sol <- data.frame(aggregate(dataset_par_etapeV1$UtilisationSol, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
util_sol ["ratio"] <- util_sol['x']/sum(util_sol['x'])
colnames(util_sol)[2]<-"quantité_émission"

ecotox <- data.frame(aggregate(dataset_par_etapeV1$Ecotoxicité, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
ecotox ["ratio"] <- ecotox['x']/sum(ecotox['x'])
colnames(ecotox)[2]<-"quantité_émission"

EpuiEau <- data.frame(aggregate(dataset_par_etapeV1$EpuisementEau, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
EpuiEau ["ratio"] <- EpuiEau['x']/sum(EpuiEau['x'])
colnames(EpuiEau)[2]<-"quantité_émission"

epuisEner <- data.frame(aggregate(dataset_par_etapeV1$Eenergétique, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
epuisEner ["ratio"] <- epuisEner['x']/sum(epuisEner['x'])
colnames(epuisEner)[2]<-"quantité_émission"

epuisMin <- data.frame(aggregate(dataset_par_etapeV1$EMineraux, by=list(Category=dataset_par_etapeV1$etape), FUN=sum))
epuisMin ["ratio"] <- epuisMin['x']/sum(epuisMin['x'])
colnames(epuisMin)[2]<-"quantité_émission"

ratio_etape<-data.frame(CO2_impact,couche_impact,RAY_impact,photo_impact,particule_impact,acid_i,Eutrot_T,Eutrot_E,EutrotM,util_sol,ecotox,EpuiEau,epuisEner,epuisMin)[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42)]
colnames(ratio_etape)<-c("Etape","CO2","Ozone","Rayonnement","Photochimique","Particules","Acidification","EutrophisationT","EutrophisationE","EutrophisationM","UtilisationSol","Ecotoxicité","EpuisEau","EpuisEnergie","EpuisMin")


#régression pls sur les viandes:
library(pls)
#données de viande
dataset_par_etapeV1<-data.frame(dataset_par_etapeV1[which(dataset_par_etapeV1$groupe_aliment=='viandes, œufs, poissons'),])

Y<-as.matrix(dataset_par_etapeV1[,c(6:19)])
X<-dataset_par_etapeV1[,c(2,20)]
#install.packages('fastDummies')
#transformation en indicatrice
X1 <- fastDummies::dummy_cols(X)
X<-as.matrix(X1[,-c(1,2,3,4)])
modele.pls <- plsr(Y ~ X, method='simpls',ncomp=5, validation="CV" ) 

#estimation du nombre d'axe par erreur quadratique moyenne de prédiction
plot(RMSEP(modele.pls))
#évaluation de la qualité de prédiction du modèle
R2(modele.pls)

summary(modele.pls)
#coefficients
coef_PLs_viande <- data.frame(modele.pls$coefficients)
#renommage des coeff
colnames(coef_PLs_viande)[1] <- "CO2(comp1)"
colnames(coef_PLs_viande)[2] <- "Ozone(comp1)"
colnames(coef_PLs_viande)[3] <- "Rayonnement(comp1)"
colnames(coef_PLs_viande)[4] <- "Fphotcohimique(comp1)"
colnames(coef_PLs_viande)[5] <- "Particules(comp1)"
colnames(coef_PLs_viande)[6] <- "Acidification(comp1)"
colnames(coef_PLs_viande)[7] <- "EutroT(comp1)"
colnames(coef_PLs_viande)[8] <- "EutroE(comp1)"
colnames(coef_PLs_viande)[9] <- "EutroM(comp1)"
colnames(coef_PLs_viande)[10] <- "UtilSol(comp1)"
colnames(coef_PLs_viande)[11] <- "Ecotox(comp1)"
colnames(coef_PLs_viande)[12] <- "EpuisEau(comp1)"
colnames(coef_PLs_viande)[13] <- "EpuisEner(comp1)"
colnames(coef_PLs_viande)[14] <- "EpuisMineraux(comp1)"

colnames(coef_PLs_viande)[15] <- "CO2(comp2)"
colnames(coef_PLs_viande)[16] <- "Ozone(comp2)"
colnames(coef_PLs_viande)[17] <- "Rayonnement(comp2)"
colnames(coef_PLs_viande)[18] <- "Fphotcohimique(comp2)"
colnames(coef_PLs_viande)[19] <- "Particules(comp2)"
colnames(coef_PLs_viande)[20] <- "Acidification(comp2)"
colnames(coef_PLs_viande)[21] <- "EutroT(comp2)"
colnames(coef_PLs_viande)[22] <- "EutroE(comp2)"
colnames(coef_PLs_viande)[23] <- "EutroM(comp2)"
colnames(coef_PLs_viande)[24] <- "UtilSol(comp2)"
colnames(coef_PLs_viande)[25] <- "Ecotox(comp2)"
colnames(coef_PLs_viande)[26] <- "EpuisEau(comp2)"
colnames(coef_PLs_viande)[27] <- "EpuisEner(comp2)"
colnames(coef_PLs_viande)[28] <- "EpuisMineraux"

colnames(coef_PLs_viande)[29] <- "CO2(comp3)"
colnames(coef_PLs_viande)[30] <- "Ozone(comp3)"
colnames(coef_PLs_viande)[31] <- "Rayonnement(comp3)"
colnames(coef_PLs_viande)[32] <- "Fphotcohimique(comp3)"
colnames(coef_PLs_viande)[33] <- "Particules(comp3)"
colnames(coef_PLs_viande)[34] <- "Acidification(comp3)"
colnames(coef_PLs_viande)[35] <- "EutroT(comp3)"
colnames(coef_PLs_viande)[36] <- "EutroE(comp3)"
colnames(coef_PLs_viande)[37] <- "EutroM(comp3)"
colnames(coef_PLs_viande)[38] <- "UtilSol(comp3)"
colnames(coef_PLs_viande)[39] <- "Ecotox(comp3)"
colnames(coef_PLs_viande)[40] <- "EpuisEau(comp3)"
colnames(coef_PLs_viande)[41] <- "EpuisEner(comp3)"
colnames(coef_PLs_viande)[42] <- "EpuisMineraux(comp3)"

colnames(coef_PLs_viande)[43] <- "CO2(comp4)"
colnames(coef_PLs_viande)[44] <- "Ozone(comp4)"
colnames(coef_PLs_viande)[45] <- "Rayonnement(comp4)"
colnames(coef_PLs_viande)[46] <- "Fphotcohimique(comp4)"
colnames(coef_PLs_viande)[47] <- "Particules(comp4)"
colnames(coef_PLs_viande)[48] <- "Acidification(comp4)"
colnames(coef_PLs_viande)[49] <- "EutroT(comp4)"
colnames(coef_PLs_viande)[50] <- "EutroE(comp4)"
colnames(coef_PLs_viande)[51] <- "EutroM(comp4)"
colnames(coef_PLs_viande)[52] <- "UtilSol(comp4)"
colnames(coef_PLs_viande)[53] <- "Ecotox(comp4)"
colnames(coef_PLs_viande)[54] <- "EpuisEau(comp4)"
colnames(coef_PLs_viande)[55] <- "EpuisEner(comp4)"
colnames(coef_PLs_viande)[56] <- "EpuisMineraux(comp4)"

#poid de X 
loadings(modele.pls)

#poids de Y
Yloadings(modele.pls)

#variance expliqué de chaque composante
explvar(modele.pls)

#score
modele.pls$model

summary(modele.pls)

#coord des ind sur chaque composante pls
modele.pls$model



summary(viande1$CO2)
summary(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),]$CO2)


summary(viande1$Particules)
summary(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),]$Particules)

summary(viande1$Acidification)
summary(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),]$Acidification)

summary(viande1$UtilisationSol)
summary(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),]$UtilisationSol)

summary(viande1$Ecotoxicité)
summary(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),]$Ecotoxicité)

summary(viande1$EutrophisationT)
summary(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),]$EutrophisationT)


summary(viande1$EutrophisationEauD)
summary(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),]$EutrophisationEauD)

summary(viande1$EpuisementEau)
summary(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),]$EpuisementEau)

summary(viande1$Eenergétique)
summary(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),]$Eenergétique)


summary(viande1$rayonnement)
summary(viande1[which(viande1$sgroupe_aliment=="viandes cuites"),]$rayonnement)

summary(viande1$Fphotochimique)
summary(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),]$Fphotochimique)

summary(viande1$EMineraux)
summary(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),]$EMineraux)


summary(viande1$Appauvrissement)
summary(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),]$Appauvrissement)

summary(viande1$EutrophisationM)
summary(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),]$EutrophisationM)





#les viandes existantes
viandes_crues<-data.frame(viande1[which(viande1$sgroupe_aliment=="viandes crues"),])
levels(as.factor(viandes_crues$produit))

poissons_cuits<-data.frame(viande1[which(viande1$sgroupe_aliment=="poissons cuits"),])
levels(as.factor(poissons_cuits$produit))

poissons_crus<-data.frame(viande1[which(viande1$sgroupe_aliment=="poissons crus"),])
levels(as.factor(poissons_crus$produit))

charcuteries<-data.frame(viande1[which(viande1$sgroupe_aliment=="charcuteries"),])
levels(as.factor(charcuteries$produit))


œufs<-data.frame(viande1[which(viande1$sgroupe_aliment=="œufs"),])
levels(as.factor(œufs$produit))


