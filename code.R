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

#données
dataset_synthese <- read_ods("data.ods",skip=1,sheet="Synthese",col_names=TRUE)
#pas de valeurs manquantes
sum(is.na(dataset_synthese))
colMeans(is.na(dataset_synthese))
head(dataset_synthese)

#suppression des colonnes inutiles
dataset_synthese1 <- dataset_synthese[,c(-1,-2,-6,-7,-8)]

#faire une ACP sur les différentes émissions de gaz à effet de serres
outputs <- dataset_synthese1[,9:22]
acp<-PCA(outputs, scale.unit = TRUE, graph = TRUE)
print(acp)
print(acp$eig)
axes<-acp$ind$coord
#d'après la règle de kaiser on retient 4 axes (vp>1)
axes<-axes[,-5]
colnames(axes) <- c("axe1","axe2","axe3","axe4")
axes <- as.data.frame(axes)

#corrélation entre le Score unique EF et l'émission des autres gaezs à effet de serre
cor(dataset_synthese1$`Score unique EF (mPt/kg de produit)` , axes$axe1, method = c("pearson", "kendall", "spearman"))#0.99
cor(dataset_synthese1$`Score unique EF (mPt/kg de produit)` , axes$axe2, method = c("pearson", "kendall", "spearman"))#0.01
cor(dataset_synthese1$`Score unique EF (mPt/kg de produit)` , axes$axe3, method = c("pearson", "kendall", "spearman"))#0.11
cor(dataset_synthese1$`Score unique EF (mPt/kg de produit)` , axes$axe4, method = c("pearson", "kendall", "spearman"))#0.03

#on garde les 3 derniers axes seulement
newdataset_synthese <- data.frame(dataset_synthese1[,1:8],axes[,2:4])
colnames(newdataset_synthese)<-c("groupe_aliment","sgroupe_aliment","produit","livraison","emballage","preparation","note_qualite","score_unique","axe1","axe2","axe3","axe4")



#régression linéaire
modele<-lm(formula = score_unique ~  note_qualite + groupe_aliment + livraison + emballage + preparation  , data=newdataset_synthese )
summary(modele)
#la proba de se tromper en rejetant H0 au niveau 5% est très faible,
#il y'a donc effet de la qualité des données sur le score unique
#effet des variables qualitatives:
drop1(modele,.~.,test="F")
#on a bien un effet de toutes les variables qualitatives sur le score unique

#données 2
##émission des gaz à effet de serre pour chaque étape du cycle de vie
dataset_par_etape <- read_ods("data.ods",skip=3,sheet="Detail etape",col_names=TRUE)
#pas de valeurs manquantes
sum(is.na(dataset_par_etape))
colMeans(is.na(dataset_par_etape))
head(dataset_par_etape)

#,dataset_par_etape[,10],dataset_par_etape[,11],dataset_par_etape[,12],dataset_par_etape[,13],dataset_par_etape[,14]
#ajout de colonnes
colnames(dataset_par_etape)[8:14]<- "score_unique"
newdataset_par_etape <- rbind(as.data.frame(dataset_par_etape[,8]), as.data.frame(dataset_par_etape[,9]))
etape <- rbind(rep("agriculture",2480), rep("transformation",2480),rep("emballage",2480),rep("transport",2480),rep("distribution",2480),rep("consommation",2480),rep("total",2480))

a<-cbind(as.data.frame(dataset_par_etape[,8]),rep("agriculture",2480))
colnames(a) <- c("score_unique","etape")
b<-cbind(as.data.frame(dataset_par_etape[,9]),rep("transformation",2480))
colnames(b) <- c("score_unique","etape")
c<-cbind(as.data.frame(dataset_par_etape[,10]),rep("emballage",2480))
colnames(c) <- c("score_unique","etape")
d<-cbind(as.data.frame(dataset_par_etape[,11]),rep("transport",2480))
colnames(d) <- c("score_unique","etape")
e<-cbind(as.data.frame(dataset_par_etape[,12]),rep("distribution",2480))
colnames(e) <- c("score_unique","etape")
f<-cbind(as.data.frame(dataset_par_etape[,13]),rep("consommation",2480))
colnames(f) <- c("score_unique","etape")
g<-cbind(as.data.frame(dataset_par_etape[,14]),rep("total",2480))
colnames(g) <- c("score_unique","etape")

newdataset_par_etape <- rbind(a,b,c,d,e,f,g)

#regression linéaire
modele1<-lm(formula = score_unique ~  etape,data=newdataset_par_etape)
summary(modele1)
#c'est l'agriculture qui émet plus de gaz à effets de serre
#par contre le R2 de la régresion linéaire est faibles pour les 2 modèles, il fut essayer alors d'autres modèles


#suppression des colonnes inutiles
dataset_par_etape1 <- dataset_par_etape[,c(-1,-2,-6,-7,-8)]

#
rbind(data frame A, data frame B)

#renommer les colonnes
colnames(newdataset_synthese)<-c("groupe_aliment","sgroupe_aliment","produit","livraison","emballage","preparation","note_qualite","score_unique","axe1","axe2","axe3","axe4")




#données 3
dataset_par_ingredient <- read_ods("data.ods",skip=3,sheet="Detail ingredient",col_names=TRUE)



data_region <- read_excel("data_region.xlsx")

