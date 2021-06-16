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
colnames(newdataset_synthese)<-c("groupe_aliment","sgroupe_aliment","produit","livraison","emballage","preparation","note_qualite","score_unique","axe2","axe3","axe4")

#stat des de la variab§les score_unique
summary(newdataset_synthese["score_unique"])
boxplot(newdataset_synthese["score_unique"])
hist(newdataset_synthese$score_unique)
porc_out <- length(newdataset_synthese$score_unique[newdataset_synthese$score_unique>2])/2480
porc_out
# ils représentent 8% de l'effectif total, on les supprime 
newdataset_synthese <- droplevels(newdataset_synthese[-which(newdataset_synthese$score_unique > 2), ] )

shapiro.test(newdataset_synthese$score_unique)

#centrer et réduire la variable cible pour s'approcher plus d'une loi normale et valider les hypothèses du modèle inéaire 
newdataset_synthese["score_unique"] <- scale(newdataset_synthese$score_unique)



#analyse bivariée
tab <- table(newdataset_synthese$groupe_aliment,newdataset_synthese$livraison)
tab1 <- table(newdataset_synthese$groupe_aliment,newdataset_synthese$emballage)
tab2 <- table(newdataset_synthese$groupe_aliment,newdataset_synthese$preparation)
tab3 <- table(newdataset_synthese$livraison,newdataset_synthese$emballage)
tab4 <- table(newdataset_synthese$livraison,newdataset_synthese$preparation)
tab5 <- table(newdataset_synthese$emballage,newdataset_synthese$preparation)

fisher.test(tab,simulate.p.value=TRUE)
chisq.test(tab1)
chisq.test(tab2)
chisq.test(tab3)
chisq.test(tab4)
chisq.test(tab5)
#on a bun certaines une corrélation entre les variables explicatives

print(levels(as.factor(newdataset_synthese$groupe_aliment)))
print(levels(as.factor(newdataset_synthese$livraison)))
print(levels(as.factor(newdataset_synthese$emballage)))
print(levels(as.factor(newdataset_synthese$preparation)))


#régression avec modèle linéaire multiple
modele1<-lm(formula = score_unique ~  note_qualite + groupe_aliment + livraison + emballage + preparation  , data=newdataset_synthese )
summary(modele1)
plot(modele1,2)
#la proba de se tromper en rejetant H0 au niveau 5% est très faible,
#il y'a donc effet de la qualité des données sur le score unique
#effet des variables qualitatives:
drop1(modele1,.~.,test="F")

modele2<-lm(formula = score_unique ~ note_qualite + groupe_aliment + livraison + emballage + preparation + groupe_aliment:note_qualite  + emballage:note_qualite + preparation:note_qualite  , data=newdataset_synthese )
summary(modele2)
drop1(modele2,.~.,test="F")

anova(modele1,modele2)
#on rejette l'hypothèse de non-intéraction entre note qualité et les autres var quali
#il y' bien un effet des intéractions de la  qualité de l'information sur les variabes quali sur l'mission de gaz


#données 2
##émission des gaz à effet de serre pour chaque étape du cycle de vie
dataset_par_etape <- read_ods("data.ods",skip=3,sheet="Detail etape",col_names=TRUE)
#pas de valeurs manquantes
sum(is.na(dataset_par_etape))
colMeans(is.na(dataset_par_etape))
head(dataset_par_etape)

#ajout de la colonne étape
colnames(dataset_par_etape)[8:14]<- "score_unique"
etape <- rbind(rep("agriculture",2480), rep("transformation",2480),rep("emballage",2480),rep("transport",2480),rep("distribution",2480),rep("consommation",2480),rep("total",2480))

a<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,8]),rep("agriculture",2480))
colnames(a) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","etape")
b<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,9]),rep("transformation",2480))
colnames(b) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","etape")
c<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,10]),rep("emballage",2480))
colnames(c) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","etape")
d<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,11]),rep("transport",2480))
colnames(d) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","etape")
e<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,12]),rep("distribution",2480))
colnames(e) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","etape")
f<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,13]),rep("consommation",2480))
colnames(f) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","etape")
g<-cbind(as.data.frame(dataset_par_etape[,c(3,4,5,7)]),as.data.frame(dataset_par_etape[,14]),rep("total",2480))
colnames(g) <- c("groupe_aliment","sgroupe_aliment","produit","note_qualite","score_unique","etape")

newdataset_par_etape <- rbind(a,b,c,d,e,f,g)

newdataset_par_etape <- droplevels(newdataset_par_etape[-which(newdataset_par_etape$score_unique > 2), ] )


#centrer et réduire la variable cible pour s'approcher plus d'une loi normale et valider les hypothèses du modèle inéaire 
newdataset_synthese["score_unique"] <- scale(newdataset_synthese$score_unique)



#regression linéaire
modele1<-lm(formula = score_unique ~  note_qualite + groupe_aliment + etape,data=newdataset_par_etape)
summary(modele1)
#c'est l'agriculture qui émet plus de gaz à effets de serre cart tous les coefficients sont négtifs
#par contre le R2 de la régresion linéaire est faibles pour les 2 modèles, il fut essayer alors d'autres modèles

modele2<-lm(formula = score_unique ~  note_qualite + groupe_aliment + etape + note_qualite*groupe_aliment*etape ,data=newdataset_par_etape)
summary(modele2)
drop1(modele2,.~.,test="F")

anova(modele1,modele2)

#données 3
dataset_par_ingredient <- read_ods("data.ods",skip=3,sheet="Detail ingredient",col_names=TRUE)
data_region <- read_excel("data_region.xlsx")

