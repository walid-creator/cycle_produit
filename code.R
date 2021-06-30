rm(list=ls())
setwd("~/dcma")
#install.packages('readODS')
#install.packages("tidyverse")
#install.packages(c("FactoMineR", "factoextra"))
#install.packages("quantreg")

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
colnames(dataset_synthese1)<-c("groupe_aliment","sgroupe_aliment","produit","livraison","emballage","preparation","note_qualite","score_unique", "CO2","appauvrissement","rayonnement","Formation","Particules","Acidifaction","EutrophisationT","EutrophisationTEau","EutrophisationMarine","UtilisationSol","Ecotoxicité","EpuisementEAu","EpuisementEnergie","EpuisementMinéraux")
#analyse desc de la variable d'intérêt 
#tab <- dataset_synthese1[, c(1)] 
#tab_disj <- tab.disjonctif(tab)
summary(dataset_synthese1['score_unique'])
hist(dataset_synthese1$score_unique,col='yellow',main='Distribution de la variable note_qualité',xlab='note', ylab='effectif')

summary(dataset_synthese1$score_unique)
summary(dataset_synthese1$CO2)
summary(dataset_synthese1$appauvrissement)
summary(dataset_synthese1$rayonnement)
summary(dataset_synthese1$Formation)
summary(dataset_synthese1$Particules)
summary(dataset_synthese1$Acidifaction)
summary(dataset_synthese1$EutrophisationT)
summary(dataset_synthese1$EutrophisationTEau)
summary(dataset_synthese1$EutrophisationMarine)
summary(dataset_synthese1$UtilisationSol)
summary(dataset_synthese1$Ecotoxicité)
summary(dataset_synthese1$EpuisementEAu)
summary(dataset_synthese1$EpuisementEnergie)
summary(dataset_synthese1$EpuisementMinéraux)
#modalité de référence
dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")

#la qualité de l'info pour chaque groupe d'aliment
reg<-lm(formula = note_qualite ~    groupe_aliment, data = dataset_synthese1)
summary(reg)
drop1(reg,.~.,test="F")
plat_comp <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'entrées et plats composés' ),] 
glaces <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'glaces et sorbets' ),] 
grasses <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'matières grasses' ),] 
sucres <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'produits sucrés' ),] 
culinaires <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'aides culinaires et ingrédients divers' ),] 
boissons <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'boissons' ),] 
viandes <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'viandes, œufs, poissons' ),] 
fruits_leg <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'fruits, légumes, légumineuses et oléagineux' ),] 
infantiles <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'aliments infantiles' ),] 
céréaliers <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'produits céréaliers' ),] 
laitiers <- dataset_synthese1[which(dataset_synthese1$groupe_aliment == 'lait et produits laitiers' ),] 





#test d'anova d'égalité des moyennes de chaque groupe d'aliment
fit <- aov(score_unique ~ groupe_aliment, data=dataset_synthese1 )
summary(fit)
#on a une pvalue  significative, on rejette l'hypothèse d'égalité des moyennes,
#il existe au moins une paire de moyenne qui ne sont pas égales entre 2 groupes


#normalité
library("car")
qqPlot(newdataset_synthese$score_unique)

#boxplot de variables quali
boxplot(newdataset_synthese$score_unique)




#pourcentage des aliments en bonne qualité pour chaque aliment:
pourc1<-length(plat_comp[which(plat_comp$note_qualite<=3 ),1])/length(plat_comp[,1])
pourc2<-length(glaces[which(glaces$note_qualite<=3 ),1])/length(glaces[,1])
pourc3<-length(grasses[which(grasses$note_qualite<=3 ),1])/length(grasses[,1])
pourc4<-length(sucres[which(sucres$note_qualite<=3 ),1])/length(sucres[,1])
pourc5<-length(culinaires[which(culinaires$note_qualite<=3 ),1])/length(culinaires[,1])
pourc6<-length(boissons[which(boissons$note_qualite<=3 ),1])/length(boissons[,1])
pourc7<-length(viandes[which(viandes$note_qualite<=3 ),1])/length(viandes[,1])
pourc8<-length(fruits_leg[which(fruits_leg$note_qualite<=3 ),1])/length(fruits_leg[,1])
pourc9<-length(infantiles[which(infantiles$note_qualite<=3 ),1])/length(infantiles[,1])
pourc10<-length(céréaliers[which(céréaliers$note_qualite<=3 ),1])/length(céréaliers[,1])
pourc11<-length(laitiers[which(laitiers$note_qualite<=3 ),1])/length(laitiers[,1])
pourc_total<-length(dataset_synthese1[which(dataset_synthese1$note_qualite<=3 ),1])/length(dataset_synthese1[,1])



#faire une ACP sur les différentes émissions de gaz à effet de serres



outputs <- dataset_synthese1[,9:22]
acp<-PCA(outputs, scale.unit = TRUE, graph = TRUE)
print(acp)
print(acp$eig)


?HSBC()
#contribution d'une variable  à un axe
#on garde les individus qui ont une contribution supérieure à la contribution
#moyenne 1/n
acp$var$contrib
contr_ind<-data.frame(acp$ind$contrib,dataset_synthese1["groupe_aliment"],dataset_synthese1["sgroupe_aliment"],dataset_synthese1["produit"])
contr_ind_axe1 <- contr_ind[which(contr_ind$Dim.1>1/2480),c(1,6,7,8)]
contr_ind_axe2 <- contr_ind[which(contr_ind$Dim.2>1/2480),c(2,6,7,8)]
contr_ind_axe3 <- contr_ind[which(contr_ind$Dim.3>1/2480),c(3,6,7,8)]
contr_ind_axe4 <- contr_ind[which(contr_ind$Dim.4>1/2480),c(4,6,7,8)]
contr_ind_axe5 <- contr_ind[which(contr_ind$Dim.5>1/2480),c(5,6,7,8)]

#effectif de chaque groupe d'aliment 
x<-data.frame(table(contr_ind$groupe_aliment))

x1<-data.frame(table(contr_ind_axe1$groupe_aliment))
freqAxe1 <- data.frame(x1['Var1'],x1['Freq']/x['Freq'])
x2<-data.frame(table(contr_ind_axe1$groupe_aliment))
freqAxe2 <- data.frame(x2['Var1'],x2['Freq']/x['Freq'])
x3<-data.frame(table(contr_ind_axe3$groupe_aliment))
freqAxe3 <- data.frame(x3['Var1'],x3['Freq']/x['Freq'])
x4<-data.frame(table(contr_ind_axe1$groupe_aliment))
freqAxe4 <- data.frame(x4['Var1'],x4['Freq']/x['Freq'])
x5<-data.frame(table(contr_ind_axe1$groupe_aliment))
freqAxe5 <- data.frame(x5['Var1'],x5['Freq']/x['Freq'])

#fréquence de chaque groupe d'aliment bien présenté dans chaque axe
tot<-data.frame(x1['Var1'],freqAxe1['Freq'],freqAxe2['Freq'],freqAxe3['Freq'],freqAxe4['Freq'],freqAxe5['Freq'])
colnames(tot)<-c("groupe produit","axe1","axe2","axe3","axe4","axe5")

acp$var$cos2
axes<-acp$ind$coord
#d'après la règle de kaiser on retient 4 axes (vp>1)
#Appauvrissement de la couche d'Ozone est complètement représentée par le cinquième axe

colnames(axes) <- c("axe1","axe2","axe3","axe4","axe5")
axes <- as.data.frame(axes)
#data.frame(newdataset_synthese['score_unique'],axes[,2:5])




library("FactoMineR")
library("factoextra")


#par défaut la fonction pca garde les 5 premières composantes de l'ACP
#c'est exactement le nbre de variables cibles qu'on a gardé 

set.seed(123)
cah <- HCPC (acp, graph = T )
set.seed(123)
classe<-cah$data.clust
cah$desc.axes
print(table(classe$clust))
cah$desc.var
#les ind les plus représentatifs de chque cluster
cah$desc.ind$para# la distance au centre de chaque indiv de chaque cluster
#les var quanti représentant le plus chaque cluster
cah$desc.var



cah$desc.ind
cah$desc.axes

plot(cah,choice="tree")
plot(cah,choice="map", draw.tree=FALSE)
plot(cah,choice="3D.map", axes = c(1,2), title = "Représentation des individus sur le premier plan factoriel")
plot(cah,choice="3D.map", axes = c(3,4), title = "Représentation des individus sur le deuxième plan factoriel" )

groupe1 <-  classe[which(classe$clust=='1'), ]
groupe2 <-  classe[which(classe$clust=='2'), ]
groupe3 <-  classe[which(classe$clust=='3'), ]



#Analyse des produits de chqaue groupes
#groupe1
dataset_synthese1[c(1464, 275, 2037, 384, 518, 2238, 2239, 2339, 2340, 2341), ]$groupe_aliment
dataset_synthese1[c(1464, 275, 2037, 384, 518, 2238, 2239, 2339, 2340, 2341), ]$produit
dataset_synthese1[c(1464, 275, 2037, 384, 518, 2238, 2239, 2339, 2340, 2341), ]$livraison
dataset_synthese1[c(1464, 275, 2037, 384, 518, 2238, 2239, 2339, 2340, 2341), ]$emballage
dataset_synthese1[c(1464, 275, 2037, 384, 518, 2238, 2239, 2339, 2340, 2341), ]$preparation

#groupe2
dataset_synthese1[c(85, 87, 89, 90, 320, 706, 1621, 630, 1523, 1702), ]$groupe_aliment
dataset_synthese1[c(85, 87, 89, 90, 320, 706, 1621, 630, 1523, 1702), ]$produit
dataset_synthese1[c(85, 87, 89, 90, 320, 706, 1621, 630, 1523, 1702), ]$livraison
dataset_synthese1[c(85, 87, 89, 90, 320, 706, 1621, 630, 1523, 1702), ]$emballage
dataset_synthese1[c(85, 87, 89, 90, 320, 706, 1621, 630, 1523, 1702), ]$preparation

#groupe3
dataset_synthese1[c(453, 568, 923, 2010, 229, 15, 24, 27, 478, 13), ]$groupe_aliment
dataset_synthese1[c(453, 568, 923, 2010, 229, 15, 24, 27, 478, 13), ]$produit
dataset_synthese1[c(453, 568, 923, 2010, 229, 15, 24, 27, 478, 13), ]$livraison
dataset_synthese1[c(453, 568, 923, 2010, 229, 15, 24, 27, 478, 13), ]$emballage
dataset_synthese1[c(453, 568, 923, 2010, 229, 15, 24, 27, 478, 13), ]$preparation




?plot

wss<-c()
for (k in 1:10){
  tmp <- kmeans(axes, centers=k)
  wss[k] <- tmp$betweenss/tmp$totss}
#prin(wss)

plot(x=c(1:10), y=wss, pch=20, type="b", xlab="Nombres de groupes", ylab="Rapport de l'inertie inter-classe et l'inertie totale", main= "Technique du coude pour optimiser le nombre de classes" )

?plot

groupes.kmeans <- kmeans(axes,centers=3)

#les centres de chaque cluster
print(groupes.kmeans$centers)

#effectifs par clusters
print(table(groupes.kmeans$cluster))

pairs(axes)
#moyenne de chaque cluster
print(colMeans(axes))
#calcul des écrts types
#on veut que les variables aient la même variance c'est pour c qu'on doit réduire dans le cs opù on travaille vec ds données bruts, il faut que les variables aient la même importance 
apply(axes, 2, sd)# 2: 2eme dimension (écart type par colonne)
pairs(axes, col=c('green', 'blue', 'black', 'red', 'yellow')[groupes.kmeans$cluster] )

print(aggregate(x = axes, by = list(groupes.kmeans$cluster), FUN = mean))




#corrélation entre le Score unique EF et l'émission des autres gazs à effet de serre
cor(dataset_synthese1$score_unique , axes$axe1, method = c("pearson", "kendall", "spearman"))#0.99
cor(dataset_synthese1$score_unique , axes$axe2, method = c("pearson", "kendall", "spearman"))#0.01
cor(dataset_synthese1$score_unique , axes$axe3, method = c("pearson", "kendall", "spearman"))#0.11
cor(dataset_synthese1$score_unique , axes$axe4, method = c("pearson", "kendall", "spearman"))#0.03
cor(dataset_synthese1$score_unique , axes$axe5, method = c("pearson", "kendall", "spearman"))#0.03

#on garde les 4 derniers axes seulement
newdataset_synthese <- data.frame(dataset_synthese1[,1:8],axes[,2:5])
colnames(newdataset_synthese)<-c("groupe_aliment","sgroupe_aliment","produit","livraison","emballage","preparation","note_qualite","score_unique","axe2","axe3","axe4","axe5")

#stat des de la variables score_unique
summary(newdataset_synthese["score_unique"])
boxplot(newdataset_synthese["score_unique"])
hist(newdataset_synthese$score_unique)
porc_out <- length(newdataset_synthese$score_unique[newdataset_synthese$score_unique>2])/2480
porc_out
# ils représentent 8% de l'effectif total, on les supprime 
#newdataset_synthese <- droplevels(newdataset_synthese[-which(newdataset_synthese$score_unique > 2), ] )

#test de normalité
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
#on a bien certaines une corrélation entre les variables explicatives

#modalité des variables
print(levels(as.factor(newdataset_synthese$groupe_aliment)))
print(levels(as.factor(newdataset_synthese$livraison)))
print(levels(as.factor(newdataset_synthese$emballage)))
print(levels(as.factor(newdataset_synthese$preparation)))

#pourcentage de chaque températures 
print(length(newdataset_synthese$livraison[newdataset_synthese$livraison == "Congelé"])/length(newdataset_synthese$livraison))
print(length(newdataset_synthese$livraison[newdataset_synthese$livraison == "Glacé"])/length(newdataset_synthese$livraison))
print(length(newdataset_synthese$livraison[newdataset_synthese$livraison == "Ambiant (court)"])/length(newdataset_synthese$livraison))
print(length(newdataset_synthese$livraison[newdataset_synthese$livraison == "Ambiant (long)"])/length(newdataset_synthese$livraison))
print(length(newdataset_synthese$livraison[newdataset_synthese$livraison == "Ambiant (moyenne)"])/length(newdataset_synthese$livraison))

#pourcentage de chaque type d'emballlage
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Acier"])/length(newdataset_synthese$emballage))#0.04
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Aluminium"])/length(newdataset_synthese$emballage))#0.0004
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Bouteille PET"])/length(newdataset_synthese$emballage))#0.0004
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Bouteille PETE"])/length(newdataset_synthese$emballage))#0.027
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Carton"])/length(newdataset_synthese$emballage))#0.12
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Déjà emballé - Aluminium"])/length(newdataset_synthese$emballage))#0.006
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Déjà emballé - LDPE"])/length(newdataset_synthese$emballage))#0.0028
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Déjà emballé - PET"])/length(newdataset_synthese$emballage))#0.0625
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Déjà emballé - PP/PE"])/length(newdataset_synthese$emballage))#0.01
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Déjà emballé - Verre"])/length(newdataset_synthese$emballage))#0.0056
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "HPDE"])/length(newdataset_synthese$emballage))#0.0125
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "LPDE"])/length(newdataset_synthese$emballage))#0.19
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Papier"])/length(newdataset_synthese$emballage))#0.014
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Pas d'emballage"])/length(newdataset_synthese$emballage))#0.063
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "PP"])/length(newdataset_synthese$emballage))#0.156
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "PS"])/length(newdataset_synthese$emballage))#0.208
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "PVC"])/length(newdataset_synthese$emballage))#0.0004
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "V (PVC)"])/length(newdataset_synthese$emballage))#0.0326
print(length(newdataset_synthese$emballage[newdataset_synthese$emballage == "Verre"])/length(newdataset_synthese$emballage))#0.037

#pourcentage de chaque type de preparation
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Bouilli"])/length(newdataset_synthese$preparation))#0.013
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Cuisson à l'eau"])/length(newdataset_synthese$preparation))#0.031
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Four"])/length(newdataset_synthese$preparation))#0.122
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Friture"])/length(newdataset_synthese$preparation))#0.0004
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Micro onde"])/length(newdataset_synthese$preparation))#0.11
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Pas de préparation"])/length(newdataset_synthese$preparation))#0.68
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Poêle"])/length(newdataset_synthese$preparation))#0.015
print(length(newdataset_synthese$preparation[newdataset_synthese$preparation == "Réfrigéré chez le consommateur"])/length(newdataset_synthese$preparation))#0.023

score_unique_microOnde<-dataset_synthese1[which(dataset_synthese1$preparation=="Micro onde"),c(1,3,8)]
summary(score_unique_microOnde)





#traitement sur variables explicatives
newdataset_synthese$emballage[which(newdataset_synthese$emballage=="Bouteille PET")]<-'Bouteille PET/PETE' 
newdataset_synthese$emballage[which(newdataset_synthese$emballage=="Bouteille PETE")]<-'Bouteille PET/PETE'
newdataset_synthese$emballage[which(newdataset_synthese$emballage=="Déjà emballé - PET")]<-'Bouteille PET/PETE'




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









#modélisations

#régression pls
#install.packages("pls")
library(pls)
Y<-as.matrix(data.frame(newdataset_synthese['score_unique'],axes[,2:5]))
X<-newdataset_synthese[,c(1,4,5,6,7)]
#install.packages('fastDummies')
X1 <- fastDummies::dummy_cols(X[,c(1,2,3,4)])
X<-as.matrix(data.frame(X1[,5:47],scale(X[,5])))
modele <- mvr(Y ~ X, method="oscorespls",ncomp=5)
validationplot(modele, val.type= 'MSE')
summary(modele)
print(coef(modele))
?validationplot


#réression quantile
library(quantreg)
?rq
help.start()
# un exemple
example(rq)

newdataset_synthese$groupe_aliment <- relevel(factor(newdataset_synthese$groupe_aliment), "produits sucrés")
set.seed(1)
#traitement de la variable emballage
newdataset_synthese[which(dataset_synthese1$emballage == 'Bouteille PET' ),] <-'Bouteille PET/PETE'
newdataset_synthese[which(dataset_synthese1$emballage == 'Bouteille PETE' ),] <-'Bouteille PET/PETE'
newdataset_synthese[which(dataset_synthese1$emballage == 'Déjà emballé - PET' ),] <-'Bouteille PET/PETE'




rqfit1 <- rq(score_unique ~ groupe_aliment + livraison + emballage + preparation  ,tau=c(.5, .99), data=newdataset_synthese)
#dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")
summary(rqfit1,se = "boot")

plot(note_qualite ~ groupe_aliment, data = dataset_synthese1, pch = 16)
abline(lm(note_qualite ~ groupe_aliment, data = dataset_synthese1), col = "red", lty = 2)
abline(rq(note_qualite ~ groupe_aliment, data = dataset_synthese1), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)
?summary.rq

rqfit2 <- rq(axe2 ~ groupe_aliment + livraison + emballage + preparation  ,tau=c(.05, .25, .5, .75, .95), data=newdataset_synthese)
#dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")
summary(rqfit2,se = "boot")

rqfit3 <- rq(axe3 ~ groupe_aliment + livraison + emballage + preparation  ,tau=c(.05, .25, .5, .75, .95), data=newdataset_synthese)
#dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")
summary(rqfit3,se = "boot")

rqfit4 <- rq(axe4 ~ groupe_aliment + livraison + emballage + preparation  ,tau=c(.05, .25, .5, .75, .95), data=newdataset_synthese)
#dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")
summary(rqfit4,se = "boot")

rqfit5 <- rq(axe5 ~ groupe_aliment + livraison + emballage + preparation  ,tau=c(.05, .25, .5, .75, .95), data=newdataset_synthese)
#dataset_synthese1$groupe_aliment <- relevel(factor(dataset_synthese1$groupe_aliment), "entrées et plats composés")
summary(rqfit5,se = "boot")


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


newdataset_par_etape <- rbind(a,b,c,d,e,f)

newdataset_par_etape <- droplevels(newdataset_par_etape[-which(newdataset_par_etape$score_unique > 2), ] )


#centrer et réduire la variable cible pour s'approcher plus d'une loi normale et valider les hypothèses du modèle inéaire 
newdataset_synthese["score_unique"] <- scale(newdataset_synthese$score_unique)



#regression linéaire
modele1<-lm(formula = score_unique ~  groupe_aliment + etape,data=newdataset_par_etape)
summary(modele1)
#c'est l'agriculture qui émet plus de gaz à effets de serre cart tous les coefficients sont négtifs
#par contre le R2 de la régresion linéaire est faibles pour les 2 modèles, il fut essayer alors d'autres modèles

modele2<-lm(formula = score_unique ~    groupe_aliment + etape + groupe_aliment:etape + groupe_aliment:etape:note_qualite,data=newdataset_par_etape)
summary(modele2)
drop1(modele2,.~.,test="F")

anova(modele1,modele2)

#données 3
dataset_par_ingredient <- read_ods("data.ods",skip=3,sheet="Detail ingredient",col_names=TRUE)
data_region <- read_excel("data_region.xlsx")

