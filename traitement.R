rm(list=ls())
setwd("~/dcma") 
#install.packages('readODS')
#install.packages("tidyverse")
#install.packages(c("FactoMineR", "factoextra"))
#install.packages("quantreg")

#libraries##
library(readODS)
library(readxl)
library("FactoMineR")
library("factoextra")
library(ggplot2)
library(dplyr)
library(tidyverse)#graphes
library(Rcmdr)
library(Factoshiny) #Factominer avec un lgiciel dédié poue acp, acm, clustering et un rapport d'analyse est aussi disponible

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
qqPlot(dataset_synthese1$score_unique)

#boxplot de variables quali
ggplot(data=dataset_synthese1,
        aes (groupe_aliment,score_unique)) +
        geom_boxplot ()
       
         
ggplot(data=dataset_synthese1,
       aes (factor(0),CO2)) +
       geom_boxplot ()



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
axes_acp <- as.data.frame(axes)
#data.frame(newdataset_synthese['score_unique'],axes[,2:5])





library("FactoMineR")
library("factoextra")

#graphes factoextra
#histogramme pourcentage de variances expliqué
fviz_screeplot(acp, addlabels = TRUE, ylim = c(0, 50))
#cercle des corrélations
fviz_pca_var(acp, col.var = "black")
#cercle des corrélations avec couleurs
fviz_pca_var(acp, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
#histogramme contribution axes
# Contributions de var aux axes
fviz_contrib(acp, choice = "var", axes = 1, top = 10)
fviz_contrib(acp, choice = "var", axes = 2, top = 10)
fviz_contrib(acp, choice = "var", axes = 3, top = 10)
fviz_contrib(acp, choice = "var", axes = 4, top = 10)
fviz_contrib(acp, choice = "var", axes = 5, top = 10)


#par défaut la fonction pca garde les 5 premières composantes de l'ACP
#c'est exactement le nbre de variables cibles qu'on a gardé 

set.seed(123)
cah <- HCPC (acp, graph = T ,nb.par=20,nb.clust=3)
set.seed(123)
classe<-cah$data.clust
#les axes qui représentent le plus chaque cluster
cah$desc.axes
print(table(classe$clust))
#les ind les plus représentatifs de chaque cluster
g1<-dataset_synthese1[as.numeric(rownames(data.frame(cah$desc.ind$para$'1'))),c(1,2,3)]# le min des distances de chaque indiv du centre  de chaque cluster
g2<-dataset_synthese1[as.numeric(rownames(data.frame(cah$desc.ind$para$'2'))),c(1,2,3)]
g3<-dataset_synthese1[as.numeric(rownames(data.frame(cah$desc.ind$para$'3'))),c(1,2,3)]


cah$desc.ind$dist# le max des distances de chaque indiv du centre des autres clusters

#les numéros d'individus de chaque cluster



#les var quanti représentant le plus chaque cluster
cah$desc.var

#coefficient de variation total et de chaque cluster

n1<-data.frame(cah$desc.var$quanti$'1')['sd.in.category']/data.frame(cah$desc.var$quanti$'1')['Mean.in.category']
colnames(n1)<-"coeff var cat"
ntot<-data.frame(cah$desc.var$quanti$'1')['Overall.sd']/data.frame(cah$desc.var$quanti$'1')['Overall.mean']
colnames(ntot)<-"coeff var tot1"
clust1<-data.frame(cah$desc.var$quanti$'1',n1,ntot)

n2<-data.frame(cah$desc.var$quanti$'2')['sd.in.category']/data.frame(cah$desc.var$quanti$'2')['Mean.in.category']
colnames(n2)<-"coeff var"
ntot<-data.frame(cah$desc.var$quanti$'2')['Overall.sd']/data.frame(cah$desc.var$quanti$'2')['Overall.mean']
colnames(ntot)<-"coeff var tot2"
clust2<-data.frame(cah$desc.var$quanti$'2',n2,ntot)

n3<-data.frame(cah$desc.var$quanti$'3')['sd.in.category']/data.frame(cah$desc.var$quanti$'3')['Mean.in.category']
colnames(n3)<-"coeff var"
ntot<-data.frame(cah$desc.var$quanti$'3')['Overall.sd']/data.frame(cah$desc.var$quanti$'3')['Overall.mean']
colnames(ntot)<-"coeff var tot3"
clust3<-data.frame(cah$desc.var$quanti$'3',n3,ntot)


  
# une représentation plus élégante
plot(cah,choice="tree")
plot(cah,choice="map", draw.tree=FALSE)
plot(cah,choice="3D.map", axes = c(1,2), title = "Représentation des individus sur le premier plan factoriel")
plot(cah,choice="3D.map", axes = c(3,4), title = "Représentation des individus sur le deuxième plan factoriel" )

groupe_tot <- data.frame(classe,dataset_synthese1$groupe_aliment) 


#recosntruction des 3 groupes

groupe1 <-  data.frame(classe[which(classe$clust=='1'), ],dataset_synthese1[which(classe$clust=='1'), ]$groupe_aliment,dataset_synthese1[which(classe$clust=='1'), ]$sgroupe_aliment,dataset_synthese1[which(classe$clust=='1'), ]$produit)
groupe2 <-  data.frame(classe[which(classe$clust=='2'), ],dataset_synthese1[which(classe$clust=='2'), ]$groupe_aliment,dataset_synthese1[which(classe$clust=='2'), ]$sgroupe_aliment,dataset_synthese1[which(classe$clust=='2'), ]$produit)
groupe3 <-  data.frame(classe[which(classe$clust=='3'), ],dataset_synthese1[which(classe$clust=='3'), ]$groupe_aliment,dataset_synthese1[which(classe$clust=='3'), ]$sgroupe_aliment,dataset_synthese1[which(classe$clust=='3'), ]$produit)
colnames(groupe1)[16]<-"groupe_aliment"
colnames(groupe2)[16]<-"groupe_aliment"
colnames(groupe3)[16]<-"groupe_aliment"
colnames(groupe1)[17]<-"sgroupe_aliment"
colnames(groupe2)[17]<-"sgroupe_aliment"
colnames(groupe3)[17]<-"sgroupe_aliment"
colnames(groupe1)[18]<-"produit"
colnames(groupe2)[18]<-"produit"
colnames(groupe3)[18]<-"produit"

#porcentage de chaque type_aliment dans chaque groupe
prop.table(table(groupe1$groupe_aliment))
prop.table(table(groupe2$groupe_aliment))
prop.table(table(groupe3$groupe_aliment))

#types de viandes de chaque groupe
groupe1_viande <- data.frame(groupe1[which(groupe1$groupe_aliment=='viandes, œufs, poissons'),c(16,17,18) ] )
groupe1_boisson <- data.frame(groupe1[which(groupe1$groupe_aliment=='boissons'),c(16,17,18) ] )
groupe1_entree <- data.frame(groupe1[which(groupe1$groupe_aliment=='entrées et plats composés'),c(16,17,18) ] )


groupe2_viande <- data.frame(groupe2[which(groupe2$groupe_aliment=='viandes, œufs, poissons'),c(16,17,18) ] )
groupe2_boisson <- data.frame(groupe2[which(groupe2$groupe_aliment=='boissons'),c(16,17,18) ] )
groupe2_entree <- data.frame(groupe2[which(groupe2$groupe_aliment=='entrées et plats composés'),c(16,17,18) ] )
groupe2_boisson$produit

groupe3_viande <- data.frame(groupe3[which(groupe3$groupe_aliment=='viandes, œufs, poissons'),c(16,17,18) ] )
groupe3_boisson <- data.frame(groupe3[which(groupe3$groupe_aliment=='boissons'),c(16,17,18) ] )
groupe3_entree <- data.frame(groupe3[which(groupe3$groupe_aliment=='entrées et plats composés'),c(16,17,18) ] )
groupe3_boisson$produit

#pourcentage des sous groupe d'aliments dans chaque groupe d'aliment dans chaque groupe de LA cah
taux_gr1_viande <- data.frame(prop.table(table(groupe1_viande$sgroupe_aliment)))
colnames(taux_gr1_viande)[1] <- "Type_aliment"
colnames(taux_gr1_viande)[2] <- "taux"
taux_gr1_viande[1,2] <- taux_gr1_viande[1,2] + taux_gr1_viande[9,2] + taux_gr1_viande [10,2] 
taux_gr1_viande[3,2] <- taux_gr1_viande[3,2] + taux_gr1_viande [4,2] 
taux_gr1_viande[6,2] <- taux_gr1_viande[6,2] + taux_gr1_viande[7,2] + taux_gr1_viande [8,2] 
taux_gr1_viande <- taux_gr1_viande[c(-4,-7,-8,-9, -10),]
taux_gr1_viande["Type_aliment"] <- c("Viandes","Charcuterie","Mollusques et crustacés","Oeufs", "Poissons")
taux_gr1_viande[,2] <- taux_gr1_viande[,2]*100

taux_gr2_viande <- data.frame(prop.table(table(groupe2_viande$sgroupe_aliment)))
colnames(taux_gr2_viande)[1] <- "Type_aliment"
colnames(taux_gr2_viande)[2] <- "taux"
taux_gr2_viande[1,2] <- taux_gr2_viande[1,2] + taux_gr2_viande[8,2] + taux_gr2_viande [9,2] 
taux_gr2_viande[3,2] <- taux_gr2_viande[3,2] + taux_gr2_viande [4,2] 
taux_gr2_viande[5,2] <- taux_gr2_viande[5,2] + taux_gr2_viande[6,2] + taux_gr2_viande [7,2] 
taux_gr2_viande <- taux_gr2_viande[c(-4,-6,-7,-8,-9),]
taux_gr2_viande["Type_aliment"] <- c("Viandes","Charcuterie","Mollusques et crustacés", "Poissons")
taux_gr2_viande[,2] <- taux_gr2_viande[,2]*100

taux_gr3_viande <- data.frame(prop.table(table(groupe3_viande$sgroupe_aliment)))
colnames(taux_gr3_viande)[1] <- "Type_aliment"
colnames(taux_gr3_viande)[2] <- "taux"
taux_gr3_viande[1,2] <- taux_gr3_viande[1,2] + taux_gr3_viande[3,2] + taux_gr3_viande [4,2]
taux_gr3_viande <- taux_gr3_viande[c(-3,-4),]
taux_gr3_viande["Type_aliment"] <- c("Viandes","Charcuterie")
taux_gr3_viande[,2] <- taux_gr3_viande[,2]*100



taux_gr1_boisson <- data.frame(prop.table(table(groupe1_boisson$sgroupe_aliment)))
colnames(taux_gr1_boisson)[1] <- "Type_aliment"
colnames(taux_gr1_boisson)[2] <- "taux"
taux_gr1_boisson[,2] <- taux_gr1_boisson[,2]*100


taux_gr2_boisson <- data.frame(prop.table(table(groupe2_boisson$sgroupe_aliment)))
colnames(taux_gr2_boisson)[1] <- "Type_aliment"
colnames(taux_gr2_boisson)[2] <- "taux"
taux_gr2_boisson[,2] <- taux_gr2_boisson[,2]*100

taux_gr3_boisson <- data.frame(prop.table(table(groupe3_boisson$sgroupe_aliment)))
colnames(taux_gr3_boisson)[1] <- "Type_aliment"
colnames(taux_gr3_boisson)[2] <- "taux"
taux_gr3_boisson[,2] <- taux_gr3_boisson[,2]*100

#les entrées et plats composées
taux_gr1_entree <- data.frame(prop.table(table(groupe1_entree$sgroupe_aliment)))
colnames(taux_gr1_entree)[1] <- "Type_aliment"
colnames(taux_gr1_entree)[2] <- "taux"
taux_gr1_entree[,2] <- taux_gr1_entree[,2]*100

taux_gr2_entree <- data.frame(prop.table(table(groupe2_entree$sgroupe_aliment)))
colnames(taux_gr2_entree)[1] <- "Type_aliment"
colnames(taux_gr2_entree)[2] <- "taux"
taux_gr2_entree[,2] <- taux_gr2_entree[,2]*100

taux_gr3_entree <- data.frame(prop.table(table(groupe3_entree$sgroupe_aliment)))
colnames(taux_gr3_entree)[1] <- "Type_aliment"
colnames(taux_gr3_entree)[2] <- "taux"
taux_gr3_entree[,2] <- taux_gr3_entree[,2]*100










colnames(groupe_tot)[16]<-"groupe_aliment"
#statistique descriptive : la fonction summary donne juste les effectifs mais c'est mieux
#de faire avec les lignes de codes suivantes
summary(groupe1["groupe_aliment"])
summary(groupe2["groupe_aliment"])
summary(groupe3["groupe_aliment"])






#effecdifs
eff<-data.frame(table(groupe_tot$groupe_aliment))[,c(1,2)]
colnames(eff)<-c("aliments","effectif")
part_total<-data.frame(data.frame(table(groupe1$groupe_aliment))[,c(1,2)],data.frame(table(groupe2$groupe_aliment))[,2],data.frame(table(groupe3$groupe_aliment))[,2])
colnames(part_total)<-c("groupe d'aliment","groupe 1","groupe 2","groupe 3")
part_total["groupe 1"] <- part_total["groupe 1"] /  eff["effectif"]
part_total["groupe 2"] <- part_total["groupe 2"] /  eff["effectif"]
part_total["groupe 3"] <- part_total["groupe 3"] /  eff["effectif"]






#règle du du coude pour le choix du nombre de clusters

wss<-c()
for (k in 1:10){
  tmp <- kmeans(axes, centers=k)
  wss[k] <- tmp$betweenss/tmp$totss}
#prin(wss)

plot(x=c(1:10), y=wss, pch=20, type="b", xlab="Nombres de groupes", ylab="Rapport de l'inertie inter-classe et l'inertie totale", main= "Technique du coude pour optimiser le nombre de classes" )



groupes.kmeans <- kmeans(axes_acp,centers=7)

#les centres de chaque cluster
print(groupes.kmeans$centers)

#effectifs par clusters
print(table(groupes.kmeans$cluster))

#les pairs de nuages de point des axes
pairs(axes)
#moyenne de chaque cluster
print(colMeans(axes))
#calcul des écarts types
#on veut que les variables aient la même variance c'est pour c qu'on doit réduire dans le cs opù on travaille vec ds données bruts, il faut que les variables aient la même importance 
apply(axes_acp, 2, sd)# 2: 2eme dimension (écart type par colonne)
pairs(axes_acp, col=c('green', 'blue', 'black')[groupes.kmeans$cluster] )

print(aggregate(x = axes, by = list(groupes.kmeans$cluster), FUN = mean))


#corrélation entre le Score unique EF et l'émission des autres gazs à effet de serre
cor(dataset_synthese1$score_unique , axes$axe1, method = c("pearson", "kendall", "spearman"))#0.99
cor(dataset_synthese1$score_unique , axes$axe2, method = c("pearson", "kendall", "spearman"))#0.01
cor(dataset_synthese1$score_unique , axes$axe3, method = c("pearson", "kendall", "spearman"))#0.11
cor(dataset_synthese1$score_unique , axes$axe4, method = c("pearson", "kendall", "spearman"))#0.03
cor(dataset_synthese1$score_unique , axes$axe5, method = c("pearson", "kendall", "spearman"))#0.03

#on garde les 5 derniers axes seulement
newdataset_synthese <- data.frame(dataset_synthese1[,1:7],axes[,1:5])
colnames(newdataset_synthese)<-c("groupe_aliment","sgroupe_aliment","produit","livraison","emballage","preparation","note_qualite","axe1","axe2","axe3","axe4","axe5")

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




#lire les tableaux de résultats dont on a besoin
#exportation des données intéressantes
#outputs <- read_ods ("outputs.ods")
#axes_acp <- read_ods("axes_acp.ods")



