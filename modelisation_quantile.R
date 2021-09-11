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

#les données de bonnes qualités
viande1 <- data.frame(viande1[which(viande1$note_qualite<3),]) 

#calcul du R2
#a<-data.frame(rqfit1$residuals)
#c<-0
#length(groupe11$score_unique)
#for (i in 1:length(groupe11$score_unique)) {b<- b+a$tau..0.90[i]^2
#c<-c + (groupe11$score_unique[i]-mean(groupe11$score_unique))^2
#e<-b/c}
#R2 <- 1-e

#ggplot(data = viande1,
 #      aes(,score_unique)) + 
  #     geom_boxplot()


#quantile plot


ggplot(data=viande1,aes(sample=score_unique)) + stat_qq() +
  geom_abline( intercept = mean(viande1$score_unique, slope = sd(viande1$score_unique)))
#ou
qqnorm(viande1$score_unique)
qqline(viande1$score_unique,col=2)

library(moments)
#déviatio,
skewness(viande1$score_unique)
#applatissement
kurtosis(viande1$score_unique)


#moyenne et quantile pour la variable score_unique selon la valeur prise par 
#la var explicative preparation   
with(viande1,tapply(score_unique,preparation,mean))
with(viande1,tapply(score_unique,preparation,quantile, probs=seq(0,1,0.1)))


#distribution des différents modes de préparation
eff<-data.frame(table(viande1$preparation))
pourc<-data.frame(prop.table(table(viande1$preparation)))
eff_pourc <- data.frame(eff,pourc[,2])
colnames(eff_pourc)[3] <- "Taux"
colnames(eff_pourc)[2] <- "Effectif"
colnames(eff_pourc)[1] <- "Préparation"


bp1<- ggplot(viande1, aes(x=preparation, y=score_unique)) +
  geom_boxplot()
bp1


bp<- ggplot(eff_pourc, aes(x=Préparation, y=Taux, fill=Effectif)) +
  geom_bar(width = 1, stat = "identity")
bp

#modalité de référence
viande1$livraison <- relevel(factor(viande1$livraison ), "Glacé")
viande1$emballage <- relevel(factor(viande1$emballage ), "Pas d'emballage")
viande1$preparation <- relevel(factor(viande1$preparation ), "Pas de préparation")

#lancement du modèle de régression quantile
# pas de centage par ce qu'o
rqfit3 <- rq(score_unique ~  preparation + emballage + livraison  ,method="fn",tau=c(0.25,0.5,0.75,0.9), data=viande1)
summary(rqfit3,se = "boot",bsmethod = "xy")
plot(rqfit3)

#pourcentage des valeurs aberrante selon le mode de préparation
pourc_aberr1 <- data.frame(table(viande1[which(viande1$score_unique>2.5027),]$preparation))
pourc_aberr <- data.frame(pourc_aberr1['Var1'],pourc_aberr1['Freq']/data.frame(table(viande1$preparation))['Freq'])

#pourcentage des valeurs supérieurs au  3ème quartile selon le mode de préparation 
x <- data.frame(prop.table(table(viande1[which(viande1$preparation=='Four' & viande1$score_unique>2.0883 ),]$emballage)))
y <- data.frame(prop.table(table(viande1[which(viande1$preparation=='Four' & viande1$score_unique>2.0883 ),]$livraison)))

x1 <- data.frame(prop.table(table(viande1[which(viande1$preparation=='Bouilli' & viande1$score_unique>2.0883 ),]$emballage)))
y1 <- data.frame(prop.table(table(viande1[which(viande1$preparation=='Bouilli' & viande1$score_unique>2.0883 ),]$livraison)))

x2 <- data.frame(prop.table(table(viande1[which(viande1$preparation=='Poêle' & viande1$score_unique>2.0883 ),]$emballage)))
y2 <- data.frame(prop.table(table(viande1[which(viande1$preparation=='Poêle' & viande1$score_unique>2.0883 ),]$livraison)))

summary(viande1$score_unique)


summary(viande1$score_unique)
boxplot(viande1$score_unique)
#résumé des modes de préparation
summary(viande1[which(viande1$preparation=='Micro onde'),]$score_unique)
summary(viande1[which(viande1$preparation=='Bouilli'),]$score_unique)
summary(viande1[which(viande1$preparation=='Four'),]$score_unique)
table(viande1[which(viande1$score_unique>2),]$preparation)

#à comparer peut être après
rqfit1 <- rq(score_unique ~ preparation  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=viande1)
rqfit2 <- rq(score_unique ~ preparation + emballage  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=viande1)
rqfit3 <- rq(score_unique ~  preparation + emballage + livraison  ,method="fn",tau=c(0.25,0.5,0.75, 0.9), data=viande1)
