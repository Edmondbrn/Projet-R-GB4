#==========================================================================================================
#                                           Stats descriptive
#==========================================================================================================

getwd()
setwd("C:/Users/Edmond/OneDrive - Université Côte d'Azur/Documents/Cours/Cours Polytech/GB4/S1/Stat/Projet-R-GB4")
data = read.csv("LC-Adductomics.csv")
data
View(data)


# Partie 1
dim(data)
summary(data)

# Détail des moyennes et écart types des Albumines
summary(data$Albumin.adduct.of.CysGly)
summary(data$Albumin.adduct.of.Nacetylcysteine)
summary(data$Albumin.adduct.of.sulfonic.acid)
summary(data$Albumin.unadducted)

table(data$gender)
# 120 hommes et 77 femmes, test stat pour voir si différence significative

table(data$centre)
# 110 personnes sur Turin et 87 à Varese

table(data$smoking_status)
# 67 fumeur actuel, 64 anciens fumeurs et 66 never

table(data$case)
# 101 Sains et 96 Cancéreux 

table(data$case, data$gender)
table(data$case, data$smoking_status)

par(mfrow = c(1, 1))
hist(data$age.recr)
qqnorm(data$age.recr)
shapiro.test(data$age.recr)
# répartition normale pas très claire


par(mfrow = c(1,1 ))
# Crée un vecteur pour les moyennes et comparaison en fonction des statut tabagique
(moyennes_par_etat = tapply(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status, mean))
# Ecart type
ecart_types_par_etat <- tapply(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status, sd)
# barplot 
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "Nacetylcystéine", ylim = c(0,5) )
# Barre d'erreur sd
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")


(moyennes_par_etat = tapply(data$Albumin.adduct.of.CysGly, data$smoking_status, mean))
ecart_types_par_etat <- tapply(data$Albumin.adduct.of.CysGly, data$smoking_status, sd)
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "CysGly", ylim = c(0,500))
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")


ecart_types_par_etat <- tapply(data$Albumin.adduct.of.sulfonic.acid, data$smoking_status, sd)
(moyennes_par_etat = tapply(data$Albumin.adduct.of.sulfonic.acid, data$smoking_status, mean))
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "Sulfonic", ylim = c(0,100) )
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")


ecart_types_par_etat <- tapply(data$Albumin.unadducted, data$smoking_status, sd)
(moyennes_par_etat = tapply(data$Albumin.unadducted, data$smoking_status, mean))
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "Non lié", ylim = c(0,700))
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")


ecart_types_par_etat <- tapply(data$Albumin.adduct.of.Nacetylcysteine, data$case, sd)
moyennes_par_etat = tapply(data$Albumin.adduct.of.Nacetylcysteine, data$case, mean)
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "Nacetylcystéine", ylim = c(0,4) )
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")

ecart_types_par_etat <- tapply(data$Albumin.adduct.of.CysGly, data$case, sd)
moyennes_par_etat = tapply(data$Albumin.adduct.of.CysGly, data$case, mean)
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "CysGly", ylim = c(0,450))
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")


ecart_types_par_etat <- tapply(data$Albumin.adduct.of.sulfonic.acid, data$case, sd)
moyennes_par_etat = tapply(data$Albumin.adduct.of.sulfonic.acid, data$case, mean)
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "Sulfonic", ylim = c(0,50))
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")

ecart_types_par_etat <- tapply(data$Albumin.unadducted, data$case, sd)
moyennes_par_etat = tapply(data$Albumin.unadducted, data$case, mean)
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "Non lié", ylim = c(0,600) )
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")




#Histogramme 
hist(log(data$Albumin.adduct.of.Nacetylcysteine))
wilcox.test(data$Albumin.adduct.of.Nacetylcysteine[data$case=="1"],data$Albumin.adduct.of.Nacetylcysteine[data$case=="0"])

hist(data$Albumin.adduct.of.CysGly)
wilcox.test(data$Albumin.adduct.of.CysGly[data$case=="1"],data$Albumin.adduct.of.CysGly[data$case=="0"])

hist(data$Albumin.adduct.of.sulfonic.acid)
wilcox.test(data$Albumin.adduct.of.sulfonic.acid[data$case=="1"],data$Albumin.adduct.of.sulfonic.acid[data$case=="0"])

hist(data$Albumin.unadducted)
wilcox.test(data$Albumin.unadducted[data$case=="1"],data$Albumin.unadducted[data$case=="0"])


mymodel = glm(data$case ~data$Albumin.adduct.of.Nacetylcysteine)
summary(mymodel)

mymodel2 = glm(data$case ~ data$Albumin.adduct.of.CysGly)
summary(mymodel2)

mymodel3 = glm(data$case ~ data$Albumin.adduct.of.sulfonic.acid)
summary(mymodel3)

mymodel4 = glm(data$case ~ data$smoking_status)
summary(mymodel4)

# Visualisation de la corrélation entre les 4 variables numériques
test = data.frame(data$Albumin.adduct.of.Nacetylcysteine, data$Albumin.adduct.of.CysGly, data$Albumin.unadducted, data$Albumin.adduct.of.sulfonic.acid)
pairs(test)

# Calcul des proportions avec prop.table
(proportion <- prop.table(table(data$smoking_status)))
Légende = names(proportion)

library(ggplot2)
# Création du graphique circulaire
ggplot(NULL, aes(x = "", y = proportion, fill = Légende)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title = "Diagramme circulaire représentant les\n proportions des états tabagiques") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm")) +
  geom_text(aes(label = paste0(round(proportion * 100), "%")), position = position_stack(vjust = 0.5), size = 5)



(proportion2 <- prop.table(table(data$smoking_status[data$case == "1"])))
Légende = names(proportion2)

library(ggplot2)
# Création du graphique circulaire
ggplot(NULL, aes(x = "", y = proportion2, fill = Légende)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title = "Diagramme circulaire représentant les\n proportions des états tabagiques") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm")) +
  geom_text(aes(label = paste0(round(proportion2 * 100), "%")), position = position_stack(vjust = 0.5), size = 5)


#============================ Valeurs aberrantes ==============================================

# Mise en forme des données
albumine = c(data$Albumin.adduct.of.Nacetylcysteine , data$Albumin.adduct.of.CysGly ,
            data$Albumin.unadducted , data$Albumin.adduct.of.sulfonic.acid)
type_albumine = rep(c("Albumin.adduct.of.Nacetylcysteine", "Albumin.adduct.of.CysGly",
                  "Albumin.unadducted", "Albumin.adduct.of.sulfonic.acid"), each = 197)

data3 = data.frame(albumine, type_albumine)
View(data3)

library(ggplot2)
# boxplot général pour voir que c'est la d Les points en dehors des traits sont aberrants
ggplot(data3, aes(x=type_albumine,y=albumine, fill=type_albumine)) +
  geom_boxplot()+ 
  xlab(label = "Différents types d'albumines") +
  ylab(label = "Concentration") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Boxplot des différentes albumines")

# Zoom pour mieux voir
ggplot(data3, aes(x=type_albumine,y=albumine, fill=type_albumine)) +
  geom_boxplot()+ 
  xlab(label = "Différents types d'albumines") +
  ylab(label = "Concentration") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Boxplot des différentes albumines")+
  ylim(c(0,10))

# Exclusion valeurs aberrantes

# https://delladata.fr/comment-detecter-les-outliers-avec-r/

#============================ CysGly ===============================

# Filtrer les données
(albu_CysGly <- data3[data3$type_albumine == "Albumin.adduct.of.CysGly",])
boxplot(albu_CysGly$albumine)
albu_CysGly2 = albu_CysGly

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.adduct.of.CysGly)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.adduct.of.CysGly %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_CysGly$albumine[index_outliers] = NaN
boxplot(albu_CysGly$albumine)


data$smoking_status = as.factor(data$smoking_status)
albu = c("Albumin.adduct.of.CysGly", "Albumin.adduct.of.Nacetylcysteine", "Albumin.unadducted", "Albumin.adduct.of.sulfonic.acid")

# Dégage les valeurs aberrantes par état tabagique
for (i in levels(data$smoking_status)){
  outliers = boxplot.stats(data$Albumin.adduct.of.CysGly[data$smoking_status == i])$out
  index_outliers = which(data$Albumin.adduct.of.CysGly %in% c(outliers))
  # Affiche les valeurs détéctées
  print(i)
  print(outliers)
  # Suppression des valeurs aberrantes
  albu_CysGly2$albumine[index_outliers] = NaN
}
boxplot(albu_CysGly2$albumine)

#============================ Nacetyl ===============================

# Filtrer les données
(albu_Nacetyl <- data3[data3$type_albumine == "Albumin.adduct.of.Nacetylcysteine",])
albu_Nacetyl2 = albu_Nacetyl

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.adduct.of.Nacetylcysteine)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.adduct.of.Nacetylcysteine %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_Nacetyl$albumine[index_outliers] = NaN
boxplot(albu_Nacetyl$albumine)

# Dégage les valeurs aberrantes par état tabagique
for (i in levels(data$smoking_status)){
  outliers = boxplot.stats(data$Albumin.adduct.of.Nacetylcysteine[data$smoking_status == i])$out
  index_outliers = which(data$Albumin.adduct.of.Nacetylcysteine %in% c(outliers))
  # Affiche les valeurs détéctées
  print(i)
  print(outliers)
  # Suppression des valeurs aberrantes
  albu_Nacetyl2$albumine[index_outliers] = NaN
}
boxplot(albu_Nacetyl2$albumine)

#============================ Acide sulfonique ===============================

# Filtrer les données
(albu_sulfo <- data3[data3$type_albumine == "Albumin.adduct.of.sulfonic.acid",])
albu_sulfo2 = albu_sulfo

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.adduct.of.sulfonic.acid)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.adduct.of.sulfonic.acid %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_sulfo$albumine[index_outliers] = NaN
boxplot(albu_sulfo$albumine)

# Dégage les valeurs aberrantes par état tabagique
for (i in levels(data$smoking_status)){
  outliers = boxplot.stats(data$Albumin.adduct.of.sulfonic.acid[data$smoking_status == i])$out
  index_outliers = which(data$Albumin.adduct.of.sulfonic.acid %in% c(outliers))
  # Affiche les valeurs détéctées
  print(i)
  print(outliers)
  # Suppression des valeurs aberrantes
  albu_sulfo2$albumine[index_outliers] = NaN
}
boxplot(albu_sulfo2$albumine)

#============================ Acide sulfonique ===============================

# Filtrer les données
(albu_non_undu <- data3[data3$type_albumine == "Albumin.unadducted",])
albu_non_undu2 = albu_non_undu

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.unadducted)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.unadducted %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_non_undu$albumine[index_outliers] = NaN
boxplot(albu_non_undu$albumine)

for (i in levels(data$smoking_status)){
  outliers = boxplot.stats(data$Albumin.unadducted[data$smoking_status == i])$out
  index_outliers = which(data$Albumin.unadducted %in% c(outliers))
  # Affiche les valeurs détéctées
  print(i)
  print(outliers)
  # Suppression des valeurs aberrantes
  albu_non_undu2$albumine[index_outliers] = NaN
}
boxplot(albu_non_undu2$albumine)

#============================ Nouveau dataset sans valeur aberrante ===============================

val_albu = c(albu_CysGly$albumine, albu_Nacetyl$albumine, albu_sulfo$albumine, albu_non_undu$albumine)
val_albu2 = c(albu_CysGly2$albumine, albu_Nacetyl2$albumine, albu_sulfo2$albumine, albu_non_undu2$albumine)
type_Albumine = c(albu_CysGly$type_albumine, albu_Nacetyl$type_albumine, albu_sulfo$type_albumine, albu_non_undu$type_albumine)

data_corr_graph = data.frame(val_albu, type_Albumine)
data_corr_graph2 = data.frame(val_albu2, type_Albumine)

data_corr_graph2

library(ggplot2)
# boxplot général pour voir que c'est mieux
ggplot(data_corr_graph2, aes(x=type_Albumine,y=val_albu, fill=type_Albumine)) +
  geom_boxplot()+ 
  xlab(label = "Différents types d'albumines") +
  ylab(label = "Concentration") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Boxplot des différentes albumines")

ggplot(data_corr_graph2, aes(x=type_Albumine,y=val_albu2, fill=type_Albumine)) +
  geom_boxplot()+ 
  xlab(label = "Différents types d'albumines") +
  ylab(label = "Concentration") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Boxplot des différentes albumines")

# Zoom pour les plus petits
ggplot(data_corr_graph, aes(x=type_Albumine,y=val_albu, fill=type_Albumine)) +
  geom_boxplot()+ 
  xlab(label = "Différents types d'albumines") +
  ylab(label = "Concentration") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Boxplot des différentes albumines")+
  ylim(c(0,5))

ggplot(data_corr_graph2, aes(x=type_Albumine,y=val_albu2, fill=type_Albumine)) +
  geom_boxplot()+ 
  xlab(label = "Différents types d'albumines") +
  ylab(label = "Concentration") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Boxplot des différentes albumines")+
  ylim(c(0,10))

# =============================== Analyse nouveau dataset ===================================

# Implémantation dans le tableau à l'origine
data$CysGly_cor = albu_CysGly$albumine
data$Nacetyl_corr = albu_Nacetyl$albumine
data$sulfo_corr = albu_sulfo$albumine
data$unadducted_corr = albu_non_undu$albumine

data$CysGly_corr2 = albu_CysGly2$albumine
data$Nacetyl_corr2 = albu_Nacetyl2$albumine
data$sulfo_corr2 = albu_sulfo2$albumine
data$unadducted_corr2 = albu_non_undu2$albumine

hist(log10(data$unadducted_corr2))
qqnorm(log(data$unadducted_corr2))
shapiro.test(log(data$Nacetyl_corr2)) # test T sur Nacetyl et non unduit

# Corrélation / ACP

(X = data[12:15])
(X2 = data[8:11])
(Y = data[, 7])
(Y2 = data[, 6])
(Y3 = data[, 2])

PCA = prcomp(na.omit(X))
PCA2 = prcomp(X2)
out <- summary(PCA)
ev <- out$importance[2, ]
cum_ev <- out$importance[3, ]

plot(cum_ev,
     pch = 19, col = "navy", las = 1, type = "b", ylim = c(0, 1),
     ylab = "Cumulative proportion of explained variance", xlab = "PCs", cex.lab = 1.5
)


(mypal <- brewer.pal(n = length(unique(Y3)), name = "Paired"))
(mycolors <- mypal[as.numeric(as.factor(Y3))])

plot(PCA$x[, 1:2],
     pch = 19, las = 1, col = mycolors, cex.lab = 1.5,
     xlab = substitute(PC[1] * " (" * a * "% e.v.)", list(a = round(ev[1] * 100, digits = 2))),
     ylab = substitute(PC[2] * " (" * a * "% e.v.)", list(a = round(ev[2] * 100, digits = 2)))
)
legend("topright", pch = 19, col = mypal, legend = levels(as.factor(Y3)), pt.cex = 1, cex = 1.2)

par(mfrow = c(1, 1))
mycor <- cor(na.omit(X), PCA$x[, 1:2])
plot(mycor[, 1:2],
     xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), cex = 0.1, pch = 19, las = 1, cex.lab = 1.5,
     xlab = substitute(PC[1] * " (" * a * "% e.v.)", list(a = round(ev[1] * 100, digits = 2))),
     ylab = substitute(PC[2] * " (" * a * "% e.v.)", list(a = round(ev[2] * 100, digits = 2)))
)
arrows(
  x0 = rep(0, nrow(mycor)), y0 = rep(0, nrow(mycor)),
  x1 = mycor[, 1], y1 = mycor[, 2], length = 0.1, col = "navy"
)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
xseq <- seq(-1, 1, length.out = 10000)
lines(xseq, sqrt(1 - xseq^2))
lines(xseq, -sqrt(1 - xseq^2))
text(mycor[, 1] + sign(mycor[, 1]) * 0.25, mycor[, 2] + 0.1, labels = colnames(X), cex = 1.2, col = "navy")



# =============================== Analyse nouveau dataset/ stat descriptive===================================





# Coorélation test
pairs(data.frame(data$CysGly_cor, data$unadducted_corr, data$sulfo_corr, data$Nacetyl_corr))
plot(data$CysGly_cor, data$unadducted_corr)

help(cor.test) #spearman cra ne suit pas loi normale
cor.test(data$Nacetyl_corr, data$CysGly_cor, method = "spearman" ) # p value =  3.979e-11
cor.test(data$Albumin.adduct.of.Nacetylcysteine, data$Albumin.adduct.of.CysGly, method = "spearman")

cor.test(data$Nacetyl_corr, data$sulfo_corr, method = "spearman")# p = 6.324e-07
cor.test(data$Albumin.adduct.of.Nacetylcysteine, data$Albumin.adduct.of.sulfonic.acid, method = "spearman")

cor.test(data$Nacetyl_corr, data$unadducted_corr, method = "spearman") #p = 0.02825
cor.test(data$Albumin.adduct.of.Nacetylcysteine, data$Albumin.unadducted, method = "spearman")

cor.test(data$CysGly_cor, data$sulfo_corr, method = "spearman") # 0.097
cor.test(data$Albumin.adduct.of.CysGly, data$Albumin.adduct.of.sulfonic.acid, method = "spearman") # 0.047

cor.test(data$CysGly_cor, data$unadducted_corr, method = "spearman") # 2.236e-09
cor.test(data$Albumin.adduct.of.CysGly, data$Albumin.unadducted, method = "spearman")

cor.test(data$unadducted_corr, data$sulfo_corr, method = "spearman")# 0.1271
cor.test(data$Albumin.unadducted, data$Albumin.adduct.of.sulfonic.acid, method = "spearman") #0.006


for (i in seq(12,15)){
  mymodel = lm(c(data[,i])~data$Nacetyl_corr)
  qqnorm(mymodel$residuals)
}

for (i in seq(12,15)){
  mymodel = lm(c(data[,i])~data$sulfo_corr)
  qqnorm(mymodel$residuals)
}

for (i in seq(12,15)){
  mymodel = lm(c(data[,i])~data$CysGly_cor)
  qqnorm(mymodel$residuals)
}
# modèle de régression linéaire non valide, car pas de normalité des résidus

# test de dépendance

chisq.test(table(data$case, data$smoking_status)) # p value = 3.058e-7 donc statut tabgique influe sur cancer
barplot(table(data$case, data$smoking_status), legend.text = c("Sain","Cancer"))
####### A refaire sur ggplot
mymodel2 = glm(data$case ~ data$smoking_status, family = "binomial")
summary(mymodel2)
# baisse logit de la proba de 1.12 si former et de 2.06 si never comparé à Current

chisq.test(table(data$centre, data$case)) # Pas de diff entre les centres pour cancer et tabgisme
chisq.test(table(data$centre, data$smoking_status))

# Test moyenne entre adductomic et cancer

# moyenne CysGly selon état tabagique
mean(na.omit(data$CysGly_cor[data$smoking_status == "Former"]),)
mean(na.omit(data$CysGly_cor[data$smoking_status == "Current"]),)
mean(na.omit(data$CysGly_cor[data$smoking_status == "Never"]),)

mean(na.omit(data$CysGly_corr2[data$smoking_status == "Former"]),)
mean(na.omit(data$CysGly_corr2[data$smoking_status == "Current"]),)
mean(na.omit(data$CysGly_corr2[data$smoking_status == "Never"]),)


# moyenne acide sulfonique selon état tabagique
mean(na.omit(data$sulfo_corr[data$smoking_status == "Former"]),)
mean(na.omit(data$sulfo_corr[data$smoking_status == "Current"]),)
mean(na.omit(data$sulfo_corr[data$smoking_status == "Never"]),)

mean(na.omit(data$sulfo_corr2[data$smoking_status == "Former"]),)
mean(na.omit(data$sulfo_corr2[data$smoking_status == "Current"]),)
mean(na.omit(data$sulfo_corr2[data$smoking_status == "Never"]),)

# moyenne Nacetyl selon état tabagique
mean(na.omit(data$Nacetyl_corr[data$smoking_status == "Former"]),)
mean(na.omit(data$Nacetyl_corr[data$smoking_status == "Current"]),)
mean(na.omit(data$Nacetyl_corr[data$smoking_status == "Never"]),)

mean(na.omit(data$Nacetyl_corr2[data$smoking_status == "Former"]),)
mean(na.omit(data$Nacetyl_corr2[data$smoking_status == "Current"]),)
mean(na.omit(data$Nacetyl_corr2[data$smoking_status == "Never"]),)


# moyenne non adducté selon état tabagique
mean(na.omit(data$unadducted_corr[data$smoking_status == "Former"]),)
mean(na.omit(data$unadducted_corr[data$smoking_status == "Current"]),)
mean(na.omit(data$unadducted_corr[data$smoking_status == "Never"]),)

mean(na.omit(data$unadducted_corr2[data$smoking_status == "Former"]),)
mean(na.omit(data$unadducted_corr2[data$smoking_status == "Current"]),)
mean(na.omit(data$unadducted_corr2[data$smoking_status == "Never"]),)

mean(na.omit(data$unadducted_corr2[data$case == "0"]),)
mean(na.omit(data$unadducted_corr2[data$case == "1"]),)



# Les tests

########## CysGly ############
wilcox.test(data$CysGly_cor[data$case == "0"], data$CysGly_cor[data$case == "1"])
summary(glm(data$case ~ data$CysGly_cor, family = "binomial"))

wilcox.test(data$CysGly_corr2[data$case == "0"], data$CysGly_corr2[data$case == "1"])
summary(glm(data$case ~ data$CysGly_corr2, family = "binomial"))

wilcox.test(data$Albumin.adduct.of.CysGly[data$case == "0"], data$Albumin.adduct.of.CysGly[data$case == "1"])
summary(glm(data$case ~ data$Albumin.adduct.of.CysGly, family = "binomial"))


############# Nacetyl ##############
wilcox.test(data$Nacetyl_corr[data$case == "0"], data$Nacetyl_corr[data$case == "1"])
summary(glm(data$case ~ data$Nacetyl_corr, family = "binomial"))

wilcox.test(data$Nacetyl_corr2[data$case == "0"], data$Nacetyl_corr2[data$case == "1"])
summary(glm(data$case ~ data$Nacetyl_corr2, family = "binomial"))

wilcox.test(data$Albumin.adduct.of.Nacetylcysteine[data$case == "0"], data$Albumin.adduct.of.Nacetylcysteine[data$case == "1"])
summary(glm(data$case ~ data$Albumin.adduct.of.Nacetylcysteine, family = "binomial"))




t.test(log(data$Nacetyl_corr2[data$case == "0"]), log(data$Nacetyl_corr2[data$case == "1"]),
       alternative = "less")





############ Non adductée ###########
wilcox.test(data$Albumin.unadducted[data$case == "0"], data$Albumin.unadducted[data$case == "1"])
summary(glm(data$case ~ data$Albumin.unadducted, family = "binomial"))

wilcox.test(data$unadducted_corr2[data$case == "0"], data$unadducted_corr2[data$case == "1"])
summary(glm(data$case ~ data$unadducted_corr2, family = "binomial"))


wilcox.test(data$unadducted_corr[data$case == "0"], data$unadducted_corr[data$case == "1"])
summary(glm(data$case ~ data$unadducted_corr, family = "binomial"))

hist(log(data$unadducted_corr))
qqnorm(log(data$unadducted_corr))

t.test(log(data$Albumin.unadducted[data$case == "0"]), log(data$Albumin.unadducted[data$case == "1"]),
       alternative = "t")







########### Acide sulfonique ##########
wilcox.test(data$sulfo_corr[data$case == "0"], data$sulfo_corr[data$case == "1"])
summary(glm(data$case ~ data$sulfo_corr, family = "binomial"))

wilcox.test(data$sulfo_corr2[data$case == "0"], data$sulfo_corr2[data$case == "1"])
summary(glm(data$case ~ data$sulfo_corr2, family = "binomial"))

wilcox.test(data$Albumin.adduct.of.sulfonic.acid[data$case == "0"], data$Albumin.adduct.of.sulfonic.acid[data$case == "1"])
summary(glm(data$case ~ data$Albumin.adduct.of.sulfonic.acid, family = "binomial"))

##### TEst kruskall/ Dunn  ####
kruskal.test(data$CysGly_cor, data$smoking_status)
kruskal.test(data$CysGly_corr2, data$smoking_status)
kruskal.test(data$Albumin.adduct.of.CysGly, data$smoking_status)

#=======================================================================
kruskal.test(data$Nacetyl_corr, data$smoking_status)
kruskal.test(data$Nacetyl_corr2, data$smoking_status)
kruskal.test(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status)

dunn.test(data$Nacetyl_corr, data$smoking_status,  method = "bonferroni")
dunn.test(data$Nacetyl_corr2, data$smoking_status,  method = "bonferroni")
dunn.test(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status,  method = "bonferroni")


boxplot(log(data$Nacetyl_corr2)~data$smoking_status)
summary(aov(log(data$Nacetyl_corr2)~ data$smoking_status))
TukeyHSD(aov(log(data$Nacetyl_corr2)~ data$smoking_status))

#Barplot moyenne Nacetyl en fonction de smoking status

#=======================================================================


kruskal.test(data$Albumin.unadducted, data$smoking_status)
dunn.test(data$Albumin.unadducted, data$smoking_status,  method = "bonferroni")

kruskal.test(data$unadducted_corr2, data$smoking_status)
dunn.test(data$unadducted_corr2, data$smoking_status,  method = "bonferroni")



kruskal.test(data$unadducted_corr, data$smoking_status)
dunn.test(data$unadducted_corr, data$smoking_status,  method = "bonferroni")


kruskal.test(data$Albumin.adduct.of.sulfonic.acid, data$smoking_status)
kruskal.test(data$sulfo_corr, data$smoking_status)
kruskal.test(data$sulfo_corr2, data$smoking_status)
dunn.test(data$sulfo_corr2, data$smoking_status)

help(tapply)
# Ecart type
ecart_types_par_etat <- tapply(data$CysGly_cor, data$smoking_status, sd)
# barplot 
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "CysGly", ylim = c(0,250) )
# Barre d'erreur sd
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")


# ========================= RAndom Forest
data = read.csv("LC-Adductomics.csv")
data$Code.participants = NULL
data$centre = NULL

data$Albumin.adduct.of.Nacetylcysteine = NULL
data$Albumin.unadducted= NULL
data$Albumin.adduct.of.sulfonic.acid = NULL
data$Albumin.adduct.of.CysGly = NULL

data$CysGly_corr2 = albu_CysGly2$albumine
data$Nacetyl_corr2 = albu_Nacetyl2$albumine
data$sulfo_corr2 = albu_sulfo2$albumine
data$unadducted_corr2 = albu_non_undu2$albumine

precision = c()
for (i in seq(1:100)){
  # Préparation des données 70 apprentissage et 30 de test
  # Ressort 70% des lignes
  (train_idx = sample(1:nrow(data), 0.7*nrow(data)))
  (train_data = data[train_idx,])
  (test_data = data[-train_idx,])
  
  dim(test_data) # 45 données
  dim(train_data) # 105 données
  
  # Prédiction de l'espèce en fonction des autres variables en utilisant 100 arbres
  (rf_model = randomForest(as.factor(case) ~ ., data = train_data, ntree = 100, na.action = na.omit))
  help(randomForest)
  # Predictions des espèces sur les données test
  (predictions = predict(rf_model, test_data))
  
  # Calcul le nombre de bonnes prédictions en %
  (accuracy = sum(na.omit(predictions) == test_data$case) / nrow(test_data))
  # 97.8% donc le modèle a une exactitude plus que satisfaisante
  
  # Extrait les variables les plus importantes
  (features_importance = importance(rf_model))
  
  (sorted_importance = features_importance[order(-features_importance),])
  precision = c(precision, accuracy)
  print(i)
  print(sorted_importance)
  
}
print(mean(precision)) # Après 100 tours, précision moyenne de 56%
  
barplot(sorted_importance, horiz = F, main = "Importance des Caractéristiques", xlab = "Scored d'importance",
        cex.names =0.8, las = 1)

(df = as.data.frame(sorted_importance))

ggplot(geom_bar(df))

ggplot(df, aes(x = rownames(df), y = df$sorted_importance , fill = rownames(df))) +
  geom_bar(stat = "identity")+
  theme_minimal()
  #labs(x = "Réaction", y = "Valeur", fill = "Traitement")

......................................................

#association entre bmi et risque de cancer :
mymodelA = lm(data$bmi ~ data$case)
summary(mymodelA)
wilcox.test(data$bmi[data$case == "0"],data$bmi[data$case == "1"])
boxplot(data$bmi ~data$case)
#-> pas d'association entre le l'imc et le risque d'avoir un cancer

#association entre bmi et la modification des differents albumines :

mymodelB = lm(data$bmi ~ data$Albumin.adduct.of.CysGly)
summary(mymodelB)
#-> Pas d'associatation entre l'imc et le fait d'avoir de l'albumine avec la Cys-Gly

mymodelC = lm(data$bmi ~ data$Albumin.adduct.of.sulfonic.acid)
summary(mymodelC)
#-> // acide sulfonique

mymodelD = lm(data$bmi ~ data$Albumin.adduct.of.Nacetylcysteine)
summary(mymodelD)
#-> // N-acetylcysteine

mymodelE = lm(data$bmi ~ data$Albumin.unadducted)
summary(mymodelE)


mymodelE = lm(data$bmi ~ data$unadducted_corr2)
summary(mymodelE)
#-> // non adduite (= non modifié)
plot(data$bmi, data$unadducted_corr2)

library(ggplot2)
#Box plot de la modif de l'albumine en f° du statut fumeur (avant suppression valeurs abbérantes)
#albumine avec N-acetylcysteine en f° du statut fumeur 
ggplot(data, aes(x = smoking_status, y = Albumin.adduct.of.Nacetylcysteine, fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de la modification de l'albumine en fonction du statut fumeur (avant suppression valeurs abbérantes)", 
       x = "Statut fumeur", 
       y = "Albumine avec N-acetylcysteine") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))

#albumine avec CysGly //
ggplot(data, aes(x = smoking_status, y = Albumin.adduct.of.CysGly, fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de la modification de l'albumine en fonction du statut fumeur (avant suppression valeurs abbérantes)", 
       x = "Statut fumeur", 
       y = "Albumine avec Cysteine-Glycine") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))

#albumine avec acide sulfonique //
ggplot(data, aes(x = smoking_status, y = Albumin.adduct.of.sulfonic.acid, fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de la modification de l'albumine en fonction du statut fumeur (avant suppression valeurs abbérantes)", 
       x = "Statut fumeur", 
       y = "Albumine avec l'acide sulfonique") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))

#albumine non modifié //
ggplot(data, aes(x = smoking_status, y = Albumin.unadducted, fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de l'albumine non modifié en fonction du statut fumeur (avant suppression valeurs abbérantes)", 
       x = "Statut fumeur", 
       y = "Albumine non modifié") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))


#Box plot de la modif de l'albumine en f° du statut fumeur (après suppression valeurs abbérantes)
#albumine avec N-acetylcysteine en f° du statut fumeur 
ggplot(data, aes(x = smoking_status, y = Nacetyl_corr2 , fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de la modification de l'albumine en fonction du statut fumeur (après suppresion valeurs abbérantes", 
       x = "Statut fumeur", 
       y = "Albumine avec N-acetylcysteine") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))

#albumine avec CysGly //
ggplot(data, aes(x = smoking_status, y = CysGly_cor , fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de la modification de l'albumine en fonction du statut fumeur (après suppresion valeurs abbérantes", 
       x = "Statut fumeur", 
       y = "Albumine avec Cysteine-Glycine") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))

#albumine avec acide sulfonique //
ggplot(data, aes(x = smoking_status, y = sulfo_corr , fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de la modification de l'albumine en fonction du statut fumeur (après suppresion valeurs abbérantes", 
       x = "Statut fumeur", 
       y = "Albumine avec l'acide sulfonique") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))

#albumine non modifié //
ggplot(data, aes(x = smoking_status, y = unadducted_corr , fill = smoking_status)) +
  geom_boxplot(alpha = 0.3) + 
  labs(title = "Boxplot de la modification de l'albumine en fonction du statut fumeur (après suppresion valeurs abbérantes", 
       x = "Statut fumeur", 
       y = "Albumine non modifié") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm"))


#affichage de chaque boxplot séparément pour les diff modifs de l'albumine (dataset : data_corr_graph2) après suppression variables abbérantes

#Pour Albumin.adduct.of.Nacetylcysteine :
ggplot(data_corr_graph2[data_corr_graph2$type_Albumine == "Albumin.adduct.of.Nacetylcysteine", ], 
       aes(x = type_Albumine, y = val_albu2)) +
  geom_boxplot(fill = "blue") + 
  labs(x = "Albumin.adduct.of.Nacetylcysteine", 
       y = "Concentration", 
       title = "Boxplot pour Albumin.adduct.of.Nacetylcysteine") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

#Pour Albumin.adduct.of.CysGly :
ggplot(data_corr_graph2[data_corr_graph2$type_Albumine == "Albumin.adduct.of.CysGly", ], 
       aes(x = type_Albumine, y = val_albu2)) +
  geom_boxplot(fill = "green") + 
  labs(x = "Albumin.adduct.of.CysGly", 
       y = "Concentration", 
       title = "Boxplot pour Albumin.adduct.of.CysGly") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

#Pour Albumin.adduct.of.sulfonic.acid :
ggplot(data_corr_graph2[data_corr_graph2$type_Albumine == "Albumin.adduct.of.sulfonic.acid", ], 
       aes(x = type_Albumine, y = val_albu2)) +
  geom_boxplot(fill = "red") + 
  labs(x = "Albumin.adduct.of.sulfonic.acid", 
       y = "Concentration", 
       title = "Boxplot pour Albumin.adduct.of.sulfonic.acid") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

#Pour Albumin.unadducted :
ggplot(data_corr_graph2[data_corr_graph2$type_Albumine == "Albumin.unadducted", ], 
       aes(x = type_Albumine, y = val_albu2)) +
  geom_boxplot(fill = "purple") + 
  labs(x = "Albumin.unadducted", 
       y = "Concentration", 
       title = "Boxplot pour Albumin.unadducted") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")
#

