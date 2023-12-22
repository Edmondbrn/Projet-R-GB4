#==========================================================================================================
#                                           Stat descriptives
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
chisq.test(table(data$case, data$smoking_status))
#p-value = 3.058e-07 donc on rejette H0 et accepte H1 = différence de proportion (current a beaucoup de cancer et never pas beacoup)

table(data$case,data$centre)
chisq.test(table(data$case, data$centre))
#p-value = 0.5459 soit <0,05 donc pas de différences significatives entre les centres)

hist(data$age.recr)
qqnorm(data$age.recr)
shapiro.test(data$age.recr)
# répartition normale pas très claire

# Crée un vecteur pour les moyennes et comparaison en fonction des statut tabagique
moyennes_par_etat = tapply(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status, mean)
# Ecart type
ecart_types_par_etat <- tapply(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status, sd)
# barplot 
bp = barplot(height = moyennes_par_etat, col = "skyblue", main = "Nacetylcystéine", ylim = c(0,5) )
# Barre d'erreur sd
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "black")


moyennes_par_etat = tapply(data$Albumin.adduct.of.CysGly, data$smoking_status, mean)
barplot(height = moyennes_par_etat, col = "skyblue", main = "CysGly" )

moyennes_par_etat = tapply(data$Albumin.adduct.of.sulfonic.acid, data$smoking_status, mean)
barplot(height = moyennes_par_etat, col = "skyblue", main = "Sulfonic" )

moyennes_par_etat = tapply(data$Albumin.unadducted, data$smoking_status, mean)
barplot(height = moyennes_par_etat, col = "skyblue", main = "Non lié" )

kruskal.test(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status)
dunn.test(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status, method = "bonferroni")
help(dunn.test)


moyennes_par_etat = tapply(data$Albumin.adduct.of.Nacetylcysteine, data$case, mean)
barplot(height = moyennes_par_etat, col = "skyblue", main = "Nacetylcystéine" )

moyennes_par_etat = tapply(data$Albumin.adduct.of.CysGly, data$case, mean)
barplot(height = moyennes_par_etat, col = "skyblue", main = "CysGly" )

moyennes_par_etat = tapply(data$Albumin.adduct.of.sulfonic.acid, data$case, mean)
barplot(height = moyennes_par_etat, col = "skyblue", main = "Sulfonic" )

moyennes_par_etat = tapply(data$Albumin.unadducted, data$case, mean)
barplot(height = moyennes_par_etat, col = "skyblue", main = "Non lié" )

#Histogramme 
hist(data$Albumin.adduct.of.Nacetylcysteine)
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

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.adduct.of.CysGly)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.adduct.of.CysGly %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_CysGly$albumine[index_outliers] = NaN
boxplot(albu_CysGly$albumine)


#============================ Nacetyl ===============================

# Filtrer les données
(albu_Nacetyl <- data3[data3$type_albumine == "Albumin.adduct.of.Nacetylcysteine",])

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.adduct.of.Nacetylcysteine)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.adduct.of.Nacetylcysteine %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_Nacetyl$albumine[index_outliers] = NaN
boxplot(albu_Nacetyl$albumine)


#============================ Acide sulfonique ===============================

# Filtrer les données
(albu_sulfo <- data3[data3$type_albumine == "Albumin.adduct.of.sulfonic.acid",])

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.adduct.of.sulfonic.acid)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.adduct.of.sulfonic.acid %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_sulfo$albumine[index_outliers] = NaN
boxplot(albu_sulfo$albumine)


#============================ Acide sulfonique ===============================

# Filtrer les données
(albu_non_undu <- data3[data3$type_albumine == "Albumin.unadducted",])

# Calculer les valeurs aberrantes
# Prend les valeurs supérieures ou inférieurs aux quartiles +-1.5 fois l'écart interquartile
(outliers <- boxplot.stats(data$Albumin.unadducted)$out)

# Afficher les indes des valeurs aberrantes
(index_outliers = which(data$Albumin.unadducted %in% c(outliers)))
# Suppression des valeurs aberrantes
albu_non_undu$albumine[index_outliers] = NaN
boxplot(albu_non_undu$albumine)

#============================ Nouveau dataset sans valeur aberrante ===============================

val_albu = c(albu_CysGly$albumine, albu_Nacetyl$albumine, albu_sulfo$albumine, albu_non_undu$albumine)
type_Albumine = c(albu_CysGly$type_albumine, albu_Nacetyl$type_albumine, albu_sulfo$type_albumine, albu_non_undu$type_albumine)
data_corr_graph = data.frame(val_albu, type_Albumine)
data_corr_graph



library(ggplot2)
# boxplot général pour voir que c'est mieux
ggplot(data_corr_graph, aes(x=type_Albumine,y=val_albu, fill=type_Albumine)) +
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
  ylim(c(0,1))



# =============================== Analyse nouveau dataset ===================================

# Implémantation dans le tableau à l'origine
data$CysGly_cor = albu_CysGly$albumine
data$Nacetyl_corr = albu_Nacetyl$albumine
data$sulfo_corr = albu_sulfo$albumine
data$unadducted_corr = albu_non_undu$albumine
pairs(data.frame(data$CysGly_cor, data$unadducted_corr, data$sulfo_corr, data$Nacetyl_corr))











































