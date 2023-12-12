getwd()
setwd("C:/Users/33695/OneDrive/Documents/Cours Polytech/4eme annee/S1/R-studio/Projet de groupe/Projet-R-GB4")
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


data[8] == data$Albumin.adduct.of.Nacetylcysteine
data$Albumin.adduct.of.Nacetylcysteine
c(data[8])

for (i in seq(8,11)){
  print(i)
  print(sd(c(data[i])))
}

help(sd)

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


# Calcul des moyennes et écart-types par état
moyennes_par_etat <- tapply(df$valeur, df$etat, mean)


# Création du barplot
bp <- barplot(moyennes_par_etat, col = "skyblue", main = "Moyennes par État", xlab = "État", ylab = "Moyenne")

# Ajout des barres d'erreur (écart type)
arrows(bp, moyennes_par_etat, bp, moyennes_par_etat + ecart_types_par_etat, angle = 90, code = 3, length = 0.1, col = "red")


mymodel = glm(data$case ~data$Albumin.adduct.of.Nacetylcysteine)
summary(mymodel)

mymodel2 = glm(data$case ~ data$Albumin.adduct.of.CysGly)
summary(mymodel2)

mymodel3 = glm(data$case ~ data$Albumin.adduct.of.sulfonic.acid)
summary(mymodel3)

mymodel4 = glm(data$case ~ data$smoking_status)
summary(mymodel4)




library(ggplot2)

# Exemple de données
data <- data.frame(
  smoking_status = c("Non-smoker", "Smoker", "Non-smoker", "Smoker", "Non-smoker")
)

# Calcul des proportions avec prop.table
proportion <- prop.table(table(data$smoking_status))

# Création du graphique circulaire
ggplot(NULL, aes(x = "", y = proportion, fill = names(proportion))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title = "Diagramme circulaire représentant les proportions de la population\natteinte d'un cancer selon son état tabagique") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  theme(plot.margin = unit(c(1, 0.5, 1, 1), "cm")) +
  geom_text(aes(label = paste0(round(proportion * 100), "%")), position = position_stack(vjust = 0.5), size = 5)



























































