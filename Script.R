getwd()
setwd("C:/Users/Edmond/OneDrive - Université Côte d'Azur/Documents/Cours/Cours Polytech/GB4/Stat/Projet-R-GB4")
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
# 101 Sains et 96 cancéreux

table(data$case, data$gender)
table(data$case, data$smoking_status)
chisq.test(table(data$case, data$smoking_status))

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
dunn.test(data$Albumin.adduct.of.Nacetylcysteine, data$smoking_status)

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





















































































