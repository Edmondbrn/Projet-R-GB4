getwd()
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
# 101 Sain et 96 cancéreux

table(data$case, data$gender)
table(data$case, data$smoking_status)
chisq.test(table(data$case, data$smoking_status))

hist(data$age.recr)
qqnorm(data$age.recr)
shapiro.test(data$age.recr)
# répartition normale pas très claire

hist(data$Albumin.adduct.of.Nacetylcysteine)
hist(data$Albumin.adduct.of.CysGly)


































































































