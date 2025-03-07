#TP1

#2. Utiliser R comme calculatrice

2+4
#6
36/5
#7.2
sqrt(4)
#2
var <- 3 * sqrt(4) + pi
print(var)
mystere <- var / 0

#3. Quelques commandes utiles

#q() quitter R
#help(topic) #afficher l’aide relative à topic ou simplement utiliser le menu Help de Rstudio
#source("fichier.r") #charger un programme stocké dans un fichier nommé ici fichier.r
#getwd() #et #setwd("chemin") #afficher et modifier le répertoire de travai

#4. LES OPERATEURS

tmp <- 7 / 2
resultat <- floor(tmp)
reste <- 7 - 2*resultat
#reste = 1
#resultat = 3

resultat <- 7 %/% 2
reste <- 7 %% 2
#reste = 1
#resultat = 3

abs(-3)
#3
sqrt(49)
#7
floor(-3.7)
#-4

sqrt(3*floor(pi))
#3

x <- 10
y <- 20
x > y
#FALSE

x <- 3.14
y <- pi
logiq <- x == y
#logiq = FALSE

!logiq == TRUE
#TRUE

x <- T
y <- F
v1 <- x && y
v2 <- x || y
v3 <- v2 && !v1
#v3 = TRUE

x <- T
y <- F
z <- F
v1 <- y && z || x
v2 <- y && (z || x)
#non v1 = TRUE et v2 = FALSE

#5. Généralités sur les variables
#5.1 Types de variables

txt <- "un texte"
class(txt)
#"character"

is.character(txt)
#TRUE
is.numeric(txt)
#FALSE

txt <- '42'
nbr <- as.integer(txt)
is.numeric(nbr)
#TRUE

#5.2 Manipulation de variables
#5.2.1 Concaténation de texte

mot <- "petite"
text1 <- paste("une", mot, "phrase")
#text1 = "une petite phrase"
text2 <- paste(text1, "compte", nchar(text1), "lettres")
#text2 = "une petite phrase compte 17 lettres"

#5.2.2 Quelques particularités

tmp <- 3 / 0
nsp <- NA
resultat <- paste(tmp, tmp+1, tmp+nsp)
#resultat = "Inf Inf NA"

tmp <- NA
tmp == NA
#NA
is.na(tmp)
#TRUE

#6. Les vecteurs de variables
#6.1 Création d’un vecteur

vecteur1 <- c(1, 3, 5, 7, 9)
vecteur2 <- seq(from=0, to=10, by=2)
vecteur3 <- 0:10
vecteur4 <- rep(1:2, 5)
vecteur1
vecteur2
vecteur3
vecteur4

#6.2 Manipulation d’un vecteur
vecteur1
# 1 3 5 7 9
vecteur1[1]
# 1
vecteur1[0]
# numeric(0)
length(vecteur1)
# 5

vecteur <- rnorm(10)
for (i in 1:length(vecteur))
print(vecteur[i])

#6.3 Pour aller plus loin

v1 <- runif(10)
v2 <- rpois(10, 1)
v3 <- rnorm(10)
matrice <- rbind(v1, v2, v3)

matricet <- t(matrice)
dim(matricet)
matrice[3, 5]

#7. Les data.frames

converti <- as.data.frame(matricet)
is.data.frame(converti)

v1 <- c(175, 182, 165, 187, 158)
v2 <- c(19, 18, 21, 22, 20)
tableau <- data.frame(taille=v1,age=v2)
names(tableau)
print(tableau$taille)
summary(tableau)
write.table(tableau, "sortie.csv", sep=";")

#8. Les bibliothèques de fonctions

install.packages("maps")
library(maps)
map("france")

datas <- rnorm(20)
barplot(datas)
hist(datas, nclass=4)

plot(seq(0,2*pi,by=0.01), sin(seq(0,2*pi,by=0.01)))

#10. Exercices
#Exercice 1.

#Q0) Charger le fichier iris avec la commande data(iris).

data(iris)

#Q1) Quelle est la nature de iris ? (matrix, vector, data.frame....)

class(iris)
# data.frame

#Q2) Les données. Combien d’individus ? de variables ? Pour chacune des variables donner leur nature (quantitative, qualitative, ...)

dim(iris)
is.numeric(iris$Sepal.Length) #TRUE (quantitative)
is.numeric(iris$Sepal.Width) #TRUE (quantitative)
is.numeric(iris$Petal.Length) #TRUE (quantitative)
is.numeric(iris$Petal.Width) #TRUE (quantitative)
is.numeric(iris$Species) #FALSE
is.data.frame(iris$Species) #FALSE
is.character(iris$Species) #FALSE
is.factor(iris$Species) #TRUE (quantitative)

#Q3) Calculer pour chaque variable lorsque cela a un sens, la moyenne et la variance empirique. Même question en divisant la population selon le niveau de la variable Species.

#Moyenne et variance empirique
mean(iris$Sepal.Length)
var(iris$Sepal.Length)
mean(iris$Sepal.Width)
var(iris$Sepal.Width)
mean(iris$Petal.Length)
var(iris$Petal.Length)
mean(iris$Petal.Width)
var(iris$Petal.Width)

mean(iris$Sepal.Length[iris$Species == "setosa"])
mean(iris$Sepal.Length[iris$Species == "versicolor"])
mean(iris$Sepal.Length[iris$Species == "virginica"])
var(iris$Sepal.Length[iris$Species == "setosa"])
var(iris$Sepal.Length[iris$Species == "versicolor"])
var(iris$Sepal.Length[iris$Species == "virginica"])

#Q4) On s’intéresse au 4 premières variables. Standardiser les données (retirer la moyenne et diviser par l’écart-type)

iris$Sepal.Length <- (iris$Sepal.Length - mean(iris$Sepal.Length)) / sd(iris$Sepal.Length)
iris$Sepal.Width <- (iris$Sepal.Width - mean(iris$Sepal.Width)) / sd(iris$Sepal.Width)
iris$Petal.Length <- (iris$Petal.Length - mean(iris$Petal.Length)) / sd(iris$Petal.Length)
iris$Petal.Width <- (iris$Petal.Width - mean(iris$Petal.Width)) / sd(iris$Petal.Width)

#Q5) Tracer le graphique variable Petal.Length versus Petal.Width en utilisant des couleurs différentes selon le niveau de la variable Species. Essayer tous les “croisements” possibles et commentez ces graphiques.

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)
plot(iris$Petal.Length, iris$Sepal.Width, col=iris$Species)
plot(iris$Sepal.Length, iris$Petal.Width, col=iris$Species)

#Exercice 2.

ms=numeric(10000);
p=0.75; x=seq(-4,4,0.025);
for (j in(1:50)){
  k=j*j; for (i in (1:10000)){
    sig=sqrt(p*(1-p)/k); mu=p; ms[i]=(mean(rbinom(k,1,p))-mu)/sig } #loi binomiale
  hist(ms, breaks=41, xlab="x-variable", xlim=c(-4,4), prob=TRUE, main=sprintf("normal curve over histogram, n = %d",k))
  curve(dnorm(x), col="darkblue", lwd=2, add=TRUE, yaxt="n")} # loi normale

ms=numeric(10000);
p=0.75; x=seq(-4,4,0.025);
for (j in(1:50)){
  k=j*j; for (i in (1:10000)){
    sig=1/lambda; mu=1/lambda; ms[i]=(mean(rexp(k,lambda))-mu)*sqrt(k)/sig } #loi binomiale
  hist(ms, breaks=41, xlab="x-variable", xlim=c(-4,4), prob=TRUE, main=sprintf("normal curve over histogram, n = %d",k))
  curve(dnorm(x), col="darkblue", lwd=2, add=TRUE, yaxt="n")} # loi normale