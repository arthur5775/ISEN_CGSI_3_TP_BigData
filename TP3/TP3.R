#TP3

#Exercice 1.

#Trente patients ont reçu un certain niveau de dosage d’agent anesthésique pendant 15
#minutes. Puis une incision leur est faite. Il est ensuite noté si le patient a bougé ou pas lors
#de l’incision. Ainsi, pour chaque patient, on dispose :
#- du dosage de l’agent anesthésique pendant 15 minutes (variable X1),
#- du fait qu’il ait bougé ou pas (variable Y , avec Y = 1 pour bougé).
#On souhaite expliquer Y en fonction de X1.
#Les données sont dans le fichier anesthesie.txt que vous trouverez sur Moodle dans le cours Big Data.

#Y X1
#0 1.0
#1 1.2
#0 1.4
#1 1.4
#1 1.2
#0 2.5
#0 1.6
#1 0.8
#0 1.6
#1 1.4
#1 0.8
#0 1.6
#0 2.5
#0 1.4
#0 1.6
#0 1.4
#0 1.4
#1 0.8
#0 0.8
#0 1.2
#1 0.8
#1 0.8
#1 1.0
#1 0.8
#1 1.0
#0 1.2
#1 1.0
#0 1.2
#1 1.0
#0 1.2
  
#Q1: Modéliser le problème à l’aide d’une régression logistique.
#rq1: on peut utilsier setwd() pour changer le répertoire de travail
#rq2: on peut utiliser read.table() pour lire le fichier anesthesie.txt
#rq3: on peut utiliser attach(w) pour attacher le fichier anesthesie.txt

X1<- c(1.0, 1.2, 1.4, 1.4, 1.2, 2.5, 1.6, 0.8, 1.6, 1.4, 0.8, 1.6, 2.5, 1.4, 1.6, 1.4, 1.4, 0.8, 0.8, 1.2, 0.8, 0.8, 1.0, 0.8, 1.0, 1.2, 1.0, 1.2, 1.0, 1.2)
Y<- c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0)
logit<- glm(Y~X1, family=binomial)
logit

#Q2: Est-ce qu’un patient ayant eu pour dosage X1 = 1.25 a plus de chance de bouger que de ne pas bouger ?

#sachant que X1=1.25, Y est il plus probable d'être égal à 1 ou à 0?
predict(logit, data.frame(X1=1.25), type="response")
#renvoie 0.379946 qui est inférieur à 0.5, donc il est plus probable que Y=0 que Y=1.

#rq: peut aussi s'écrire:
predict.glm(logit, data.frame(X1=1.25), type="response")

#rq:type="response": probabilité sur échelle standard et non logarithmique.

#Q3: Faire le graphique des données et de la courbe de régression logistique.

plot(X1, Y)
curve(predict(logit, data.frame(X1=x), type="response"), add=TRUE)

#Q4: Calculer un taux d’erreur par validation croisée. Les résultats sont-ils bons ?

#le taux d'erreur est le nombre de fois où la prédiction est différente de la réalité.
#Quand le modèle prédit 0 et que la réalité est 1, c'est une erreur et vice versa.
#te=nombre d'erreurs/nombre d'observations=(C01+C10)/(C00+C01+C10+C11)

pred.logit = predict.glm(logit, data.frame(X1=X1), type="response")
pred.logit
pred.mod = factor(ifelse(pred.logit>0.5, "1", "0"))
pred.mod
Y
#matrice de confusion
mc = table(Y, pred.mod)
mc
#taux d'erreur par validation croisée
t = (mc[2,1]+mc[1,2])/sum(mc)
t

#Exercice 2.
#Dans cet exercice, nous considérons les statistiques de crimes commis aux États-Unis en 1977. 
#Ce jeu de données contient les nombres de crimes pour 100 000 habitants pour p = 7 types de 
#crimes dans les n = 50 états. Pour récupérer ces données, il faut charger dans R le fichier 
#crimes.txt. Explorer rapidement ces données et calculer l’ACP.
#q1: A l’aide de l’éboulis des valeurs propres, quel nombre d de variables principales utiliseriez-vous ?
#q2: A quelle part d’inertie expliquée cela correspond-il ?
#q3: Calculer la contribution des variables initiales à l’inertie des variables principales ?
#q4: Discuter des résultats obtenus par rapport à votre choix de d
#q5: Représenter les variables sur le cercle des corrélations
#q6: A l’aide de ce graphique, comment allez-vous interpréter les axes dans le plan principal ?
#q7: Représenter les individus dans le plan principal avec la fonction plot.
#Cette représentation est-elle suffisante pour résumer correctement l’information des données ?
#q8: Pour d > 2, représenter les données dans les plans engendrés par toutes les paires d’axes principaux parmi les d premiers.
#q9: Calculer la contribution de chaque état à l’inertie des axes.
#q10: Repérer quelques états dont certaines contributions sont importantes.
#q11: Pouvez-vous donner une interprétation géographique aux axes de l’ACP?

#Exercice 3.

#q1: Générer 50 données issues de 3 groupes de centres (1 ;2), (6 ;6) et (6 ;-2) avec 
# probabilité 0.2, 0.3 et 0.5 issues d’une loi gaussienne. (voir exemple ci-dessus)

library(MASS)  # Charger le package contenant mvrnorm()
set.seed(1)  # Pour la reproductibilité
# Définition des centres et proportions
centers <- matrix(c(1, 2, 6, 6, 6, -2), ncol=2, byrow=TRUE)
probabilities <- c(0.2, 0.3, 0.5)
n <- 50  # Nombre total de points
# Attribution des classes selon les probabilités
groupes <- sample(1:3, size=n, replace=TRUE, prob=probabilities)
# Génération des données avec bruit gaussien
data <- matrix(NA, nrow=n, ncol=2)
for (i in 1:3) {
  indices <- which(groupes == i)
  data[indices, ] <- mvrnorm(n=length(indices), mu=centers[i, ], Sigma=diag(2))}
# Attribution des couleurs et formes pour l'affichage
colors <- c("red", "blue", "green")
pch_symbols <- c(4, 5, 6)  # Différents symboles pour chaque groupe
# Tracé des points
plot(data, col=colors[groupes], pch=pch_symbols[groupes], xlim=c(-1, 10), ylim=c(-4, 8))

#q2: Faire une analyse discriminante sur ces données en séparant le plan en trois régions (comme sur le graphique)

set.seed(1)
# Définition des centres et proportions
centers <- matrix(c(1, 2, 6, 6, 6, -2), ncol=2, byrow=TRUE)
probabilities <- c(0.2, 0.3, 0.5)
n <- 50  # Nombre total de points
# Attribution des classes selon les probabilités
groupes <- sample(1:3, size=n, replace=TRUE, prob=probabilities)
# Génération des données avec bruit gaussien
data <- matrix(NA, nrow=n, ncol=2)
for (i in 1:3) {
  indices <- which(groupes == i)
  if (length(indices) > 0) {
    data[indices, ] <- mvrnorm(n=length(indices), mu=centers[i, ], Sigma=diag(2))
  }
}
# Conversion en data frame
df <- data.frame(x = data[,1], y = data[,2], classe = as.factor(groupes))
# Analyse discriminante linéaire (LDA)
lda_model <- lda(classe ~ x + y, data=df)
# Création d'une grille de points pour la visualisation
x_min <- min(df$x) - 1
x_max <- max(df$x) + 1
y_min <- min(df$y) - 1
y_max <- max(df$y) + 1
grid_x <- seq(x_min, x_max, length.out = 100)
grid_y <- seq(y_min, y_max, length.out = 100)
grid <- expand.grid(x = grid_x, y = grid_y)
# Prédiction des classes pour chaque point de la grille
grid$classe <- predict(lda_model, newdata = grid)$class
# Affichage du résultat
plot(df$x, df$y, col=c("red", "blue", "green")[df$classe], pch=c(4, 5, 6)[df$classe],
     xlim=c(x_min, x_max), ylim=c(y_min, y_max), xlab="X", ylab="Y")
contour(grid_x, grid_y, matrix(as.numeric(grid$classe), length(grid_x), length(grid_y)), 
        add=TRUE, drawlabels=FALSE, col="black", lwd=1)

#q3: Quel est le taux d’erreur de classification avec cette méthode.

# Prédiction des classes sur les données d'origine
predictions <- predict(lda_model, newdata = df)$class
# Calcul du taux d'erreur
taux_erreur <- mean(predictions != df$classe)
print(paste("Taux d'erreur de classification :", round(taux_erreur * 100, 2), "%"))

#q4: Proposer une autre méthode et donner son taux d’erreur.

#Une alternative à l'analyse discriminante linéaire (LDA) est l'analyse discriminante 
#quadratique (QDA), qui permet de mieux gérer des distributions non linéaires des classes. 
#Contrairement à LDA, QDA n'impose pas d'hypothèse de variance-covariance commune entre les 
#classes, ce qui le rend plus flexible pour des données dont les formes ne sont pas séparables linéairement.

# Appliquer l'Analyse discriminante quadratique (QDA)
qda_model <- qda(classe ~ x + y, data=df)
# Prédiction des classes sur les données d'origine
qda_predictions <- predict(qda_model, newdata=df)$class
# Calcul du taux d'erreur
qda_taux_erreur <- mean(qda_predictions != df$classe)
print(paste("Taux d'erreur de classification (QDA) :", round(qda_taux_erreur * 100, 2), "%"))
