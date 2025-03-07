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
#Exercice 3.