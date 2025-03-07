#TP2

#Exercice 1.

#Q1: représenter les nuages de points
vecteur_xi <- c(420, 380, 350, 400, 440, 380, 450, 420)
vecteur_yi <- c(5.5, 6, 6.5, 6, 5, 6.5, 4.5, 5)

plot(vecteur_xi, vecteur_yi)
# avec la représentation graphique, il semble que xi et yi soient 
#liés donc une regression linéaire peut etre une bonne technique.


#Q2: créer la matrice X associée
#matrice_X <- cbind(rep(1, length(vecteur_x)), vecteur_xi)
matrice_X <- matrix(c(rep(1, length(vecteur_xi)), vecteur_xi), nrow=length(vecteur_xi), ncol=2)
matrice_X
#rq: on peut aussi utiliser cbind()


#Q3: en posant y=t(y), calculer b = (tX^X)^(-1)tX^y
y <- matrix(t(vecteur_yi), byrow=TRUE)
y
#utiliser solve(A, B) permet de resoudre Ax=B ou x=A**(-1) B
#avec un seul argument on obtient alors : A=tXX et B=tXy
#pour vérifier que tous les éléments sont au bon format
t(matrice_X)
matrice_X
y

b<-solve((t(matrice_X) %*% matrice_X), (t(matrice_X)%*%y))
b
#donc beta1chapeau = 13.978125 et beta0chapeau-0.020625


#Q4: vérifier que l'on a b=t(b0, b1)

#Sx<- var(vecteur_xi)
#b1=1/(7*Sx) *sum((vecteur_xi-mean(vecteur_xi))*(vecteur_yi-mean(vecteur_yi)))
#b1
#b0<-mean(vecteur_yi)-b1*mean(vecteur_xi)
#b0

b1<- cov(vecteur_xi, vecteur_yi)/var(vecteur_xi)
b0<- mean(vecteur_yi)-b1*mean(vecteur_xi)
b1
b0
#on a bien b=t(b0, b1)
model1<-lm(vecteur_yi~vecteur_xi)
model1
coefficients(model1)

#Q5: tracer la droite de régression sur le nuage de points
plot(vecteur_xi, vecteur_yi)
abline(b0, b1)


#Q6
RSS<-sum((vecteur_yi-(b0+b1*vecteur_xi))^2)
TSS<-var(vecteur_yi)*(length(vecteur_yi)-1)
#TSS<-var(vecteur_yi)*(length(vecteur_yi)-1)
#ATTENTION variance empirique avec 1/(n-1) et non 1/n
R2<-1-(RSS/TSS)
RSS
TSS
R2

R2<-(cor(vecteur_xi, vecteur_yi)**2)
R2

R2<-(cov(vecteur_xi, vecteur_yi)/(sqrt(var(vecteur_xi)*var(vecteur_yi))))**2
R2
#R2=0.88

R2ajust<-(R2)-(2*(1-R2)/(length(vecteur_yi)-2-1))
R2ajust
#R2ajust=0.85

#coefficeint au dessus de 0.8, donc bon modèle qui les modélise plutôt précisement 


#Q7
summary(model1)
anova(model1)

#Exercice 2.

#Q0
Y<-c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
X1<-c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
X2<-c(4, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)


#Q1: régression de la résitance à la rupture Y en fonction de X1

model2<-lm(Y~X1)
summary(model2)
anova(model2)

#Q2

model3<-lm(Y~X2)
summary(model3)
anova(model3)

#Q3

model4<-lm(Y~X1+X2)
summary(model4)
anova(model4)



#Q4

980.63/(980.63+440.03)
643.57/(643.57+777.10)
1204.85/(1204.85+215.81)

#Q5

model2<-lm(Y~X1)
summary(model2)
anova(model2)

model3<-lm(Y~X2)
summary(model3)
anova(model3)

model4<-lm(Y~X1+X2)
summary(model4)
anova(model4)

#Q6

1204.85/2
602.425+23.98

#Q7

#La p_vlaue de la statistique de Fischer est inférieure à 0.05 (p-value: 0.0002075)
#Donc nous sommes dans la zone de rejet de l'hypothès nulle H0.
#Par conséquent nous accetons l'hypothèse alternative H1.

#Q8

#cf slide 9 cours 2
#IC95%=^B1+-1.96*SE(B1)

6.036+1.96*1.279
6.036-1.96*1.279

#t=^B1/SE(B1) = 6.036/1.279 = 4.719
#La probabilité associée à la statistique de Student est inférieure à 0.05.
#On rejette l'hypothèse H0 (B1=0) et on accepte l'hypothèse alternative H1 (B1!=0).

#Q9

#L'apport marginal de la varibale explicative X2 lorsqu'elle est introduite à la suite de X1 est égal à
1204.858-980.635 #=224.223

#Q10

#predict(<DF>, <mod>)

predict(model4, data.frame(X1=4, X2=3.8))
predict(model4, data.frame(X1=3, X2=3.4))
predict(model4, data.frame(X1=4, X2=2.9))

#rq1: peut aussi s'écrire en utilisant des vecteurs dans le data.frame
predict(model4, data.frame(X1=c(4,3,4), X2=c(2.9, 3.4, 2.9)))
#rq2: le modèle entrainé sur des données connues peut maintenant prédire des valeurs 
#pour des données inconnues et générer des prédictions