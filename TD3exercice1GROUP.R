# Importation des données
install.packages("data.table")
library("data.table")
don1 <- read.csv("C:/Users/User/Downloads/projet/sportcourse.csv", encoding="UTF-8", sep=";")
View(don1)

don <- read.table("C:/Users/User/Downloads/projet/sportcourse.csv", encoding="UTF-8", sep=";")
View(don)

# Nonm des colonnes
colnames(don)
colnames(don1)

#3) Renommer les colonnes avec les X
colnames(don1)[2:8] <- paste('X',colnames(don1)[2:8], sep ='')
Ren <- colnames(don1)[2:8]

#4) Tracé de la droite de regression X400m et X100m
X11()
plot(don1$X400m, don1$X100m, main = "Resultats 100m vs 400m", pch = 5,
     xlab = "X400m", ylab = "X100m")
text(don1$X400m, don1$X100m,don1$Country, pos = 1, cex = 0.5, col = "blue")

#5)
Reg1 <- lm(don1$X100m~don1$X400m)
Reg1
#6)
abline(Reg1, col = "green", lwd = 2, lty = 1)


# 7) afficher l'estimateur
summary(lm(X100m ~ X400m, data=don1))  #l'estimateur de la constante du mod?le"


#8) moyenne du 100m et 400m
summary(don1)
moy <- sapply(subset(don1, select = c(X100m, X400m)), MARGIN = 1, FUN = mean)
moy
# On remarque que la plupart des points sont concentrés autour de la moyenne sauf USA et Cook Islands
# si un pays est bon au 400m alors il est aussi bon au 100m 

#9) 

# Tracé des moyennes
X11()
plot(don1$X400m, don1$X100m, main = "Resultats 100m vs 400m", pch = 5,
     xlab = "X400m", ylab = "X100m")
text(don1$X400m, don1$X100m,don1$Country, pos = 1, cex = 0.5, col = "blue")
abline(Reg1, col = "green", lwd = 2, lty = 1) # droite de regressionn
##
abline(v= moy[2],col = "purple", lwd = 1, lty = 1 )  #moy v pour tracer en verticale
abline(h= moy[1],col = "red", lwd = 1, lty = 1 )  #moy h pour tracer en  horizontale

# 10)
X11()
plot(don1$X400m, don1$X100m, main = "Resultats 100m vs 400m", pch = 5,
     xlab = "X400m", ylab = "X100m")
text(don1$X400m, don1$X100m,don1$Country, pos = 1, cex = 0.5, col = "blue")
abline(Reg1, col = "green", lwd = 2, lty = 1) # droite de regressionn
abline(v= moy[2],col = "purple", lwd = 1, lty = 1 )  #moy v pour tracer en verticale
abline(h= moy[1],col = "red", lwd = 1, lty = 1 )  #moy h pour tracer en  horizontale
##
Reg11 <- lm(don1$X100m~ -1 + don1$X400m)
abline(Reg11, col = "pink", lwd = 2, lty = 1)
legend('bottomright',
       legend=c('régression linéaire','régression linéaire à l origine','Moyenne du 100m','Moyenne du 400m'),
       col=c('green','pink','red','purple'),
       lwd=c(1,1,1,1))  

summary(Reg11)
summary(Reg1) # droite ne passant ? l'origine
# on remarque que toutes les droites se croisent en un même point
# Grâce à la fonction summary, on remarque que l'ecart-type de la droite de régression
#passant par l'origine est plus petit que celui dont la droite n'est pas passée par l'origine
# Le modèle avec l'ordonnée à l'origine est donc meilleur au sens où on retrouve plus de pays plus proche de la droite  

#11) variable 400m centrée 

don1 <- transform(don1, CX400m = (X400m - moy[2])) 

X11()
plot(don1$X400m, don1$X100m, main = "Resultats 100m vs 400m", pch = 5,
     xlab = "X400m", ylab = "X100m")
text(don1$X400m, don1$X100m,don1$Country, pos = 1, cex = 0.5, col = "blue")
abline(Reg1, col = "green", lwd = 2, lty = 1) # droite de regressionn
abline(v= moy[2],col = "purple", lwd = 1, lty = 1 )  #moy v pour tracer en verticale
abline(h= moy[1],col = "red", lwd = 1, lty = 1 )  #moy h pour tracer en  horizontale
Reg11 <- lm(don1$X100m~ -1 + don1$X400m)
abline(Reg11, col = "pink", lwd = 2, lty = 1)
legend('bottomright',
       legend=c('régression linéaire','régression linéaire à l origine','Moyenne du 100m','Moyenne du 400m'),
       col=c('green','pink','red','purple'),
       lwd=c(1,1,1,1)) 
##
plot(don1$CX400m, don1$X100m, main = "Resultats 100m vs 400m centré ", pch = 5,
     xlab = "X400m centré", ylab = "X100m")
text(don1$CX400m, don1$X100m,don1$Country, pos = 1, cex = 0.5, col = "blue")
Reg1C <- lm(don1$X100m~don1$CX400m)
abline(Reg1C, col = "yellow", lwd = 2, lty = 1)
summary(lm(X100m ~ CX400m, data=don1))
moyC <- sapply(subset(don1, select = c(X100m, CX400m)), MARGIN = 1, FUN = mean)
abline(v= moyC[2],col = "purple", lwd = 1, lty = 1 )  #moy v pour tracer en verticale
abline(h= moyC[1],col = "red", lwd = 1, lty = 1 )  #moy h pour tracer en  horizontale
Reg11C <- lm(don1$X100m~ -1 + don1$CX400m)
abline(Reg11C, col = "pink", lwd = 2, lty = 1)
legend('bottomright',
       legend=c('régression linéaire','régression linéaire à l origine','Moyenne du 100m','Moyenne du 400m'),
       col=c('green','pink','red','purple'),
       lwd=c(1,1,1,1))  

#On remarque qu'on ne peut pas obtenir une droite qui résume le nuage de points en partant de l'origine


# 12) idem avec 100m et 5kilomètres


X11()
plot(don1$X5K, don1$X100m, main = "Resultats 100m vs 5 kilomètres", pch = 5,
     xlab = "5 kilomètres ", ylab = "100 mètres")
text(don1$X5K, don1$X100m,don1$Country, pos = 1, cex = 0.5, col = "blue")
Reg2 <- lm(don1$X100m~don1$X5K)
abline(Reg2, col = "green", lwd = 2, lty = 1)
summary(lm(X100m ~ X5K, data=don1))
moy2 <- sapply(subset(don1, select = c(X100m, X5K)), MARGIN = 1, FUN = mean)
abline(v= moy2[2],col = "purple", lwd = 1, lty = 1 )  #moy v pour tracer en verticale
abline(h= moy2[1],col = "red", lwd = 1, lty = 1 )  #moy h pour tracer en  horizontale

Reg22 <- lm(don1$X100m~ -1 + don1$X5K)
abline(Reg22, col = "pink", lwd = 2, lty = 1)
legend('topleft',
       legend=c('régression linéaire','régression linéaire à l origine','Moyenne du 100m','Moyenne du 400m'),
       col=c('green','pink','red','purple'),
       lwd=c(1,1,1,1))  


# 5 kilomètres centré

don1 <- transform(don1, CX5K = (X5K - moy2[2])) 

X11()
plot(don1$CX5K, don1$X100m, main = "Resultats 100 mètres vs 5 kilomètres centré ", pch = 5,
     xlab = "5 kilomètres centré", ylab = " 100 mètres")
text(don1$CX5K, don1$X100m,don1$Country, pos = 1, cex = 0.5, col = "blue")
Reg2C <- lm(don1$X100m~don1$CX5K)
abline(Reg2C, col = "green", lwd = 2, lty = 1)
summary(lm(X100m ~ CX5K, data=don1))
moy2C <- sapply(subset(don1, select = c(X100m, CX5K)), MARGIN = 1, FUN = mean)
abline(v= moy2C[2],col = "purple", lwd = 1, lty = 1 )  #moy v pour tracer en verticale
abline(h= moy2C[1],col = "red", lwd = 1, lty = 1 )  #moy h pour tracer en  horizontale
Reg22C <- lm(don1$X100m~ -1 + don1$CX5K)
abline(Reg22C, col = "pink", lwd = 2, lty = 1)
legend('topleft',
       legend=c('régression linéaire','régression linéaire à l origine','Moyenne du 100m','Moyenne du 400m'),
       col=c('green','pink','red','purple'),
       lwd=c(1,1,1,1))  

# on remarque également qu'il n'y pas la droite de régression de l'ordonnée à l'origine


#14)

Reg3 = lm(X100m ~ X400m + X5K, data=don1)
summary(Reg3)
# grace à la fonction summary on remarque qu'un coureur qui est rapide au 400m le seraaussi au 100m
# mais un coureur qui court vite au 5 kilomètres ne sera pas forcément rapide au 100m
# ( on regarde la significativité des codes et on voit que l'erreur est plus importante pour le 5kilomètre que pour le 400m)


#15)

Reg4 = lm(X400m + X5K ~ X100m, data=don1)
summary(Reg4)
# le 100 mètre explique bien le 5 kilomètre et le 400m
# donc les meilleurs au 100 mètre sont aussi bon au 5 kilomètre et au 400 mètre Or, on a vu tout à l'heure que l'inverse n'est pas forcément vrai 

#16)
#transformer en seconde
don1 <- transform(don1, X5KS = (X5K * 60)) 

Reg5 = lm(X400m + X5KS ~ X100m, data=don1)
summary(Reg5)
# avec la conversion, on constate que l'erreur pour le 100mètre est plus importante : en moyenne ceux qui courent vite au 100m courent aussi vite au 400m et au 5k
# Mais, l'erreur est plus importante donc les pays ne suivent pas tous cette tendance. 


#17)
# Construction de la variable aleatoire
don1 <- transform(don1, alea = runif(55,0,2))


#18)
# Le graphique de 5K et alea
X11()
plot(don1$alea, don1$X5KS, main = "Resultats 5K vs alea", pch = 5,
     xlab = "aleatoire", ylab = "5 kilomètres")
text(don1$alea, don1$X5KS,don1$Country, pos = 1, cex = 0.5, col = "blue")
Reg5 <- lm(don1$X5KS~don1$alea)
abline(Reg5, col = "red", lwd = 2, lty = 1)
summary(Reg5)

# on remarque que alea n'explique pas bien le modele

#19)
#Covariance entre 400m et 5K en secondes
cov(don1$X100m, don1$X5KS)

summary(Reg1)
summary(Reg11)
summary(Reg2)
summary(Reg22)
summary(Reg3)
summary(Reg4)
summary(Reg5)
