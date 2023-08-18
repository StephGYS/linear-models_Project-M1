################# Activité exploratoire n°1 #############

## Importation des fichiers 
#don1<-read_csv("conso_vehicules.csv")
library(readxl)
data <- read_excel("C:/Users/gnago/Downloads/data.xls", 
                   sheet = "DEP")
View(data)

names(data)
class(Departement)
class(Nbre_demplois_salaries)
edit(data)

                       ########################

   #agriculture VS industrie
reg=lm(industrie~agriculture,data=data)
X11()
plot(data$agriculture, data$industrie, main = "Resultats agriculture vs industrie", pch = 5,
     xlab = "agriculture", ylab = "industrie")
abline(reg=coef(reg), col="red")
text(data$agriculture, data$industrie,data$Departement, pos = 1, cex = 0.5, col = "blue")
summary(reg) # pas significative #cor(data$agriculture,data$industrie)^2 
  #industrie vs construction
reg=lm(construction~industrie, data = data)
X11()
plot(data$industrie, data$construction, main = "Resultats industrie vs construction", pch = 5,
     xlab = "industrie", ylab = "construction")
abline(reg=coef(reg), col="red")
text(data$industrie,data$construction,data$Departement, pos = 1, cex = 0.5, col = "blue")
summary(reg) # pas significative

  #industrie et tertiaire_marchand
reg=lm(tertiaire_marchand~industrie, data = data)
X11()
plot(data$industrie, data$tertiaire_marchand, main = "Resultats industrie vs tertiaire_marchand", pch = 5,
     xlab = "industrie", ylab = "tertiaire_marchand")
abline(reg=coef(reg), col="red")
text(data$industrie,data$tertiaire_marchand,data$Departement, pos = 1, cex = 0.5, col = "blue")
summary(reg)

  #l'agriculture et tertiaire_marchand
reg=lm(tertiaire_marchand~agriculture, data = data)
X11()
plot(data$agriculture, data$tertiaire_marchand, main = "Resultats agriculture vs tertiaire_marchand", pch = 5,
     xlab = "agriculture", ylab = "tertiaire_marchand")
abline(reg=coef(reg), col="red")
text(data$agriculture,data$tertiaire_non_marchand,data$Departement, pos = 1, cex = 0.5, col = "blue")
summary(reg) #significative
coef(reg) # affiche les coefficients de la droite
residuals(reg) # donne les résidus
x11()
library(lmtest)
library(car)
plot(reg, which = 1) #pas de relation de linéarité
durbinWatsonTest(reg) #p-valeur=0.988 donc indépendance entre les varaiables

 #l'evol trimestriel vs agriculture
reg=lm(Evol_trimestriel~agriculture, data = data)
X11()
plot(data$agriculture, data$Evol_trimestriel, main = "Resultats agriculture vs Evol_trimestriel", pch = 5,
     xlab = "agriculture", ylab = "Evol_trimestriel")
abline(reg=coef(reg), col="red")
text(data$agriculture,data$Evol_trimestriel,data$Departement, pos = 1, cex = 0.5, col = "blue")
summary(reg) #significatice
x11()
plot(reg, which = 1) #pas de relation de linéarité
raintest(reg) #p-value = 0.03678 donc pas de linearité
durbinWatsonTest(reg) #p-valeur=0.756 donc indépendance entre les varaiables

  #l'evol trimestriel et tertiaire_marchand
reg=lm(Evol_trimestriel~tertiaire_marchand, data = data)
X11()
plot(data$tertiaire_marchand, data$Evol_trimestriel, main = "Resultats tertiaire_marchand vs Evol_trimestriel", pch = 5,
     xlab = "tertiaire_marchand", ylab = "Evol_trimestriel")
abline(reg=coef(reg), col="red")
text(data$tertiaire_marchand,data$Evol_trimestriel,data$Departement, pos = 1, cex = 0.5, col = "blue")
summary(reg) #significative
x11()
plot(reg, which = 1) # relation de linéarité
raintest(reg) #p-value = 0.05846 donc il y'a une relation de lineairité
durbinWatsonTest(reg) #p-valeur=0.182 donc indépendance des residus entre les varaiables
x11()
plot(reg, which = 2)   #ok pour la normalité de la distribution des résidus
shapiro.test(residuals(reg))    #ok pour la normalité
x11()
plot(reg, which = 3)  #ok pour homogeneité
bptest(reg)
x11()
plot(reg, which = 5) # presence de points influents
summary(reg)

                           ##############
Reg1<-lm(Evol_trimestriel~agriculture, data=data)
Reg1 # Evol_trimestriel=0.30821   +      0.05012 *agriculture
Reg2<-lm(Evol_trimestriel~industrie, data=data)
Reg2 # Evol_trimestriel=0.2445   +      0.1327 *industrie
Reg3<-lm(Evol_trimestriel~construction, data=data)
Reg3 # Evol_trimestriel=0.27232   +      0.09213 *construction
Reg4<-lm(Evol_trimestriel~tertiaire_marchand, data=data)
Reg4 # Evol_trimestriel=-0.00802   +      0.50947 *tertiaire_marchand
Reg5<-lm(Evol_trimestriel~tertiaire_non_marchand, data=data)
Reg5 # Evol_trimestriel=0.2206   +      0.3980 *tertiaire_non_marchand
                        #####interpretation###

                      ##############
Reg<-lm(Evol_trimestriel~agriculture+tertiaire_marchand, data=data)
Reg # Evol_trimestriel= 0.04434+ 0.02287*agriculture  + 0.45047 *tertiaire_marchand  
summary(Reg)
# Colinearité 
cor(data$agriculture,data$tertiaire_marchand) #pas de colinéarité
library(car)
vif(Reg) # pas de multicolinéairité

#effet de levier
hatvalues(Reg) #hii proche de 0 alors Yi joue un role mineur dans le calcul de Yichapeau
#distance de cooks
cooks.distance(Reg)
cooks.distance(reg) > 1 # Donc aucune variable joue un role important dans la prediction

#Evaluation des hypothèses de normalité et d’homoscédasticité des résidus
library(performance)
check_model(Reg)# la linéarité n'est pas parfaite. 
#La normalité et l’homscédasticité des résidus ne sont pas parfaites, mais me semblent également raisonnable.
#test de normalité
check_normality(Reg)  #pas de normalité
shapiro.test(residuals(Reg)) # la valeur n'est pas normalement distribuée
# Orthogonalite visualisation
x11()
plot(Reg,1)
#Homoscedasticite
x11()
plot(Reg,which=3) # homoscedasticité
library(olsrr)
ols_test_score(Reg)
check_heteroscedasticity(Reg) #ok L’homoscédasticité n’est pas rejetée
#Recherche des outliers
check_outliers(Reg) #pas outliers
#Interprétation du modèle 
Reg
summary(Reg) #2 variables significatives l'agriculture et le tertiaire


                      #############
Reg2<-lm(Evol_trimestriel~agriculture+industrie+construction+tertiaire_marchand+tertiaire_non_marchand, data=data)
Reg2 # Evol_trimestriel=-0.01135+ 0.01820*agriculture  + 0.13271*industrie  + 0.03145*construction  + 0.45408*tertiaire_marchand  + 0.34111*tertiaire_non_marchand
summary(Reg2)
# Colinearité 
x<-as.matrix(data[, -1:-3])
x
Y<-data[,1]
Reg2<-lm(Evol_trimestriel~agriculture+industrie+construction+tertiaire_marchand+tertiaire_non_marchand, data=data)
Reg2 # Evol_trimestriel=-0.01135+ 0.01820*agriculture  + 0.13271*industrie  + 0.03145*construction  + 0.45408*tertiaire_marchand  + 0.34111*tertiaire_non_marchand
#linéarité
raintest(Reg2) #Ok linéarité
#analyse de la correlation
cor(x)
#toutes les relation linéaire positive qui existent sont positives, à part  (industrie et tertiaire_non_marchand)=-0.03535801,
# |r| < 0.8  pas de colinéarité entre les variables exogénes
det(cor(x)) #pas de colinearité 

vif(Reg2) #vif <=4 Il n'ya pas de multicolinearité
#effet de levier
hatvalues(Reg2) #hii proche de 0 alors Yi joue un role mineur dans le calcul de Yichapeau
#distance de cooks
cooks.distance(Reg2)
cooks.distance(reg2) > 1 # Donc aucune variable joue un role important dans la prediction

#Evaluation des hypothèses de normalité et d’homoscédasticité des résidus
check_model(Reg2)# la linéarité n'est pas bonne. 
#La normalité et l’homscédasticité des résidus ne sont pas parfaites, mais me semblent également raisonnable.
#test de normalité
check_normality(Reg2)  #pas de normalité
shapiro.test(residuals(Reg2)) # la valeur n'est pas normalement distribuée
# Orthogonalite visualisation
x11()
plot(Reg2,1)
#Homoscedasticite
x11()
plot(Reg2,which=3) # homoscedasticité
library(olsrr)
ols_test_score(Reg2)
check_heteroscedasticity(Reg2) #ok L’homoscédasticité n’est pas rejetée
#Recherche des outliers
check_outliers(Reg2) #pas outliers
#Interprétation du modèle 
Reg2
summary(Reg2)
#4 variables sont significatives : agriculture, l'industrie et le tertiaire marchand et non marchande

                  ######## Comparaison des regressions multiples ########
summary(Reg) #0.8466
summary(Reg2) #0.9677
#le modele 2 est meilleur
anova(Reg,Reg2)
#le modele 2 est bcp significative que le modele





