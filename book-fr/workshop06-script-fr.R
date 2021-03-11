##Section: 01-preparation-pour-l-atelier.R 

install.packages("ggplot2")
install.packages("lme4")
install.packages("MASS")
install.packages("vcdExtra")
install.packages("bbmle")
install.packages("DescTools")

library(ggplot2)
library(lme4)
library(MASS)
library(vcdExtra)
library(bbmle)
library(DescTools)


##Section: 02-introduction-fr.R 

setwd("~/Desktop")
mites <- read.csv('mites.csv')

head(mites)
str(mites)

# 70 communautés de mites échantillonées à partir de carottes de mousse prises à la Station de Biologie des Laurentides, QC.
# Pour chaque carotte/échantillon, les valeurs suivantes sont fournies
# $Galumna: abondance de mites du genre Galumna
# $pa: présence (1) ou absence (0) de Galumna, peu importe l'abondance
# $totalabund: abondance totale de mites, toutes espèces confondues
# $prop: proportion de Galumna dans la communauté de mites, i.e. Galumna/totalabund
# $SubsDens: densité du substrat
# $WatrCont: contenu d'eau de la carotte
# $Substrate: type de substrat
# $Shrub: abondance de buissons. Traité comme un facteur (une variable qualitative).
# $Topo: microtopographie du sol. "couverture" (blanket) ou "hammac" (hummock).

plot(mites)

par(mfrow=c(1,3)) #division de la fenêtre de diagramme en une ligne et 3 colonnes pour avoir 3 diagrammes sur la même figure.
plot(Galumna ~ WatrCont, data = mites, xlab = 'Water content', ylab='Abundance')
boxplot(WatrCont ~ pa, data = mites, xlab='Presence/Absence', ylab = 'Water content')
plot(prop ~ WatrCont, data = mites, xlab = 'Water content', ylab='Proportion')
par(mfrow=c(1,1)) #retour aux paramètres par défaut (un seul diagramme par figure)

lm.abund <- lm(Galumna ~ WatrCont, data = mites)
summary(lm.abund)
lm.pa <- lm(pa ~ WatrCont, data = mites)
summary(lm.pa)
lm.prop <- lm(prop ~ WatrCont, data = mites)
summary(lm.prop)

plot(Galumna ~ WatrCont, data = mites)
abline(lm.abund)

plot(lm.abund)

#Proportion
plot(prop ~ WatrCont, data = mites)
abline(lm.prop)
plot(lm.prop)
#Présence/Absence
plot(pa ~ WatrCont, data = mites)
abline(lm.pa)
plot(lm.pa)

coef(lm.abund)
#(Intercept)     WatrCont
#3.439348672 -0.006044788

summary(lm.abund)$sigma


##Section: 03-distributions.R 

hist(mites$Galumna)
mean(mites$Galumna)

hist(mites$pa)

sum(mites$pa) / nrow(mites)


##Section: 04-glm-binaire.R 

model.lm <- lm(pa ~ WatrCont + Topo, data = mites)
fitted(model.lm)
# La fonction "fitted()" extraie les valeurs prédites de la variable réponse du modèle linéaire.
# Certaines valeurs sont en-dessous de 0, ce qui ne fait pas de sens pour une régression logistique.
# Essayong le même modèle, mais avec une distribution binomiale cette fois-ci.
# Remarquez l'argument "family" pour spécifier la distribution.
model.glm <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
fitted(model.glm)
# Toutes les valeurs sont comprises entre 0 et 1.

# Chargez le jeu de données CO2 utilisé dans un atelier précédent
data(CO2)
head(CO2)
# Construisez un modèle linéaire de l'absorption de CO2 en fonction de la concentration ambiante de CO2.
model.CO2 <- lm(uptake ~ conc, data = CO2)
# On extraie la matrice du modèle avec la fonction model.matrix().
X <- model.matrix(model.CO2)
# Les paramètres estimés sont extraits ainsi :
B <- model.CO2$coefficients
# On multiple X et B pour obtenir le prédicteur linéaire.
# Le symbole "%*%" indique qu'on veut effectuer le produit matriciel.
XB <- X %*% B
# On compare les valeurs de XB aux valeurs obtenues avec la fonction fitted().
# Toutes les déclarations devraient être vraies (i.e. TRUE).
# On utilise la fonction round() pour que tous les éléments aient cinq décimales.
round(fitted(model.CO2), digits = 5) == round(XB, digits = 5)

# On construit un modèle de régression de la présence/absence d'une espèce de mite (Galumna sp.)
# en fonction du contenu en eau du sol et de la topographie.
# On utilise la fonction glm() et on spécifie l'argument "family".
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial(link = "logit"))
# La fonction de lien "logit" est la fonction par défaut pour une régression logistique,
# ce qui signifie qu'il n'est pas nécessaire de l'indiquer avec l'argument "family":
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
summary(logit.reg)

model.bact1 <- glm(y ~ trt * week, family = binomial('logit'), data = bacteria)
model.bact2 <- glm(y ~ trt + week, family = binomial('logit'), data = bacteria)
model.bact3 <- glm(y ~ week, family = binomial('logit'), data = bacteria)
anova(model.bact1, model.bact2, model.bact3, test = 'LRT')
# Analysis of Deviance Table
# Model 1: y ~ trt * week
# Model 2: y ~ trt + week
# Model 3: y ~ week
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1       214     203.12
# 2       216     203.81 -2  -0.6854  0.70984
# 3       218     210.91 -2  -7.1026  0.02869 *
# En se basant sur ces résultats, on doit choisir le modèle 2
# comme le modèle représentant le mieux le jeu de données.

# Pour obtenir les pentes, il faut utiliser la fonction exponentielle exp().
# Ceci mettra les coefficients sur l'échelle des cotes.
# Mathématiquement, ceci correspond à :
# exp(model coefficients) = exp(log(μ / (1 - μ)) = u / (1 - μ)
# C'est notre rapport de cotes !
exp(logit.reg$coefficients[2:3])
#  WatrCont    TopoHummock
#  0.9843118   8.0910340
# Pour obtenir l'intervalle de confiance sur l'échelle des cotes :
exp(confint(logit.reg)[2:3,])
#               2.5 %      97.5 %
#  WatrCont     0.9741887  0.9919435
#  TopoHummock  2.0460547  38.6419693

# On commence avec la valeur de cote pour la topographie du modèle logit.reg :
µ/ (1 - µ) = 8.09
# On réarrange pour isoler µ :
µ = 8.09(1 - µ) = 8.09 - 8.09µ
8.09µ + µ = 8.09
µ(8.09 + 1) = 8.09
µ = 8.09 / (8.09 + 1)
µ = 1 / (1 + (1 / 8.09)) = 0.89
# On obtient le même résultat sans utiliser la fonction logit inverse !

# Les déviances résiduelle et nulle sont déjà enregistrées dans un objet de type glm.
objects(logit.reg)
pseudoR2 <- (logit.reg$null.deviance – logit.reg$deviance) / logit.reg$null.deviance
pseudoR2
# [1]  0.4655937

install.packages("binomTools")
library("binomTools")
# La fonctions Rsq calcule indices d'ajustement
# dont le coefficient de discrimination.
# Pour plus d'information sur les autres indices, consultez Tjur (2009).
# Les histogrammes montrent la distribution des valeurs attendues lorsque le résultat est observé
# et non observé.
# Idéalement, le recouvrement entre les deux histogrammes doit être minimal.
fit <- Rsq(object = logit.reg)
fit
# R-square measures and the coefficient of discrimination, 'D':
#
#    R2mod     R2res     R2cor     D
#    0.5205221 0.5024101 0.5025676 0.5114661
#
# Number of binomial observations:  70
# Number of binary observation:  70
# Average group size:  1
plot(fit, which = "hist")

fit <- Rsq(object = logit.reg)
HLtest(object = fit)
# La valeur de p est de 0.9051814. Donc, on ne rejète pas notre modèle.
# L'ajustement du modèle est bon.

null.d <- model.bact2$null.deviance
resid.d <- model.bact2$deviance
bact.pseudoR2 <- (null.d - resid.d) / null.d
bact.pseudoR2
# 0.0624257
# C'est très faible !
library(binomTools)
HLtest(Rsq(model.bact2))
# Chi-square statistic:  7.812347  with  8  df
# P-value:  0.4520122
# L'ajustement est adéquat.

library(ggplot2)
ggplot(mites, aes(x = WatrCont, y = pa)) + geom_point() +
stat_smooth(method = "glm", family= "binomial", se = FALSE) + xlab("Water content") +
ylab("Probability of presence") +
ggtitle("Probability of presence of Galumna sp. as a function of water content")


##Section: 05-glm-proportion.R 

# Commençons par générer des données en se basant sur l'exemple précédent :
# On choisit un nombre aléatoire entre 1 et 10 pour déterminer le nombre de cerfs infectés (objet 'n.infected').
# On échantillonne dix individus dans dix populations différentes (objet 'n.total').
# 'res.avail' est un indice de qualité de l'habitat (disponibilité de ressources).
set.seed(123)
n.infected <- sample(x = 1:10, size = 10, replace = TRUE)
n.total <- rep(x = 10, times = 10)
res.avail <- rnorm(n = 10, mean = 10, sd = 1)
# Ensuite, on spécifie le modèle. Prenez note comment on spécifie la variable réponse.
# Il faut indiquer le nombre de fois où la maladie a été détectée
# et le nombre de fois où celle-ci n'a pas été détectée.
prop.reg <- glm(cbind(n.infected, n.total - n.infected) ~ res.avail, family = binomial)
summary(prop.reg)
# Si vos données sont déjà sous la forme de proportions, voici comment faire dans R :
# Créons tout d'abord un vecteur de proportions :
prop.infected <- n.infected / n.total
# On doit spécifier l'argument "weights" de la fonction glm() pour indiquer le nombre d'essais par site.
prop.reg2 <- glm(prop.infected ~ res.avail, family = binomial, weights = n.total)
summary(prop.reg2)
# Les sommaires des modèles prop.reg et prop.reg2 sont identiques !


##Section: 06-glm-abondances.R 

faramea <- read.csv(‘faramea.csv’, header = TRUE)

hist(faramea$Faramea.occidentalis, breaks=seq(0,45,1), xlab=expression(paste("Number of ",
italic(Faramea~occidentalis))), ylab="Frequency", main="", col="grey")

glm.poisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=poisson)
summary(glm.poisson)

# ordonnée à l'origine
summary(glm.poisson)$coefficients[1,1]
# coefficient de regression de l'élévation
summary(glm.poisson)$coefficients[2,1]

summary(glm.poisson)
# Null deviance: 414.81  on 42  degrees of freedom
# Residual deviance: 388.12  on 41  degrees of freedom

mean(faramea$Faramea.occidentalis)
var(faramea$Faramea.occidentalis)

# Option 1, nous ajustons un nouveau modèle GLM quasi-Poisson
glm.quasipoisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=quasipoisson)
# Option 2, nous actualisons le modèle précédent :
glm.quasipoisson = update(glm.poisson,family=quasipoisson)
# regardons le résumé
summary(glm.quasipoisson)

ifelse(length(which(installed.packages() == "MASS")) == 0,
      {print("MASS not installed. Installing... "); install.packages("MASS")},
      print("MASS already installed"))

install.packages("MASS")

library("MASS")

glm.negbin = glm.nb(Faramea.occidentalis~Elevation, data=faramea)
summary(glm.negbin)

# représenter les données observées
plot(faramea$Elevation, faramea$Faramea.occidentalis, xlab="Elevation (m)", ylab=expression(paste("Number of", "  ", italic(Faramea~occidentalis))), pch=16, col=rgb(4,139,154,150,maxColorValue=255))

# récupérons les valeurs de l'ordonnée à l'origine et de beta à partir du résumé et ajoutons les dans l'équation du modèle
curve(exp(summary(glm.negbin)$coefficients[1,1]+summary(glm.negbin)$coefficients[2,1]*x),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T, lwd=2, col="orangered")

# récupérons les écarts-types pour construire l'intervalle de confiance du modèle
curve(exp(summary(glm.negbin)$coefficients[1,1]+1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x+1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")
curve(exp(summary(glm.negbin)$coefficients[1,1]-1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x-1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")


##Section: 07-autres-distributions.R 




##Section: 08-considerations-finales.R 




##Section: 09-references-fr.R 




