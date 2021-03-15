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

mites <- read.csv('mites.csv')

head(mites)
str(mites)

plot(mites)

par(mfrow=c(1,3), cex =1) #division de la fenêtre de diagramme en une ligne et 3 colonnes pour avoir 3 diagrammes sur la même figure.
plot(Galumna ~ WatrCont, data = mites, xlab = 'Water content', ylab='Abundance')
boxplot(WatrCont ~ pa, data = mites, xlab='Presence/Absence', ylab = 'Water content')
plot(prop ~ WatrCont, data = mites, xlab = 'Water content', ylab='Proportion')

lm.abund <- lm(Galumna ~ WatrCont, data = mites)
##summary(lm.abund)
lm.pa <- lm(pa ~ WatrCont, data = mites)
##summary(lm.pa)
lm.prop <- lm(prop ~ WatrCont, data = mites)
##summary(lm.prop)

# Extracting the Pr(>|t|)
summary(lm.abund)$coefficients[, 4]

summary(lm.pa)$coefficients[, 4]

summary(lm.prop)$coefficients[, 4]


plot(Galumna ~ WatrCont, data = mites)
abline(lm.abund)

par(mfrow = c(2, 2), cex = 1.4)
plot(lm.abund)

#Proportion
plot(prop ~ WatrCont, data = mites)
abline(lm.prop)
par(mfrow = c(2, 2), cex = 1.4)
plot(lm.prop)
#Présence/Absence
par(mfrow = c(1, 1), cex = 1.4)
plot(pa ~ WatrCont, data = mites)
abline(lm.pa)
par(mfrow = c(2, 2), cex = 1.4)
plot(lm.pa)

coef(lm.abund)

summary(lm.abund)$sigma


##Section: 03-distributions.R 

mites <- read.csv('mites.csv')
hist(mites$Galumna)
mean(mites$Galumna)

hist(mites$pa)

sum(mites$pa) / nrow(mites)


##Section: 04-glm-binaire.R 

mites <- read.csv('mites.csv')
model.lm <- lm(pa ~ WatrCont + Topo, data = mites)
fitted(model.lm)
# La fonction «fitted()» extrait les valeurs prédites de la variable réponse du modèle linéaire.
# Certaines valeurs sont en-dessous de 0, ce qui ne fait pas de sens pour une régression logistique.
# Essayons le même modèle, mais avec une distribution binomiale cette fois-ci.
# Remarquez l'argument «family» pour spécifier la distribution.
model.glm <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
fitted(model.glm)
# Toutes les valeurs sont comprises entre 0 et 1.

#Petite fonction ajoutée pour que seules les lignes désirées sortent dans le prochain bloc!
#Source: https://bookdown.org/yihui/rmarkdown-cookbook/hook-truncate.html
# save the built-in output hook
hook_output <- knitr::knit_hooks$get("output")

# set a new output hook to truncate text output
knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- options$out.lines)) {
    x <- xfun::split_lines(x)
    if (length(x) > n) {
      # truncate the output
      x <- c(head(x, n), "....\n")
    }
    x <- paste(x, collapse = "\n")
  }
  hook_output(x, options)
})
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
# Le symbole «%*%» indique qu'on veut effectuer le produit matriciel.
XB <- X %*% B
# On compare les valeurs de XB aux valeurs obtenues avec la fonction fitted().
# Toutes les déclarations devraient être vraies (i.e. TRUE).
# On utilise la fonction round() pour que tous les éléments aient cinq décimales.
round(fitted(model.CO2), digits = 5) == round(XB, digits = 5)

# On construit un modèle de régression de la présence/absence d'une espèce de mite (Galumna sp.)
# en fonction du contenu en eau du sol et de la topographie.
# On utilise la fonction glm() et on spécifie l'argument «family».
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial(link = "logit"))
# La fonction de lien «logit» est la fonction par défaut pour une régression logistique,
# ce qui signifie qu'il n'est pas nécessaire de l'indiquer avec l'argument «family»:
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
summary(logit.reg)

library(MASS)
data(bacteria)
str(bacteria)

model.bact1 <- glm(y ~ trt * week, family = binomial('logit'), data = bacteria)
model.bact2 <- glm(y ~ trt + week, family = binomial('logit'), data = bacteria)
model.bact3 <- glm(y ~ week, family = binomial('logit'), data = bacteria)
anova(model.bact1, model.bact2, model.bact3, test = 'LRT')

logit.reg

exp(logit.reg$coefficients[2])

exp(confint(logit.reg)[2,])

# Les déviances résiduelle et nulle sont déjà enregistrées dans un objet de type glm.
objects(logit.reg)
pseudoR2 <- (logit.reg$null.deviance - logit.reg$deviance) / logit.reg$null.deviance
pseudoR2

logit.reg <- glm(pa ~ WatrCont + Topo, 
                 data = mites, family = binomial(link = "logit"))
DescTools::PseudoR2(logit.reg, which = "all")

#ENLEVÉ PARCE QUE LE PACKAGE binomTools A ÉTÉ RETIRÉ DU CRAN (à supprimer?)

Récemment, [Tjur
(2009)](http://www.tandfonline.com/doi/abs/10.1198/tast.2009.08210#.VFpKZYcc4ow)
a proposé une nouvelle statistique, le coefficient de discrimination
(*D*), afin d'évaluer le pouvoir prédictif d'une régression
logistique. Intuitivement, *D* évalue si la régression logistique peut
classer adéquatement chaque résultat comme un succès ou un échec.
Mathématiquement, c'est la différence entre la moyenne des valeurs
prédites des succès (*i.e.* Y = 1) et des échecs (*i.e.* Y = 0) :

*D = π~1~ - π~0~*

où *π~1~* est la moyenne attendue des probabilités
lorsqu'un événement est observé et *π~0~* est la
moyenne attendue des probabilités lorsqu'un événement n'est pas
observé. Une valeur de *D* près de 1 indique que le modèle accorde une
probabilité élevée d'observer un événement lorsque celui-ci a été
observé et une probabilité faible d'observer un événement lorsque
celui-ci n'a pas été observé. Une valeur de *D* près de 0 indique que
le modèle n'est pas utile pour distinguer entre les observations et les
«non-observations» d'un résultat.



# Le code suivant montre comment obtenir la valeur de *D* et comment représenter #visuellement les valeurs
de *π~1~* et *π~0~*.

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


Pour évaluer l'ajustement (*i.e.* goodness-of-fit) d'une régression
logistique, les graphiques de diagnostic ne sont pas très utiles (voir
atelier 4). On utilise plutôt un [test de
Hosmer-Lemeshow](http://en.wikipedia.org/wiki/Hosmer-Lemeshow_test) pour
évaluer si un modèle est approprié ou non. Ce test sépare les valeurs
attendues (ordonnées de la plus faible à la plus grande) en groupes de
même taille. Dix groupes est généralement le nombre de groupes
recommandé. Pour chaque groupe, on compare les valeurs observées aux
valeurs attendues. Le test est similaire à un test de khi-carré avec G -
2 degrés de liberté (G est le nombre de groupes). Dans R, ce test est
disponible dans le paquet `binomTools` ou `vcdExtra`.

fit <- Rsq(object = logit.reg)
HLtest(object = fit)
# La valeur de p est de 0.9051814. Donc, on ne rejète pas notre modèle.
# L'ajustement du modèle est bon.

null.d <- model.bact2$null.deviance
resid.d <- model.bact2$deviance
bact.pseudoR2 <- (null.d - resid.d) / null.d
bact.pseudoR2
#ENLEVÉ parce que le package binomTools retiré du CRAN

library(binomTools)
HLtest(Rsq(model.bact2))
# Chi-square statistic:  7.812347  with  8  df
# P-value:  0.4520122
# L'ajustement est adéquat.

library(ggplot2)
ggplot(mites, aes(x = WatrCont, y = pa)) + geom_point() +
stat_smooth(method = "glm", family= "binomial", se = FALSE) + xlab("Water content") +
ylab("Probabilité de présence") +
ggtitle("Probabilité de présence de Galumna sp. en fonction du contenu en eau")


##Section: 05-glm-proportion.R 

mites <- read.csv('mites.csv')
prop.reg <- glm(cbind(Galumna, totalabund - Galumna) ~ Topo + WatrCont,
                data = mites,
                family = binomial)
summary(prop.reg)

prop.reg2 <- glm(prop ~ Topo + WatrCont,
                 data = mites,
                 family = binomial,
                 weights = totalabund)
summary(prop.reg2)


##Section: 06-glm-abondances.R 

faramea <- read.csv("faramea.csv", header = TRUE)

hist(faramea$Faramea.occidentalis, breaks=seq(0,45,1), xlab=expression(paste("Nombre de ",
italic(Faramea~occidentalis))), ylab="Fréquence", main="", col="grey")

glm.poisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=poisson)
summary(glm.poisson)

# ordonnée à l'origine
summary(glm.poisson)$coefficients[1,1]
# coefficient de regression de l'élévation
summary(glm.poisson)$coefficients[2,1]

    Null deviance: 414.81  on 42  degrees of freedom
Residual deviance: 388.12  on 41  degrees of freedom

mean(faramea$Faramea.occidentalis)
var(faramea$Faramea.occidentalis)

# Option 1, nous ajustons un nouveau modèle GLM quasi-Poisson
glm.quasipoisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=quasipoisson)
# Option 2, nous actualisons le modèle précédent :
glm.quasipoisson = update(glm.poisson,family=quasipoisson)
# regardons le résumé
summary(glm.quasipoisson)

null.model <- glm(Faramea.occidentalis ~ 1, 
                  data = faramea,
                  family = quasipoisson)
anova(null.model, glm.quasipoisson, test = "Chisq")


library("MASS")
glm.negbin = glm.nb(Faramea.occidentalis~Elevation, data=faramea)
summary(glm.negbin)

summary(glm.negbin)$coefficients[1, 1]
summary(glm.negbin)$coefficients[2, 1]

summary(glm.negbin)$coefficients[1, 2]
summary(glm.negbin)$coefficients[2, 2]

pp <- predict(glm.negbin, newdata = data.frame(Elevation = 1:800), se.fit = TRUE)
linkinv <- family(glm.negbin)$linkinv inverse-link function
pframe <- as.data.frame(pp$fit)
names(pframe) <- "pred0"
pframe$pred <- linkinv(pp$fit)
sc <- abs(qnorm((1-0.95)/2))  Normal approx. to likelihood
pframe <- transform(pframe, lwr = linkinv(pred0-sc*pp$se.fit), upr = linkinv(pred0+sc*pp$se.fit))
plot(faramea$Elevation, faramea$Faramea.occidentalis, ylab = 'Nombre de F. occidentalis', xlab = 'Élévation(m)')
lines(pframe$pred, lwd = 2)
lines(pframe$upr, col = 2, lty = 3, lwd = 2)
lines(pframe$lwr, col = 2, lty = 3, lwd = 2)

mites <- read.csv("mites.csv", header = TRUE)

drop1(MyGLM, test = "Chi")

anova(MyGLM, MyGLM2, test = "Chi")

# GLM Poisson
glm.p = glm(Galumna~WatrCont+SubsDens, data=mites, family=poisson)
# GLM quasi-Poisson
glm.qp = update(glm.p,family=quasipoisson)
# sélection du modèle
drop1(glm.qp, test = "Chi")
# ou
glm.qp2 = glm(Galumna~WatrCont, data=mites, family=quasipoisson)
anova(glm.qp2, glm.qp, test="Chisq")


##Section: 07-autres-distributions.R 




##Section: 08-considerations-finales.R 




##Section: 09-references-fr.R 




