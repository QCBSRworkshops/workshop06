##Section: 01-preparation-pour-l-atelier.R 

#Avis ###
                                                                            #
Ceci est un script généré automatiquement basé sur les morceaux de code du  #
livre pour cet atelier.                                                     #
                                                                            #
Il est minimalement annoté pour permettre aux participants de fournir leurs #
commentaires : une pratique que nous encourageons vivement.                 #
                                                                            #
Notez que les solutions aux défis sont également incluses dans ce script.   #
Lorsque vous résolvez les défis par vous-méme, essayez de ne pas parcourir  #
le code et de regarder les solutions.                                       #
                                                                            #
#Bon codage !                                                               #

install.packages(c('ggplot2',
                   'MASS',
                   'vcdExtra',
                   'bbmle',
                   'DescTools',
                   'GlmSimulatoR',
                   'cplm')
                 )



library(ggplot2)
library(MASS)
library(vcdExtra)
library(bbmle)
library(DescTools)
library(GlmSimulatoR)
library(cplm)


##Section: 02-introduction-fr.R 

# Set the coefficients:
N = 50
beta_0 = 1
beta_1 = 0.5

# Generate sample data:
x <- 0:N
e <- rnorm(mean = 0, sd = 1.5, n = length(x))
y <- beta_0 + beta_1 * x + e

# Plot the data
plot(x, y)

# The regression equation:
y_dgp <- beta_0 + beta_1 * x

# Plot regression:
lines(x = x, y = y_dgp, col = "darkgreen", lty = 2)

legend(x = 0, y = 25,
       legend = c(expression(paste("Y = ", beta[0] + beta[1] * X))),
       lty = c(2, 1), lwd = c(1, 1), pch = c(NA, NA), col = c("darkgreen", "blue"))

nSamples <- 250
ID <- factor(c(seq(1:nSamples)))

PredVar <- runif(nSamples,min = 0,max = 50)

simNormData <- data.frame(ID = ID,PredVar = PredVar,RespVar = (2*PredVar + rnorm(nSamples,mean = 0,sd = 2)))

lm.simNormData <- lm(RespVar ~ PredVar, 
                     data = simNormData)

layout(matrix(c(1,2,3,4),2,2)) 
plot(lm.simNormData)

# Use setwd() to set your working directory

mites <- read.csv('data/mites.csv',
                  stringsAsFactors = TRUE)

head(mites)

str(mites)

plot(Galumna ~ WatrCont,
     data = mites,
     xlab = 'Water content',
     ylab = 'Abundance')

boxplot(WatrCont ~ pa,
        data = mites,
        xlab='Presence/Absence',
        ylab = 'Water content')

plot(prop ~ WatrCont,
     data = mites,
     xlab = 'Water content',
     ylab = 'Proportion')

# Fit the models

# Abundance model
lm.abund <- lm(Galumna ~ WatrCont, data = mites)

# Presence-absence model
lm.pa <- lm(pa ~ WatrCont, data = mites)

# Proportion model
lm.prop <- lm(prop ~ WatrCont, data = mites)

# Check the model output with the summary() function
summary(lm.abund)

summary(lm.pa)

summary(lm.prop)

# Extracting the Pr(>|t|)

summary(lm.abund)$coefficients[, 4]
summary(lm.pa)$coefficients[, 4]
summary(lm.prop)$coefficients[, 4]

# Plot the abundance model
plot(Galumna ~ WatrCont, data = mites)
abline(lm.abund)

# Diagnostic plots
plot(lm.abund)

# Plot the proportion model
plot(prop ~ WatrCont, data = mites)
abline(lm.prop)

# Diagnostic plots
plot(lm.prop)

# Plot the presence/absence model
plot(pa ~ WatrCont, data = mites)
abline(lm.pa)

# Diagnostic plots
plot(lm.pa)

# Demonstrating normal distributions with different means
x = seq(1, 50, 0.1)
plot(x, dnorm(x, mean = 20, sd = 5),
type = 'l', lwd = 3,
xlab = '# galumna', ylab = 'Probabilité')
points(x, dnorm(x, mean = 25, sd = 5),
type = 'l', lwd = 3, col = 2)
points(x, dnorm(x, mean = 30, sd = 5), type = 'l', lwd = 3, col = 4)
legend('topleft', legend = c('20', '25', '30'), lty = 1, col = c(1,2,4), bty = 'n', lwd = 2, cex = 1.1)

# Demonstrating normal distributions with different variance
x = seq(1, 50, 0.1)
plot(x, dnorm(x, mean = 25, sd = 5), type = 'l', lwd = 3, xlab = '# galumna', ylab = 'Probabilité')
points(x, dnorm(x, mean = 25, sd = 7.5), type = 'l', lwd = 3, col = 2)
points(x, dnorm(x, mean = 25, sd = 10), type = 'l', lwd = 3, col = 4)
legend('topleft', legend = c('5', '7.5', '10'), lty = 1, col = c(1,2,4), bty = 'n', lwd = 2, cex = 1.1)

# Extract model coefficients
coef(lm.abund)

# Extract variance from the model summary
summary(lm.abund)$sigma


##Section: 03-distributions.R 

# examples of Poisson distributions with different values of lambda
par(cex = 2)
x = seq(1, 50, 1)
plot(x, dpois(x, lambda = 1), type = "h", lwd = 3, xlab = "Frequency of Galumna", ylab = "Probability", main = "lambda = 1")
plot(x, dpois(x, lambda = 10), type = "h", lwd = 3, xlab = "Frequency of Galumna", ylab = "Probability", main = "lambda = 10")
plot(x, dpois(x, lambda = 30), type = "h", lwd = 3, xlab = "Frequency of Galumna", ylab = "Probability", main = "lambda = 30")

mites <- read.csv('mites.csv')
hist(mites$Galumna)
mean(mites$Galumna)

hist(mites$pa)

# examples of Bernoulli distributions with various probabilities of presence (p)
par(cex = 2.1)
barplot(setNames(c(.9, .1), c('absent (0)', 'present (1)')),
        ylim = c(0, 1),
        xlab = '', ylab = 'probability',
        main = 'p = 0.1')
barplot(setNames(c(.5, .5), c('absent (0)', 'present (1)')),
        ylim = c(0, 1),
        xlab = '', ylab = 'probability',
        main = 'p = 0.5')
barplot(setNames(c(.1, .9), c('absent (0)', 'present (1)')),
        xlab = '', ylim = c(0, 1),
        ylab = 'probability',
        main = 'p = 0.9')

sum(mites$pa) / nrow(mites)

# examples of binomial distributions with n = 50 and 3 different values of p
par(cex = 2.1)
x = seq(1, 50, 1)
plot(x, dbinom(x, size = 50, prob = 0.1), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probability', main = 'p = 0.1 n = 50')
plot(x, dbinom(x, size = 50, prob = 0.5), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probability', main = 'p = 0.5 n = 50')
plot(x, dbinom(x, size = 50, prob = 0.9), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probability', main = 'p = 0.9 n = 50')

#Code to generate figure that illustrates a poisson glm
glm.pois <- glm(Galumna ~ WatrCont, data = mites, family='poisson')
plot.poiss<-function(x,mymodel,mult=1,mycol='LightSalmon') {
  yvar<-mymodel$model[,1]
  xvar<-mymodel$model[,2]
  lambd<-mymodel$fitted[x]
  stick.val<-rep(xvar[x],9)+mult*dpois(0:8,lambd=lambd)
  segments(rep(xvar[x],9),0:8,stick.val,0:8,col=mycol,lwd=3)
}
plot(Galumna ~ WatrCont, data = mites,cex.axis=1.2,cex.lab=1)
points(Galumna ~ WatrCont, data = mites,pch=21)
lines(x=seq(min(mites$WatrCont),max(mites$WatrCont),by=1),y=predict(glm.pois,newdata=data.frame('WatrCont' = seq(min(mites$WatrCont),max(mites$WatrCont),by=1)),type='response'))
par(lend=3)
plot.poiss(8,glm.pois,200)
abline(v=mites$WatrCont[8],col='red',lty=2)
plot.poiss(11,glm.pois,200)
abline(v=mites$WatrCont[11],col='red',lty=2)
plot.poiss(36,glm.pois,200)
abline(v=mites$WatrCont[36],col='red',lty=2)
plot.poiss(52,glm.pois,200)
abline(v=mites$WatrCont[52],col='red',lty=2)
text(x = mites$WatrCont[8]+50,y=7.5,expression(lambda == 1.7), cex=0.7, col = 'red')
text(x = mites$WatrCont[11]+50,y=7.5,expression(lambda == 4.7), cex=0.7, col = 'red')
text(x = mites$WatrCont[36]+50,y=7.5,expression(lambda == 0.5), cex=0.7, col = 'red')
text(x = mites$WatrCont[52]+50,y=7.5,expression(lambda == 0.1), cex=0.7, col = 'red')


##Section: 04-glm-fr.R 

# This is what the glm() function syntax looks like (don't run this)
glm(formula,
    family = gaussian(link = "identity"),
    data,
    ...)


##Section: 05-glm-binaire.R 

# set up some binary data
Pres <- c(rep(1, 40), rep(0, 40))
rnor <- function(x) rnorm(1, mean = ifelse(x == 1, 12.5, 7.5), sd = 2)
ExpVar <- sapply(Pres, rnor)

# linear model with binary data...
lm(Pres ~ ExpVar)

par(cex = 1.2)
Pres <- c(rep(1, 40), rep(0, 40))
rnor <- function(x) rnorm(1, mean = ifelse(x == 1, 12.5, 7.5), sd = 2)
ExpVar <- sapply(Pres, rnor)
plot(ExpVar, Pres,
     ylim = c(-.5, 1.5),
     xlab = 'Explanatory variable',
     ylab = 'Presence',
     main = 'Binary variables and fitted values',
     pch = 16)
abline(lm(Pres ~ ExpVar), col = 'orange', lwd = 2)
mtext(expression(symbol("\255")), at = 1.25, side = 4, line = 0.1, cex = 6, col = 'blue')
mtext(expression(symbol("\256")), at = 3, side = 1, line = -2.2, cex = 6, col = 'blue')

# histogram
hist(Pres)

glm(formula,
    family = ???, # this argument allows us to set a probability distribution!
    data,
    ...)

# This is the syntax for a binomial GLM with a logit link
glm(formula,
    family = binomial(link = "logit"), # this is also known as logistic
    data,
    ...)

# setwd('...')

mites <- read.csv("data/mites.csv", header = TRUE)
str(mites)

logit.reg <- glm(pa ~ WatrCont + Topo,
                 data = mites,
                 family = binomial(link = "logit"))

summary(logit.reg)

# Challenge 1 - Set up!
library(MASS)
data(bacteria)

# what does the data look like?
str(bacteria)

# Challenge 1 - Solution

# Fit models (full to most parsimonious)
model.bact1 <- glm(y ~ trt * week, data = bacteria, family = binomial)
model.bact2 <- glm(y ~ trt + week, data = bacteria, family = binomial)
model.bact3 <- glm(y ~ week, data = bacteria, family = binomial)

# Let's compare these models using a likelihood ratio test (LRT).
anova(model.bact1, model.bact2, model.bact3, test = "LRT")

# Which model is the best candidate?

# Extracting model coefficients
summary(logit.reg)$coefficients

exp(logit.reg$coefficients[2])

exp(confint(logit.reg)[2,])

library(ggplot2)
ggplot(mites, aes(x = WatrCont, y = pa)) + geom_point() + xlab("Contenu en eau") +
ylab("Probabilité de présence")

exp(logit.reg$coefficients[1])

# Les déviances résiduelle et nulle sont déjà enregistrées dans un objet de type glm.
objects(logit.reg)

# calcule pseudo-R2
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
stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) + xlab("Contenu en eau") +
ylab("Probabilité de présence") +
ggtitle("Probabilité de présence de Galumna sp. en fonction du contenu en eau")+theme_classic()


##Section: 06-glm-proportion.R 

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


##Section: 07-glm-abondances.R 

faramea <- read.csv("faramea.csv", header = TRUE)

# Histogram of F. occidentalis count data
hist(faramea$Faramea.occidentalis, breaks=seq(0,45,1), xlab=expression(paste("Nombre de ",
italic(Faramea~occidentalis))), ylab="Fréquence", main="", col="grey")

plot(faramea$Elevation, faramea$Faramea.occidentalis, ylab = 'F. occidentalis individuals', xlab = 'Élévation(m)')

# Fit a Poisson GLM
glm.poisson = glm(Faramea.occidentalis ~ Elevation,
  data = faramea,
  family = poisson) # this is what makes it a Poisson GLM! Note the default link is log.
summary(glm.poisson)

# Ordonnée à l'origine
summary(glm.poisson)$coefficients[1,1]
# pente de elevation
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


##Section: 08-autres-distributions.R 

#install.packages(c('GlmSimulatoR','cplm'))
library(GlmSimulatoR)
library(ggplot2)
library(cplm, quietly = TRUE)

simdata <- simulate_tweedie(weight = .2, ancillary = 1.15, link = "log")

ggplot(simdata, aes(x = Y)) + 
  geom_histogram(bins = 30)


##Section: 09-considerations-finales.R 




##Section: 10-references-fr.R 




