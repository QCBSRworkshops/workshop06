## Ateliers R du CSBQ
## GLMs and GLMMs
## Auteurs : Cédric Frenette-Dussault, Vincent Fugère, Thomas Lamy, Zofia Taranu
## Date : Novembre 2014
## R version 3.0.2

#### Charger les données ####

setwd("~/Desktop")
mites <- read.csv('mites.csv')

head(mites)
str(mites)

# 70 communautés de mites échantillonées à partir de carottes de mousse
# prises à la Station de Biologie des Laurentides, QC.
# Pour chaque carotte/échantillon, les valeurs suivantes sont fournies :
# $Galumna: abondance de mites du genre Galumna
# $pa: présence (1) ou absence (0) de Galumna, peu importe l'abondance
# $totalabund: abondance totale de mites, toutes espèces confondues
# $prop: proportion de Galumna dans la communauté de mites, i.e. Galumna/totalabund
# $SubsDens: densité du substrat
# $WatrCont: contenu d'eau de la carotte
# $Substrate: type de substrat
# $Shrub: abondance de buissons. Traité comme un facteur (une variable qualitative).
# $Topo: microtopographie du sol. Traité comme un factuer : "plat" (blanket) ou "hummock".

#### Limites des modèles linéaires ####

# Question: est-ce que l'abondance, la présence, et la proportion de Galumna 
# varient en fonction des 5 variables environnementales?

# Peut-on voir des relations à l'aide de simples diagrammes bivariés?
plot(mites)

# Les 3 variables réponses semblent montrer une relation négative avec WatrCont:
par(mfrow=c(1,3)) #division de la fenêtre de diagramme en une ligne et 3 colonnes
# pour avoir 3 diagrammes sur la même figure.
plot(Galumna ~ WatrCont, data = mites, xlab = 'Water content', ylab='Abundance')
boxplot(WatrCont ~ pa, data = mites, xlab='Presence/Absence', ylab = 'Water content')
plot(prop ~ WatrCont, data = mites, xlab = 'Water content', ylab='Proportion')
par(mfrow=c(1,1)) #retour aux paramètres par défaut (un seul diagramme par figure)
# Galumna préfère les sites moins humides?

# Peut-on tester ces relations à l'aide de modèles linéaires?
lm.abund <- lm(Galumna ~ WatrCont, data = mites)
summary(lm.abund)
lm.pa <- lm(pa ~ WatrCont, data = mites)
summary(lm.pa)
lm.prop <- lm(prop ~ WatrCont, data = mites)
summary(lm.prop)
# Tout est significatif, super ! Attends une minute...

#Validons et représentons graphiquement ces modèles
plot(Galumna ~ WatrCont, data = mites)
abline(lm.abund) #Le modèle représente mal les données: prédit des valeurs négatives
# lorsque le contenu d'eau excède 600, prédit mal les valeurs élevées d'abondance
# lorsque le contenu d'eau est faible, etc.
plot(lm.abund) # pas super... Les résidus ne sont pas normaux, la variance est hétérogène.

# même problème avec prop
plot(prop ~ WatrCont, data = mites)
abline(lm.prop)
plot(lm.prop)

#encore pire pour présence/absence...
plot(pa ~ WatrCont, data = mites)
abline(lm.pa)
plot(lm.pa)

# #Code pour générer la figure qui montre les paramètres d'une distribution normale
par(mfrow=c(1,2))
plot(0:50, dnorm(0:50,20,5), type='l', lwd=1, xlab='#galumna', ylab='probability',cex.lab=0.8,cex.axis=0.8)
lines(0:50, dnorm(0:50,25,5), lwd=1, col='red')
lines(0:50, dnorm(0:50,30,5), lwd=1, col='blue')
legend(-3,0.08,legend = c(expression(italic(mu) == 20),expression(italic(mu) == 25),expression(italic(mu) == 30)), bty='n', text.col=c('black','red','blue'), cex=0.7)
mtext(side=3,line=.5,cex=0.7,expression("varying"~italic(mu)*","~~italic(sigma) == 5))
plot(0:50, dnorm(0:50,25,5), type='l', lwd=1, xlab='#galumna', ylab='probability',cex.lab=0.8,cex.axis=0.8)
lines(0:50, dnorm(0:50,25,7.5), lwd=1, col='red')
lines(0:50, dnorm(0:50,25,10), lwd=1, col='blue')
legend(-3,0.08,legend = c(expression(italic(sigma) == 5),expression(italic(sigma) == 7.5),expression(italic(sigma) == 10)), bty='n', text.col=c('black','red','blue'), cex=0.7)
mtext(side=3,line=.5,cex=0.7,expression(italic(mu) ==25*","~~"varying"~italic(sigma)))

#coefficients du modèle linéaire pour galumna abondance
coef(lm.abund)

#Code pour générer la figure qui montre les suppositions des modèles linéaires
plot.norm<-function(x,mymodel,mult=1,sd.mult=3,mycol='LightSalmon',howmany=150) {
yvar<-mymodel$model[,1]
xvar<-mymodel$model[,2]
sigma<-summary(mymodel)$sigma
stick.val<-rep(xvar[x],howmany)+mult*dnorm(seq(predict(mymodel)[x]-sd.mult*sigma, predict(mymodel)[x]+sd.mult*sigma, length=howmany), mean=predict(mymodel)[x],sd=sigma)
steps<-seq(predict(mymodel)[x]-sd.mult*sigma,predict(mymodel)[x]+sd.mult*sigma,length=howmany)
polygon(c(stick.val,rep(xvar[x],howmany)),c(sort(steps,decreasing=T),steps),col=mycol,border=NA)
}
#function adaptée de http://www.unc.edu/courses/2010fall/ecol/563/001/notes/lecture4%20Rcode.txt
plot(Galumna ~ WatrCont, data = mites,ylim=c(-4,8),cex.axis=1,cex.lab=1,type='n')
plot.norm(8,lm.abund,200)
plot.norm(11,lm.abund,200)
plot.norm(36,lm.abund,200)
plot.norm(52,lm.abund,200)
abline(h=0,lty=3)
points(Galumna ~ WatrCont, data = mites,pch=21)
abline(lm.abund,lty=1)
abline(v=mites$WatrCont[c(8,11,36,52)],col='red',lty=2)
text(x = mites$WatrCont[8]+50,y=7.5,expression(mu == 1.8),cex=1,col='red')
text(x = mites$WatrCont[11]+50,y=7.5,expression(mu == 2.6),cex=1,col='red')
text(x = mites$WatrCont[36]+50,y=7.5,expression(mu == 0.9),cex=1,col='red')
text(x = mites$WatrCont[52]+60,y=7.5,expression(mu == -0.1),cex=1,col='red')
text(x = mites$WatrCont[52]+105,y=6.5,expression(sigma == 'always' ~ 1.51),cex=1,col='red')

#calculer sigma pour lm.abund:
summary(lm.abund)$sigma

#### Lois de probabilités et distributions ####

# Comment sont distribuées nos données?
hist(mites$Galumna) #nombres entiers, pas une variable continue
mean(mites$Galumna)

hist(mites$pa) #0 ou 1
sum(mites$pa) / nrow(mites)

#Code pour créer les 3 figures montrant les lois de Bernoulli, binomiale et poisson 
# Loi de Bernoulli
par(mfrow=c(1,3), lend=3)
plot(0:1, dbinom(0:1,1,.1), xlim=c(-0.5,1.5), type='h', lwd=15, xaxt='n', xlab='p/a', ylab='probability')
axis(1,at=c(0,1),labels=c('absent(0)','present(1)'),tick=F)
mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.1))
plot(0:1, dbinom(0:1,1,.5), xlim=c(-0.5,1.5), type='h', lwd=15, xaxt='n', xlab='p/a', ylab='probability')
axis(1,at=c(0,1),labels=c('absent(0)','present(1)'),tick=F)
mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.5))
plot(0:1, dbinom(0:1,1,.9), xlim=c(-0.5,1.5), type='h', lwd=15, xaxt='n', xlab='p/a', ylab='probability')
axis(1,at=c(0,1),labels=c('absent(0)','present(1)'),tick=F)
mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.9))
par(mfrow=c(1,1))

# Loi binomiale
par(mfrow=c(1,3), lend=3)
plot(0:50, dbinom(0:50,50,.1), type='h', lwd=1, xlab='#galumna', ylab='probability')
mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.1*","~~italic(n) == 50))
plot(0:50, dbinom(0:50,50,.5), type='h', lwd=1, xlab='#galumna', ylab='probability')
mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.5*","~~italic(n) == 50))
plot(0:50, dbinom(0:50,50,.9), type='h', lwd=1, xlab='#galumna', ylab='probability')
mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.9*","~~italic(n) == 50))
par(mfrow=c(1,1))

# Loi de Poisson
par(mfrow=c(1,3), lend=3)
plot(0:50, dpois(0:50,1), type='h', lwd=1, xlab='#galumna', ylab='probability')
mtext(side=3,line=.5, cex=0.7, expression(lambda==1))
plot(0:50, dpois(0:50,10), type='h', lwd=1, xlab='#galumna', ylab='probability')
mtext(side=3,line=.5, cex=0.7, expression(lambda==10))
plot(0:50, dpois(0:50,30), type='h', lwd=1, xlab='#galumna', ylab='probability')
mtext(side=3,line=.5, cex=0.7, expression(lambda==30))
par(mfrow=c(1,1))

#Code pour illustrer le GLM de Poisson
glm.pois <- glm(Galumna ~ WatrCont, data = mites, family='poisson')
plot.poiss<-function(x,mymodel,mult=1,mycol='LightSalmon') {
 yvar<-mymodel$model[,1]
 xvar<-mymodel$model[,2]
 lambd<-mymodel$fitted[x]
 stick.val<-rep(xvar[x],9)+mult*dpois(0:8,lambd=lambd)
 segments(rep(xvar[x],9),0:8,stick.val,0:8,col=mycol,lwd=3)
}
plot(Galumna ~ WatrCont, data = mites,cex.axis=0.7,cex.lab=0.7)
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
text(x = mites$WatrCont[8]+50,y=7.5,expression(lambda == 1.7),cex=0.7,col='red')
text(x = mites$WatrCont[11]+50,y=7.5,expression(lambda == 4.7),cex=0.7,col='red')
text(x = mites$WatrCont[36]+50,y=7.5,expression(lambda == 0.5),cex=0.7,col='red')
text(x = mites$WatrCont[52]+50,y=7.5,expression(lambda == 0.1),cex=0.7,col='red')


#### Un GLM avec une variable réponse binaire ####

## Utilisation inappropriée d'un modèle linéaire ##
## avec une variable réponse binaire ##
model.lm <- lm(pa ~ WatrCont + Topo, data = mites)
fitted(model.lm)
# La fonction "fitted()" donne les valeurs prédites pour la variable réponse.
# Certaines valeurs n'ont aucun sens, car elles sont inférieures à zéro.
# Pour éviter cela, on refait le modèle avec une distribution de Bernoulli.
# Remarquez l'argument "family" qui spécifie la distribution.
# Dans ce cas-ci, on choisit 'binomial', même si la distribution qui nous intéresse
# est celle de Bernoulli. Rappelez-vous qu'une distribution de Bernoulli
# est un cas spécifique de la distribution binomiale lorsque N = 1.
model.glm <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
fitted(model.glm)
# Toutes les valeurs sont comprises entre 0 et 1.


## Le concept du prédicteur linéaire ##
# On charge le jeu de données CO2 utilisé dans un atelier précédent
data(CO2)
head(CO2)
# Construisez un modèle linéaire de l'absorption du CO2 en fonction de la concentration de CO2 ambiant.
model.CO2 <- lm(uptake ~ conc, data = CO2)
# Extrayez la matrice du modèle avec la fonction model.matrix()
X <- model.matrix(model.CO2)
# ainsi que les coefficients estimés du modèle.
B <- model.CO2$coefficients
# On multiplie les deux matrices X et B matrices afin d'obtenir le prédicteur linéaire.
# Le symbole %*% signifie qu'on effectue un produit matriciel.
XB <- X %*% B
# Comparez les valeurs de XB à celles obtenues avec la fonction predict().
# Toutes les déclarations devraient être vraies.
# On utilise la fonction round() pour que tous les éléments aient 5 décimales.
round(fitted(model.CO2), digits = 5) == round(XB, digits = 5)


## Un exemple simple de régression logistique ##
# Pour cet exemple, nous allons créer une régression logistique
# de la présence/absence d'une espèce de mite (Galumna sp.) 
# en fonction du contenu en eau et de la topographie.
# Pour ce faire, on utilise la fonction glm() et on spécifie l'argument famille.
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial(link = "logit"))
# La fonction de lien logit est utilisé par défaut avec une distribution de Bernoulli, 
# donc il n'est pas nécessaire de l'inclure dans l'argument "family" :
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
summary(logit.reg)


## Interpréter la sortie d'une régression logistique ##
# Obtenir la cote (odds) pour la pente.
# On utilise la fonction exp() pour mettre les coefficients sur l'échelle des cotes.
# Mathématiquement, ça correspond à :
# exp(coefficients du modèle) = exp(log(μ / (1 - μ)) = u / (1 - μ)
# C'est bien la cote qu'on cherche à obtenir !
exp(logit.reg$coefficients[2:3])
#  WatrCont    TopoHummock 
#  0.9843118   8.0910340
# Pour obtenir les intervalles de confiance sur l'échelle de la cote :
exp(confint(logit.reg)[2:3,])
#               2.5 %      97.5 %
#  WatrCont     0.9741887  0.9919435
#  TopoHummock  2.0460547  38.6419693


## Un peu de mathématique sur la fonction logit inverse ##
# On commence avec la cote de la topographie de notre modèle logit.reg :
# µ/ (1 - µ) = 8.09
# On réarrange pour isoler µ :
# µ = 8.09(1 - µ) = 8.09 - 8.09µ
# 8.09µ + µ = 8.09
# µ(8.09 + 1) = 8.09
# µ = 8.09 / (8.09 + 1)
# µ = 1 / (1 + (1 / 8.09)) = 0.89
# On obtient le même résultat sans utiliser la fonction exp() !


## Comment calculer un pseudo-R2 d'un GLM ##
# On a besoin des déviances nulle et résiduelle.
# Ces quantités sont déjà enregistrées dans notre modèle.
objects(logit.reg)
pseudoR2 <- (logit.reg$null.deviance - logit.reg$deviance) / logit.reg$null.deviance
pseudoR2
# [1]  0.4655937


## Le coefficient de discrimination et sa visualisation ##
install.packages("binomTools")
library("binomTools")
# La fonction Rsq calcule plusieurs indices d'ajustement,
# incluant le coefficient de discrimination.
# Pour plus d'information sur les autres indices, consultez Tjur (2009).
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
# Les histogrammes montrent les valeurs attendues
# lorsque la variable est observée et non observée.
# Idéalement, le chevauchement entre les deux histogrammes doit être minimal.
plot(fit, which = "hist")


## Le test de Hosmer-Lemeshow ##
fit <- Rsq(object = logit.reg)
HLtest(object = fit)
# La valeur de p est de 0.9051814. Donc, on ne rejète pas notre modèle.
# On peut le considérer approprié pour nos données.


## Représenter graphiquement les résultats de la régression logistique ##
library(ggplot2)
ggplot(mites, aes(x = WatrCont, y = pa)) + geom_point() + 
stat_smooth(method = "glm", family= "binomial", se = FALSE) + xlab("Contenu en eau") +
ylab("Probabilité de présence") + 
ggtitle("Probabilité de présence de Galumna sp. en fonction du contenu en eau")


## Un exemple avec des données de proportions ##
# On génère des données basées sur l'exemple du cerf :
# On choisit un nombre aléatoire entre 1 et 10 pour déterminer le nombre de cerfs infectés.
# Dix cerfs sont échantillonnés dans chacune des dix populations.
# 're.avail' est un indice pour caractériser la qualité de l'habitat.
set.seed(123)
n.infected <- sample(x = 1:10, size = 10, replace = TRUE)
n.total <- rep(x = 10, times = 10)
res.avail <- rnorm(n = 10, mean = 10, sd = 1)
# Ensuite, on construit le modèle. Remarquez comment les proportions sont spécifées.
# On doit indiquer le nombre de cas où l'infection a été détectée
# et le nombre de cas où l'infection n'a pas été détectée.
prop.reg <- glm(cbind(n.infected, n.total - n.infected) ~ res.avail, family = binomial)
summary(prop.reg)
# Si vous avez déjà des données de proportions, voici comment faire :
# Créons tout d'abord un vecteur de proportions.
prop.infected <- n.infected / n.total
# On spécfie l'argument "weights" de la fonction glm()
# pour indiquer le nombre total d'individus échntillonnés par site.
prop.reg2 <- glm(prop.infected ~ res.avail, family = binomial, weights = n.total)
summary(prop.reg2)
# Les résumés (i.e. summary) des modèles prop.reg et prop.reg2 sont identiques !



#### Un GLM avec des données d'occurrences ####

# Charger le jeu de données "faramea"
faramea <- read.csv('faramea.csv', header = TRUE) 

# Regardons la structure du jeu de données 
str(faramea)

# Histogramme du nombre de Faramea occidentalis
# et graphique du nombre en fonction de l'altitude
par(mfrow=c(1,2))
hist(faramea$Faramea.occidentalis, breaks=seq(0,45,1), xlab=expression(paste("Number of ", italic(Faramea~occidentalis))), ylab="Frequency", main="", col="grey")
plot(faramea$Elevation, faramea$Faramea.occidentalis, xlab="Elevation (m)", ylab=expression(paste("Number of", "  ", italic(Faramea~occidentalis))), pch=1, col="black") 

# Un GLM avec distribution de Poisson (une simple régression de Poisson) semble un bon choix
# pour modéliser le nombre de Faramea occidentalis en fonction de l'altitude
glm.poisson <- glm(Faramea.occidentalis~Elevation, data=faramea, family=poisson) 
summary(glm.poisson)

# Les paramètres estimés du modèles peuvent être extraits ainsi :
# intercept
summary(glm.poisson)$coefficients[1,1]
# pente de l'altitude
summary(glm.poisson)$coefficients[2,1]

# Le sommaire suggère que l'altitude a un effet négatif sur l'abondance de Faramea occidentalis
# On peut aussi évaluer cette relation par une analyse de déviance
null.model <- glm(Faramea.occidentalis~1, data=faramea, family=poisson)
anova(null.model, glm.poisson, test="Chisq")
# Gain en déviance de 26.69 en incluant l'altitude dans le modèle
# Donc, 1 degré de liberté de moins. 
# La différence de déviance suit approximativement une distribution de Khi carré
# avec 1 degré de liberté
1-pchisq(26.686, 1)

# Le modèle est significatif, mais est-il approprié pour nos données ?
# Rappelez-vous que la distribution de Poisson suppose que moyenne = variance
# Une inspection des données peut nous donner une bonne idée de la valeur du modèle
mean(faramea$Faramea.occidentalis) # Abondance moyenne de F. occidentalis est de 3.88 individus par quadrat
var(faramea$Faramea.occidentalis)  # La variance est extrêmement élevée en comparaison à la moyenne

# ========== Facultatif
# Qu'est-ce que ça signifie ?
# On génère des valeurs d'abondance selon une loi de Poisson avec une moyenne de 3.88
y.sim <- dpois(0:50, mean(faramea$Faramea.occidentalis))
# Le graphique suivant décrit la distribution attendue de F. occidentalis en considérant que l'altitude n'a pas d'effet
# C'est la distribution du modèle nul - toutes les observations ont la même moyenne)
plot(0:50, y.sim*dim(faramea)[1], type = "h", xlab = expression(paste("Number of ", italic(Faramea~occidentalis))), ylab = "Frequency", main = "Poisson distribution with parameter 3.88")
# ========== Facultatif

# La déviance résiduelle du modèle est de 388.12 pour 41 degrés de liberté résiduels
# Une distribution de Poisson suppose que la déviance résiduelle est égale aux degrés de liberté résiduels
# Lorsque la déviance résiduelle est supérieure aux degrés de libertés résiduels,
# on dit que le modèle est surdispersé !

# Contrôler la surdispersion en utilisant une distribution quasi-Poisson 
glm.quasipoisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=quasipoisson) 
# ou
glm.quasipoisson = update(glm.poisson,family=quasipoisson)
summary(glm.quasipoisson)
# L'altitude n'est plus significative !
null.model <- glm(Faramea.occidentalis~1, data=faramea, family=quasipoisson)
anova(null.model, glm.quasipoisson, test="Chisq")
# Ce modèle est probablement mieux, mais il reste quand même pas mal de surdispersion (Phi = 15.969 >> 1)

# Lorsque la surdispersion est très élevée (i.e. Phi>15-20), une distribution négative binomiale est plus appropriée.
# La distribution négative binomiale n'est pas disponible dans la fonction glm().
# Il faut installer et charger le package MASS :
install.packages("MASS") 
library("MASS")
# Un GLM avec une distribution négative binomiale
glm.negbin <- glm.nb(Faramea.occidentalis~Elevation, data=faramea) 
summary(glm.negbin)
# k = 0.259 (le paramètre de surdispersion) et l'altitude est significative.

# Les coefficients sont : 
# intercept
summary(glm.negbin)$coefficients[1,1]
# pente de l'altitude
summary(glm.negbin)$coefficients[2,1]

# Représentation graphique du modèle
plot(faramea$Elevation, faramea$Faramea.occidentalis, xlab="Elevation (m)", ylab=expression(paste("Number of", "  ", italic(Faramea~occidentalis))), pch=16, col=rgb(4,139,154,150,maxColorValue=255))
# On ajoute la pente et l'intercepte du modèle avec la fonction exponentielle (car on a utilisé un lien log)
curve(exp(summary(glm.negbin)$coefficients[1,1]+summary(glm.negbin)$coefficients[2,1]*x),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T, lwd=2, col="orangered")
# On ajoute les intervalles de confiance en se basant sur les erreurs-types du sommaire
curve(exp(summary(glm.negbin)$coefficients[1,1]+1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x+1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")
curve(exp(summary(glm.negbin)$coefficients[1,1]-1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x-1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")

#### Défi 
# Avec lele jeu de données mites
mites <- read.csv("mites.csv", header = TRUE) 
# On inspecte les données
par(mfrow=c(2,2))
hist(mites$Galumna, breaks=c(0:10), xlab=expression(paste("Number of ", italic(Galumna~sp))), ylab="Frequency", main="", col="grey")
plot(mites$SubsDens, mites$Galumna, xlab="Substrate density (g/L)", ylab=expression(paste("Number of", "  ", italic(Galumna~sp))), pch=1, col="black")  
plot(mites$WatrCont, mites$Galumna, xlab="Water content of the substrate (g/L)", ylab=expression(paste("Number of", "  ", italic(Galumna~sp))), pch=1, col="black") 

# GLM avec distribution de Poisson
glm.p = glm(Galumna~WatrCont+SubsDens, data=mites, family=poisson) 
summary(glm.p)

# GLM avec quasi-Poisson
glm.qp = update(glm.p,family=quasipoisson)
summary(glm.qp)

# Sélection de modèle
drop1(glm.qp, test = "Chi")
glm.qp2 = glm(Galumna~WatrCont, data=mites, family=quasipoisson)
anova(glm.qp2, glm.qp, test="Chisq")



#### GLMM ####

# Chargez et affichez le jeu de données Banta.
# Assurez-vous que votre fichier csv se trouve au bon endroit.
# Spécifiez correctement votre répertoire de travail.
setwd("")
dat.tf <- read.csv("Banta_TotalFruits.csv")
str(dat.tf)
# 'data.frame':  625 obs. of  9 variables:
# $ X           : int  1 2 3 4 5 6 7 8 9 10 ...
# $ reg         : Factor w/ 3 levels "NL","SP","SW": 1 1 1 1 1 1 1 1 1 1 ...
# $ popu        : Factor w/ 9 levels "1.SP","1.SW",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ gen         : int  4 4 4 4 4 4 4 4 4 5 ...
# $ rack        : int  2 1 1 2 2 2 2 1 2 1 ...
# $ nutrient    : int  1 1 1 1 8 1 1 1 8 1 ...
# $ amd         : Factor w/ 2 levels "clipped","unclipped": 1 1 1 1 1 2 1 1 2 1 ...
# $ status      : Factor w/ 3 levels "Normal","Petri.Plate",..: 3 2 1 1 3 2 1 1 1 2 ...
# $ total.fruits: int  0 0 0 0 0 0 0 3 2 0 ...

# 2-3 génotypes nichés dans chacune des neuf populations
table(dat.tf$popu,dat.tf$gen)

# On change les nombres entiers en facteurs,
# on rajuster les niveaux d'herbivorie (amd) et  
# on renomme les niveaux de nutriments
dat.tf <- transform(dat.tf,
                    X=factor(X),
                    gen=factor(gen),
                    rack=factor(rack),
                    amd=factor(amd,levels=c("unclipped","clipped")),
                    nutrient=factor(nutrient,label=c("Low","High")))

# Installer / charger les librairies
if(!require(lme4)){install.packages("lme4")}
require(lme4)
if(!require(coefplot2)){install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R",type="source")}
require(coefplot2)     
if(!require(reshape)){install.packages("reshape")}
require(reshape) 
if(!require(ggplot2)){install.packages("ggplot2")}
require(ggplot2)
if(!require(plyr)){install.packages("plyr")}
require(plyr)
if(!require(gridExtra)){install.packages("gridExtra")}
require(gridExtra)
if(!require(emdbook)){install.packages("emdbook")}
require(emdbook)
if(!require(bbmle)){install.packages("bbmle")}
require(bbmle)
source("glmm_funs.R")

# La structure dans le jeu de données: Variable réponse vs effets fixes
ggplot(dat.tf,aes(x=amd,y=log(total.fruits+1),colour=nutrient)) +
  geom_point() +
  ## nous devons utiliser as.numeric(amd) pour obtenir des lignes
  stat_summary(aes(x=as.numeric(amd)),fun.y=mean,geom="line") +
  theme_bw() + theme(panel.margin=unit(0,"lines")) +
  scale_color_manual(values=c("#3B9AB2","#F21A00")) + # de la palette Wes Anderson Zissou
  facet_wrap(~popu)

ggplot(dat.tf,aes(x=amd,y=log(total.fruits+1),colour=nutrient)) +
  geom_point() +
  ## nous devons utiliser as.numeric(amd) pour obtenir des lignes
  stat_summary(aes(x=as.numeric(amd)),fun.y=mean,geom="line") +
  theme_bw() + theme(panel.margin=unit(0,"lines")) +
  scale_color_manual(values=c("#3B9AB2","#F21A00")) + # de la palette Wes Anderson Zissou
  facet_wrap(~gen)

# Visualiser l'hétérogénéité entre les groupes

# Créez de nouvelles variables qui représentent toutes les combinaisons de 
# nutriments x facteur clipping x facteur aléatoire
dat.tf <- within(dat.tf,
{
  # génotype x nutriments x clipping
  gna <- interaction(gen,nutrient,amd)
  gna <- reorder(gna, total.fruits, mean)
  # population x nutriments x clipping
  pna <- interaction(popu,nutrient,amd)
  pna <- reorder(pna, total.fruits, mean)
})

# Boîte à moustaches du total des fruits vs. nouvelle variable (génotype x nutriments x clipping)
ggplot(data = dat.tf, aes(factor(x = gna),y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21, outlier.colour = "skyblue2") + 
  theme_bw() + theme(axis.text.x=element_text(angle=90)) + 
  stat_summary(fun.y=mean, geom="point", colour = "red") 

# Boîte à moustaches du total des fruits vs. nouvelle variable (population x nutriments x clipping)
ggplot(data = dat.tf, aes(factor(x = pna),y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21, outlier.colour = "skyblue2") + 
  theme_bw() + theme(axis.text.x=element_text(angle=90)) + 
  stat_summary(fun.y=mean, geom="point", colour = "red") 


# Importante hétérogénéité parmi la variance de chaque groupe, même lorsque la variable réponse 
# est transformée
# Par exemple, entre les niveaux du facteur génotype
grpVars <- tapply(dat.tf$total.fruits, dat.tf$gna, var)
summary(grpVars)

grpMeans <- tapply(dat.tf$total.fruits,dat.tf$gna, mean)
summary(grpMeans)

# Quasi-Poisson
lm1 <- lm(grpVars~grpMeans-1) 
phi.fit <- coef(lm1)
# Le -1 spécifie un modèle avec les intercepts fixés à zéro

# Binomiale négative
lm2 <- lm(grpVars ~ I(grpMeans^2) + offset(grpMeans)-1)
k.fit <- 1/coef(lm2)
# L'argument offset() est utilisé pour spécifier que nous voulons
# que la moyenne du groupe soit fixée à 1

# Ajustement loess non paramétrique
Lfit <- loess(grpVars~grpMeans)

plot(grpVars ~ grpMeans, xlab="group means", ylab="group variances" )
abline(a=0,b=1, lty=2)
text(105,500,"Poisson")
curve(phi.fit*x, col=2,add=TRUE)
# bquote() est utilisé pour remplacer les valeurs numériques dans les équations avec les symboles
text(110,3900,
     bquote(paste("QP: ",sigma^2==.(round(phi.fit,1))*mu)),col=2)
curve(x*(1+x/k.fit),col=4,add=TRUE)
text(104,7200,paste("NB: k=",round(k.fit,1),sep=""),col=4)
mvec <- 0:120
lines(mvec,predict(Lfit,mvec),col=5)
text(118,2000,"loess",col=5)

# Même graphique mais avec ggplot
ggplot(data.frame(grpMeans,grpVars),
       aes(x=grpMeans,y=grpVars)) + geom_point() +
  geom_smooth(colour="blue",fill="blue") +
  geom_smooth(method="lm",formula=y~x-1,colour="red",fill="red") +
  geom_smooth(method="lm",formula=y~I(x^2)+offset(x)-1,
              colour="purple",fill="purple")

### GLMM avec distribution de Poisson ####

# On commence avec un modèle comprenant
# tous les effets fixes et des intercepts aléatoires pour popu et gen
mp1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
               (1|popu)+
               (1|gen),
             data=dat.tf, family="poisson")

# Surdispersion?
overdisp_fun(mp1)

# Ou comme ci-dessus, on peut l’estimer en divisant la déviance résiduelle par les degrés de liberté des 
# résidus
summary(mp1)
# déviance résiduelle = 18253.7 and dl resid = 616
mp1.df.resid  <- as.numeric(summary(mp1)$AICtab["df.resid"])
deviance(mp1)/mp1.df.resid

### GLMM avec distribution binomiale négative ####
# Note : Ce modèle converge si vous utilisez la version 3.0.2 de R, mais peut ne pas converger avec des 
# versions plus récentes. Si vous avez des problèmes de convergence, svp essayer le code suivant avec la
# version 3.0.2.
mnb1 <- glmer.nb(total.fruits ~ nutrient*amd + rack + status + 
                   (1|popu)+
                   (1|gen),
                 data=dat.tf, control=glmerControl(optimizer="bobyqa"))

# Surdispersion?
overdisp_fun(mnb1)
# Beaucoup mieux mais la valeur p est encore inférieure à 0,05

#### GLMM avec distribution "Poisson-lognormal" ####
mpl1 <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                (1|X) +
                (1|popu)+
                (1|gen),
              data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))

overdisp_fun(mpl1)
# Nous avons réussi! Le rapport entre la déviance et les degrés de liberté est maintenant 
# conforme avec notre critère (en fait, il est plus petit 1).

# Évaluaer les interceptes aléatoires
summary(mpl1)$varcor

# popu seulement
mpl1.popu <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                     (1|X) +
                     (1|popu), 
                   data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))

# gen seulement
mpl1.gen <-glmer(total.fruits ~ nutrient*amd + rack + status + 
                   (1|X) +
                   (1|gen), 
                 data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))

# Approche AICc
ICtab(mpl1, mpl1.popu, mpl1.gen, type = c("AICc"))

# Approche fréquentiste (Likelihood Ratio Test)
anova(mpl1,mpl1.popu)
anova(mpl1,mpl1.gen)

# Graphiques de diagnostic
locscaleplot(mpl1,col=ifelse(dat.tf$total.fruits==0,"blue","black"))

# Représentation graphique des paramètres du modèle
# Paramètres de la variance
coefplot2(mpl1,ptype="vcov",intercept=TRUE,main="Random effect variance")
# Effets fixes
coefplot2(mpl1,intercept=TRUE,main="Fixed effect coefficient")

# Représentation graphique des intercepts aléatoires
pp <- list(layout.widths=list(left.padding=0, right.padding=0),
           layout.heights=list(top.padding=0, bottom.padding=0))
r2 <- ranef(mpl1,condVar=TRUE)
d2 <- dotplot(r2, par.settings=pp)
grid.arrange(d2$gen,d2$popu,nrow=1)

# Pentes aléatoires
# Poisson-lognormal avec pentes aléatoires pour popu et amd
mpl2 <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                (1|X) +
                (amd|popu) +
                (amd|gen),
              data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))

# Examiner les coefficients de variance
summary(mpl2) # option 1
attr(VarCorr(mpl2)$gen,"correlation") # option 2
printvc(mpl2) # option 3

# Modèle final - Évaluation des effets fixes

# Approche AICc
mpl2 <- update(mpl1, . ~ . - rack) # modèle sans rack
mpl3 <- update(mpl1, . ~ . - status) # modèle sans status
mpl4 <- update(mpl1, . ~ . - amd:nutrient) # modèle sans l’interaction amd:nutrient 
ICtab(mpl1,mpl2,mpl3,mpl4, type = c("AICc"))

# Approche drop1
(dd_LRT <- drop1(mpl1,test="Chisq"))
(dd_AIC <- dfun(drop1(mpl1)))

# Supprimer l'interaction entre clipping et nutriments
mpl2 <- update(mpl1, . ~ . - amd:nutrient)

# Avec AICc:
mpl3 <- update(mpl2, . ~ . - rack) # modèle sans rack ou l’interaction
mpl4 <- update(mpl2, . ~ . - status) # modèle sans status ou l’interaction
mpl5 <- update(mpl2, . ~ . - nutrient) # modèle sans nutrient ou l’interaction
mpl6 <- update(mpl2, . ~ . - amd) # modèle sans clipping ou l’interaction
ICtab(mpl2,mpl3,mpl4,mpl5,mpl6, type = c("AICc"))

# Ou, avec drop1:
(dd_LRT <- drop1(mpl2,test="Chisq"))
(dd_AIC <- dfun(drop1(mpl2)))

summary(mpl2)

### GLMM Défi: Solution ####

# Effet du type d'alimentation et de la température sur PLD
# Charger les données
inverts <- read.csv("inverts.csv")
str(inverts)

mlm1 <- lm(PLD ~ temp*feeding.type, data=inverts)
anova(mlm1) # toutes les variables sont significatives

# Relations entre les taxons 

# Réponse vs effets fixes
ggplot(inverts,aes(x=temp,y=log(PLD+1),colour=feeding.type)) +
  geom_point() +
  stat_summary(aes(x=as.numeric(temp)),fun.y=mean,geom="line") +
  theme_bw() +
  scale_color_manual(values=c("#3B9AB2","#F21A00")) + # palette Wes Anderson Zissou
  facet_wrap(~taxon)

# Créer de nouvelles variables qui représentent toutes les combinaisons de feeding type x temp x taxa 
# (effets aléatoires)
inverts <- within(inverts,
{
  # taxon x feeding.type
  tft <- interaction(taxon,feeding.type,temp)
  tft <- reorder(tft, PLD, mean)
})

# Boîte à moustaches total des fruits vs nouvelle variable (feeding type x temp x taxa)
ggplot(data = inverts, aes(factor(x = tft),y = log(PLD))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21, outlier.colour = "skyblue2") + 
  theme_bw() + theme(axis.text.x=element_text(angle=90)) + 
  stat_summary(fun.y=mean, geom="point", colour = "red") 

# Comparaison des familles de distribution 

grpVars <- tapply(inverts$PLD, inverts$tft, var)
summary(grpVars)

grpMeans <- tapply(inverts$PLD,inverts$tft, mean)
summary(grpMeans)

# Quasi-Poisson
lm1 <- lm(grpVars~grpMeans-1) 
phi.fit <- coef(lm1)
# Le -1 spécifie un modèle avec l'intercept fixé à zéro

# Binomiale négative
lm2 <- lm(grpVars ~ I(grpMeans^2) + offset(grpMeans)-1)
k.fit <- 1/coef(lm2)
# Offset () est utilisé pour fixer la moyenne de chaque groupe à 1

# Ajustement Loess non-paramétrique
Lfit <- loess(grpVars~grpMeans)

plot(grpVars ~ grpMeans, xlab="group means", ylab="group variances" )
abline(a=0,b=1, lty=2)
text(60,200,"Poisson")
curve(phi.fit*x, col=2,add=TRUE)
# bquote()est utilisé pour remplacer des valeurs numériques dans les équations avec des symboles
text(60,800, bquote(paste("QP: ",sigma^2==.(round(phi.fit,1))*mu)),col=2)
curve(x*(1+x/k.fit),col=4,add=TRUE)
text(60,1600,paste("NB: k=",round(k.fit,1),sep=""),col=4)
mvec <- 0:120
lines(mvec,predict(Lfit,mvec),col=5)
text(50,1300,"loess",col=5)

# Poisson GLMM 
mp1 <- glmer(PLD ~ temp*feeding.type + (1|taxon), data=inverts, family="poisson")
overdisp_fun(mp1)
# rapport est significativement > 1

# NB GLMM
mnb1 <- glmer.nb(PLD ~ temp*feeding.type + (1|taxon), data=inverts)
overdisp_fun(mnb1)
# Semble bon!

# Ré-évaluer les effets aléatoires et fixes

# Ré-évaluer les intercepts aléatoires
summary(mnb1)$varcor

mnb1.taxless <- glm.nb(PLD ~ temp*feeding.type, data=inverts)

# Ici, parce que nous comparons un glmer avec un glm, nous devons faire quelque chose de différent que 
# anova(). Pour tester l'importance de l’intercept aléatoire, nous allons comparer la vraisemblance de 
# chaque modèle :
NL1 <- -logLik(mnb1)
NL0 <- -logLik(mnb1.taxless)
devdiff <- 2*(NL0-NL1)
dfdiff <- attr(NL1,"df")-attr(NL0,"df")
pchisq(devdiff,dfdiff,lower.tail=FALSE)

# Nous pourrions aussi comparer l'AIC du modèle avec (mnb1) et sans (mnb1.taxless) effets aléatoires avec 
# la fonction AICtab() 
AICtab(mnb1,mnb1.taxless) 
# Changement important du AIC si nous supprimons l’intercept aléatoire. Donc, ça vaut la peine de garder 
# cet effet.

# Graphique diagnostic 
locscaleplot(mnb1)

# Graphique des paramètres de variance
coefplot2(mnb1,ptype="vcov",intercept=TRUE,main="Random effect variance")

# Graphique des effets fixes
coefplot2(mnb1,intercept=TRUE,main="Fixed effect coefficient")


# Graphique des intercepts aléatoires 
pp <- list(layout.widths=list(left.padding=0, right.padding=0))
r2 <- ranef(mnb1,condVar=TRUE)
d2 <- dotplot(r2, par.settings=pp)
grid.arrange(d2$taxon,nrow=1)

# Évaluer pentes aléatoires
mnb2 <- glmer.nb(PLD ~ temp*feeding.type + (PLD|taxon), data=inverts)

# Examiner composante de variance-covariance
summary(mnb2) # option 1
attr(VarCorr(mnb2)$taxon,"correlation") # option 2
printvc(mnb2) # option 3
# Forte corrélation entre les effets aléatoires -> pas assez de puissance pour tester pentes aléatoires

# Ré-évaluer les effets fixes
# Remarque : pour utiliser la fonction drop1 nous devons spécifier le paramètre thêta et exécuter 
# le modèle NB avec glmer :
theta.mnb1 <- theta.md(inverts$PLD, fitted(mnb1), dfr = df.residual(mnb1))
mnb1 <- glmer(PLD ~ temp*feeding.type + (1|taxon),
              data=inverts, family=negative.binomial(theta=theta.mnb1))

(dd_LRT <- drop1(mnb1,test="Chisq"))
(dd_AIC <- dfun(drop1(mnb1)))
# Lorsque l’interaction feeding.type x température est supprimée, dAIC change de plus de 2 unités
# Ça suggère de garder l'interaction dans le modèle
