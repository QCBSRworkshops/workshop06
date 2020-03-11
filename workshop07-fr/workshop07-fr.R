## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#",
  collapse = TRUE,
  cache = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width=6, fig.height=6,
  fig.retina = 3,
  fig.align = 'center'
)
mypar = list(mar = c(3,3,1,0.5), mgp = c(1.6, 0.3, 0), tck = -.02)
options(repos=structure(c(CRAN="http://cran.r-project.org")))


## ----install_pkgs, echo = FALSE, results = "asis"-----------------------------
cat(
  qcbsRworkshops::first_slides(7,
    c('ggplot2', 'lme4', 'MASS', 'vcdExtra', 'bbmle', 'DescTools'),
    lang = "fr")
)


## ----echo=FALSE---------------------------------------------------------------
mites <- read.csv('data/mites.csv')


## ----eval=F-------------------------------------------------------------------
## # vérifiez que vous êtes dans le bon répertoire de travail
## mites <- read.csv('data/mites.csv')
## head(mites)
## str(mites)


## ----echo = -1----------------------------------------------------------------
par(mypar)
plot(mites)


## ----fig.width=12,fig.height=4.5,echo=-1--------------------------------------
par(mypar)
par(mfrow = c(1, 3), cex = 1.4)
plot(Galumna ~ WatrCont, data = mites, xlab = 'Contenu en eau', ylab='Abondance')
boxplot(WatrCont ~ pa, data = mites, xlab='Présence/Absence', ylab = 'Contenu en eau')
plot(prop ~ WatrCont, data = mites, xlab = 'Contenu en eau', ylab='Proportion')


## ---- eval = -c(2, 4, 6)------------------------------------------------------
lm.abund <- lm(Galumna ~ WatrCont, data = mites)
summary(lm.abund)
lm.pa <- lm(pa ~ WatrCont, data = mites)
summary(lm.pa)
lm.prop <- lm(prop ~ WatrCont, data = mites)
summary(lm.prop)


## -----------------------------------------------------------------------------
summary(lm.abund)$coefficients[, 4]
summary(lm.abund)$coefficients[, 4]
summary(lm.abund)$coefficients[, 4]


## ----echo=-1------------------------------------------------------------------
par(mypar);par(cex = 1.4)
plot(Galumna ~ WatrCont, data = mites)
abline(lm.abund)


## ----echo=-1------------------------------------------------------------------
par(mypar)
par(mfrow = c(2, 2), cex = 1.4)
plot(lm.abund)


## ----echo=-1------------------------------------------------------------------
par(mypar);par(cex = 1.4)
plot(prop ~ WatrCont, data = mites)
abline(lm.prop)


## ----echo=-1------------------------------------------------------------------
par(mypar)
par(mfrow = c(2, 2), cex = 1.4)
plot(lm.prop)


## ----echo=-1------------------------------------------------------------------
par(mypar);par(cex = 1.4)
plot(pa ~ WatrCont, data = mites)
abline(lm.pa)


## ----echo=-1------------------------------------------------------------------
par(mypar)
par(mfrow = c(2, 2), cex = 1.4)
plot(lm.pa)


## ---- echo=FALSE--------------------------------------------------------------
x = seq(1, 50, 0.1)
par(mypar);par(cex = 1.4)
plot(x, dnorm(x, mean = 20, sd = 5), type = 'l', lwd = 3, xlab = '# galumna', ylab = 'Probabilité')
points(x, dnorm(x, mean = 25, sd = 5), type = 'l', lwd = 3, col = 2)
points(x, dnorm(x, mean = 30, sd = 5), type = 'l', lwd = 3, col = 4)
legend('topleft', legend = c('20', '25', '30'), lty = 1, col = c(1, 2, 4), bty = 'n', lwd = 2, cex = 1.1)


## ---- echo=FALSE--------------------------------------------------------------
par(mypar);par(cex = 1.4)
x = seq(1, 50, 0.1)
plot(x, dnorm(x, mean = 25, sd = 5), type = 'l', lwd = 3, xlab = '# galumna', ylab = 'Probabilité')
points(x, dnorm(x, mean = 25, sd = 7.5), type = 'l', lwd = 3, col = 2)
points(x, dnorm(x, mean = 25, sd = 10), type = 'l', lwd = 3, col = 4)
legend('topleft', legend = c('5', '7.5', '10'), lty = 1, col = c(1, 2, 4), bty = 'n', lwd = 2, cex = 1.1)


## -----------------------------------------------------------------------------
coef(lm.abund)
summary(lm.abund)$sigma


## ----echo=F,fig.width=15------------------------------------------------------
x = seq(1, 50, 1)
par(mypar);par(mfrow = c(1, 3), cex = 1.4)
plot(x, dpois(x, lambda = 1), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probabilité', main = 'lambda = 1')
plot(x, dpois(x, lambda = 10), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probabilité', main = 'lambda = 10')
plot(x, dpois(x, lambda = 30), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probabilité', main = 'lambda = 30')


## -----------------------------------------------------------------------------
  mean(mites$Galumna)
  hist(mites$Galumna)


## ---- fig.height=4, echo=-1---------------------------------------------------
  par(mypar);par(cex=1.4)
  hist(mites$pa)


## ----echo=-F,fig.width=12,fig.height=4----------------------------------------
  par(mypar);par(mfrow = c(1, 3), cex=1.4)
  barplot(setNames(c(.9, .1), c('absent (0)', 'present (1)')), ylim = c(0, 1), xlab = 'pa', ylab = 'probability', main = 'p = 0.1')
  barplot(setNames(c(.5, .5), c('absent (0)', 'present (1)')), ylim = c(0, 1), xlab = 'pa', ylab = 'probability', main = 'p = 0.5')
  barplot(setNames(c(.1, .9), c('absent (0)', 'present (1)')), ylim = c(0, 1), xlab = 'pa', ylab = 'probability', main = 'p = 0.9')


## ----echo=F,fig.width=15------------------------------------------------------
x = seq(1, 50, 1)
par(mypar);par(mfrow = c(1, 3), cex = 1.4)
plot(x, dbinom(x, size = 50, prob = 0.1), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probabilité', main = 'p = 0.1 n = 50')
plot(x, dbinom(x, size = 50, prob = 0.5), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probabilité', main = 'p = 0.5 n = 50')
plot(x, dbinom(x, size = 50, prob = 0.9), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probabilité', main = 'p = 0.9 n = 50')


## ----echo=F,fig.width=10, fig.height=4----------------------------------------
x = seq(1, 50, 1)
par(mypar);par(mfrow = c(1, 2), cex = 1.4)
plot(x, dbinom(x, size = 50, prob = 0.9), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probability', main = 'p = 0.9 n = 50')
plot(x, dpois(x, lambda = 30), type = 'h', lwd = 3, xlab = '# galumna', ylab = 'Probability', main = 'lambda = 30')


## ----echo=F-------------------------------------------------------------------
Site <- LETTERS[1:6]
Presence <- c(1, 0, 1, 1, 0, 1)
dat = data.frame(Site, Presence)


## ---- echo=F------------------------------------------------------------------
print(dat)


## ----echo=F,fig.width=7, fig.height=6-----------------------------------------
par(mypar);par(cex = 1.4)
hist(Presence)


## ----echo=F,fig.width=7.5, fig.height=5.6-------------------------------------
Pres <- c(rep(1, 40), rep(0, 40))
rnor <- function(x) rnorm(1, mean = ifelse(x == 1, 12.5, 7.5), sd = 2)
ExpVar <- sapply(Pres, rnor)
par(mypar);par(cex = 1.4)
plot(ExpVar, Pres, ylim = c(-.5, 1.5), xlab = 'Variable explicative', ylab = 'Présence', main = "Valeurs prédites d'un lm() avec une variable binaire", pch = 16)
abline(lm(Pres ~ ExpVar), col = 'orange', lwd = 2)
mtext(expression(symbol("\255")), at = 1.25, side = 4, line = 0.1, cex = 6, col = 'blue')
mtext(expression(symbol("\256")), at = 3, side = 1, line = -2.2, cex = 6, col = 'blue')


## ----eval=F-------------------------------------------------------------------
## #setwd('...')
## mites <- read.csv("mites.csv", header = TRUE)
## str(mites)
## 

## ----echo=F-------------------------------------------------------------------
mites <- read.csv("data/mites.csv", header = TRUE)
str(mites)


## -----------------------------------------------------------------------------
logit.reg <- glm(pa ~ WatrCont + Topo, data=mites,
family = binomial(link = "logit"))

## ----eval=F-------------------------------------------------------------------
## summary(logit.reg)


## -----------------------------------------------------------------------------
summary(logit.reg)


## -----------------------------------------------------------------------------
#install.packages("MASS")
library(MASS)
data(bacteria)
str(bacteria)


## -----------------------------------------------------------------------------
model.bact1 <- glm(y ~ trt * week, data = bacteria, family = binomial)


## -----------------------------------------------------------------------------
model.bact2 <- glm(y ~ trt + week, data = bacteria, family = binomial)


## -----------------------------------------------------------------------------
model.bact3 <- glm(y ~ week, data = bacteria, family = binomial)


## -----------------------------------------------------------------------------
anova(model.bact1, model.bact2, model.bact3, test = "LRT")


## -----------------------------------------------------------------------------
summary(logit.reg)$coefficients


## -----------------------------------------------------------------------------
objects(logit.reg)


## -----------------------------------------------------------------------------
pseudoR2 <- (logit.reg$null.deviance - logit.reg$deviance) / logit.reg$null.deviance
pseudoR2


## ---- echo = -1---------------------------------------------------------------
logit.reg <- glm(mites$pa ~ mites$WatrCont + mites$Topo, family = binomial(link = "logit")) # needed for the rmd presentation, or else problem with PseudoR2 environment...
library(DescTools)
fit <- PseudoR2(logit.reg, which = "all")
fit


## -----------------------------------------------------------------------------
library(vcdExtra)
HLtest(logit.reg)


## ----eval=F-------------------------------------------------------------------
## null.d <- model.bact2$null.deviance
## resid.d <- model.bact2$deviance
## bact.pseudoR2 <- (null.d - resid.d) / null.d
## HLtest(model.bact2)


## -----------------------------------------------------------------------------
prop.reg <- glm(cbind(Galumna, totalabund - Galumna) ~ Topo + WatrCont, data = mites, family = binomial)


## -----------------------------------------------------------------------------
summary(prop.reg)


## -----------------------------------------------------------------------------
prop.reg2 <- glm(prop ~ Topo + WatrCont, data = mites,
                 family = binomial, weights = totalabund)


## ----echo=F-------------------------------------------------------------------
faramea <- read.csv('data/faramea.csv', header = TRUE)

## ----eval=F-------------------------------------------------------------------
## faramea <- read.csv('faramea.csv', header = TRUE)


## ---- echo=F,fig.height=5-----------------------------------------------------
par(mypar);par(cex = 1.4)
hist(faramea$Faramea.occidentalis, breaks = 30, col = 'gray', xlab = 'Nombre de F. occidentalis', ylab = 'Fréquence', main = '')


## ---- echo=F,fig.height=5-----------------------------------------------------
par(mypar);par(cex = 1.4)
plot(faramea$Elevation, faramea$Faramea.occidentalis, ylab = 'Nombre de F. occidentalis', xlab = 'Élevation(m)')


## -----------------------------------------------------------------------------
glm.poisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=poisson)


## ----eval=F-------------------------------------------------------------------
## summary(glm.poisson)


## -----------------------------------------------------------------------------
summary(glm.poisson)


## -----------------------------------------------------------------------------
glm.quasipoisson = glm(Faramea.occidentalis ~ Elevation, data = faramea,
                       family=quasipoisson)
glm.quasipoisson = update(glm.poisson, family = quasipoisson)


## -----------------------------------------------------------------------------
summary(glm.quasipoisson)


## -----------------------------------------------------------------------------
null.model <- glm(Faramea.occidentalis ~ 1, data = faramea,
                  family = quasipoisson)
anova(null.model, glm.quasipoisson, test = "Chisq")


## -----------------------------------------------------------------------------
glm.negbin = glm.nb(Faramea.occidentalis ~ Elevation, data = faramea)


## ----echo=-1------------------------------------------------------------------
summary(glm.negbin)


## ----eval=F-------------------------------------------------------------------
## pp <- predict(glm.negbin, newdata = data.frame(Elevation = 1:800), se.fit = TRUE)
## linkinv <- family(glm.negbin)$linkinv ## inverse-link function
## pframe$pred0 <- pp$fit
## pframe$pred <- linkinv(pp$fit)
## sc <- abs(qnorm((1-0.95)/2))  ## Normal approx. to likelihood
## pframe <- transform(pframe, lwr = linkinv(pred0-sc*pp$se.fit), upr = linkinv(pred0+sc*pp$se.fit))
## # sinon, utiiser predic() avec type="response"
## 
## plot(faramea$Elevation, faramea$Faramea.occidentalis, ylab = 'Number of F. occidentalis', xlab = 'Elevation(m)')
## lines(pframe$pred, lwd = 2)
## lines(pframe$upr, col = 2, lty = 3, lwd = 2)
## lines(pframe$lwr, col = 2, lty = 3, lwd = 2)


## ----echo=F, fig.height=4-----------------------------------------------------
pframe <- data.frame(Elevation = 1:800)
pp <- predict(glm.negbin, newdata = pframe, se.fit = TRUE)
linkinv <- family(glm.negbin)$linkinv ## inverse-link function

pframe$pred0 <- pp$fit
pframe$pred <- linkinv(pp$fit)
alpha <- 0.95
sc <- abs(qnorm((1-alpha)/2))  ## Normal approx. to likelihood
alpha2 <- 0.5
pframe <- transform(pframe,
                    lwr=linkinv(pred0-sc*pp$se.fit),
                    upr=linkinv(pred0+sc*pp$se.fit))

par(mypar);par(cex = 1.4)
plot(faramea$Elevation, faramea$Faramea.occidentalis,
      ylab = 'Nombre de F. occidentalis', xlab = 'Élevation(m)')
lines(pframe$pred, lwd = 2)
lines(pframe$upr, col = 2, lty = 3, lwd = 2)
lines(pframe$lwr, col = 2, lty = 3, lwd = 2)


## ----echo=F-------------------------------------------------------------------
mites <- read.csv("data/mites.csv", header = TRUE)

## ----eval=F-------------------------------------------------------------------
## mites <- read.csv("mites.csv", header = TRUE)


## -----------------------------------------------------------------------------
# GLM Poisson
glm.p = glm(Galumna~WatrCont+SubsDens, data=mites, family=poisson)
# GLM quasi-Poisson
glm.qp = update(glm.p,family=quasipoisson)
# sélection du modèle
drop1(glm.qp, test = "Chi")


## ----echo=F, fig.height=5,fig.width=6-----------------------------------------
glm.qp = glm(Galumna~WatrCont, data=mites, family=poisson)
glm.qp2 = update(glm.qp, family=quasipoisson)
pframe <- data.frame(WatrCont = 100:850)
pp <- predict(glm.qp2, newdata = pframe, se.fit = TRUE)
linkinv <- family(glm.qp2)$linkinv ## inverse-link function

pframe$pred0 <- pp$fit
pframe$pred <- linkinv(pp$fit)
alpha <- 0.95
sc <- abs(qnorm((1-alpha)/2))  ## Normal approx. to likelihood
alpha2 <- 0.5
pframe <- transform(pframe,
                    lwr=linkinv(pred0-sc*pp$se.fit),
                    upr=linkinv(pred0+sc*pp$se.fit))

par(mypar);par(cex = 1.4)
plot(mites$WatrCont, mites$Galumna,
      ylab = 'Nombre de Galumna', xlab = "Contenu d'eau du substrat (g/L)")
lines(pframe$pred, lwd = 2)
lines(pframe$upr, col = 2, lty = 3, lwd = 2)
lines(pframe$lwr, col = 2, lty = 3, lwd = 2)


## ---- echo=F------------------------------------------------------------------
dat.tf <- read.csv("data/banta_totalfruits.csv")


## ----echo=F, fig.height=5, fig.width=6----------------------------------------
par(mypar);par(cex = 1.4)
hist(dat.tf$total.fruits, breaks = 50, col = 'blue', main = '',
     xlab = 'Fruits totaux', ylab = 'Compte')


## -----------------------------------------------------------------------------
dat.tf <- within(dat.tf,
{
  # génotype x nutriment x herbivorie
  gna <- interaction(gen,nutrient,amd)
  gna <- reorder(gna, total.fruits, mean)
  # population x nutriment x herbivorie
  pna <- interaction(popu,nutrient,amd)
  pna <- reorder(pna, total.fruits, mean)
})


## ---- fig.height=3.5, fig.width=8---------------------------------------------
# Boxplot du total des fruits vs interaction génotype x nutriment x herbivorie
library(ggplot2)
ggplot(data = dat.tf, aes(factor(x = gna),y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21,
  outlier.colour = "skyblue2") +
  theme_bw() + theme(axis.text.x=element_blank()) +
  stat_summary(fun.y=mean, geom="point", colour = "red")


## -----------------------------------------------------------------------------
library(lme4)
mp1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
             (1|popu)+
             (1|gen),
             data = dat.tf, family = "poisson")


## -----------------------------------------------------------------------------
# Téléchargez le code glmm_funs.R de la page wiki et sourcez le pour exécuter la fonction dans R
source(file="data/glmm_funs.R")
# Surdispersion?
overdisp_fun(mp1)


## -----------------------------------------------------------------------------
mnb1 <- glmer.nb(total.fruits ~ nutrient*amd + rack + status +
                 (1|popu)+
                 (1|gen),
                 data=dat.tf, control=glmerControl(optimizer="bobyqa"))
# Control spécifie la façon dont nous optimisons les valeurs des paramètres


## -----------------------------------------------------------------------------
mpl1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
              (1|X) +
              (1|popu)+
              (1|gen),
data=dat.tf, family="poisson",
control = glmerControl(optimizer = "bobyqa"))


## -----------------------------------------------------------------------------
overdisp_fun(mpl1)


## ----install_coefplot2--------------------------------------------------------
if (!require("coefplot2"))
  remotes::install_github("palday/coefplot2", subdir = "pkg")
library(coefplot2)


## ---- fig.height=5, fig.width=6, echo=-1--------------------------------------
par(mypar);par(cex = 1.4)
# Paramètres de la variance
coefplot2(mpl1, ptype = "vcov", intercept = TRUE)


## ---- fig.height=5, fig.width=6,echo=-1---------------------------------------
par(mypar);par(cex = 1.4)
# Effets fixes
coefplot2(mpl1, intercept = TRUE)


## ---- echo= FALSE, fig.width=9------------------------------------------------
library(gridExtra)
library(lattice)
pp <- list(layout.widths=list(left.padding=0, right.padding=0),
           layout.heights=list(top.padding=0, bottom.padding=0))
r2 <- ranef(mpl1, condVar = TRUE)
d2 <- dotplot(r2, par.settings = pp, scales=list(x=list(cex=1.4),y=list(cex=1.3)))
grid.arrange(d2$gen, d2$popu, nrow = 1)


## -----------------------------------------------------------------------------
mpl2 <- update(mpl1, . ~ . - rack) # modèle sans rack
mpl3 <- update(mpl1, . ~ . - status) # modèle sans status
mpl4 <- update(mpl1, . ~ . - amd:nutrient) # modèle sans interaction amd:nutrient
bbmle::ICtab(mpl1, mpl2, mpl3, mpl4, type = c("AICc"))


## -----------------------------------------------------------------------------
dd_LRT <- drop1(mpl1,test="Chisq")
(dd_AIC <- dfun(drop1(mpl1)))


## -----------------------------------------------------------------------------
dd_LRT <- drop1(mpl1,test="Chisq")
(dd_AIC <- dfun(drop1(mpl1)))


## -----------------------------------------------------------------------------
mpl2 <- update(mpl1, . ~ . - and:nutrient)
# Utiliser AIC
mpl3 <- update(mpl2, . ~ . - rack) # pas de rack ou interaction
mpl4 <- update(mpl2, . ~ . - status) # pas de status ou interaction
mpl5 <- update(mpl2, . ~ . - nutrient) # pas de nutrient ou interaction
mpl6 <- update(mpl2, . ~ . - amd) # pas d'herbivorie ou interaction
# bbmle::ICtab(mpl2, mpl3, mpl4, mpl5, mpl6,
#              type = c("AICc"))

# Ou utiliser drop1
dd_LRT2 <- drop1(mpl2,test="Chisq")
dd_AIC2 <- dfun(drop1(mpl2))


## -----------------------------------------------------------------------------
library(bbmle)
ICtab(mpl2, mpl3 ,mpl4,
      mpl5, mpl6,
      type = c("AICc"))


## ---- eval = FALSE------------------------------------------------------------
## # inverts <- read.csv('data/inverts.csv', header = TRUE)
## # head(inverts)
## # table(inverts$temp, inverts$feeding.type)
## #
## # mod.glm <- glm(PLD ~ temp + feeding.type, family = poisson(), data = inverts)
## # summary(mod.glm)
## # drop1(mod.glm, test = "Chisq")
## #
## # boxplot(PLD ~ temp,  data = inverts)
## # boxplot(PLD ~ feeding.type ,  data = inverts)
## #
## # boxplot(predict(mod.glm, type = "response")~inverts$temp)
## #
## # plot()
## #
## # modglm <- glm(PLD ~ temp + feeding.type, family = poisson(), data = inverts)
## 
## 
## #
## # r2 <- ranef(mpl1, condVar = TRUE)
## # d2 <- dotplot(r2, par.settings = pp)
## #
## #
## # plot(aggregate(PLD ~ taxon, FUN=mean, data = inverts)[,2],aggregate(PLD ~ taxon, FUN=var, data = inverts)[,2], pch = 19)
## # abline(a=0, b = 1, lty =2)
## #
## # mod.glmer <- glmer.nb(PLD ~ temp + feeding.type + (1|taxon), data = inverts)
## # mod.glm <- glm.nb(PLD ~ temp + feeding.type, family = poisson(), data = inverts)
## 

