##Section: 01-preparing-for-the-workshop.R 

install.packages(c("ggplot2",
                   'MASS',
                   'vcdExtra',
                   'bbmle',
                   'DescTools')
                 )

library(ggplot2)
library(MASS)
library(vcdExtra)
library(bbmle)
library(DescTools)


##Section: 02-introduction.R 

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

# Use setwd() to set your working directory

mites <- read.csv('data/mites.csv',
                  stringsAsFactors = TRUE)

head(mites)

str(mites)

par(mfrow = c(1, 3), cex = 1.4)

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

lm.abund <- lm(Galumna ~ WatrCont, data = mites)
summary(lm.abund)

lm.pa <- lm(pa ~ WatrCont, data = mites)
summary(lm.pa)

lm.prop <- lm(prop ~ WatrCont, data = mites)

summary(lm.abund)

summary(lm.pa)

summary(lm.prop)

# Extracting the Pr(>|t|)

summary(lm.abund)$coefficients[, 4]
summary(lm.pa)$coefficients[, 4]
summary(lm.prop)$coefficients[, 4]

plot(Galumna ~ WatrCont, data = mites)
abline(lm.abund)

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

x = seq(1, 50, 0.1)
plot(x, dnorm(x, mean = 20, sd = 5),
type = 'l', lwd = 3,
xlab = '# galumna', ylab = 'Probability')
points(x, dnorm(x, mean = 25, sd = 5),
type = 'l', lwd = 3, col = 2)
points(x, dnorm(x, mean = 30, sd = 5), type = 'l', lwd = 3, col = 4)
legend('topleft', legend = c('20', '25', '30'), lty = 1, col = c(1,2,4), bty = 'n', lwd = 2, cex = 1.1)

x = seq(1, 50, 0.1)
plot(x, dnorm(x, mean = 25, sd = 5), type = 'l', lwd = 3, xlab = '# galumna', ylab = 'Probability')
points(x, dnorm(x, mean = 25, sd = 7.5), type = 'l', lwd = 3, col = 2)
points(x, dnorm(x, mean = 25, sd = 10), type = 'l', lwd = 3, col = 4)
legend('topleft', legend = c('5', '7.5', '10'), lty = 1, col = c(1,2,4), bty = 'n', lwd = 2, cex = 1.1)

coef(lm.abund)

summary(lm.abund)$sigma


##Section: 03-distributions.R 

par(cex = 2)
x = seq(1, 50, 1)
plot(x, dpois(x, lambda = 1), type = "h", lwd = 3, xlab = "Frequency of Galumna", ylab = "Probability", main = "lambda = 1")
plot(x, dpois(x, lambda = 10), type = "h", lwd = 3, xlab = "Frequency of Galumna", ylab = "Probability", main = "lambda = 10")
plot(x, dpois(x, lambda = 30), type = "h", lwd = 3, xlab = "Frequency of Galumna", ylab = "Probability", main = "lambda = 30")

mites <- read.csv('mites.csv')

hist(mites$Galumna)
mean(mites$Galumna)

hist(mites$pa)

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


##Section: 04-glm.R 

glm(formula,
    family = gaussian(link = "identity"),
    data,
    ...)


##Section: 05-glm-binary.R 

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
    family = ???,
    data,
    ...)

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

library(MASS)
data(bacteria)

# what does the data look like?
str(bacteria)

model.bact1 <- glm(y ~ trt * week, data = bacteria, family = binomial)
model.bact2 <- glm(y ~ trt + week, data = bacteria, family = binomial)
model.bact3 <- glm(y ~ week, data = bacteria, family = binomial)

# Let's compare these models using a likelihood ratio test (LRT).
anova(model.bact1, model.bact2, model.bact3, test = "LRT")

summary(logit.reg)$coefficients

logit.reg

exp(logit.reg$coefficient[2:3])

# .
objects(logit.reg)

pseudoR2 <- (logit.reg$null.deviance - logit.reg$deviance) / logit.reg$null.deviance
pseudoR2

logit.reg <- glm(pa ~ WatrCont + Topo,
                 data = mites, family = binomial(link = "logit"))
DescTools::PseudoR2(logit.reg, which = "all")

null.d <- model.bact2$null.deviance
resid.d <- model.bact2$deviance
bact.pseudoR2 <- (null.d - resid.d) / null.d
bact.pseudoR2
#REMOVED BECAUSE IT USES binomTools PACKAGE, NO LONGER AVAILABLE ON CRAN
library(binomTools)
HLtest(Rsq(model.bact2))
# Chi-square statistic:  7.812347  with  8  df
# P-value:  0.4520122
# Fit is adequate.


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


##Section: 07-glm-count.R 

faramea <- read.csv("faramea.csv", header = TRUE)

hist(faramea$Faramea.occidentalis, breaks=seq(0,45,1), xlab=expression(paste("Number of ",
italic(Faramea~occidentalis))), ylab="Frequency", main="", col="grey")

plot(faramea$Elevation, faramea$Faramea.occidentalis, ylab = 'F. occidentalis individuals', xlab = 'Elevation(m)')

glm.poisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=poisson)
summary(glm.poisson)

# intercept
summary(glm.poisson)$coefficients[1,1]
# slope of elevation
summary(glm.poisson)$coefficients[2,1]

    Null deviance: 414.81  on 42  degrees of freedom
Residual deviance: 388.12  on 41  degrees of freedom

mean(faramea$Faramea.occidentalis)
var(faramea$Faramea.occidentalis)

# Option 1, fit a new quasi-Poisson GLM
glm.quasipoisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=quasipoisson)
# Option 2, build from the previous model and update it:
glm.quasipoisson = update(glm.poisson,family=quasipoisson)
# output
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
plot(faramea$Elevation, faramea$Faramea.occidentalis, ylab = 'Number of F. occidentalis', xlab = 'Elevation(m)')
lines(pframe$pred, lwd = 2)
lines(pframe$upr, col = 2, lty = 3, lwd = 2)
lines(pframe$lwr, col = 2, lty = 3, lwd = 2)

mites <- read.csv("data/mites.csv", header = TRUE)

drop1(MyGLM, test = "Chi")

anova(MyGLM, MyGLM2, test = "Chi")

# Poisson GLM
glm.p = glm(Galumna~WatrCont+SubsDens, data=mites, family=poisson)
# quasi-Poisson GLM
glm.qp = update(glm.p,family=quasipoisson)
# model selection
drop1(glm.qp, test = "Chi")
# or
glm.qp2 = glm(Galumna~WatrCont, data=mites, family=quasipoisson)
anova(glm.qp2, glm.qp, test="Chisq")


##Section: 08-other-distributions.R 




##Section: 09-final-considerations.R 




##Section: 10-references.R 




