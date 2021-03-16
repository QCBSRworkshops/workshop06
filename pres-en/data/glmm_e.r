## QCBS R workshops
## GLMs and GLMMs
## Authors: Cédric Frenette-Dussault, Vincent Fugère, Thomas Lamy, Zofia Taranu
## Date: November 2014
## R version 3.0.2

#### Loading data ####

mites <- read.csv('data/mites.csv')

head(mites) 
str(mites)

# 70 mite communities sampled from moss cores collected at the Station de Biologie des Laurentides, QC.
# For each core/sample, the following data is provided:
# $Galumna: abundance of mite genus 'Galumna'
# $pa: presence (1) or absence (0) of Galumna, irrespective of abundance
# $totalabund: total abundance of mites of all species
# $prop: proportion of Galumna in the mite community. i.e. Galumna/totalabund
# $SubsDens: substrate density
# $WatrCont: water content
# $Substrate: type of substrate.
# $Shrub: abundance of shrubs. Coded as a factor.
# $Topo: microtopography. blanket or hummock.

#### Limitations of linear models ####

# Research question: do the abundance, occurrence (presence/absence), and
# proportion of Galumna vary as a function of the 5 environmental variables?

# Can any relationships be seen graphically?
plot(mites)

# Galumna abundance, occurrence, and proportion seems to vary with WatrCont:
par(mfrow=c(1,3)) #divide plot area in 1 row and 3 columns to have 3 plots in same figure (click zoom to see better)
plot(Galumna ~ WatrCont, data = mites, xlab = 'Water content', ylab='Abundance')
boxplot(WatrCont ~ pa, data = mites, xlab='Presence/Absence', ylab = 'Water content')
plot(prop ~ WatrCont, data = mites, xlab = 'Water content', ylab='Proportion')
par(mfrow=c(1,1)) #resets to default plot settings for future plots
# Galumna prefers dryer sites?

# Can we test that relationship statistically?
lm.abund <- lm(Galumna ~ WatrCont, data = mites)
summary(lm.abund)
lm.pa <- lm(pa ~ WatrCont, data = mites)
summary(lm.pa)
lm.prop <- lm(prop ~ WatrCont, data = mites)
summary(lm.prop)
# all significant, great! Wait a minute...

#Lets validate and plot the results of these models:
plot(Galumna ~ WatrCont, data = mites)
abline(lm.abund) #does not fit well: predicts negative abundance at WatrCont > 600, and can't predict high abundance at low WatrCont
plot(lm.abund) # not great. Data is non-normal, variance is heterogeneous.

# same problem with prop
plot(prop ~ WatrCont, data = mites)
abline(lm.prop)
plot(lm.prop)

#even worse for presence/absence...
plot(pa ~ WatrCont, data = mites)
abline(lm.pa)
plot(lm.pa)

# #Code to generate figure that shows the parameters of a normal distribution
# par(mfrow=c(1,2))
# plot(0:50, dnorm(0:50,20,5), type='l', lwd=1, xlab='#galumna', ylab='probability',cex.lab=0.8,cex.axis=0.8)
# lines(0:50, dnorm(0:50,25,5), lwd=1, col='red')
# lines(0:50, dnorm(0:50,30,5), lwd=1, col='blue')
# legend(-3,0.08,legend = c(expression(italic(mu) == 20),expression(italic(mu) == 25),expression(italic(mu) == 30)), bty='n', text.col=c('black','red','blue'), cex=0.7)
# mtext(side=3,line=.5,cex=0.7,expression("varying"~italic(mu)*","~~italic(sigma) == 5))
# plot(0:50, dnorm(0:50,25,5), type='l', lwd=1, xlab='#galumna', ylab='probability',cex.lab=0.8,cex.axis=0.8)
# lines(0:50, dnorm(0:50,25,7.5), lwd=1, col='red')
# lines(0:50, dnorm(0:50,25,10), lwd=1, col='blue')
# legend(-3,0.08,legend = c(expression(italic(sigma) == 5),expression(italic(sigma) == 7.5),expression(italic(sigma) == 10)), bty='n', text.col=c('black','red','blue'), cex=0.7)
# mtext(side=3,line=.5,cex=0.7,expression(italic(mu) ==25*","~~"varying"~italic(sigma)))

#coefficient of galumna abundance lm
coef(lm.abund)

# #Code to generate figure that illustrates assumptions of linear models
# plot.norm<-function(x,mymodel,mult=1,sd.mult=3,mycol='LightSalmon',howmany=150) {
#   yvar<-mymodel$model[,1]
#   xvar<-mymodel$model[,2]
#   sigma<-summary(mymodel)$sigma
#   stick.val<-rep(xvar[x],howmany)+mult*dnorm(seq(predict(mymodel)[x]-sd.mult*sigma, predict(mymodel)[x]+sd.mult*sigma, length=howmany), mean=predict(mymodel)[x],sd=sigma)
#   steps<-seq(predict(mymodel)[x]-sd.mult*sigma,predict(mymodel)[x]+sd.mult*sigma,length=howmany)
#   polygon(c(stick.val,rep(xvar[x],howmany)),c(sort(steps,decreasing=T),steps),col=mycol,border=NA)
# }
# #function adapted from http://www.unc.edu/courses/2010fall/ecol/563/001/notes/lecture4%20Rcode.txt
# plot(Galumna ~ WatrCont, data = mites,ylim=c(-4,8),cex.axis=1,cex.lab=1,type='n')
# plot.norm(8,lm.abund,200)
# plot.norm(11,lm.abund,200)
# plot.norm(36,lm.abund,200)
# plot.norm(52,lm.abund,200)
# abline(h=0,lty=3)
# points(Galumna ~ WatrCont, data = mites,pch=21)
# abline(lm.abund,lty=1)
# abline(v=mites$WatrCont[c(8,11,36,52)],col='red',lty=2)
# text(x = mites$WatrCont[8]+50,y=7.5,expression(mu == 1.8),cex=1,col='red')
# text(x = mites$WatrCont[11]+50,y=7.5,expression(mu == 2.6),cex=1,col='red')
# text(x = mites$WatrCont[36]+50,y=7.5,expression(mu == 0.9),cex=1,col='red')
# text(x = mites$WatrCont[52]+60,y=7.5,expression(mu == -0.1),cex=1,col='red')
# text(x = mites$WatrCont[52]+105,y=6.5,expression(sigma == 'always' ~ 1.51),cex=1,col='red')

#calculating sigma for lm.abund:
summary(lm.abund)$sigma

#### Distributions ####

# how is our data distributed?
hist(mites$Galumna) #integers not continuous variable.
mean(mites$Galumna)

hist(mites$pa) #zeros and ones
sum(mites$pa) / nrow(mites)

# #Code to generate 3 figures that illustrate bernouilli, binomial, and poisson distributions
# par(mfrow=c(1,3), lend=3)
# plot(0:1, dbinom(0:1,1,.1), xlim=c(-0.5,1.5), type='h', lwd=15, xaxt='n', xlab='p/a', ylab='probability')
# axis(1,at=c(0,1),labels=c('absent(0)','present(1)'),tick=F)
# mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.1))
# plot(0:1, dbinom(0:1,1,.5), xlim=c(-0.5,1.5), type='h', lwd=15, xaxt='n', xlab='p/a', ylab='probability')
# axis(1,at=c(0,1),labels=c('absent(0)','present(1)'),tick=F)
# mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.5))
# plot(0:1, dbinom(0:1,1,.9), xlim=c(-0.5,1.5), type='h', lwd=15, xaxt='n', xlab='p/a', ylab='probability')
# axis(1,at=c(0,1),labels=c('absent(0)','present(1)'),tick=F)
# mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.9))
# par(mfrow=c(1,1))
# # 
# par(mfrow=c(1,3), lend=3)
# plot(0:50, dbinom(0:50,50,.1), type='h', lwd=1, xlab='#galumna', ylab='probability')
# mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.1*","~~italic(n) == 50))
# plot(0:50, dbinom(0:50,50,.5), type='h', lwd=1, xlab='#galumna', ylab='probability')
# mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.5*","~~italic(n) == 50))
# plot(0:50, dbinom(0:50,50,.9), type='h', lwd=1, xlab='#galumna', ylab='probability')
# mtext(side=3,line=.5,cex=0.7,expression(italic(p) ==.9*","~~italic(n) == 50))
# par(mfrow=c(1,1))
# 
# par(mfrow=c(1,3), lend=3)
# plot(0:50, dpois(0:50,1), type='h', lwd=1, xlab='#galumna', ylab='probability')
# mtext(side=3,line=.5, cex=0.7, expression(lambda==1))
# plot(0:50, dpois(0:50,10), type='h', lwd=1, xlab='#galumna', ylab='probability')
# mtext(side=3,line=.5, cex=0.7, expression(lambda==10))
# plot(0:50, dpois(0:50,30), type='h', lwd=1, xlab='#galumna', ylab='probability')
# mtext(side=3,line=.5, cex=0.7, expression(lambda==30))
# par(mfrow=c(1,1))
# 
# #Code to generate figure that illustrates a poisson glm
# glm.pois <- glm(Galumna ~ WatrCont, data = mites, family='poisson')
# plot.poiss<-function(x,mymodel,mult=1,mycol='LightSalmon') {
#   yvar<-mymodel$model[,1]
#   xvar<-mymodel$model[,2]
#   lambd<-mymodel$fitted[x]
#   stick.val<-rep(xvar[x],9)+mult*dpois(0:8,lambd=lambd)
#   segments(rep(xvar[x],9),0:8,stick.val,0:8,col=mycol,lwd=3)
# }
# plot(Galumna ~ WatrCont, data = mites,cex.axis=0.7,cex.lab=0.7)
# points(Galumna ~ WatrCont, data = mites,pch=21)
# lines(x=seq(min(mites$WatrCont),max(mites$WatrCont),by=1),y=predict(glm.pois,newdata=data.frame('WatrCont' = seq(min(mites$WatrCont),max(mites$WatrCont),by=1)),type='response'))
# par(lend=3)
# plot.poiss(8,glm.pois,200)
# abline(v=mites$WatrCont[8],col='red',lty=2)
# plot.poiss(11,glm.pois,200)
# abline(v=mites$WatrCont[11],col='red',lty=2)
# plot.poiss(36,glm.pois,200)
# abline(v=mites$WatrCont[36],col='red',lty=2)
# plot.poiss(52,glm.pois,200)
# abline(v=mites$WatrCont[52],col='red',lty=2)
# text(x = mites$WatrCont[8]+50,y=7.5,expression(lambda == 1.7),cex=0.7,col='red')
# text(x = mites$WatrCont[11]+50,y=7.5,expression(lambda == 4.7),cex=0.7,col='red')
# text(x = mites$WatrCont[36]+50,y=7.5,expression(lambda == 0.5),cex=0.7,col='red')
# text(x = mites$WatrCont[52]+50,y=7.5,expression(lambda == 0.1),cex=0.7,col='red')


#### A GLM with binary variables ####

## Inappropriate use of a linear model with a binary response variable ##
model.lm <- lm(pa ~ WatrCont + Topo, data = mites)
fitted(model.lm)
# The "fitted()" function gives us expected values for the response variable.
# Some values are lower than 0, which does not make sense for a logistic regression.
# Let’s try the same model with a binomial distribution instead.
# Notice the "family" argument to specify the distribution.
model.glm <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
fitted(model.glm)
# All values are bound between 0 and 1.


## The concept of the linear predictor ##
# Load the CO2 dataset we used in a previous workshop
data(CO2)
head(CO2)
# Build a linear model of plant CO2 uptake as a function of CO2 ambient concentration
model.CO2 <- lm(uptake ~ conc, data = CO2)
# Extract the design matrix of the model with the model.matrix() function.
X <- model.matrix(model.CO2)
# And the estimated coefficients.
B <- model.CO2$coefficients
# Let’s multiply both X and B matrices to obtain the linear predictor.
# The "%*%" symbol indicates that it is a matrix product.
XB <- X %*% B
# Compare the values of XB to the values obtained with the predict() function.
# All statements should be TRUE.
# We use the round() function so that all elements have 5 digits.
round(fitted(model.CO2), digits = 5) == round(XB, digits = 5)


## A simple example of logistic regression ##
# Let’s build a regression model of the presence/absence of a mite species (Galumna sp.) 
# as a function of water content and topography.
# To do this, we need to use the glm() function and specify the family argument.
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial(link = "logit"))
# The logit function is the default for the binomial distribution, 
# so it is not necessary to include it in the "family" argument:
logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
summary(logit.reg)


## Interpreting the output of a logistic regression ##
# Obtaining the odds of the slope.
# Use the "exp()" function to put the coefficients back on the odds scale.
# Mathematically, this line of code corresponds to:
# exp(model coefficients) = exp(log(μ / (1 - μ)) = u / (1 - μ)
# This corresponds to an odds ratio!
exp(logit.reg$coefficients[2:3])
#  WatrCont    TopoHummock 
#  0.9843118   8.0910340
# To obtain confidence intervals on the odds scale:
exp(confint(logit.reg)[2:3,])
#               2.5 %      97.5 %
#  WatrCont     0.9741887  0.9919435
#  TopoHummock  2.0460547  38.6419693


## Some extra math about the inverse logic ##
# Let's start with our odds ratio for topography in our logit.reg model:
# µ/ (1 - µ) = 8.09
# Let's rearrange this to isolate µ
# µ = 8.09(1 - µ) = 8.09 - 8.09µ
# 8.09µ + µ = 8.09
# µ(8.09 + 1) = 8.09
# µ = 8.09 / (8.09 + 1)
# µ = 1 / (1 + (1 / 8.09)) = 0.89
# We obtained the same result without using the exp() function!


## How to compute a pseudo-R2 for a GLM ##
# Residual and null deviances are already stored in the glm object.
objects(logit.reg)
pseudoR2 <- (logit.reg$null.deviance - logit.reg$deviance) / logit.reg$null.deviance
pseudoR2
# [1]  0.4655937


## The coefficient of discrimination and its visualisation ##
install.packages("binomTools")
library("binomTools")
# The Rsq function computes several fit indices,
# including the coefficient of discrimination.
# For information on the other fit indices, see Tjur (2009).
# The plot shows the distribution of expected values when the outcome is observed
# and not observed.
# Ideally, the overlap between the two histograms should be small.
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


## The Hosmer-Lemeshow test ##
fit <- Rsq(object = logit.reg)
HLtest(object = fit)
# The p value is 0.9051814. Hence, we do not reject the model.
# We can consider it as appropriate for the data.


## Plotting the results of a logistic regression ##
library(ggplot2)
ggplot(mites, aes(x = WatrCont, y = pa)) + geom_point() + 
stat_smooth(method = "glm", family= "binomial", se = FALSE) + xlab("Water content") +
ylab("Probability of presence") + 
ggtitle("Probability of presence of Galumna sp. as a function of water content")


## An example with proportion data ##
# Let’s generate some data based on the deer example:
# We randomly choose a number between 1 and 10 for the number of infected deer.
# Ten deers were sampled in ten populations.
# Resource availability is an index to characterise the habitat.
set.seed(123)
n.infected <- sample(x = 1:10, size = 10, replace = TRUE)
n.total <- rep(x = 10, times = 10)
res.avail <- rnorm(n = 10, mean = 10, sd = 1)
# Next, let’s build the model. Notice how the proportion data is specified.
# We have to specify the number of cases where disease was detected
# and the number of cases where the disease was not detected.
prop.reg <- glm(cbind(n.infected, n.total - n.infected) ~ res.avail, family = binomial)
summary(prop.reg)
# If your data is directly transformed into proportions, here is the way to do it in R:
# Let's first create a vector of proportions
prop.infected <- n.infected / n.total
# We have to specify the "weights" argument in the glm function to indicate the number of trials per site
prop.reg2 <- glm(prop.infected ~ res.avail, family = binomial, weights = n.total)
summary(prop.reg2)
# The summaries of both prop.reg and prop.reg2 are identical!


#### GLMs with count data ####

# load the faramea dataset
faramea <- read.csv('faramea.csv', header = TRUE) 

# let's look at the data 
str(faramea)

# plot the histogram of the number of Faramea occidentalis and plot its relationship with elevation 
par(mfrow=c(1,2))
hist(faramea$Faramea.occidentalis, breaks=seq(0,45,1), xlab=expression(paste("Number of ", italic(Faramea~occidentalis))), ylab="Frequency", main="", col="grey")
plot(faramea$Elevation, faramea$Faramea.occidentalis, xlab="Elevation (m)", ylab=expression(paste("Number of", "  ", italic(Faramea~occidentalis))), pch=1, col="black") 

# a Poisson GLM (a simple Poisson regression) seems to be a good choice 
# to model the number of Faramea occidentalis as a function of elevation 
glm.poisson <- glm(Faramea.occidentalis~Elevation, data=faramea, family=poisson) 
summary(glm.poisson)

# The parameter estimates of the model can also be extracted as follow
# intercept
summary(glm.poisson)$coefficients[1,1]
# slope of elevation 
summary(glm.poisson)$coefficients[2,1]

# the summary suggest that evelation has a negative effect on the abundance of Faramea occidentalis 
# this can also be evaluated based on deviance 
null.model <- glm(Faramea.occidentalis~1, data=faramea, family=poisson)
anova(null.model, glm.poisson, test="Chisq")
# gain 26.69 deviance by including elevation at the cost of 1 df 
# the difference in deviance approximately follows a Chi-square distribution
# with 1 degree of freedom
1-pchisq(26.686, 1)

# Though significant, does the model provide a good fit to the data?
# remember that the Poisson distribution assumes mean = variance
# an inspection of the data can provide a good idea whether the model will correclty fit the data
mean(faramea$Faramea.occidentalis) # mean abundance of F. occidentalis is 3.88 individuals per quadrat 
var(faramea$Faramea.occidentalis)  # Variance is the data is extremly high compare to its mean 

# ========== Facultative
# what does it mean?
# let's generate values of abundance according to a Poisson distribution of mean 3.88 
y.sim <- dpois(0:50, mean(faramea$Faramea.occidentalis))
# the following plot describes the expected distribution of F. occidentalis given that elevation has no effect
# this is the distribution of the null model - all observations have the same mean)
plot(0:50, y.sim*dim(faramea)[1], type = "h", xlab = expression(paste("Number of ", italic(Faramea~occidentalis))), ylab = "Frequency", main = "Poisson distribution with parameter 3.88")
# ========== Facultative

# In addition, the residual deviance is 388.12 for 41 degrees of freedom
# A Poisson distribution assumes residual deviance equals residual degrees of freedom
# When residual deviance is much greater than residual degrees of freedom we say
# the model is overdispersed!

# Controling for overdispersion using a quasi-Poisson distribution
# fit a quasi-Poisson GLM to the data 
glm.quasipoisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=quasipoisson) 
# or
glm.quasipoisson = update(glm.poisson,family=quasipoisson)
summary(glm.quasipoisson)
# Elevation is no longer significant!
null.model <- glm(Faramea.occidentalis~1, data=faramea, family=quasipoisson)
anova(null.model, glm.quasipoisson, test="Chisq")
# This model is probably better, but that's a lot of overdispersion (Phi = 15.969 >> 1)

# In cases of extreme overdispersion (i.e. Phi>15-20) negative binomial works better
# NB is not in the glm() function so you need to install and charge the MASS library.
# You do not remember if you already installed this package? No problem, you can use the following function:
ifelse(length(which(installed.packages() == "MASS")) == 0,
      {print("MASS not installed. Installing... "); install.packages("MASS")},
      print("MASS already installed"))
# Alternatively, if you know that this package is not installed you can directly use the command
install.packages("MASS") 
library("MASS")
# fit a negative binomial GLM to the data
glm.negbin = glm.nb(Faramea.occidentalis~Elevation, data=faramea) 
summary(glm.negbin)
# k = 0.259, and Elevation is significant

# our coefficient are 
# intecetp
summary(glm.negbin)$coefficients[1,1]
# slope of elevation
summary(glm.negbin)$coefficients[2,1]

# plot the model
# we plot the data
plot(faramea$Elevation, faramea$Faramea.occidentalis, xlab="Elevation (m)", ylab=expression(paste("Number of", "  ", italic(Faramea~occidentalis))), pch=16, col=rgb(4,139,154,150,maxColorValue=255))
# use values for alpha and beta of the summary and put them in the exponential equation
curve(exp(summary(glm.negbin)$coefficients[1,1]+summary(glm.negbin)$coefficients[2,1]*x),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T, lwd=2, col="orangered")
# use the standard error of the summary to draw the confidence envelope
curve(exp(summary(glm.negbin)$coefficients[1,1]+1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x+1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")
curve(exp(summary(glm.negbin)$coefficients[1,1]-1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x-1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")

#### Challenge 
# use the mite data
# mites <- read.csv("mites.csv", header = TRUE) 
# let's look at the data 
par(mfrow=c(2,2))
hist(mites$Galumna, breaks=c(0:10), xlab=expression(paste("Number of ", italic(Galumna~sp))), ylab="Frequency", main="", col="grey")
plot(mites$SubsDens, mites$Galumna, xlab="Substrate density (g/L)", ylab=expression(paste("Number of", "  ", italic(Galumna~sp))), pch=1, col="black")  
plot(mites$WatrCont, mites$Galumna, xlab="Water content of the substrate (g/L)", ylab=expression(paste("Number of", "  ", italic(Galumna~sp))), pch=1, col="black") 

# Poisson GLM
glm.p = glm(Galumna~WatrCont+SubsDens, data=mites, family=poisson) 
summary(glm.p)

# quasi-Poisson GLM
glm.qp = update(glm.p,family=quasipoisson)
summary(glm.qp)

# model selection
drop1(glm.qp, test = "Chi")
glm.qp2 = glm(Galumna~WatrCont, data=mites, family=quasipoisson)
anova(glm.qp2, glm.qp, test="Chisq")


#### GLMM ####

# First load and view the dataset
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

# 2-3 genotypes nested within each of the 9 populations
table(dat.tf$popu,dat.tf$gen)

# Housekeeping: make integers into factors, relevel clipping (amd) and rename nutrient levels
dat.tf <- transform(dat.tf,
                    X=factor(X),
                    gen=factor(gen),
                    rack=factor(rack),
                    amd=factor(amd,levels=c("unclipped","clipped")),
                    nutrient=factor(nutrient,label=c("Low","High")))

# Install/ load packages
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

# Structure in dataset: Response vs fixed effects
ggplot(dat.tf,aes(x=amd,y=log(total.fruits+1),colour=nutrient)) +
  geom_point() +
  ## need to use as.numeric(amd) to get lines
  stat_summary(aes(x=as.numeric(amd)),fun.y=mean,geom="line") +
  theme_bw() + theme(panel.margin=unit(0,"lines")) +
  scale_color_manual(values=c("#3B9AB2","#F21A00")) + # from Wes Anderson Zissou palette
  facet_wrap(~popu)

ggplot(dat.tf,aes(x=amd,y=log(total.fruits+1),colour=nutrient)) +
  geom_point() +
  ## need to use as.numeric(amd) to get lines
  stat_summary(aes(x=as.numeric(amd)),fun.y=mean,geom="line") +
  theme_bw() + theme(panel.margin=unit(0,"lines")) +
  scale_color_manual(values=c("#3B9AB2","#F21A00")) + # from Wes Anderson Zissou palette
  facet_wrap(~gen)

# Exploring heterogeneity across groups 

# Create new variables that represents every combination nutrient x clipping x random factor
dat.tf <- within(dat.tf,
{
  # genotype x nutrient x clipping
  gna <- interaction(gen,nutrient,amd)
  gna <- reorder(gna, total.fruits, mean)
  # population x nutrient x clipping
  pna <- interaction(popu,nutrient,amd)
  pna <- reorder(pna, total.fruits, mean)
})

# Boxplot of total fruits vs new variable (genotype x nutrient x clipping)
ggplot(data = dat.tf, aes(factor(x = gna),y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21, outlier.colour = "skyblue2") + 
  theme_bw() + theme(axis.text.x=element_text(angle=90)) + 
  stat_summary(fun.y=mean, geom="point", colour = "red") 

# Boxplot of total fruits vs new variable (population x nutrient x clipping)
ggplot(data = dat.tf, aes(factor(x = pna),y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21, outlier.colour = "skyblue2") + 
  theme_bw() + theme(axis.text.x=element_text(angle=90)) + 
  stat_summary(fun.y=mean, geom="point", colour = "red") 


# Substantial variation among the sample variances on the transformed data
# For example, among genotypes:
grpVars <- tapply(dat.tf$total.fruits, dat.tf$gna, var)
summary(grpVars)

grpMeans <- tapply(dat.tf$total.fruits,dat.tf$gna, mean)
summary(grpMeans)

# Quasi-Poisson
lm1 <- lm(grpVars~grpMeans-1) 
phi.fit <- coef(lm1)
# The -1 specifies a model with the intercept set to zero

# Negative binomial
lm2 <- lm(grpVars ~ I(grpMeans^2) + offset(grpMeans)-1)
k.fit <- 1/coef(lm2)
# The offset() is used to specify that we want the group means added as a term with its coefficient fixed to 1

# Non-parametric loess fit
Lfit <- loess(grpVars~grpMeans)

plot(grpVars ~ grpMeans, xlab="group means", ylab="group variances" )
abline(a=0,b=1, lty=2)
text(105,500,"Poisson")
curve(phi.fit*x, col=2,add=TRUE)
# bquote() is used to substitute numeric values in equations with symbols
text(110,3900,
     bquote(paste("QP: ",sigma^2==.(round(phi.fit,1))*mu)),col=2)
curve(x*(1+x/k.fit),col=4,add=TRUE)
text(104,7200,paste("NB: k=",round(k.fit,1),sep=""),col=4)
mvec <- 0:120
lines(mvec,predict(Lfit,mvec),col=5)
text(118,2000,"loess",col=5)

# Same graph but with ggplot
ggplot(data.frame(grpMeans,grpVars),
       aes(x=grpMeans,y=grpVars)) + geom_point() +
  geom_smooth(colour="blue",fill="blue") +
  geom_smooth(method="lm",formula=y~x-1,colour="red",fill="red") +
  geom_smooth(method="lm",formula=y~I(x^2)+offset(x)-1,
              colour="purple",fill="purple")

### Poisson GLMM ####

# Start with all fixed effects in and random intercepts for popu and gen
mp1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
               (1|popu)+
               (1|gen),
             data=dat.tf, family="poisson")

# Overdispersion?
overdisp_fun(mp1)

# Or as above, we can approximate this by dividing the deviance and deviance df
summary(mp1)
# deviance = 18253.7 and df.resid = 616
# mp1.df.resid  <- as.numeric(summary(mp1)$AICtab["df.resid"])
# deviance(mp1)/mp1.df.resid

### NB GLMM ####
mnb1 <- glmer.nb(total.fruits ~ nutrient*amd + rack + status + 
                   (1|popu)+
                   (1|gen),
                 data=dat.tf, control=glmerControl(optimizer="bobyqa"))

# Overdispersion?
overdisp_fun(mnb1)
# Much better but still not significant!

#### Poisson-lognormal ####
mpl1 <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                (1|X) +
                (1|popu)+
                (1|gen),
              data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))

overdisp_fun(mpl1)

# Evaluating the random intercepts 
summary(mpl1)
summary(mpl1)$varcor

mpl1.popu <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                     (1|X) +
                     (1|popu), # popu only (drop gen)
                   data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))
anova(mpl1,mpl1.popu)

mpl1.gen <-glmer(total.fruits ~ nutrient*amd + rack + status + 
                   (1|X) +
                   (1|gen), # gen only (drop popu)
                 data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))
anova(mpl1,mpl1.gen)

# Diagnostic plots 
locscaleplot(mpl1,col=ifelse(dat.tf$total.fruits==0,"blue","black"))

# Plotting variance terms 
coefplot2(mpl1,ptype="vcov",intercept=TRUE,main="Random effect variance")

# Plot of fixed effects 
coefplot2(mpl1,intercept=TRUE,main="Fixed effect coefficient")

# Plotting random intercepts 
pp <- list(layout.widths=list(left.padding=0, right.padding=0),
           layout.heights=list(top.padding=0, bottom.padding=0))
r2 <- ranef(mpl1,condVar=TRUE)
d2 <- dotplot(r2, par.settings=pp)
grid.arrange(d2$gen,d2$popu,nrow=1)

# Longnormal-Poisson with random slopes for popu and amd
mpl2 <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                (1|X) +
                (amd|popu) +
                (amd|gen),
              data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))

# View variance-covariance components
summary(mpl2) # option 1
attr(VarCorr(mpl2)$gen,"correlation") # option 2
printvc(mpl2) # option 2

# Evaluating fixed effects 
(dd_LRT <- drop1(mpl1,test="Chisq"))
(dd_AIC <- dfun(drop1(mpl1)))

# Drop the interaction between clipping and nutrients
mpl2 <- update(mpl1, . ~ . - amd:nutrient)
anova(mpl1,mpl2)

(dd_LRT <- drop1(mpl2,test="Chisq"))
(dd_AIC <- dfun(drop1(mpl2)))

summary(mpl2)

### GLMM Challenge: Solution ####

inverts <- read.csv("inverts.csv")
str(inverts)

# Create new variables that represents every combination nutrient x clipping x random factor
inverts <- within(inverts,
{
  # taxon x feeding.type
  tft <- interaction(taxon,feeding.type,temp)
  tft <- reorder(tft, PLD, mean)
})

# Boxplot of total fruits vs new variable (genotype x nutrient x clipping)
ggplot(data = inverts, aes(factor(x = tft),y = log(PLD))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21, outlier.colour = "skyblue2") + 
  theme_bw() + theme(axis.text.x=element_text(angle=90)) + 
  stat_summary(fun.y=mean, geom="point", colour = "red") 
# Substantial variation among the sample variances on the transformed data

# For example, among genotypes:
grpVars <- tapply(inverts$PLD, inverts$tft, var)
summary(grpVars)

grpMeans <- tapply(inverts$PLD,inverts$tft, mean)
summary(grpMeans)

# Quasi-Poisson
lm1 <- lm(grpVars~grpMeans-1) 
phi.fit <- coef(lm1)
# The -1 specifies a model with the intercept set to zero

# Negative binomial
lm2 <- lm(grpVars ~ I(grpMeans^2) + offset(grpMeans)-1)
k.fit <- 1/coef(lm2)
# The offset() is used to specify that we want the group means added as a term with its coefficient fixed to 1

# Non-parametric loess fit
Lfit <- loess(grpVars~grpMeans)

plot(grpVars ~ grpMeans, xlab="group means", ylab="group variances" )
abline(a=0,b=1, lty=2)
text(60,200,"Poisson")
curve(phi.fit*x, col=2,add=TRUE)
# bquote() is used to substitute numeric values in equations with symbols
text(60,800,
     bquote(paste("QP: ",sigma^2==.(round(phi.fit,1))*mu)),col=2)
curve(x*(1+x/k.fit),col=4,add=TRUE)
text(60,1600,paste("NB: k=",round(k.fit,1),sep=""),col=4)
mvec <- 0:120
lines(mvec,predict(Lfit,mvec),col=5)
text(50,1300,"loess",col=5)

# Response vs fixed effects
ggplot(inverts,aes(x=temp,y=log(PLD+1),colour=feeding.type)) +
  geom_point() +
  stat_summary(aes(x=as.numeric(temp)),fun.y=mean,geom="line") +
  theme_bw() +
  scale_color_manual(values=c("#3B9AB2","#F21A00")) + # from Wes Anderson Zissou palette
  facet_wrap(~taxon)

# Poisson GLMM 
mp1 <- glmer(PLD ~ temp*feeding.type +
               (1|taxon),
             data=inverts, family="poisson")
overdisp_fun(mp1)

# NB GLMM
mnb1 <- glmer.nb(PLD ~ temp*feeding.type +
                   (1|taxon),
                 data=inverts)
overdisp_fun(mnb1)
# Looks good!

# Evaluating random intercepts
summary(mnb1)$varcor

mnb1.taxless <- glm.nb(PLD ~ temp*feeding.type,
                       data=inverts)
# Here, because we are comparing a glmer with a glm, we must do something different than calling anova()
# to test importance of random intercept. We will compare the likelihood of each model:
NL1 <- -logLik(mnb1)
NL0 <- -logLik(mnb1.taxless)
devdiff <- 2*(NL0-NL1)
dfdiff <- attr(NL1,"df")-attr(NL0,"df")
pchisq(devdiff,dfdiff,lower.tail=FALSE)

# Could also look at dAIC to compare model with (mnb1) and without (mnb1.taxless) random effects
# Using AICtab() function
AICtab(mnb1,mnb1.taxless) 

# Diagnostic plots 
locscaleplot(mnb1)

# Plotting variance terms 
coefplot2(mnb1,ptype="vcov",intercept=TRUE,intercept=TRUE,main="Random effect variance")

# Plot of fixed effects 
coefplot2(mnb1,intercept=TRUE,main="Fixed effect coefficient")

# Plotting random intercepts 
pp <- list(layout.widths=list(left.padding=0, right.padding=0))
r2 <- ranef(mnb1,condVar=TRUE)
d2 <- dotplot(r2, par.settings=pp)
grid.arrange(d2$taxon,nrow=1)

# Longnormal-Poisson with random slopes for popu and amd
mnb2 <- glmer.nb(PLD ~ temp*feeding.type +
                   (PLD|taxon),
                 data=inverts)
# View variance-covariance components
summary(mnb2) # option 1
attr(VarCorr(mnb2)$taxon,"correlation") # option 2
printvc(mnb2) # option 2

# Evaluating fixed effects 
# Note: to run the drop1 we need to speficy the theta parameter and run the NB model with glmer:
theta.mnb1 <- theta.md(inverts$PLD, fitted(mnb1), dfr = df.residual(mnb1))
mnb1 <- glmer(PLD ~ temp*feeding.type +
                (1|taxon),
              data=inverts, family=negative.binomial(theta=theta.mnb1))

(dd_LRT <- drop1(mnb1,test="Chisq")
(dd_AIC <- dfun(drop1(mnb1)))
# dAIC when feeding.type x temp interaction is dropped is greater than 2
 
 