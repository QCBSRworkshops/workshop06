

#### Section: index.R 

## THIS SECTION MUST ONLY BE EDITED BY THE COORDINATORS OF THE WORKSHOP SERIES

library(knitr)
opts_chunk$set(tidy.opts = list(width.cutoff = 60),
               tidy = TRUE)


#### Section: 01-preparing-for-the-workshop.R 

## install.packages("ggplot2")
## install.packages("lme4")
## install.packages("MASS")
## install.packages("vcdExtra")
## install.packages("bbmle")
## install.packages("DescTools")
## 
## library(ggplot2)
## library(lme4)
## library(MASS)
## library(vcdExtra)
## library(bbmle)
## library(DescTools)


#### Section: 02-introduction.R 

## setwd("~/Desktop")
## mites <- read.csv('mites.csv')

## head(mites)
## str(mites)
## 
## # 70 mite communities sampled from moss cores collected at the Station de Biologie des Laurentides, QC.
## # For each core/sample, the following data is provided:
## # $Galumna: abundance of mite genus 'Galumna'
## # $pa: presence (1) or absence (0) of Galumna, irrespective of abundance
## # $totalabund: total abundance of mites of all species
## # $prop: proportion of Galumna in the mite community. i.e. Galumna/totalabund
## # $SubsDens: substrate density
## # $WatrCont: water content
## # $Substrate: type of substrate.
## # $Shrub: abundance of shrubs. Coded as a factor.
## # $Topo: microtopography. blanket or hummock.

## plot(mites)

## par(mfrow=c(1,3)) #divide plot area in 1 row and 3 columns to have 3 plots in same figure
## plot(Galumna ~ WatrCont, data = mites, xlab = 'Water content', ylab='Abundance')
## boxplot(WatrCont ~ pa, data = mites, xlab='Presence/Absence', ylab = 'Water     content')
## plot(prop ~ WatrCont, data = mites, xlab = 'Water content', ylab='Proportion')
## par(mfrow=c(1,1)) #resets to default setting

## lm.abund <- lm(Galumna ~ WatrCont, data = mites)
## summary(lm.abund)
## lm.pa <- lm(pa ~ WatrCont, data = mites)
## summary(lm.pa)
## lm.prop <- lm(prop ~ WatrCont, data = mites)
## summary(lm.prop)

## plot(Galumna ~ WatrCont, data = mites)
## abline(lm.abund)

## plot(lm.abund)

## #Proportion
## plot(prop ~ WatrCont, data = mites)
## abline(lm.prop)
## plot(lm.prop)
## #Presence/Absence
## plot(pa ~ WatrCont, data = mites)
## abline(lm.pa)
## plot(lm.pa)

## coef(lm.abund)
## #(Intercept)     WatrCont
## #3.439348672 -0.006044788

## summary(lm.abund)$sigma


#### Section: 03-distributions.R 

## hist(mites$Galumna)
## mean(mites$Galumna)

## hist(mites$pa)

## sum(mites$pa) / nrow(mites)


#### Section: 04-glm-binary.R 

## model.lm <- lm(pa ~ WatrCont + Topo, data = mites)
## fitted(model.lm)
## # The "fitted()" function gives us expected values for the response variable.
## # Some values are lower than 0, which does not make sense for a logistic regression.
## # Let’s try the same model with a binomial distribution instead.
## # Notice the "family" argument to specify the distribution.
## model.glm <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
## fitted(model.glm)
## # All values are bound between 0 and 1.

## # Load the CO2 dataset. We used it during workshop 4!
## data(CO2)
## head(CO2)
## # Build a linear model of plant CO2 uptake as a function of CO2 ambient concentration
## model.CO2 <- lm(uptake ~ conc, data = CO2)
## # Extract the design matrix of the model with the model.matrix() function.
## X <- model.matrix(model.CO2)
## # And the estimated coefficients.
## B <- model.CO2$coefficients
## # Let’s multiply both X and B matrices to obtain the linear predictor.
## # The "%*%" symbol indicates that it is a matrix product.
## XB <- X %*% B
## # Compare the values of XB to the values obtained with the predict() function.
## # All statements should be TRUE.
## # We use the round() function so that all elements have 5 digits.
## round(fitted(model.CO2), digits = 5) == round(XB, digits = 5)

## # Let’s build a regression model of the presence/absence of a mite species (Galumna sp.)
## # as a function of water content and topography.
## # To do this, we need to use the glm() function and specify the family argument.
## logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial(link = "logit"))
## # The logit function is the default for the binomial distribution,
## # so it is not necessary to include it in the "family" argument:
## logit.reg <- glm(pa ~ WatrCont + Topo, data = mites, family = binomial)
## summary(logit.reg)

## model.bact1 <- glm(y ~ trt * week, family = binomial('logit'), data = bacteria)
## model.bact2 <- glm(y ~ trt + week, family = binomial('logit'), data = bacteria)
## model.bact3 <- glm(y ~ week, family = binomial('logit'), data = bacteria)
## anova(model.bact1, model.bact2, model.bact3, test = 'LRT')
## # Analysis of Deviance Table
## # Model 1: y ~ trt * week
## # Model 2: y ~ trt + week
## # Model 3: y ~ week
## #   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## # 1       214     203.12
## # 2       216     203.81 -2  -0.6854  0.70984
## # 3       218     210.91 -2  -7.1026  0.02869 *
## #Based on these results, we select model #2 as the best candidate to model these data.

## # Obtaining the odds of the slope.
## # Use the "exp()" function to put the coefficients back on the odds scale.
## # Mathematically, this line of code corresponds to:
## # exp(model coefficients) = exp(log(μ / (1 - μ)) = u / (1 - μ)
## # This corresponds to an odds ratio!
## exp(logit.reg$coefficients[2:3])
## #  WatrCont    TopoHummock
## #  0.9843118   8.0910340
## # To obtain confidence intervals on the odds scale:
## exp(confint(logit.reg)[2:3,])
## #               2.5 %      97.5 %
## #  WatrCont     0.9741887  0.9919435
## #  TopoHummock  2.0460547  38.6419693

## # Let's start with our odds ratio for topography from the logit.reg model:
## µ/ (1 - µ) = 8.09
## # Let's rearrange this to isolate µ
## µ = 8.09(1 - µ) = 8.09 - 8.09µ
## 8.09µ + µ = 8.09
## µ(8.09 + 1) = 8.09
## µ = 8.09 / (8.09 + 1)
## µ = 1 / (1 + (1 / 8.09)) = 0.89
## # We obtained the same result without using the exp() function!

## # Residual and null deviances are already stored in the glm object.
## objects(logit.reg)
## pseudoR2 <- (logit.reg$null.deviance – logit.reg$deviance) / logit.reg$null.deviance
## pseudoR2
## # [1]  0.4655937

## install.packages("binomTools")
## library("binomTools")
## # The Rsq function computes several fit indices,
## # including the coefficient of discrimination.
## # For information on the other fit indices, see Tjur (2009).
## # The plot shows the distribution of expected values when the outcome is observed
## # and not observed.
## # Ideally, the overlap between the two histograms should be small.
## fit <- Rsq(object = logit.reg)
## fit
## # R-square measures and the coefficient of discrimination, 'D':
## #
## #    R2mod     R2res     R2cor     D
## #    0.5205221 0.5024101 0.5025676 0.5114661
## #
## # Number of binomial observations:  70
## # Number of binary observation:  70
## # Average group size:  1
## plot(fit, which = "hist")

## fit <- Rsq(object = logit.reg)
## HLtest(object = fit)
## # The p value is 0.9051814. Hence, we do not reject the model.
## # We can consider it as appropriate for the data.

## null.d <- model.bact2$null.deviance
## resid.d <- model.bact2$deviance
## bact.pseudoR2 <- (null.d - resid.d) / null.d
## bact.pseudoR2
## # 0.0624257
## # This is very low!
## library(binomTools)
## HLtest(Rsq(model.bact2))
## # Chi-square statistic:  7.812347  with  8  df
## # P-value:  0.4520122
## # Fit is adequate.

## library(ggplot2)
## ggplot(mites, aes(x = WatrCont, y = pa)) + geom_point() +
## stat_smooth(method = "glm", family= "binomial", se = FALSE) + xlab("Water content") +
## ylab("Probability of presence") +
## ggtitle("Probability of presence of Galumna sp. as a function of water content")


#### Section: 05-glm-proportion.R 

## # Let’s generate some data based on the deer example:
## # We randomly choose a number between 1 and 10 for the number of infected deer.
## # Ten deers were sampled in ten populations.
## # Resource availability is an index to characterise the habitat.
## set.seed(123)
## n.infected <- sample(x = 1:10, size = 10, replace = TRUE)
## n.total <- rep(x = 10, times = 10)
## res.avail <- rnorm(n = 10, mean = 10, sd = 1)
## # Next, let’s build the model. Notice how the proportion data is specified.
## # We have to specify the number of cases where disease was detected
## # and the number of cases where the disease was not detected.
## prop.reg <- glm(cbind(n.infected, n.total - n.infected) ~ res.avail, family = binomial)
## summary(prop.reg)
## # If your data is directly transformed into proportions, here is the way to do it in R:
## # Let's first create a vector of proportions
## prop.infected <- n.infected / n.total
## # We have to specify the "weights" argument in the glm function to indicate the number of trials per site
## prop.reg2 <- glm(prop.infected ~ res.avail, family = binomial, weights = n.total)
## summary(prop.reg2)
## # The summaries of both prop.reg and prop.reg2 are identical!


#### Section: 06-glm-count.R 

## faramea <- read.csv(‘faramea.csv’, header = TRUE)

## hist(faramea$Faramea.occidentalis, breaks=seq(0,45,1), xlab=expression(paste("Number of ",
## italic(Faramea~occidentalis))), ylab="Frequency", main="", col="grey")

## glm.poisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=poisson)
## summary(glm.poisson)

## # intercept
## summary(glm.poisson)$coefficients[1,1]
## # slope of elevation
## summary(glm.poisson)$coefficients[2,1]

## summary(glm.poisson)
## # Null deviance: 414.81  on 42  degrees of freedom
## # Residual deviance: 388.12  on 41  degrees of freedom

## mean(faramea$Faramea.occidentalis)
## var(faramea$Faramea.occidentalis)

## # Option 1, fit a new quasi-Poisson GLM
## glm.quasipoisson = glm(Faramea.occidentalis~Elevation, data=faramea, family=quasipoisson)
## # Option 2, build from the previous model and update it:
## glm.quasipoisson = update(glm.poisson,family=quasipoisson)
## # output
## summary(glm.quasipoisson)

## ifelse(length(which(installed.packages() == "MASS")) == 0,
##       {print("MASS not installed. Installing... "); install.packages("MASS")},
##       print("MASS already installed"))

## install.packages("MASS")

## library("MASS")

## glm.negbin = glm.nb(Faramea.occidentalis~Elevation, data=faramea)
## summary(glm.negbin)

## # plot the observed data
## plot(faramea$Elevation, faramea$Faramea.occidentalis, xlab="Elevation (m)", ylab=expression(paste("Number of", "  ", italic(Faramea~occidentalis))), pch=16, col=rgb(4,139,154,150,maxColorValue=255))
## 
## # pull values for intercept and beta from the summary and put them in the exponential equation
## curve(exp(summary(glm.negbin)$coefficients[1,1]+summary(glm.negbin)$coefficients[2,1]*x),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T, lwd=2, col="orangered")
## 
## # pull the standard error as well to plot the equations for confidence envelope
## curve(exp(summary(glm.negbin)$coefficients[1,1]+1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x+1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")
## curve(exp(summary(glm.negbin)$coefficients[1,1]-1.96*summary(glm.negbin)$coefficients[1,2]+summary(glm.negbin)$coefficients[2,1]*x-1.96*summary(glm.negbin)$coefficients[2,2]),from=range(faramea$Elevation)[1],to=range(faramea$Elevation)[2],add=T,lty=2, col="orangered")


#### Section: 07-other-distributions.R 




#### Section: 08-final-considerations.R 




#### Section: 09-references.R 


