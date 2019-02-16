plot.lmList <-
  function(object,ord.var=1,...) {
    ## Ord.var indicates which coefficient to
    ##     sort the data frame of coefficients by.
    require(reshape)
    ## create a data.frame of coefs from list of glm fits.
    cL <- coef(object)
    ## Adds a column for group ID --
    ## rownames (the variable upon which the fits were conditioned)
    ## -- here genotype.
    cL$grp  <- rownames(cL)
    if (is.numeric(ord.var) & length(ord.var)==1) {
      ## identify the ordering coefficient by name
      ord.var <- names(cL)[ord.var+1]
      if (!ord.var %in% names(cL)) stop("unknown ordering variable")
      ## create a new order of factor levels for group ID
      cL$grp <- reorder(cL$grp,cL[[ord.var]])
    } else
      ##otherwise just use the original ordering
      cL$grp <- reorder(cL$grp,ord.var)
    ##"melt" or stack the data frame to
    ##   allow a dotplot of each coefficient.
    dotplot(grp~value|variable,data=melt(cL),...)
  }


qqmath.lmList <- function(object,...) {
  require(reshape)
  qqmath(~value|variable,data=melt(coef(object)),
         prepanel = prepanel.qqmathline,
         panel = function(x, ...) {
           panel.qqmathline(x, ...)
           panel.qqmath(x, ...)
         },
         scale=list(y=list(relation="free")))
}

overdisp_fun <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  (rdf <- nrow(model@frame)-model.df)
  rp <- residuals(model)
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE,log.p=TRUE)
  c(chisq=Pearson.chisq,ratio=prat,p=exp(pval),logp=pval)
}

sprint <- function(m) lme4:::printMer(m,correlation=FALSE)

locscaleplot <- function(model,col="black") {
  f <- fitted(model)
  r <- sqrt(abs(residuals(model)))  ## had better be Pearson resids
  plot(f,r,col=col)
  L1 <- loess(r~f)
  fvec = seq(min(f),max(f),length.out=150)
  lines(fvec,predict(L1,fvec),col=2)
}

dfun <- function(x) {
  x$AIC <- x$AIC-min(x$AIC)
  names(x)[2] <- "dAIC"
  x
}

badcorr <- function(x) {
  cc <- attr(x,"correlation")
  diag(cc) <- 0
  any(abs((abs(cc)-1))<1e-5)
}
anybadcorr <- function(x) {
  any(sapply(VarCorr(x),badcorr))
}

locscaleplot <- function(model,col="black") {
  f <- fitted(model)
  r <- abs(residuals(model))
  plot(f,r,col=col)
  L1 <- loess(r~f)
  fvec = seq(min(f),max(f),length.out=150)
  lines(fvec,predict(L1,fvec),col=2)
}

printvc <- function(m,digits=2,ctol=1e-3) {
  v <- VarCorr(m)
  prtfun <- function(x) {
    cc <- attr(x,"correlation")
    diag(cc) <- 0
    corstr <- ifelse(abs(abs(cc)-1)<ctol,"="," ")
    ss <- format(x,digits=digits) ## sprintf(fmt,x)
    ss <- paste(ss,corstr,sep="")
    m <- matrix(ss,nrow=nrow(x))
    m[upper.tri(m)] <- ""
    dimnames(m) <- dimnames(x)
    ## writeLines(apply(m,1,paste,collapse=" "))
    print(m,quote=FALSE)
  }
  for (i in seq_along(v)) {
    cat(names(v)[i],":\n",sep="")
    prtfun(v[[i]])
    cat("\n")
  }
}

plot.ICtab <- function(x,sort=TRUE,within) {
  z <- with(x,data.frame(n=attr(x,"row.names"),dAIC))
  if (sort) z <- transform(z,n=reorder(n,dAIC))
  if (!missing(within)) z$dAIC[z$dAIC>within] <- NA
  dotplot(n~dAIC,data=z)
}
