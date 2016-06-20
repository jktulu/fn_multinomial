#################################################################################
# SPECIAL FUNCTIONS FOR CHOICE MODELLING	                                #
# requires: <na>								#
# last update: 2016-06-20							#
#################################################################################

#################################################################################
# --- stepwise multinomial logit ------------------------------------------------

fitStepMultinom <- function(dx,y,x,stepw=TRUE,reflevel=1,clean=TRUE){
  require(MASS);require(nnet);require(mlogit);require(stringr)
  
  dx.clean <- if(clean==TRUE) as.data.frame(lapply(dx[complete.cases(dx),],droplevels)) else dx
  if(stepw==TRUE) {
    x <- str_replace(x,'[|]','+')
    m0 <- stepAIC(multinom(as.formula(paste(y,x,sep='~ ')),dx.clean))
    fm <- str_replace(as.character(m0$call)[[2]],'~[ ]*[1]*[ ]*[+]*','~ 1 | ')
  } else fm <- str_replace(as.character(paste(y,x,sep='~ ')),'[1][ ]*[+]','1 |')
  dy <- mlogit.data(data=dx.clean,choice=y,shape='wide')
  m <- mlogit(as.formula(fm),data=dy,reflevel=reflevel)
  return(m)
}

#################################################################################
# --- variance decomposition for mlogit -----------------------------------------

decompMultinom <- function(m){ 
  require(pbapply)
  fme <- str_trim(strsplit(strsplit(as.character(m$formula)[3],'|',TRUE)[[1]][2],'+',TRUE)[[1]])
  p <- length(fme)
  l.fmi <- lapply(1:p,function(i) combn(1:p,i))
  l.fmr <- lapply(1:p,function(i) combn(fme,i))
  l.fms <- lapply(l.fmr,function(x) apply(x,2,function(z) paste('1 |',paste(z,collapse='+'))))
  
  print(paste('   order:',p))
  v.r2 <- pblapply(l.fms,function(x) 
    sapply(x,function(z) as.numeric(summary(
      eval(parse(text=paste0('mlogit(',paste(as.character(m$formula)[2],'~',z,collapse=''),', data= m$model)')))
      #fitStepMultinom(dat,as.character(m$formula)[2],z,stepw=F,reflevel=1,clean=FALSE)
      )[[18]])))
  cm <- lapply(2:p,function(h)
    sapply(1:p,function(i) sum(v.r2[[h]][apply(l.fmi[[h]],2,is.element,el=i)]-v.r2[[h-1]][apply(l.fmi[[h-1]],2,function(j) !is.element(i,j))])/choose(p-1,h-1))
  )
  c2 <- rbind(v.r2[[1]],do.call(rbind,cm))
  v.dom <- colMeans(c2)*100
  return(v.dom)
}


#################################################################################
# --- calculating elasticities --------------------------------------------------

effectsMultinom <- function(m, covariate = NULL, type = c("aa", "ar", "rr","ra"), data = NULL){
  # adapted from from mlogit 
  type <- match.arg(type)
  if (is.null(data)) {
    P <- predict(m, returnData = TRUE)
    data <- attr(P, "data")
    attr(P, "data") <- NULL
  }
  else P <- predict(m, data)
  newdata <- data
  J <- length(P)
  alt.levels <- names(P)
  pVar <- substr(type, 1, 1)
  xVar <- substr(type, 2, 2)
  cov.list <- strsplit(as.character(attr(formula(m), "rhs")), " + ", fixed = TRUE) 
  # modified: 
  rhs <- sapply(cov.list, function(x) length(na.omit(match(x, 
                                                           covariate))) > 0)
  rhs <- (1:length(cov.list))[rhs]
  eps <- sd(data[[covariate]][1:nrow(P) %% ncol(P) == 0]) * .001
  # 1/1000 of standard deviation used as change in x as recommended by Cameron and Trivedi (Microeconometrics using Stata, Revised Edition, 2010)
  if (rhs %in% c(1, 3)) {
    if (rhs == 3) {
      theCoef <- paste(alt.levels, covariate, sep = ":")
      theCoef <- coef(m)[theCoef]
    }
    else theCoef <- coef(m)[covariate]
    me <- c()
    for (l in 1:J) {
      newdata[l, covariate] <- data[l, covariate] + eps
      newP <- predict(m, newdata)
      me <- rbind(me, (newP - P)/eps)
      newdata <- data
    }
    if (pVar == "r") 
      me <- t(t(me)/P)
    if (xVar == "r") 
      me <- me * matrix(rep(data[[covariate]], J), J)
    dimnames(me) <- list(alt.levels, alt.levels)
  }
  if (rhs == 2) {
    newdata[, covariate] <- data[, covariate] + eps
    newP <- predict(m, newdata)
    me <- (newP - P)/eps
    if (pVar == "r") 
      me <- me/P
    if (xVar == "r") 
      me <- t(t(me) * data[[covariate]])
    names(me) <- alt.levels
  }
  me
}

effectsMultinomWrap <- function(m,type='rr'){
  require(pbapply)
  m2 <- eval(parse(text=paste0('mlogit(',paste(deparse(formula(m)),collapse=''),', data= m$model)')))
  covs <- unlist(strsplit(as.character(attr(formula(m2), "rhs"))[[2]], " + ", fixed = TRUE))
  dme <- data.frame(t(pbsapply(covs, function(x) colMeans(effectsMultinom(m2, covariate = x, type = type, data = m2$model)))))
  dme$wt <- apply(dme,1,function(x) sum(abs(x)*m2$freq/sum(m2$freq)))
  return(dme)
}