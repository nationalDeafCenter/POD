effByScale <- function(mod){
  re <- ranef(mod,condVar=TRUE)
  eff <- sweep(re$scale,2,fixef(mod),'+')

  se <-
    re$scale%>%
    attr('postVar')%>%
    apply(3,diag)%>%
    t()%>%
    sweep(2,summary(mod)$coef[,2]^2,'+')%>%
    sqrt()%>%
    as.data.frame()

  dimnames(se) <- dimnames(eff)

  list(eff=eff,se=se,fe=fixef(mod))
}

byScaleCoef <- function(ebs){
  out <- tibble(
    Estimate= if(ncol(ebs$eff)==2) ebs$eff[,2] else do.call("c",ebs$eff[,-1]),
    `Std. Error`= if(ncol(ebs$se)==2) ebs$se[,2] else do.call("c",ebs$se[,-1]),
    df=NA,
    `t value`=Estimate/`Std. Error`,
    `Pr(>|t|)`=2*pnorm(-abs(`t value`))
  )
  out <- as.data.frame(out)
  rownames(out) <- map(names(ebs$fe)[-1],~paste0(.,':',rownames(ebs$eff)))%>%do.call('c',.)

  out
}

model1 <- function(varb,long){
  form <- rating~(1|id)
  form1 <- update(form,paste('.~.+(1|scale)+',varb))
  form2 <- update(form,paste('.~.+(',varb,'|scale)+',varb))
  LLL <- long
  list(tp=lmer(form1,data=LLL,REML=FALSE),pp=lmer(form2,data=LLL,REML=FALSE))
}

proc1 <- function(varb,long){
  models <- model1(varb,long)
  effs <- effByScale(models[[2]])
  out <- rbind(summary(models[[1]])$coef,byScaleCoef(effs))
  attr(out,'models') <- models
  attr(out,'anova') <- anova(models[[1]],models[[2]])
  out
}


## varbs <- c('deafDisabled',#'InterpreterSaturation',
##     '`age/10`','white','gender',
##     'deafHS+deafHSprog',
##     'Interpreters+SpeechtoText+Notetaking+ExtendedTesttime',
##     'Community.College',

transMIdat <- function(impDat,id=NULL){
  tran1 <- function(imp1,id){
    imp1 <- as.data.frame(
      lapply(imp1,function(x){
        if(nlevels(x)==2){
          if(all(sort(levels(x))==c('FALSE','TRUE'))) return(as.numeric(as.logical(x)))
          if(all(sort(levels(x))==c(0,1))) return(as.numeric(x)-1)
        }
        return(x)
      }))
    imp1$id <- if(is.null(id)) factor(1:nrow(imp1)) else id
    imp1[,7:ncol(imp1)] <- imp1[,7:ncol(imp1)]%>%mutate_if(is.numeric,~scale(.,center=TRUE,scale=FALSE))
    long <- gather(imp1,"scale","rating",tech:scap)
    names(long) <- gsub(' |-','',names(long))
    long$`age/10` <- long$age/10
    long$InterpreterSaturation <- factor(long$InterpreterSaturation,ordered=FALSE,levels=c("low (0-50)",   "med (51-125)", "high (126+)" ))
    long
  }
  out <- map(seq(impDat$m),
    function(mm){
      imp1 <- complete(impDat,mm)
      tran1(imp1,id)
    })
  out
}



miV <- function(vv,longDats){
  m <- length(longDats)

  mods <- map(longDats,~proc1(vv,.))

  getFixefTab(mods)

}

getFixefTab <- function(mods){
    mods <- suppressWarnings(suppressMessages(map(mods,~summary(.)$coef)))
    ests <- do.call('cbind',map(mods,~.[,'Estimate']))
    ses <- do.call('cbind',map(mods,~.[,'Std. Error']))
    m <- ncol(ests)
    Ubar <- rowMeans(ses^2)
    B <- apply(ests,1,var)
    if(all(B==0)){
      if(!'Pr(>|t|)'%in%colnames(mods[[1]]))
        mods[[1]] <- cbind(mods[[1]],`Pr(>|t|)`=2*pnorm(-abs(mods[[1]][,'t value'])))
      return(as.data.frame(mods[[1]]))
    }
    corr <- (1+1/m)
    v0 <- if('df'%in%colnames(mods[[1]])) mods[[1]][,'df'] else Inf
    vm <- ifelse(B==0,v0,(m-1)*(1+Ubar/(corr*B)))

  out <- tibble(
    Estimate= rowMeans(ests),
    `Std. Error`=sqrt(Ubar+corr*B),
    df=ifelse(is.na(v0),Inf,v0),
    `t value`=Estimate/`Std. Error`,
    `Pr(>|t|)`=2*pt(-abs(`t value`),df)
  )
  out <- as.data.frame(out)
  rownames(out) <- rownames(mods[[1]])
  #attr(out,'components') <- map(mods,attributes)
  out
}




mi <- function(varbs,impDat,subst=NULL){
  longDats <- transMIdat(impDat)

  if(!is.null(subst))
    longDats <- lapply(longDats,function(x) x[eval(parse(text=subst),x),])

  ovarbs <- sapply(sapply(varbs,strsplit,split='+',fixed=TRUE),str_extract,pattern='[:alpha:]+')
  out <- list()
  on.exit(cat('\n'))
  for(vv in varbs){
    cat(vv,' ')
    out[[vv]] <-
      if(all(impDat$nmis[ovarbs[[vv]]]==0)) proc1(vv,longDats[[1]]) else miV(vv,longDats)
  }
  out
}

miRE <- function(varb,longDats,subst=NULL){
  if(!is.null(subst))
    longDats <- lapply(longDats,function(x) x[eval(parse(text=subst),x),])

  form <- as.formula(paste0('rating~(1|id)+(1|scale)+(1|',varb,')+(1|',varb,':scale)'))
  mods <- lapply(longDats,function(dd) lmer(form,data=dd))
  re <- lapply(mods,ranef,condVar=TRUE)
  effs <- do.call('cbind',lapply(re,function(rr) rr[[varb]]))
  vars <- do.call('cbind',lapply(re,function(rr) attr(rr[[varb]],'postVar')[1,1,]))
  eff <- rowMeans(effs)
  SE <- sqrt(rowMeans(vars)+(1+1/length(longDats))*apply(effs,1,var))
  T <- eff/SE
  P <- 2*pnorm(-abs(T))

  mainRes <- cbind(eff,SE,T,P)

  effs2 <- do.call('cbind',lapply(re,function(rr) rr[[paste0(varb,':scale')]]))
  vars2 <- do.call('cbind',lapply(re,function(rr) attr(rr[[paste0(varb,':scale')]],'postVar')[1,1,]))
  eff2 <- rowMeans(effs2)
  SE2 <- sqrt(rowMeans(vars2)+(1+1/length(longDats))*apply(effs2,1,var))
  T2 <- eff2/SE2
  P2 <- 2*pnorm(-abs(T2))

  byScale <- cbind(eff2,SE2,T2,P2)

  list(main=mainRes,byScale=byScale)
}

getRanefTab <- function(mods,intr=FALSE){
    re <- lapply(mods,ranef,condVar=TRUE)
    vvv <- names(re[[1]])
    if(!intr){
      intrN <- grep(':',vvv,fixed=TRUE)
      if(length(intrN)) vvv <- vvv[-intrN]
    }
    vvv <- vvv[vvv!='id']

    res <- do.call('rbind',lapply(vvv,getOneReSet,re,'(Intercept)'))

    if(intr)
      for(varb in vvv)
        if(ncol(re[[1]][[varb]])>1)
          res <- rbind(res,
            do.call(
              'rbind',
              lapply(names(re[[1]][[varb]])[-1],
                function(nn) getOneReSet(varb,re,nn)
              )
            )
          )

    res <- as.data.frame(res)
    names(res) <- c('Estimate', 'Std. Error', 't value'   ,    'Pr(>|t|)')
    res
}

getOneReSet <- function(varb,re,cname='(Intercept)'){
  effs <- do.call('cbind',lapply(re,function(rr) rr[[varb]][[cname]]))
  vars <- do.call('cbind',lapply(re,function(rr) attr(rr[[varb]],'postVar')[1,1,]))
  eff <- rowMeans(effs)
  SE <- sqrt(rowMeans(vars)+(1+1/length(longDats))*apply(effs,1,var))
  T <- eff/SE
  P <- 2*pnorm(-abs(T))
  newRes <- cbind(eff,SE,T,P)
  rownames(newRes) <- rownames(re[[1]][[varb]])
  rownames(newRes) <-
    paste0(
      if(cname=='(Intercept)') paste0(varb,':') else cname,
      ':',rownames(newRes))

  newRes
}

jcoefsNoMI <- function(mod,intr=FALSE){
  res <- summary(mod)$coef[,c('Estimate','Std. Error','t value','Pr(>|t|)')]
  re <- ranef(mod,condVar=TRUE)
  vvv <- names(re)
  if(!intr){
    intrN <- grep(':',vvv,fixed=TRUE)
    if(length(intrN)) vvv <- vvv[-intrN]
  }
  vvv <- vvv[vvv!='id']
  for(varb in vvv){
    newRes <-

getGof <- function(mods,nums=FALSE,covs=FALSE){
  ext <- map(mods,extract)
  nms <- ext[[1]]@gof.names
  dec <- ext[[1]]@gof.decimal
  gof <- do.call('rbind',map(ext,~.@gof))
  gof <- colMeans(gof,na.rm=TRUE)
  gof[grep('Var:',nms,fixed=TRUE)] <- sqrt(gof[grep('Var:',nms,fixed=TRUE)])
  nms <- gsub('Var:','SD:',nms,fixed=TRUE)

  drp <- numeric(0)
  if(!nums)
    drp <- c(drp,setdiff(grep('Num. groups:',nms,fixed=TRUE),grep('id',nms)))
  if(!covs)
    drp <- c(drp,grep('Cov:',nms,fixed=TRUE))

  if(length(drp)){
    gof <- gof[-drp]
    dec <- dec[-drp]
    nms <- nms[-drp]
  }

  list(gof.names=nms,gof=gof,gof.decimal=dec)
}

testRE <- function(mod,vv){
  cnms <- mod@cnms
  f2 <- '.~.'
  for(v in grep(vv,names(cnms),value=TRUE,fixed=TRUE)){
    if(all(cnms[[v]]=='(Intercept)')){
      f2 <- paste0(f2,'-(1|',v,')')
    } else f2 <- paste0(f2,'-(',paste(cnms[[v]][cnms[[v]]!='(Intercept)'],collapse='+'),'|',v,')')
  }
  m1 <- update(mod,data=model.frame(mod))
  m2 <- update(mod,f2,data=model.frame(mod))
  anova(m1,m2)
}

fullTrObj <- function(mods){
  ft <- getFixefTab(mods)
  rt <- getRanefTab(mods)
  mlf <- rbind(rt,ft[,names(rt)])
  gof <- getGof(mods)

  createTexreg(rownames(mlf),mlf[,'Estimate'],mlf[,'Std. Error'],mlf[,'Pr(>|t|)'],gof.names=gof$gof.names,
    gof=gof$gof,gof.decimal=gof$gof.decimal)
}

## lrtMI <- function(mods,vv){
##   m <- length(mods)
##   aovs <- lapply(mods,testRE,vv=vv)
##   ws <- map_dbl(aovs,~.$Chisq[2])
##   k <- aovs[[1]][['Chi Df']][2]
##   a <- k*(m-1)
##   r3 <- (m+1)/a*mean(ws)

##   varComp <- function(mod){
##   vc <- VarCorr(mod)

