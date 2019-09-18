library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)

source('code/regDatPrep.r')

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


varbs <- c('deafDisabled',#'InterpreterSaturation',
    '`age/10`','white','gender',
    'deafHS+deafHSprog',
    'Interpreters+SpeechtoText+Notetaking+ExtendedTesttime',
    'Community.College',

transMIdat <- function(impDat,regDat){
  tran1 <- function(imp1){
    long <- gather(imp1,"scale","rating",tech:scap)
    names(long) <- gsub(' |-','',names(long))
    long$`age/10` <- long$age/10
    long$InterpreterSaturation <- factor(long$InterpreterSaturation,ordered=FALSE,levels=c("low (0-50)",   "med (51-125)", "high (126+)" ))
    long
  }
  out <- map(seq(impDat$m),
    function(mm){
      imp1 <- complete(impDat,mm)
      tran1(imp1)
    })
  if(!missing(regDat)){
    out2 <- list(tran1(regDat))
    for(i in 1:length(out)) out2[[i+1]] <- out[[i]]
    out <- out2
  }
  out
}



miV <- function(vv,longDats){
  m <- length(longDats)

  mods <- map(longDats,~proc1(vv,.))
  ests <- do.call('cbind',map(mods,~.[['Estimate']]))
  ses <- do.call('cbind',map(mods,~.[['Std. Error']]))
  Ubar <- rowMeans(ses^2)
  B <- apply(ests,1,var)
  if(all(B==0)) return(mods[[1]])
  corr <- (1+1/impDat$m)
  v0 <- mods[[1]][['df']]
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
  attr(out,'components') <- map(mods,attributes)
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



longDats <- transMIdat(impDat)



res <- mi(varbs,impDat)
intSat <- mi('InterpreterSaturation',impDat,subst="Interpreters==1")
res$InterpreterSaturation <- intSat[[1]]

## #mods <- map(res,~if('components'%in%names(attributes(.))) attr(.,'components')[[1]]$models$tp else attr(.,'models')$tp)
## coefs <- map(res,~.$Estimate)
## ses <- map(res,~.[['Std. Error']])
## pvals <- map(res,~.[['Pr(>|t|)']])

trobj <- map(res, function(rrr){
  rrr <- rrr[-grep(':',rownames(rrr),fixed=TRUE),]
  createTexreg(rownames(rrr),rrr$Estimate,rrr[['Std. Error']],rrr[['Pr(>|t|)']])
}
)


htmlreg(trobj,#mods,override.coef=coefs,override.se=ses,override.pvalues=pvals,
  file=paste0('PODregressions',Sys.Date(),'.html'))





ethMods <- lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+(1|Ethnicity)+(1|Ethnicity:scale),data=dd))
reEth <- lapply(ethMods,ranef,condVar=TRUE)
ethEffs <- do.call('cbind',lapply(reEth,function(rr) rr$Ethnicity))
ethVars <- do.call('cbind',lapply(reEth,function(rr) attr(rr$Ethnicity,'postVar')[1,1,]))
ethEff <- rowMeans(ethEffs)
ethSE <- sqrt(rowMeans(ethVars)+(1+1/length(longDats))*apply(ethEffs,1,var))
ethT <- ethEff/ethSE
ethP <- 2*pnorm(-abs(ethT))


### eth by scale?
ethEffs2 <- do.call('cbind',lapply(reEth,function(rr) rr$`Ethnicity:scale`))
ethVars2 <- do.call('cbind',lapply(reEth,function(rr) attr(rr$`Ethnicity:scale`,'postVar')[1,1,]))
ethEff2 <- rowMeans(ethEffs2)
ethSE2 <- sqrt(rowMeans(ethVars2)+(1+1/length(longDats))*apply(ethEffs2,1,var))
ethT2 <- ethEff2/ethSE2
ethP2 <- 2*pnorm(-abs(ethT2))


## age by category
ageMods <- lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+(1|ageCat)+(1|ageCat:scale),data=dd))
reAge <- lapply(ageMods,ranef,condVar=TRUE)
ageEffs <- do.call('cbind',lapply(reAge,function(rr) rr$ageCat))
ageVars <- do.call('cbind',lapply(reAge,function(rr) attr(rr$ageCat,'postVar')[1,1,]))
ageEff <- rowMeans(ageEffs)
ageSE <- sqrt(rowMeans(ageVars)+(1+1/length(longDats))*apply(ageEffs,1,var))
ageT <- ageEff/ageSE
ageP <- 2*pnorm(-abs(ageT))


### age by scale?
ageEffs2 <- do.call('cbind',lapply(reAge,function(rr) rr$`ageCat:scale`))
ageVars2 <- do.call('cbind',lapply(reAge,function(rr) attr(rr$`ageCat:scale`,'postVar')[1,1,]))
ageEff2 <- rowMeans(ageEffs2)
ageSE2 <- sqrt(rowMeans(ageVars2)+(1+1/length(longDats))*apply(ageEffs2,1,var))
ageT2 <- ageEff2/ageSE2
ageP2 <- 2*pnorm(-abs(ageT2))


### multiple
modLessFull <- lapply(
  longDats,
  function(dd)
    lmer(rating~
           deafDisabled+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+

           (1|id)+(1|ageCat)+(1|Ethnicity)+(1|scale)+(1|Ethnicity:scale)+(1|ageCat:scale),
           data=dd)
  )



