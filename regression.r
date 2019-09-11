library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)
source('cleanData.r')

big3 <- dat$What.school.or.training.program.are.you.currently.attending.%in%c('Gallaudet University','Rochester Institute Technology','Calif St Univ Northridge')
naSchool <- dat$What.school.or.training.program.are.you.currently.attending.%in%c("#N/A","")

## datBin <- dat
## for(nn in names(survBin)) datBin[[nn]] <- survBin[[nn]]

cats <- c("tech","atti","comm","phys","serv","scap")

## summB <- list()
## for(cc in cats) summB[[cc]] <- rowMeans(survBin[!big3,grep(cc,names(survBin))],na.rm=TRUE)
## summB <- as.data.frame(summB)

## datBin <- cbind(datBin,summB)


## datBin3 <- datBin[big3,]
## datBin <- datBin[!big3,]

## summPlot <- gather(datBin,dim,avg,tech,atti,comm,phys,serv,scap)



dat[dat==''] <- NA
dat3 <- dat[big3,]
dat <- dat[!big3,]
dat <- droplevels(dat)

surv3 <- surv[big3,]
surv <- droplevels(surv[!big3,])

acc <- acc[!big3,]

levs <- c('Not Likely','Somewhat Likely','Likely','Extremely likely')
levsAb <- c('NL','SL','L','EL')

summ <- list()
resp <- list()
cont <- list()

type <- c(type,rep('demo',ncol(dat)-length(type)))
for(cc in cats){
    summ[[cc]] <- apply(dat[,type==cc],1,
      function(x) mean(x=='Extremely likely'|x=='Likely',na.rm=TRUE))
    cont[[cc]] <- rowMeans(surv[,c(type[1],type[type%in%cats])==cc],na.rm=TRUE)
  for(i in 1:4)
      resp[[paste0(cc,'.',levsAb[i])]] <- apply(dat[,type==cc],1,
                                                function(x) mean(x==levs[i],na.rm=TRUE))
}


cont <- as.data.frame(cont)
summ <- as.data.frame(summ)
resp <- as.data.frame(resp)

dat$Accrediation[dat$Accrediation=='#N/A'] <- NA
intSat <- quantile(dat$Interpreter.Saturation,c(.33,.66),na.rm=TRUE)
## instead do 0-50, 51-125, 126+
dat[['Interpreter Saturation']] <- cut(dat$Interpreter.Saturation,c(-1,50,125,Inf),labels=c('low (0-50)','med (51-125)','high (126+)'),ordered=TRUE)

preferredLanguageVarbs <- grep('Pref.',names(dat),fixed=TRUE,value=TRUE)
    ## gsub(
    ##     'What  is your | in the following settings at school or in your training program',
    ##     '',
    ##     rownames(varInf)[grep('preferred language',varInf$V3)]
    ##     )
for(vvv in preferredLanguageVarbs)
    dat[[vvv]] <- factor(
        dat[[vvv]],
        levels=c('ASL',"Spoken English","Written/Text Communication","Other")
        )


ddd <- dat[,c(1,(max(grep('DA[0-9]+',names(dat)))+1):ncol(dat))] ## demographic variables I've messed with
ddd$age <- as.numeric(dat$Age) ## continuous


regDat <- bind_cols(cont,ddd)
## add some more/simplify predictors

regDat$white <- startsWith(regDat$Ethnicity,'White')
regDat$white[regDat$Ethnicity=='NA'] <- NA
regDat$main <- regDat$MainstreamingHS=='mainstream only'
regDat$main[regDat$MainstreamingHS=='NA'] <- NA

##
hst <- data.frame(
  id=dat$id,
  deafHS=hsType[['Deaf School (or school with all deaf students)']][!big3],
  deafHSprog=hsType[['Deaf program within a public school']][!big3]
)
hsrs <- rowSums(hsType[!big3,1:4])
hst$deafHS[hsrs==0] <- hst$deafHSprog[hsrs==0] <- NA

regDat <- left_join(regDat,hst)

accrs <- rowSums(accomodations[,-1])
for(i in 2:ncol(accomodations))
  accomodations[accrs==0,i] <- NA

regDat <- left_join(regDat,select(accomodations,-AssistiveListeningDevice,-`Other`))

### deal with NAs
nad <- select(regDat,deafDisabled,age,gender,`Interpreter Saturation`,white,deafHS,deafHSprog,Interpreters,
  `Speech-to-Text`,Notetaking,ExtendedTesttime)
nad[nad=='NA'] <- NA
nnn <- apply(nad,1,function(x) sum(is.na(x)))
table(nnn)
sapply(nad[which(nnn==7),],function(x) sum(is.na(x))) ##
regDat <- regDat[nnn<8,]

select(regDat,deafDisabled,age,gender,`Interpreter Saturation`,white,deafHS,deafHSprog,Interpreters,
  `Speech-to-Text`,Notetaking,ExtendedTesttime)%>%
  map_dbl(~sum(is.na(.)))

cat(
  ' ----------------------------------\n',
  'dropping ',nrow(dat)-nrow(regDat),' people due to too much missingness\n',
  '----------------------------------\n'
)

names(regDat) <- gsub(' |-','',names(regDat))

impDat <- mice(regDat,m=20)



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
    'Interpreters+SpeechtoText+Notetaking+ExtendedTesttime')

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


modLessFull <- lmer(rating~(1|id)+deafDisabled+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
  (age+white|scale),
   data=long)


anovas <-
  map(
    res,
    ~if('components'%in%names(attributes(.))) attr(.,'components')[[1]]$anova else attr(.,'anova')
  )
map(anovas,~c(which.min(.[['AIC']]),which.min(.[['BIC']]),.[['Pr(>Chisq)']][2]))


summary(modSelect2 <- step(modLessFull))


res <- mi(varbs,impDat)
intSat <- mi('InterpreterSaturation',impDat,subst="Interpreters==1")
res$intSat <- intSat[[1]]

mods <- map(res,~if('components'%in%names(attributes(.))) attr(.,'components')[[1]]$models$tp else attr(.,'models')$tp)
coefs <- map(res,~.$Estimate)
ses <- map(res,~.[['Std. Error']])
pvals <- map(res,~.[['Pr(>|t|)']])

names(mods) <- NULL

mods[[length(mods)+1]] <- mlf
coefs[[length(coefs)+1]] <- fixef(mlf)
ses[[length(ses)+1]] <- summary(mlf)$coef[,2]
pvals[[length(pvals)+1]] <- 2*pnorm(-abs(summary(mlf)$coef[,3]))


screenreg(mods,override.coef=coefs,override.se=ses,override.pvalues=pvals)




  ## out <- map(mods,function(mod){
##     tibble(

##     `Std. Error`= if(ncol(ebs$se)==2) ebs$se[,2] else do.call("c",ebs$se[,-1]),
##     df=NA,
##     `t value`=Estimate/`Std. Error`,
##     `Pr(>|t|)`=2*pnorm(-abs(`t value`))
##     )


## mods <- map(models,~attr(.,"models")[[1]])
## mods2 <- map(models,~attr(.,"models")[[2]])

### automatic model selection time

## modFull <- lmer(rating~(1|id)+deafDisabled+InterpreterSaturation+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
##  (deafDisabled+InterpreterSaturation+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime|scale),
##   data=long)



## summary(modSelect <- step(modFull))
modLessFull <- lmer(rating~(1|id)+deafDisabled+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
  (age+white|scale),
   data=transMIdat(impDat)[[1]])

ttt <- transMIdat(impDat,regDat)
for(i in 0:20) ttt[[i+1]] <- cbind(imp=i,id=1:nrow(ttt[[1]]),ttt[[i+1]])
ttt <- as.mids(do.call('rbind',ttt))
mlf <- with(ttt,lmer(rating~(1|id)+deafDisabled+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
  (age+white|scale)))

#modSelect2 <- step(modLessFull)

## summary(selected <- update(modLessFull,rating ~ `age/10` + white + Notetaking + (1 | id) + (white | scale)))

## mods$selected <- selected
## mods2$selected <- selected
## names(mods) <- NULL
## names(mods2) <- NULL

## htmlreg(mods,file='regressions1.html')


## mod1a <- lmer(rating~(1|id)+(`Interpreter Saturation`|scale)+`Interpreter Saturation`,data=long)

## mod2 <- lmer(rating~(1|id)+(1|scale)+age,data=long)
## mod2a <- lmer(rating~(1|id)+(age|scale)+age,data=long)

## mod3 <- lmer(rating~(1|id)+(1|scale)+white,data=long)
## mod3b <- lmer(rating~(1|id)+(white|scale)+white,data=long)

## #mod4 <-

##   ## c(
##       ##  'ruralUrban',
##       ##  'ruralUrbanIP',
##       ##  'Interpreter Saturation',
##       ##  'demo68',
##       ##  'demo67',
##       ##  'demo65',
##       ##  'Ethnicity',
##       ##  'Disability',
##       ##  'deafDisabled',
##       ##  'HStype',
##       ##  'MainstreamingHS',
##       ##  'age',
##       ##  'demo5',
##       ##  'gender',
##       ##  'demo26',
##       ##  'HS Class Language',
##       ##  'intSatCat',
##       ##  preferredLanguageVarbs


