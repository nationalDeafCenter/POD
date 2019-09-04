library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)
source('cleanData.r')

big3 <- dat$demo46%in%c('Gallaudet University','Rochester Institute Technology','Calif St Univ Northridge')
naSchool <- dat$demo46%in%c("#N/A","")

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



for(cc in cats){
  summ[[cc]] <- apply(dat[,grep(cc,names(dat))],1,
    function(x) mean(x=='Extremely likely'|x=='Likely',na.rm=TRUE))
  cont[[cc]] <- rowMeans(surv[,grep(cc,names(surv))],na.rm=TRUE)
  for(i in 1:4)
    resp[[paste0(cc,'.',levsAb[i])]] <- apply(dat[,grep(cc,names(dat))],1,
      function(x) mean(x==levs[i],na.rm=TRUE))
}

cont <- as.data.frame(cont)
summ <- as.data.frame(summ)
resp <- as.data.frame(resp)

dat$demo68[dat$demo68=='#N/A'] <- NA
intSat <- quantile(dat$demo69,c(.33,.66),na.rm=TRUE)
## instead do 0-50, 51-125, 126+
dat[['Interpreter Saturation']] <- cut(dat$demo69,c(-1,50,125,Inf),labels=c('low (0-50)','med (51-125)','high (126+)'),ordered=TRUE)

preferredLanguageVarbs <-
    gsub(
        'What  is your | in the following settings at school or in your training program',
        '',
        rownames(varInf)[grep('preferred language',varInf$desc)]
        )
for(vvv in preferredLanguageVarbs)
    dat[[vvv]] <- factor(
        dat[[vvv]],
        levels=c('ASL',"Spoken English","Written/Text Communication","Other")
        )


ddd <- dat[,c(1,(max(grep('demo',names(dat)))+1):ncol(dat))] ## demographic variables I've messed with
ddd$age <- as.numeric(dat$demo4) ## continuous


regDat <- bind_cols(cont,ddd)
## add some more/simplify predictors

regDat$white <- regDat$Ethnicity=='White, European'
regDat$main <- regDat$MainstreamingHS=='mainstream only'

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

regDat <- left_join(regDat,select(accomodations,-`Assistive Listening Device`,-`Other`))

### deal with NAs
nad <- select(regDat,deafDisabled,age,gender,`Interpreter Saturation`,white,deafHS,deafHSprog,Interpreters,
  `Speech-to-Text`,`Note taking`,`Extended Test time`)
nad[nad=='NA'] <- NA
nnn <- apply(nad,1,function(x) sum(is.na(x)))
table(nnn)
sapply(nad[which(nnn>4),],function(x) sum(is.na(x))) ## 7 or 8 means nearly all missing
regDat <- regDat[nnn<5,]

names(regDat) <- gsub(' |-','',names(regDat))

impDat <- mice(regDat,m=20)

imp1 <- complete(impDat)

long <- gather(imp1,"scale","rating",tech:scap)

names(long) <- gsub(' |-','',names(long))

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

model1 <- function(varb){
  form <- rating~(1|id)
  form1 <- update(form,paste('.~.+(1|scale)+',varb))
  form2 <- update(form,paste('.~.+(',varb,'|scale)+',varb))
  list(tp=lmer(form1,data=long,REML=FALSE),pp=lmer(form2,data=long,REML=FALSE))
}

proc1 <- function(varb){
  models <- model1(varb)
  effs <- effByScale(models[[2]])
  out <- rbind(summary(models[[1]])$coef,byScaleCoef(effs))
  attr(out,'models') <- models
  attr(out,'anova') <- anova(models[[1]],models[[2]])
  out
}


long$`age/10` <- long$age/10
long$InterpreterSaturation <- factor(long$InterpreterSaturation,ordered=FALSE,levels=c("low (0-50)",   "med (51-125)", "high (126+)" ))

models <- sapply(
  c('deafDisabled','InterpreterSaturation','`age/10`','white','gender',
    'deafHS+deafHSprog',
    'Interpreters+SpeechtoText+Notetaking+ExtendedTesttime'),
  proc1,
  simplify=FALSE)

mods <- map(models,~attr(.,"models")[[1]])
mods2 <- map(models,~attr(.,"models")[[2]])

### automatic model selection time

## modFull <- lmer(rating~(1|id)+deafDisabled+InterpreterSaturation+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
##  (deafDisabled+InterpreterSaturation+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime|scale),
##   data=long)



## summary(modSelect <- step(modFull))

modLessFull <- lmer(rating~(1|id)+deafDisabled+InterpreterSaturation+age+white+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
 (age+white|scale),
  data=long)

summary(modSelect2 <- step(modLessFull))

summary(selected <- update(modLessFull,rating ~ `age/10` + white + Notetaking + (1 | id) + (white | scale)))

mods$selected <- selected
mods2$selected <- selected
names(mods) <- NULL
names(mods2) <- NULL

htmlreg(mods,file='regressions1.html')


mod1a <- lmer(rating~(1|id)+(`Interpreter Saturation`|scale)+`Interpreter Saturation`,data=long)

mod2 <- lmer(rating~(1|id)+(1|scale)+age,data=long)
mod2a <- lmer(rating~(1|id)+(age|scale)+age,data=long)

mod3 <- lmer(rating~(1|id)+(1|scale)+white,data=long)
mod3b <- lmer(rating~(1|id)+(white|scale)+white,data=long)

mod4 <-

  ## c(
      ##  'ruralUrban',
      ##  'ruralUrbanIP',
      ##  'Interpreter Saturation',
      ##  'demo68',
      ##  'demo67',
      ##  'demo65',
      ##  'Ethnicity',
      ##  'Disability',
      ##  'deafDisabled',
      ##  'HStype',
      ##  'MainstreamingHS',
      ##  'age',
      ##  'demo5',
      ##  'gender',
      ##  'demo26',
      ##  'HS Class Language',
      ##  'intSatCat',
      ##  preferredLanguageVarbs


