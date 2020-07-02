library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)
library(texreg)
select <- dplyr::select

#source('code/regDatPrep.r')
load('data/dataForRegression.RData')

source('code/regFunctions.r')

longDats <- transMIdat(impDat,id=idVars$id)


models <- list(
  A=lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+`age/10`,data=dd)),
  B=lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+white,data=dd)),
  C=lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+female,data=dd)),
  D=lapply(longDats,function(dd)
    lmer(rating~(1|id)+(1|scale)+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime,data=dd)),
  E=lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+deafDisabled,data=dd)),
  F=lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+Community.College,data=dd))
  )


models <- sapply(models,fullTrObj,simplify=FALSE)

htmlreg(models,file='results/simpleRegressions.html')
