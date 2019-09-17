library(tidyverse)
library(mirt)

source('cleanData.r')

big3 <- dat$demo46%in%c('Gallaudet University','Rochester Institute Technology','Calif St Univ Northridge')
naSchool <- dat$demo46%in%c("#N/A","")

survBin2 <- survBin[!big3,2:39]
mirtMod <- ''
for(cc in setdiff(tolower(unique(varInf$category)),c('pre','demo')))
  mirtMod <- paste(mirtMod,cc,'=',paste(grep(cc,names(survBin2)),collapse=','),'\n')
mirtMod <- paste(mirtMod,'COV=',paste(setdiff(tolower(unique(varInf$category)),c('pre','demo')),collapse='*'),'\n')


mirt1 <- mirt(survBin2,mirtMod,method='MCEM')

mirt2 <- mirt(survBin2,mirtMod,method='QMCEM',itemtype='2PL')
save(mirt2,file='mirt2.RData')

m2.2 <- M2(mirt2,impute=10,QMC=TRUE)
if2 <- itemfit(mirt2,impute=10,QMC=TRUE)

mirt3 <- mirt(survBin2,mirtMod,method='QMCEM',itemtype='spline')

bfModel <-  as.numeric(droplevels(varInf$category[!varInf$category%in%c('pre','DEMO')]))
bifac <- bfactor(survBin2,bfModel,itemtype='2PL')

