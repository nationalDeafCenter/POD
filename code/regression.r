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



## res <- mi(varbs,impDat)
## intSat <- mi('InterpreterSaturation',impDat,subst="Interpreters==1")
## res$InterpreterSaturation <- intSat[[1]]

## ## #mods <- map(res,~if('components'%in%names(attributes(.))) attr(.,'components')[[1]]$models$tp else attr(.,'models')$tp)
## ## coefs <- map(res,~.$Estimate)
## ## ses <- map(res,~.[['Std. Error']])
## ## pvals <- map(res,~.[['Pr(>|t|)']])

## trobj <- map(res, function(rrr){
##   rrr <- rrr[-grep(':',rownames(rrr),fixed=TRUE),]
##   createTexreg(rownames(rrr),rrr$Estimate,rrr[['Std. Error']],rrr[['Pr(>|t|)']])
## }
## )


## htmlreg(trobj,#mods,override.coef=coefs,override.se=ses,override.pvalues=pvals,
##   file=paste0('PODregressions',Sys.Date(),'.html'))





ethMods <- lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+(1|Ethnicity)+(1|Ethnicity:scale),data=dd))

ageMods <- lapply(longDats,function(dd) lmer(rating~(1|id)+(1|scale)+(1|ageCat)+(1|ageCat:scale),data=dd))

disMods <- lapply(longDats,function(dd) lmer(rating~(1|id)+(deafDisabled|scale)+deafDisabled,data=dd))
hsMods <- lapply(longDats,function(dd) lmer(rating~deafHS+deafHSprog+(1|id)+(deafHS+deafHSprog|scale),data=dd))
accMods <- lapply(longDats,function(dd)
  lmer(rating~Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
         (1|id)+(Interpreters+SpeechtoText+Notetaking+ExtendedTesttime|scale),data=dd))

### multiple
modLessFull <- lapply(
  longDats,
  function(dd)
    lmer(rating~
           deafDisabled+deafHS+deafHSprog+Interpreters+SpeechtoText+Notetaking+ExtendedTesttime+
           (1|id)+(1|ageCat)+(1|Ethnicity)+(1|scale)+(1|Ethnicity:scale)+(1|ageCat:scale),
           data=dd)
  )



regDat$deafSchoolEntryAge <- scale(dat$What.year.did.you.first.attend.a.Deaf.School.-dat$What.year.were.you.born.,center=TRUE,scale=FALSE)

years <- lmer(rating~deafSchoolEntryAge+(1|id)+(deafSchoolEntryAge|scale),data=gather(cbind(regDat,idVars),"scale","rating",tech:scap))

intSat <-
  lmer(rating~log(IntSatCont+1)+(1|id)+(log(IntSatCont+1)|scale),data=gather(cbind(regDat,idVars),"scale","rating",tech:scap),subset=Interpreters==1)


trObjs <- list()#lapply(
for(mm in c('disMods','hsMods','accMods','ethMods','ageMods','modLessFull','years','intSat'))#,fullTrObj)
  trObjs[[mm]] <- fullTrObj(get(mm))
screenreg(trObjs)


htmlreg(trObjs,file=paste0('results/regressions',Sys.Date(),'.html'),custom.model.names=c('disability','hs type','accomodations','ethnicity','age','combined','deaf school entry age','interpreter saturation'),
  caption='Regressors are centered so that (Intercept) can be interpreted as overall mean; categorical regressosors are models as random intercepts, and named as variable::category (so, e.g., scale::atti is the random intercept for observations of the atti scale); all models except "deaf school entry year" and "interpreter saturation" were fit using multiple imputation for missing values; "deaf school entry year" was fit to the subset of students with an entry year listed, and "interpreter saturation" was fit to the subset of students who reported using interpreters and who reported their institution')
