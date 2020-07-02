select <- dplyr::select
source('code/cleanData.r')

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
ddd$ageCat <- ddd$age
ddd$age <- as.numeric(dat$Age) ## continuous
ddd$IntSatCont <- dat$Interpreter.Saturation

ddd[ddd=='NA'] <- NA

regDat <- bind_cols(cont,ddd)
## add some more/simplify predictors

regDat$white <- startsWith(regDat$Ethnicity,'White')
regDat$white[regDat$Ethnicity=='NA'] <- NA
regDat$mainstreamingHS <- regDat$MainstreamingHS=='mainstream only'
regDat$mainstreamingHS[regDat$MainstreamingHS=='NA'] <- NA

regDat$Community.College <- dat$Community.College
regDat$Were.you.born.in.the.United.States. <- dat$Were.you.born.in.the.United.States.
regDat$Institution.Type <- dat$Institution.Type



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

regDat <- left_join(regDat,dplyr::select(accomodations,-AssistiveListeningDevice,-`Other`))

### deal with NAs
nad <- select(regDat,deafDisabled,age,gender,`Interpreter Saturation`,white,deafHS,deafHSprog,Interpreters,
  `Speech-to-Text`,Notetaking,ExtendedTesttime)
nad[nad=='NA'] <- NA
nnn <- apply(nad,1,function(x) sum(is.na(x)))
table(nnn)
sapply(nad[which(nnn==7),],function(x) sum(is.na(x))) ##
#regDat <- regDat[nnn<8,]

select(regDat,deafDisabled,age,gender,`Interpreter Saturation`,white,deafHS,deafHSprog,Interpreters,
  `Speech-to-Text`,Notetaking,ExtendedTesttime)%>%
  map_dbl(~sum(is.na(.)))

## cat(
##   ' ----------------------------------\n',
##   'dropping ',nrow(dat)-nrow(regDat),' people due to too much missingness\n',
##   '----------------------------------\n'
## )

names(regDat) <- gsub(' |-','',names(regDat))

regDat[regDat=='NA'] <- NA

for(i in 1:ncol(regDat))
  if(is.character(regDat[[i]])|is.logical(regDat[[i]])|n_distinct(regDat[[i]])<6)
    regDat[[i]] <- as.factor(regDat[[i]])

idVars <- select(regDat,id,inst)
regDat <- select(regDat,-id,-inst)

regDat$female <- regDat$gender=='Female'
regDat <- select(regDat,-gender)
## pred <- matrix(1,ncol(regDat),ncol(regDat))
## rownames(pred) <- colnames(pred) <- colnames(regDat)
## pred[,'id'] <- 0
## pred['id',] <- 0
## pred['inst',] <- pred[,'inst'] <- 0
## diag(pred) <- 0

impDat <- mice(regDat,m=20)#,method='rf')

save(impDat,regDat,dat,acc,idVars,file='data/dataForRegression.RData')
#load('impDat.Rdata')
