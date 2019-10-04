library(tidyverse)
source('code/cleanData.r')
library(ruca)
library(zipcode)
#library(rgeolocate)

only <-
   'both'
  ## 'new'
  ## 'old'

if(!exists('only')) only <- 'both'

if(only%in%c('old','new')){
  oldDat <- read.csv('data/POD Data 22March2019.csv',header=FALSE,stringsAsFactors=FALSE)
  varInfOld <- read.csv('data/varDesc725.csv',header=FALSE,stringsAsFactors=FALSE)
  varInfOld$V2[varInfOld$V2==''] <- 'pre'
  varNames <- character(nrow(varInfOld))
  for(cc in unique(varInfOld$V2))
    varNames[varInfOld$V2==cc] <- paste0(cc,seq(sum(varInfOld$V2==cc)))

  names(oldDat) <- tolower(varNames)
  names(oldDat)[1] <- 'id'
  oldDat <- subset(oldDat,id!='') ## remove NAs
  surv <- select(oldDat,id,tech1:scap10)
  surv[surv=='0'] <- NA
  surv[surv==''] <- NA

  for(i in 1:ncol(surv)) surv[surv[,i]%in%c('#N/A','NA','#VALUE!'),i] <- NA
  ## remove rows that are completely empty
  allmiss <- apply(surv,1,function(x) all(is.na(x[-1])))
  oldIDs <- oldDat$id[!allmiss]
  if(only=='old') dat <- dat[dat$id%in%oldIDs,]
  else if(only=='new') dat <- dat[!dat$id%in%oldIDs,]
}





#big3 <- dat$What.school.or.training.program.are.you.currently.attending.%in%c('Gallaudet University','Rochester Institute Technology','Calif St Univ Northridge')
#naSch ool <- dat$What.school.or.training.program.are.you.currently.attending.%in%c("#N/A","")

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

levs <- c('Not Likely','Somewhat Likely','Likely','Extremely likely')
levsAb <- c('NL','SL','L','EL')

summ <- list()
resp <- list()
cont <- list()
type <- c(type,rep('demo',ncol(dat)-length(type)))
for(cc in cats){
  cols <- names(dat)[type==cc]
  summ[[cc]] <- apply(dat[,cols],1,
    function(x) mean(x=='Extremely likely'|x=='Likely',na.rm=TRUE))
  cont[[cc]] <- rowMeans(surv[,cols],na.rm=TRUE)
  for(i in 1:4)
    resp[[paste0(cc,'.',levsAb[i])]] <- apply(dat[,cols],1,
                                                function(x) mean(x==levs[i],na.rm=TRUE))
}

summ <- as.data.frame(summ)
resp <- as.data.frame(resp)
cont <- as.data.frame(cont)



dat$ruralUrban <-
    dat%>%
        select(Zipcode)%>%
            getruca('Zipcode')%>%
                pull(classC)


for(dd in c('dat','acc','summ','resp','cont')){
  assign(paste0(dd,'3'),get(dd)[big3,])
  assign(dd,get(dd)[!big3,])
}



### location information from IP address
## location <- db_ip(as.character(dat$IP.Address),'free')
## save(location,file='artifacts/ipAdressLocation.RData')
## loc <- do.call('rbind',location[sapply(location,length)==4])
## loc <- as.data.frame(loc,stringsAsFactors=FALSE)
## instLoc <- dat%>%select(IP.Address,inst)%>%left_join(loc,by=c("IP.Address"="address"))

## ##
##load('ipAdressLocation.RData')

## for(i in which(sapply(location,length)<4))
##     location[[i]] <- rep(NA,4)

## location <- as.data.frame(do.call('rbind',location),stringsAsFactors=FALSE)
## location$state <- state.abb[match(location$stateprov,state.name)]
## location$state[location$stateprov=='District of Columbia'] <- 'DC'
## location$city[location$state=='DC'] <- 'Washington'
## location$city <- gsub(' \\(.+\\)','',location$city)

## data(zipcode,package='zipcode')
## zipcode$city <- tolower(zipcode$city)

## ruca2 <- read.csv('ruca.csv',colClasses=c('character','integer','numeric',NULL,NA))
## names(ruca2)[1] <- 'zip'

## location <-
##     location %>%
##         mutate(
##             zip=map_chr(
##                 1:n(),
##                 ~intersect(
##                     zipcode$zip[zipcode$state==state[.] & zipcode$city==tolower(city[.])],
##                     ruca2$zip
##                     )[1]
##                 )
##             )
## location$zip[location$city=='Windemere'&location$state=='TX'] <- 78660
## location$zip[location$city=='Tukwila'&location$state=='WA'] <- 98057
## location$zip[location$city=='Hopkinton'&location$state=='NH'] <- '03229'
## location$zip[location$city=='Boardman'&location$state=='OH'] <- 44512
## location$zip[location$city=='Temple Terrace Golfview'&location$state=='FL'] <- 33617
## location$zip[location$city=='St Louis'&location$state=='MO'] <- zipcode$zip[zipcode$state=='MO'&zipcode$city=='saint louis'][1]
## location$zip[location$city=='Gold Hill'&location$state=='CO'] <- 80302
## location$zip[location$city=='Highlands Ranch'&location$state=='CO'] <- 80124
## location$zip[location$city=='Burien'&location$state=='WA'] <- 98062
## location$zip[location$city=='Town and Country'&location$state=='MO'] <- 63011

## location$ruca <- ruca2[['RUCA2.0']][match(location$zip,ruca2$zip)]

## location <- left_join(location,select(classifications,ruca,classC))

## location$classC[location$city%in%c('Barcelona','Mexico City','Hamilton')] <- 'Urban'

## dat$ruralUrbanIP <- location$classC


dat$Accrediation[dat$Accrediation=='#N/A'] <- NA
intSat <- quantile(dat$Interpreter.Saturation,c(.33,.66),na.rm=TRUE)
## instead do 0-50, 51-125, 126+
dat[['Interpreter Saturation']] <- cut(dat$Interpreter.Saturation,c(-1,50,125,Inf),labels=c('low (0-50)','med (51-125)','high (126+)'),ordered=TRUE)
dat[['Interpreter Saturation']][acc$Interpreters!=1] <- NA

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

crossTabs <- tibble(Table='Overall',Subgroup='',n=nrow(dat),`%`=100)
crossTabs <- cbind(crossTabs,matrix(colMeans(cont,na.rm=TRUE),nrow=1))
names(crossTabs)[5:ncol(crossTabs)] <- names(cont)

oneRow <- function(newTab,vv,memb)
    do.call('add_row',
            append(list(.data=newTab),
                   c(Subgroup=vv,
                     n=sum(memb,na.rm=TRUE),
                     `%`=round(mean(memb,na.rm=TRUE)*100),
                     as.list(colMeans(cont[memb,],na.rm=TRUE)))))

addVarbSimp <- function(varb){
    newTab <- crossTabs[0,]

    vvv <- dat[[varb]]
    vvv[vvv=='NA'] <- NA
    vvv[vvv=='#N/A'] <- NA

    newTab <- add_row(newTab,
                         Table=paste0(varb,#ifelse(varb%in%rownames(varInf),varInf[varb,'V3'],varb),
                             ' (',sum(is.na(vvv)),' NAs = ',round(mean(is.na(vvv))*100),'%)'))

    levs <- if(is.factor(vvv)) levels(vvv) else unique(na.omit(vvv))

    for(vv in levs)
        newTab <- oneRow(newTab,vv,vvv==vv)
    newTab
}

crossTabs <- add_row(crossTabs,Table=paste0('Disability (not mut. exclusive; ',sum(is.na(dis[,1])),' NAs = ',round(mean(is.na(dis[,1]))*100),'%)'))
dis <- dis[!big3,]
for(i in 1:ncol(dis))
    crossTabs <- do.call('add_row',
                          append(list(.data=crossTabs),
                                 c(Subgroup=names(dis)[i],
                                   n=sum(dis[[i]],na.rm=TRUE),
                                   `%`=round(mean(dis[[i]],na.rm=TRUE)*100),
                                   as.list(colMeans(cont[dis[[i]]==1,],na.rm=TRUE)))))


crossTabs <- bind_rows(
  crossTabs,
  map_dfr(
      c(
       #'ruralUrban',
       #       'ruralUrbanIP',
          'Institution.Type',
          'College.Ranking',
       'deafDisabled',
        'Interpreter Saturation',
       'Accrediation',
       'Community.College',
       'Institution.Size',
       'Ethnicity',
       #'Disability',
       'HStype',
       'MainstreamingHS',
       'age',
       'Were.you.born.in.the.United.States.',
       'gender',
       'Did.you.complete.high.school.',
       'HS Class Language',
       #'intSatCat',
       preferredLanguageVarbs
       ),
      addVarbSimp)
    )


crossTabs <- add_row(crossTabs,Table='Accomodations')
acc$`None/NA`=as.numeric(rowSums(acc)==0)
for(i in 1:ncol(acc))
    crossTabs <- do.call('add_row',
                          append(list(.data=crossTabs),
                                 c(Subgroup=names(acc)[i],
                                   n=sum(acc[[i]],na.rm=TRUE),
                                   `%`=round(mean(acc[[i]],na.rm=TRUE)*100),
                                   as.list(colMeans(cont[acc[[i]]==1,],na.rm=TRUE)))))

### interpreters
crossTabs <- add_row(crossTabs,Table='Accomodation: Interpreter')%>%
  oneRow('Interpreting all',acc$Interpreters==1)%>%
  oneRow('Interpreting + notetaking', acc$Interpreters&acc$Notetaking)%>%
  oneRow('Interpreting + Extended time',acc$Interpreters&acc$ExtendedTesttime)%>%
  oneRow('Interpreting + notetaking + extended time',acc$Interpreters&acc$ExtendedTesttime&acc$Notetaking)



### speech to tex
crossTabs <- add_row(crossTabs,Table='Accomodation: Speech-to-Text')%>%
    oneRow('Speech to text all',acc$`Speech-to-Text`==1)%>%
    oneRow('Speech to text + notetaking', acc$`Speech-to-Text`&acc$Notetaking)%>%
    oneRow('Speech to text + Extended time',acc$`Speech-to-Text`&acc$ExtendedTesttime)%>%
    oneRow('Speech to text + notetaking + extended time',acc$`Speech-to-Text`&acc$ExtendedTesttime&acc$Notetaking)


### assistive listening
crossTabs <- add_row(crossTabs,Table='Accomodation: Assistive Listening')%>%
    oneRow('Assistive Listening all',acc$AssistiveListeningDevice==1)%>%
    oneRow('Assistive Listening + notetaking', acc$AssistiveListeningDevice&acc$Notetaking)%>%
    oneRow('Assistive Listening + Extended time',acc$AssistiveListeningDevice&acc$ExtendedTesttime)%>%
    oneRow('Assistive Listening + notetaking + extended time',acc$AssistiveListeningDevice&acc$ExtendedTesttime&acc$Notetaking)



crossTabs <- add_row(crossTabs,Table='Big 3 Schools')
crossTabs <- do.call('add_row',
                          append(list(.data=crossTabs),
                                 c(n=sum(big3),
                                   `%`=round(mean(big3)*100),
                                   as.list(colMeans(cont3,na.rm=TRUE)))))




crossTabs <- add_row(crossTabs,
                     Table='Excludes Big 3 schools (except where noted)')
crossTabs <- add_row(crossTabs,
                     Table='"Accomodation" & "Disability" subgroups are NOT mutually exclusive')


#demographics: age, not born in US, gender, HS/GED, preferred language (are there any left??)
#POD category rating by:
##interpreter v other, preferred language sign v spoken language, interpreter saturation (0-49, 50-99, 100)


## rural urban
## use "ruca" package
## install.packages("remotes")
## remotes::install_github("jbryer/ruca")

suffix <- if(only!='both') only else ''
write.csv(crossTabs,paste0('results/crossTabs1to4',suffix,'.csv'))
openxlsx::write.xlsx(crossTabs,paste0('results/crossTabs1to4',suffix,'.xlsx'),row.names=FALSE,col.names=TRUE)

