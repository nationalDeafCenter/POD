library(tidyverse)
source('code/cleanData.r')
library(ruca)
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

acc <- acc[!big3,]

levs <- c('Not Likely','Somewhat Likely','Likely','Extremely likely')
levsAb <- c('NL','SL','L','EL')

summ <- list()
resp <- list()
type <- c(type,rep('demo',ncol(dat)-length(type)))
for(cc in cats){
    summ[[cc]] <- apply(dat[,type==cc],1,
                        function(x) mean(x=='Extremely likely'|x=='Likely',na.rm=TRUE))
  for(i in 1:4)
      resp[[paste0(cc,'.',levsAb[i])]] <- apply(dat[,type==cc],1,
                                                function(x) mean(x==levs[i],na.rm=TRUE))
}

summ <- as.data.frame(summ)
resp <- as.data.frame(resp)

dat$ruralUrban <-
    dat%>%
        select(Zipcode)%>%
            getruca('Zipcode')%>%
                pull(classC)




### location information from IP address
## location <- db_ip(as.character(dat$pre2),'free')
## save(location,file='ipAdressLocation.RData')
## load('ipAdressLocation.RData')

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
crossTabs <- cbind(crossTabs,matrix(colMeans(summ,na.rm=TRUE),nrow=1))
names(crossTabs)[5:ncol(crossTabs)] <- names(summ)

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
        newTab <- do.call('add_row',
                             append(list(.data=newTab),
                                    c(Subgroup=vv,
                                      n=sum(vvv==vv,na.rm=TRUE),
                                      `%`=round(mean(vvv==vv,na.rm=TRUE)*100),
                                      as.list(colMeans(summ[vvv==vv,],na.rm=TRUE)))))
    newTab
}

crossTabs <- bind_rows(
  crossTabs,
  map_dfr(
      c(
       #'ruralUrban',
       #       'ruralUrbanIP',
       #'Institution.Type',
       'Interpreter Saturation',
       'Accrediation',
       'Community.College',
       'Institution.Size',
       'Ethnicity',
       'Disability',
       'deafDisabled',
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
                                   as.list(colMeans(summ[acc[[i]]==1,],na.rm=TRUE)))))

if(length(type)<ncol(dat)) type <- c(type,rep('demo',ncol(dat)-length(type)))
summ3 <- as.data.frame(
  sapply(
    cats,
    function(cc)
      apply(
        dat3[,type==cc],
        1,
        function(x) mean(x=='Extremely likely'|x=='Likely',na.rm=TRUE)
      ),
    simplify=FALSE
  )
)

crossTabs <- add_row(crossTabs,Table='Big 3 Schools')
crossTabs <- do.call('add_row',
                          append(list(.data=crossTabs),
                                 c(n=sum(big3),
                                   `%`=round(mean(big3)*100),
                                   as.list(colMeans(summ3,na.rm=TRUE)))))




crossTabs <- add_row(crossTabs,
                     Table='Excludes Big 3 schools (except where noted)')
crossTabs <- add_row(crossTabs,
                     Table='"Accomodation" subgroups are NOT mutually exclusive')


#demographics: age, not born in US, gender, HS/GED, preferred language (are there any left??)
#POD category rating by:
##interpreter v other, preferred language sign v spoken language, interpreter saturation (0-49, 50-99, 100)


## rural urban
## use "ruca" package
## install.packages("remotes")
## remotes::install_github("jbryer/ruca")

suffix <- if(only!='both') only else ''
write.csv(crossTabs,paste0('results/crossTabs',suffix,'.csv'))
openxlsx::write.xlsx(crossTabs,paste0('results/crossTabs',suffix,'.xlsx'),row.names=FALSE,col.names=TRUE)


#################### accomodations cross tab
accCTp <- accCTn <- acc[0,]
accCTp['nReceiving',] <- accCTn['nReceiving',] <- colSums(acc)

for(nn in names(acc)){
  accCTp <- rbind(accCTp, map_dbl(names(acc),~mean(acc[[nn]][acc[[.]]==1])))
  accCTn <- rbind(accCTn, map_dbl(names(acc),~sum(acc[[.]][acc[[nn]]==1])))
  rownames(accCTn)[nrow(accCTn)] <- rownames(accCTp)[nrow(accCTp)] <- nn
}


#### does the same stuff:
accCTn <- crossprod(as.matrix(acc))
accCTp <- sweep(accCTn,2,diag(accCTn),'/')
accCTp <- rbind(nReceiving=diag(accCTn),accCTp)
accCTn <- rbind(nReceiving=diag(accCTn),accCTn)

for(i in 1:(max(rowSums(acc))-1)){
  accCTp <- rbind(accCTp,map_dbl(1:ncol(acc),~mean(rowSums(acc[acc[,.]==1,-.])>=i)))
  rownames(accCTp)[nrow(accCTp)] <- paste0('prop.AtLeast',i,'addnlAcc')
  accCTn <- rbind(accCTn,map_dbl(1:ncol(acc),~sum(rowSums(acc[acc[,.]==1,-.])>=i)))
  rownames(accCTn)[nrow(accCTn)] <- paste0('atLeast',i,'addnlAcc')
}

if(only=='both'){
  write.csv(accCTp,'results/accCTp.csv')
  write.csv(accCTn,'results/accCTn.csv')
}
