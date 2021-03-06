library(tidyverse)
library(reshape2)
library(httr)
library(jsonlite)

#library(plotrix)
varInf <- read.csv('data/varDesc725.csv',header=FALSE,stringsAsFactors=FALSE)
dat <- read.csv('data/POD9-1-19.csv',stringsAsFactors=FALSE,skip=1)
dat <- dat[,!vapply(dat,function(x) all(is.na(x)),TRUE)]

varInf$V2[varInf$V2==''] <- 'pre'

names(dat)[1] <- 'id'

dat <- subset(dat,id!='') ## remove NAs


type <- tolower(varInf$V2)
cats <- c("tech","atti","comm","phys","serv","scap")
### concise variable names
## nnn <- strsplit(names(dat),'.',fixed=TRUE)
## nnn <- c(nnn[[1]],sapply(nnn[-1],function(x) ifelse(length(x)==1,paste0(x[1],1),paste0(x[1],as.numeric(x[2])+1))))

#names(dat) <- nnn

## survey responses

surv <- dat[,c(1,which(type%in%cats))]
surv[surv=='0'] <- NA
surv[surv==''] <- NA

for(i in 1:ncol(surv)) surv[surv[,i]%in%c('#N/A','NA','#VALUE!'),i] <- NA

stopifnot(
  setequal(
    tolower(unique(unlist(surv[,-1]))),
    tolower(c( "Likely","Extremely likely","Not Likely","Somewhat Likely",NA))
  )
)

## remove rows that are completely empty
#dat[dat==''] <- NA
allmiss <- apply(surv,1,function(x) all(is.na(x[-1])))

missingConsent <- dat[dat$Consent==''&!allmiss,c(1,which(names(dat)=='Consent'),which(type%in%cats))]
rownames(missingConsent) <- as.character(as.numeric(rownames(missingConsent))+2)
write.csv(
  missingConsent,
  'results/missingConsent.csv')


cat('----------------------------\nDropping ',sum(allmiss),' rows all NA\n-------------------------------\n')

surv <- surv[!allmiss,]
dat <- dat[!allmiss,]

### multiple spaces-> single spaces in every character variable
for(i in 1:ncol(dat))
  if(is.character(dat[,i]))
    dat[,i] <- gsub(' +',' ',dat[,i])

dat[dat==''] <- NA
dat[dat==' '] <- NA

## NA patterns: do people just stop responding, or do they pick and choose which Qs to respond to?


dat%>%
  `names<-`(1:ncol(dat))%>%
  mutate(rowNum=rank(apply(dat,1,function(x) sum(is.na(x))),ties='first'))%>%
  melt('rowNum')%>%
  mutate(na=factor(is.na(value)))%>%
  ggplot(aes(variable,rowNum,fill=na))+
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(trans='reverse')

ggsave('graphics/NAplot.pdf')

naPat <- function(x){
  x[x==''|x==' '] <- NA
  lastObs <- max(which(!is.na(x)))
  NAs <- which(is.na(x))
  nNA <- length(NAs)
  nAfter <- sum(NAs>lastObs)
  nBefore <- sum(NAs<lastObs)
  pAfter <- ifelse(nNA,nAfter/nNA,0)
  c(nNA=nNA,lastObs=lastObs,nAfter=nAfter,nBefore=nBefore,pAfter=pAfter)
}

naPattern <- apply(dat,1,naPat)

## it looks like people don't just stop responding




### make race variable
race <- select(dat,American.Indian.or.Alaska.Native:White)
for(i in 1:ncol(race)) race[,i] <- as.numeric(!is.na(race[,i]))

dat$Ethnicity <- apply(race,1,
                       function(x) ifelse(sum(x)==0,'NA',
                                          ifelse(sum(x)>1,'Multiracial',
                                                 names(race)[which(x==1)])))

### make disability variable
dis <- dat[,grep('DA[1-9]',names(dat))]#

dis <- map_dfc(dis,function(x)
  x%>%#[,i]%>%
    gsub('Sensory (visual)','Visual',.,fixed=TRUE)%>%
    gsub('Sensory (hearing)','Hearing',.,fixed=TRUE)%>%
    gsub(' |\\(.+\\)','',.)
)

names(dis) <-
  sapply(dis,function(x) unique(na.omit(x))[1])

for(i in 1:ncol(dis)) dis[,i] <- as.numeric(!is.na(dis[,i]))

dis$Other <- as.numeric(!is.na(dat$A.disability.not.listed))

dat$deafDisabled <- rowSums(select(dis,-Hearing))>0

dis$Multi <- as.numeric(rowSums(select(dis,-Hearing))>1)

dis$deafOnly <- as.numeric(!dat$deafDisabled)

## #dat$Disability <- apply(select(dis,-Hearing),1,
##                        function(x) ifelse(sum(x)==0,'none (i.e. deaf non-disabled, excl. NAs)',
##                                           ifelse(sum(x)>1,'Multi',names(x)[which(x==1)])))
## ttt <- table(dat$Disability)
## dat$Disability[dat$Disability%in%c(names(ttt)[ttt<5],'A disability not listed')] <- 'Other'


### if all disability variables are NA, make the whole row of dis NA
dis[
  apply(
    select(dat,Do.not.Identify.w.a.disability:DisabiltyDescribe),
    1,
    function(x) all(is.na(x))
  )
,] <- NA




#                           ifelse(dat$Disability=='NA','NA',



### make HS type variables
#hsTypeV <- rownames(varInf)[grep('type of high school',varInf$V3)]
hsType <- select(dat, Deaf.School..or.school.with.all.deaf.students.:HSOther) #dat[,hsTypeV[-length(hsTypeV)]]

names(hsType) <- sapply(hsType,function(x) unique(na.omit(x))[1])
for(i in 1:ncol(hsType)) hsType[,i] <- as.numeric(!is.na(hsType[,i]))

dat$HStype <- apply(hsType,1,
                       function(x) ifelse(sum(x)==0,'NA',
                                          ifelse(sum(x)>1,
                                                 'Multi',names(hsType)[which(x==1)])))

dat$mainstream <- rowSums(hsType[,grep('Mainstreamed',names(hsType))])
dat$deafonly <- rowSums(hsType[,grep('Deaf',names(hsType))])

dat <- within(dat,
  MainstreamingHS <- ifelse(mainstream>0,
    ifelse(deafonly>0,'both','mainstream only'),
    ifelse(deafonly>0,'deaf only',
      ifelse(dat$HStype=='NA','NA','other'))))



### accomodations
acc <- dat%>%select(starts_with('ACC.'))%>%select(-ends_with('describe'))%>%
  map_dfc(~gsub(' |\\(.+\\)','',.))

names(acc) <- sapply(acc,function(x) unique(na.omit(x))[1])

for(i in 1:ncol(acc)) acc[,i] <- as.numeric(!is.na(acc[,i]))

accomodations <- cbind(id=dat$id,acc)

write.csv(accomodations,'results/accomodations.csv',row.names=FALSE)


for(dd in which(names(dat)=="I.identify.my.gender.as...select.one."):ncol(dat))
  if(is.character(dat[,dd]))
        dat[is.na(dat[,dd]),dd] <- 'NA'


## age categories
dat <- mutate(dat,age=ifelse(Age=='#VALUE!',NA,
          ifelse(Age<23,'19-22',ifelse(Age<30,'23-29',ifelse(Age<40,'30-39','40+')))))

## gender
dat <- dat%>%
  rename(Gender=I.identify.my.gender.as...select.one.)%>%
  mutate(gender=ifelse(Gender=='NA',NA,ifelse(Gender%in%c('Male','Female'),Gender,'Other')))


## hs language
hsLang <- dat%>%
  select(starts_with('HS.'))%>%
  select(-starts_with('HS.English'))%>%
  map_dfc(~gsub('(My classes were taught in )|\\(.+\\)','',.))%>%
  map_dfc(~gsub('and I had a(n?)','with',.))

hsLang[hsLang=='NA'] <- NA


names(hsLang) <- sapply(hsLang,function(x) unique(na.omit(x))[1])
for(i in 1:ncol(hsLang)) hsLang[,i] <- as.numeric(!is.na(hsLang[,i]))

names(hsLang) <- gsub('Another.+','Other',names(hsLang))

dat$hsLang <- NA
dat$hsLang[rowSums(hsLang)==0] <- NA
dat$hsLang[rowSums(hsLang)==1] <- names(hsLang)[sapply(which(rowSums(hsLang)==1),function(i) which(hsLang[i,]==1))]
dat$hsLang[rowSums(hsLang)>1] <- 'Multiple'

#dat$hsLang[grep('Another',dat$hsLang)] <- 'English+Other Acc'
#dat$hsLang[grep('provider',dat$hsLang)] <- 'English+Other Acc'

### write-ins
## dat$hsLang[grep('Full',dat$hsLang)] <- 'sign language.'
## dat$hsLang[grep('hear better',dat$hsLang)] <- 'English+no accommodations.'
## dat$hsLang[grep('Bi-Bi|one asl',dat$hsLang)] <- 'Multiple'

dat <- rename(dat,`HS Class Language`=hsLang)

### preferred language
prefLangV <- grep('.Language..',names(dat),fixed=TRUE)#rownames(varInf)[grep('preferred language',varInf$V3)]

for(vv in prefLangV)
  dat[[vv]] <- ifelse(dat[[vv]]=='NA',NA,
    ifelse(dat[[vv]]=='ASL','ASL',
      ifelse(dat[[vv]]=='Spoken English','Spoken English',
        ifelse(dat[[vv]]=='Written/Text Communication','Written/Text Communication', 'Other'))))

prefLang <- strsplit(names(dat)[prefLangV],'\\.{2,3}')
names(dat)[prefLangV] <- paste0('Pref.',map_chr(prefLang,~paste0(.[1],'.',.[3])))


### clean up institutition a little
dat$inst <- dat$What.school.or.training.program.are.you.currently.attending.
dat$inst[dat$inst%in%c('NA','#N/A','N/a','Na','nope')] <- NA

## ### look up locations from IP addresses
## #ipInfo <- map(as.character(dat$IP.Address),~fromJSON(paste0("https://ipinfo.io/",.,'?token=36ab4f8eb7c36d')))
## ## ipInfo <- map(1:nrow(dat),function(x) list())
## ## for(i in 1:nrow(dat)){
## ##   if(i%%10==0) cat(i,' ')
## ##   if(i%%100==0) cat('\n')
## ##   ipInfo[[i]] <- try(fromJSON(paste0("https://ipinfo.io/",as.character(dat$IP.Address[i]),'?token=36ab4f8eb7c36d')))
## ## }
## ## save(ipInfo,file='artifacts/ipInfo.RData')
## orgs <- map_chr(ipInfo,~ifelse(inherits(.,'try-error'),NA,.$org))

## print(sum(is.na(dat$inst)))
## for(i in 1:nrow(dat))
##   if(is.na(dat$inst[i]))
##     if(!inherits(ipInfo[[i]],'try-error'))
##       if(grepl('University|Institute|College',ipInfo[[i]]$org,ignore.case=TRUE))
##         dat$inst[i] <- gsub('A.*? (.+)','\\1',ipInfo[[i]]$org)
## print(sum(is.na(dat$inst)))



write.csv(dat,'data/cleanedData.csv',row.names=FALSE)


### print summary. want factors instead of character so that it prints (some) levels
dat2 <- dat
dat2[,sapply(dat2,is.character)] <- lapply(dat2[,sapply(dat2,is.character)],as.factor)

sink('fullSummary.txt')
print(summary(dat2))
sink()

big3 <- dat$inst%in%c('Gallaudet University','Rochester Institute Technology','Calif St Univ Northridge','Rochester Institute of Technology','RIT')

sink('big3Summary.txt')
print(summary(dat2[big3,]))
sink()

sink('noBig3Summary.txt')
print(summary(dat2[!big3,]))
sink()

rm(dat2)


## make into ordered variables with levels 1,2,3,4
levs <- c(`Not Likely`=1,`Somewhat Likely`=2,`Likely`=3,`Extremely likely`=4)

survNum <- NULL
for(i in 2:ncol(surv))
    survNum <- cbind(survNum,
                     levs[as.character(surv[,i])])

survNum <- as.data.frame(survNum)
survNum <- cbind(surv$id,survNum)
names(survNum) <- names(surv)
surv <- survNum

write.csv(surv,'data/survNumeric.csv',row.names=FALSE)






### doubled descriptions
## varInf$V3 <- vapply(strsplit(as.character(varInf$V3),'\\.[A-Z]'),function(x) x[1],'a')
## varInf$V3 <- vapply(strsplit(varInf$V3,'What'),function(x) ifelse(length(x)==1,x[1],paste('What',x[2])),'a')
## varInf$V3 <- vapply(strsplit(varInf$V3,'\\)[A-Z]'),function(x) x[1],'a')
## varInf$V3 <- vapply(strsplit(varInf$V3,'\\?[A-Z]'),function(x) x[1],'a')
## varInf$V3 <- vapply(strsplit(varInf$V3,'Which'),function(x) ifelse(length(x)==1,x[1],paste('Which',x[2])),'a')
## varInf$V3 <- vapply(strsplit(varInf$V3,'How'),function(x) ifelse(length(x)==1,x[1],paste('How',x[2])),'a')
## varInf$V3 <- vapply(strsplit(varInf$V3,'I identify'),function(x) ifelse(length(x)==1,x[1],paste('I identify',x[2])),'a')

## write.csv(varInf,'cleanedVarInf.csv',row.names=TRUE)


## ### dichotomized version
## survBin <- surv%>%mutate_at(vars(-id),function(x) as.numeric(x>2))

## write.csv(survBin,'survBin.csv',row.names=FALSE)


