library(tidyverse)
library(reshape2)
#library(plotrix)
varInf <- read.csv('varDesc725.csv',header=FALSE,stringsAsFactors=FALSE)
dat <- read.csv('POD Data 22March2019.csv',header=FALSE,stringsAsFactors=FALSE)

varInf$V2[varInf$V2==''] <- 'pre'
varNames <- character(nrow(varInf))
for(cc in unique(varInf$V2))
  varNames[varInf$V2==cc] <- paste0(cc,seq(sum(varInf$V2==cc)))

names(dat) <- tolower(varNames)

names(dat)[1] <- 'id'

rownames(varInf) <- names(dat)

dat <- subset(dat,id!='') ## remove NAs

### concise variable names
## nnn <- strsplit(names(dat),'.',fixed=TRUE)
## nnn <- c(nnn[[1]],sapply(nnn[-1],function(x) ifelse(length(x)==1,paste0(x[1],1),paste0(x[1],as.numeric(x[2])+1))))

#names(dat) <- nnn

## survey responses
surv <- select(dat,id,tech1:scap10)
surv[surv=='0'] <- NA
surv[surv==''] <- NA

for(i in 1:ncol(surv)) surv[surv[,i]%in%c('#N/A','NA','#VALUE!'),i] <- NA



## remove rows that are completely empty
allmiss <- apply(surv,1,function(x) all(is.na(x[-1])))

surv <- surv[!allmiss,]
dat <- dat[!allmiss,]

for(i in 1:ncol(dat))
  if(is.character(dat[,i]))
    dat[,i] <- gsub(' +',' ',dat[,i])

dat[dat==''] <- NA
dat[dat==' '] <- NA

## NA patterns: do people just stop responding, or do they pick and choose which Qs to respond to?


dat%>%
  mutate(rowNum=1:n())%>%
  melt('rowNum')%>%
  mutate(na=factor(is.na(value)))%>%
  ggplot(aes(variable,rowNum,fill=na))+
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(trans='reverse')

ggsave('NAplot.pdf')

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
race <- dat[,rownames(varInf)[grep('race/ethnicity',varInf$V3,fixed=TRUE)]]
names(race) <- sapply(race,function(x) unique(na.omit(x[x!='NA']))[1])
for(i in 1:ncol(race)) race[,i] <- as.numeric(!is.na(race[,i]))

dat$Ethnicity <- apply(race,1,
                       function(x) ifelse(sum(x)==0,'NA',
                                          ifelse(sum(x)>1,'Multiracial',
                                                 names(race)[which(x==1)])))

### make disability variable
dis0 <- dat[,#grep('dis',names(dat))]#
  paste0('demo',c(17:24))]
names(dis0) <- #gsub(' | \\(.+\\)','',
  sapply(dis0,function(x) unique(na.omit(x))[1])
#)
for(i in 1:ncol(dis0)) dis0[,i] <- as.numeric(!is.na(dis0[,i]))

dis1 <- dat[,grep('dis',names(dat))]
names(dis1) <- #gsub(' | \\(.+\\)','',
  sapply(dis1,function(x) unique(na.omit(x))[1])
#)
for(i in 1:ncol(dis1)) dis1[,i] <- as.numeric(!is.na(dis1[,i]))

ccc <- crossprod(as.matrix(dis0),as.matrix(dis1))
ccc <- cbind(ccc,Total=colSums(dis0))
ccc <- rbind(ccc,Total=c(colSums(dis1),NA))
openxlsx::write.xlsx(ccc,'oldDisabilityVsNewDisability2.xlsx',row.names=TRUE,colnames=TRUE)


dis <- dis0[,-c(1,3)]

dat$Disability <- apply(dis,1,
                       function(x) ifelse(sum(x)==0,'none',
                                          ifelse(sum(x)>1,'Multi',names(dis)[which(x==1)])))
ttt <- table(dat$Disability)
dat$Disability[dat$Disability%in%c(names(ttt)[ttt<5],'A disability not listed')] <- 'Other'

dat$Disability[dat$Disability=='none'&is.na(dat$demo17)&is.na(dat$demo19)] <- 'NA'


dat$deafDisabled <- ifelse(dat$Disability=='none','Deaf-Nondisabled','Deaf-Disabled')
#                           ifelse(dat$Disability=='NA','NA',



### make HS type variables
hsTypeV <- rownames(varInf)[grep('type of high school',varInf$V3)]
hsType <- dat[,hsTypeV[-length(hsTypeV)]]

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
accV <- rownames(varInf)[grep('accommodations do you receive',varInf$V3)]
acc <- dat[,accV[-length(accV)]]
names(acc) <- sapply(acc,function(x) unique(na.omit(x))[1])
for(i in 1:ncol(acc)) acc[,i] <- as.numeric(!is.na(acc[,i]))

names(acc) <- sapply(strsplit(names(acc),' (',fixed=TRUE),function(x) x[1])

accomodations <- cbind(id=dat$id,acc)

write.csv(accomodations,'accomodations.csv',row.names=FALSE)

for(dd in grep('demo',names(dat)))
    if(is.character(dat[,dd]))
        dat[is.na(dat[,dd]),dd] <- 'NA'


## age categories
dat <- mutate(dat,age=ifelse(demo4=='#VALUE!',NA,
          ifelse(demo4<23,'19-22',ifelse(demo4<30,'23-29',ifelse(demo4<40,'30-39','40+')))))

## gender
dat <- mutate(dat,gender=ifelse(demo1=='NA',NA,ifelse(demo1%in%c('Male','Female'),demo1,'Other')))


## hs language
hsLangV <- rownames(varInf)[grep('language of instruction at your high school',varInf$V3)]
hsLang <- dat[,hsLangV[-length(hsLangV)]]
hsLang[hsLang=='NA'] <- NA
names(hsLang) <- sapply(hsLang,function(x) unique(na.omit(x))[1])
names(hsLang) <- gsub('My classes were taught in ','',names(hsLang))
names(hsLang) <- gsub(' and I had ','+',names(hsLang))
for(i in 1:ncol(hsLang)) hsLang[,i] <- as.numeric(!is.na(hsLang[,i]))

dat$hsLang <- NA
dat$hsLang[rowSums(hsLang)==0] <- NA
dat$hsLang[rowSums(hsLang)==1] <- names(hsLang)[sapply(which(rowSums(hsLang)==1),function(i) which(hsLang[i,]==1))]
dat$hsLang[rowSums(hsLang)>1] <- 'Multiple'

dat$hsLang[grep('Another',dat$hsLang)] <- 'English+Other Acc'
dat$hsLang[grep('provider',dat$hsLang)] <- 'English+Other Acc'
### write-ins
## dat$hsLang[grep('Full',dat$hsLang)] <- 'sign language.'
## dat$hsLang[grep('hear better',dat$hsLang)] <- 'English+no accommodations.'
## dat$hsLang[grep('Bi-Bi|one asl',dat$hsLang)] <- 'Multiple'

dat <- rename(dat,`HS Class Language`=hsLang)

### preferred language
prefLangV <- rownames(varInf)[grep('preferred language',varInf$V3)]

for(vv in prefLangV)
  dat[[vv]] <- ifelse(dat[[vv]]=='NA',NA,
    ifelse(dat[[vv]]=='ASL','ASL',
      ifelse(dat[[vv]]=='Spoken English','Spoken English',
        ifelse(dat[[vv]]=='Written/Text Communication','Written/Text Communication', 'Other'))))


write.csv(dat,'cleanedData.csv',row.names=FALSE)



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

write.csv(surv,'survNumeric.csv',row.names=FALSE)






### doubled descriptions
varInf$V3 <- vapply(strsplit(as.character(varInf$V3),'\\.[A-Z]'),function(x) x[1],'a')
varInf$V3 <- vapply(strsplit(varInf$V3,'What'),function(x) ifelse(length(x)==1,x[1],paste('What',x[2])),'a')
varInf$V3 <- vapply(strsplit(varInf$V3,'\\)[A-Z]'),function(x) x[1],'a')
varInf$V3 <- vapply(strsplit(varInf$V3,'\\?[A-Z]'),function(x) x[1],'a')
varInf$V3 <- vapply(strsplit(varInf$V3,'Which'),function(x) ifelse(length(x)==1,x[1],paste('Which',x[2])),'a')
varInf$V3 <- vapply(strsplit(varInf$V3,'How'),function(x) ifelse(length(x)==1,x[1],paste('How',x[2])),'a')
varInf$V3 <- vapply(strsplit(varInf$V3,'I identify'),function(x) ifelse(length(x)==1,x[1],paste('I identify',x[2])),'a')

write.csv(varInf,'cleanedVarInf.csv',row.names=TRUE)


### dichotomized version
survBin <- surv%>%mutate_at(vars(-id),function(x) as.numeric(x>2))

write.csv(survBin,'survBin.csv',row.names=FALSE)


