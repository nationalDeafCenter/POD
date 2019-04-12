library(tidyverse)
varInf <- read.csv('varDesc322.csv')
dat <- read.csv('POD322.csv',header=FALSE,stringsAsFactors=FALSE)

varNames <- character(nrow(varInf))
for(cc in unique(varInf$type))
  varNames[varInf$type==cc] <- paste0(cc,seq(sum(varInf$type==cc)))

names(dat) <- tolower(varNames)

names(dat)[1] <- 'id'

rownames(varInf) <- names(dat)

dat <- subset(dat,id!='') ## remove NAs

### concise variable names
## nnn <- strsplit(names(dat),'.',fixed=TRUE)
## nnn <- c(nnn[[1]],sapply(nnn[-1],function(x) ifelse(length(x)==1,paste0(x[1],1),paste0(x[1],as.numeric(x[2])+1))))

#names(dat) <- nnn

## survey responses
surv <- dat[,-grep('pre|demo',names(dat))]
surv[surv=='0'] <- NA
surv[surv==''] <- NA

## remove rows that are completely empty
allmiss <- apply(surv,1,function(x) all(is.na(x[-1])))

surv <- surv[!allmiss,]
dat <- dat[!allmiss,]


dat[dat==''] <- NA

### make race variable
race <- dat[,paste0('demo',8:15)]
names(race) <- sapply(race,function(x) unique(na.omit(x))[1])
for(i in 1:ncol(race)) race[,i] <- as.numeric(!is.na(race[,i]))

dat$Ethnicity <- apply(race,1,
                       function(x) ifelse(sum(x)==0,'NA',
                                          ifelse(sum(x)>1,'Multiracial',
                                                 names(race)[which(x==1)])))

### make disability variable
dis <- dat[,paste0('demo',c(18,20:24))]
names(dis) <- sapply(dis,function(x) unique(na.omit(x))[1])
for(i in 1:ncol(dis)) dis[,i] <- as.numeric(!is.na(dis[,i]))

dat$Disability <- apply(dis,1,
                       function(x) ifelse(sum(x)==0,'none',
                                          ifelse(sum(x)>1,'Multi',names(dis)[which(x==1)])))
ttt <- table(dat$Disability)
dat$Disability[dat$Disability%in%c(names(ttt)[ttt<5],'A disability not listed')] <- 'Other'

dat$Disability[dat$Disability=='none'&is.na(dat$demo17)&is.na(dat$demo19)] <- 'NA'


dat$deafDisabled <- ifelse(dat$Disability=='none','Deaf-Nondisabled','Deaf-Disabled')
#                           ifelse(dat$Disability=='NA','NA',



### make HS type variables
hsTypeV <- rownames(varInf)[grep('type of high school',varInf$desc)]
hsType <- dat[,hsTypeV[-length(hsTypeV)]]

names(hsType) <- sapply(hsType,function(x) unique(na.omit(x))[1])
for(i in 1:ncol(hsType)) hsType[,i] <- as.numeric(!is.na(hsType[,i]))

dat$HStype <- apply(hsType,1,
                       function(x) ifelse(sum(x)==0,'NA',
                                          ifelse(sum(x)>1,
                                                 'Multi',names(hsType)[which(x==1)])))

mainstream <- rowSums(hsType[,grep('Mainstreamed',names(hsType))])
deafonly <- rowSums(hsType[,grep('Deaf',names(hsType))])

dat$MainstreamingHS <- ifelse(mainstream>0,
                              ifelse(deafonly>0,'both','mainstream only'),
                              ifelse(deafonly>0,'deaf only',
                                     ifelse(dat$HStype=='NA','NA','other')))



### accomodations
accV <- rownames(varInf)[grep('accommodations do you receive',varInf$desc)]
acc <- dat[,accV[-length(accV)]]
names(acc) <- sapply(acc,function(x) unique(na.omit(x))[1])
for(i in 1:ncol(acc)) acc[,i] <- as.numeric(!is.na(acc[,i]))

names(acc) <- sapply(strsplit(names(acc),' (',fixed=TRUE),function(x) x[1])

accomodations <- cbind(id=dat$id,acc)

write.csv(accomodations,'accomodations.csv',row.names=FALSE)

for(dd in grep('demo',names(dat)))
    if(is.character(dat[,dd]))
        dat[is.na(dat[,dd]),dd] <- 'NA'


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
varInf$desc <- vapply(strsplit(as.character(varInf$desc),'\\.[A-Z]'),function(x) x[1],'a')
varInf$desc <- vapply(strsplit(varInf$desc,'What'),function(x) ifelse(length(x)==1,x[1],paste('What',x[2])),'a')
varInf$desc <- vapply(strsplit(varInf$desc,'\\)[A-Z]'),function(x) x[1],'a')
varInf$desc <- vapply(strsplit(varInf$desc,'\\?[A-Z]'),function(x) x[1],'a')
varInf$desc <- vapply(strsplit(varInf$desc,'Which'),function(x) ifelse(length(x)==1,x[1],paste('Which',x[2])),'a')
varInf$desc <- vapply(strsplit(varInf$desc,'How'),function(x) ifelse(length(x)==1,x[1],paste('How',x[2])),'a')
varInf$desc <- vapply(strsplit(varInf$desc,'I identify'),function(x) ifelse(length(x)==1,x[1],paste('I identify',x[2])),'a')

write.csv(varInf,'cleanedVarInf.csv',row.names=TRUE)


### dichotomized version
survBin <- surv%>%mutate_at(vars(-id),function(x) as.numeric(x>2))

write.csv(survBin,'survBin.csv',row.names=FALSE)


