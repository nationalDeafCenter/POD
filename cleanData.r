dat <- read.csv('POD.csv')
dat$X <- NULL
names(dat) <- tolower(names(dat))
dat <- subset(dat,id!='') ## remove NAs

### concise variable names
nnn <- strsplit(names(dat),'.',fixed=TRUE)
nnn <- c(nnn[[1]],sapply(nnn[-1],function(x) ifelse(length(x)==1,paste0(x[1],1),paste0(x[1],as.numeric(x[2])+1))))

names(dat) <- nnn

## survey responses
surv <- dat[,-grep('pre|demo',names(dat))]
surv[surv=='0'] <- NA
surv[surv==''] <- NA

## remove rows that are completely empty
allmiss <- apply(surv,1,function(x) all(is.na(x[-1])))

surv <- surv[!allmiss,]
dat <- dat[!allmiss,]


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


### variable information (second sheet on spreadsheet)
varInf <- read.csv('PODinfo.csv',header=FALSE)
varInf <- subset(varInf,V2!='') ## otherwise spreadsheet goes on forever
varInf$V1 <- nnn

names(varInf) <- c('name','category','desc')

rownames(varInf) <- varInf$name

### doubled descriptions
varInf$desc <- vapply(strsplit(as.character(varInf$desc),'\\.[A-Z]'),function(x) x[1],'a')
varInf$desc <- vapply(strsplit(varInf$desc,'What'),function(x) ifelse(length(x)==1,x[1],paste('What',x[2])),'a')
varInf$desc <- vapply(strsplit(varInf$desc,'\\)[A-Z]'),function(x) x[1],'a')
varInf$desc <- vapply(strsplit(varInf$desc,'\\?[A-Z]'),function(x) x[1],'a')
varInf$desc <- vapply(strsplit(varInf$desc,'Which'),function(x) ifelse(length(x)==1,x[1],paste('Which',x[2])),'a')
varInf$desc <- vapply(strsplit(varInf$desc,'How'),function(x) ifelse(length(x)==1,x[1],paste('How',x[2])),'a')
varInf$desc <- vapply(strsplit(varInf$desc,'I identify'),function(x) ifelse(length(x)==1,x[1],paste('I identify',x[2])),'a')
