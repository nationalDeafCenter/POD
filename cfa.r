### try cfa
library(lavaan)

cfa1 <- '\n'
for(ccc in tolower(unique(varInf$category))){
    if(ccc%in%c('','pre','demo')) next
    cfa1 <- paste(cfa1,ccc,'=~',paste(grep(ccc,names(surv),value=TRUE),collapse='+'),'\n')
}

for(x in 2:ncol(surv)) surv[,x] <- as.ordered(surv[,x])

summary(fit1 <- cfa(cfa1,surv,missing='ML'),fit=TRUE)

meas <- c('pvalue','baseline.pvalue','cfi','tli','rmsea','rmsea.ci.lower','rmsea.ci.upper')
inds <- data.frame(modelNum=1,rbind(fitMeasures(fit1)[meas]))
inds$model <- "full"
inds$sample <- "full"

sink('CFA-fullModel-FullData.txt')
print(summary(fit1,fit=TRUE))
sink()



mi <- modindices(fit1)
head(mi[order(mi$mi,decreasing=TRUE),])

### remove scap3
cfa2 <- '
 tech =~ tech1+tech2+tech3+tech4
 atti =~ atti1+atti2+atti3+atti4+atti5+atti6
 comm =~ comm1+comm2+comm3+comm4+comm5+comm6
 phys =~ phys1+phys2+phys3+phys4+phys5+phys6
 serv =~ serv1+serv2+serv3+serv4+serv5+serv6
 scap =~ scap1+scap2+scap4+scap5+scap6+scap7+scap8+scap9+scap10
'

cfa2.1 <- '
 tech =~ tech1+tech2+tech3+tech4
 atti =~ atti1+atti2+atti3+atti4+atti5+atti6+scap3
 comm =~ comm1+comm2+comm3+comm4+comm5+comm6
 phys =~ phys1+phys2+phys3+phys4+phys5+phys6
 serv =~ serv1+serv2+serv3+serv4+serv5+serv6+scap3
 scap =~ scap1+scap2+scap3+scap4+scap5+scap6+scap7+scap8+scap9+scap10
 scap2~~scap3
'

summary(fit2 <- cfa(cfa2,surv,missing='ML'),fit=TRUE)

sink('CFA-NoScap3-fullData.txt')
print(summary(fit2,fit=TRUE))
sink()

inds <- rbind(inds,data.frame(modelNum=2,rbind(fitMeasures(fit2)[meas]),model='-scap3',sample='full'))


surv$big3 <- dat$demo46%in%c('Gallaudet University','Rochester Institute Technology','Calif St Univ Northridge')
surv$naSchool <- dat$demo46%in%c("#N/A","")

summary(fit4 <- cfa(cfa1,subset(surv,!big3),missing='ML'),fit=TRUE)
mi4 <- modindices(fit4)
head(mi4[order(mi4$mi,decreasing=TRUE),])

inds <- rbind(inds,data.frame(modelNum=4,rbind(fitMeasures(fit4)[meas]),model='full',sample='-big 3'))

sink('CFA-fullModel-noBig3.txt')
print(summary(fit4))
sink()

summary(fit4.1 <- cfa(cfa2,subset(surv,!big3),missing='ML'),fit=TRUE)
mi4.1 <- modindices(fit4.1)
head(mi4.1[order(mi4.1$mi,decreasing=TRUE),])

summary(fit4.2 <- cfa(cfa2.1,subset(surv,!big3),missing='ML'),fit=TRUE)


sink('CFA-noScap3-noBig3.txt')
print(summary(fit4.1))
sink()

inds <- rbind(inds,data.frame(modelNum=4.1,rbind(fitMeasures(fit4.1)[meas]),model='-scap3',sample='-big 3'))

cfa4 <- '
 tech =~ tech1+tech2+tech3+tech4
 atti =~ atti1+atti2+atti3+atti4+atti5+atti6
 comm =~ comm1+comm2+comm3+comm4+comm5+comm6
 phys =~ phys1+phys2+phys3+phys4+phys5+phys6
 serv =~ serv1+serv2+serv3+serv5+serv6
 scap =~ scap1+scap2+scap4+scap5+scap6+scap7+scap8+scap9+scap10
 tech ~~ atti+comm+phys+serv+scap
 atti ~~ comm+phys+serv+scap
 comm ~~ phys+serv+scap
 phys ~~ serv+scap
 serv ~~ scap
'
summary(fit4.3 <- cfa(cfa4,subset(surv,!big3),missing='ML'),fit=TRUE)

inds <- rbind(inds,data.frame(modelNum=4.3,rbind(fitMeasures(fit4.3)[meas]),model='-scap3 -serv4',sample='-big 3'))

sink('CFA-noScap3noServ4-noBig3.txt')
print(summary(fit4.3))
sink()

summary(fit5 <- cfa(cfa1,subset(surv,!big3&!naSchool),missing='ML'),fit=TRUE)
inds <- rbind(inds,data.frame(modelNum=5,rbind(fitMeasures(fit5)[meas]),model='full',sample='-big 3 -NAs'))

sink('CFA-fullModel-noBig3noSchoolNA.txt')
print(summary(fit5))
sink()

mi5 <- modindices(fit5)
head(mi5[order(mi5$mi,decreasing=TRUE),])

cfa5 <- '
 tech =~ tech2+tech3+tech4
 atti =~ atti1+atti2+atti3+atti4+atti5+atti6
 comm =~ comm1+comm2+comm3+comm4+comm5+comm6
 phys =~ phys2+phys3+phys4+phys5+phys6
 serv =~ serv1+serv2+serv3+serv4+serv5+serv6
 scap =~ scap1+scap2+scap4+scap5+scap6+scap7+scap8+scap9+scap10
'
summary(fit5.1 <- cfa(cfa5,subset(surv,!big3&!naSchool),missing='ML'),fit=TRUE)
inds <- rbind(inds,data.frame(modelNum=5.1,rbind(fitMeasures(fit5.1)[meas]),model='-tech1 - phys1',sample='-big 3 -NAs'))

sink('CFA-noTech1noPhys1-noBig3-noSchoolNA.txt')
print(summary(fit5.1))
sink()

openxlsx::write.xlsx(inds,'fitIndices.xlsx')

## galludet rochester calif-northridge?

## interpreter use?

### not bad actually. RMSEA is good, chi-squared rejects, but who cares about chi squared these days anyway

#### try EFA
library(psych)

corMat <- polychoric(surv[,-1])
rho <- corMat$rho
colnames(rho) <- rownames(rho) <- names(surv)[-1]

efa <- fa(rho,5)
nfac <- fa.parallel(rho,n.obs=nrow(surv))
efa6 <- fa(rho,6)
### format demos
## demoFix <- varInf$desc[grep(' - ',varInf$desc,fixed=TRUE)]
## demoFix <- demoFix[!demoFix%in%c("What  year were you born? - Select a year",
##                                  "What  year did you move to the United States? - Select a year")] ## these r ok
## demoFix <- demoFix[-grep(' - Text',demoFix)] #also OK

## demoFix <- gsub(' - Receptive','Receptive',demoFix)
## demoFix <- gsub(' - Expressive','Expressive',demoFix)
## #demoFix <- gsub(' - Selected Choice','',demoFix)

## demoFix <- strsplit(demoFix,' - ',fixed=TRUE)
## ## demoVars <- sapply(demoFix,function(x) x[1])

## ### deaf or hearing or...?

## dat[dat[,71]=='',71] <- NA
## dat[,71] <- as.character(dat[,71])

## dat$demo25.2 <- substr(dat$demo25,1,20)

## xtabs(~demo25.2+demo19,dat,addNA=TRUE)

table(dat[,65])



