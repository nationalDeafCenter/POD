library(lavaan)

source('cleanData.r')
survBin$big3 <- dat$demo46%in%c('Gallaudet University','Rochester Institute Technology','Calif St Univ Northridge')

meas <- c('pvalue','baseline.pvalue','cfi','tli','rmsea','rmsea.ci.lower','rmsea.ci.upper')

cfa1 <- '\n'
for(ccc in tolower(unique(varInf$category))){
    if(ccc%in%c('','pre','demo')) next
    cfa1 <- paste(cfa1,ccc,'=~',paste(grep(ccc,names(surv),value=TRUE),collapse='+'),'\n')
}

meas <- c('pvalue','baseline.pvalue','cfi','tli','rmsea','rmsea.ci.lower','rmsea.ci.upper')

fff <- cfa(cfa1,subset(survBin,!big3),missing='ML')

sink('cfaBin.txt')
summary(fff,fit=TRUE)
sink()

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

fff2 <- cfa(cfa4,subset(survBin,!big3),missing='ML')


#### 2-level model
cfa2lev <- '
 tech =~ tech1+tech2+tech3+tech4
 atti =~ atti1+atti2+atti3+atti4+atti5+atti6
 comm =~ comm1+comm2+comm3+comm4+comm5+comm6
 phys =~ phys1+phys2+phys3+phys4+phys5+phys6
 serv =~ serv1+serv2+serv3+serv4+serv5+serv6
 scap =~ scap1+scap2+scap3+scap4+scap5+scap6+scap7+scap8+scap9+scap10
 gen =~ tech+atti+comm+phys+serv+scap
'
twoLev1 <- cfa(cfa2lev,subset(survBin,!big3),missing='ML')

mi <- modificationIndices(twoLev1)

cfa2lev2 <- '
 tech =~ tech1+tech2+tech3+tech4
 atti =~ atti1+atti2+atti3+atti4+atti5+atti6
 comm =~ comm1+comm2+comm3+comm4+comm5+comm6
 phys =~ phys1+phys2+phys3+phys4+phys5+phys6
 serv =~ serv1+serv2+serv3+serv4+serv5+serv6
 scap =~ scap1+scap2+scap4+scap6+scap7+scap8+scap9+scap10
 gen =~ tech+atti+comm+phys+serv+scap
'
twoLev2 <- cfa(cfa2lev2,subset(survBin,!big3),missing='ML')

mi <- modificationIndices(twoLev2)
head(mi[order(mi$mi,decreasing=TRUE),])


survBinOrd <- survBin
survBinOrd[,2:39] <- lapply(survBin[,2:39],ordered)



twoLev3 <- cfa(cfa2lev2,subset(survBinOrd,!big3))

cfa2lev3 <- '
 tech =~ tech1+tech2+tech3+tech4
 atti =~ atti1+atti2+atti3+atti4+atti6
 comm =~ comm1+comm2+comm3+comm4+comm5+comm6
 phys =~ phys1+phys2+phys4+phys5+phys6
 serv =~ serv1+serv2+serv3+serv4+serv5+serv6
 scap =~ scap1+scap2+scap4+scap6+scap7+scap8+scap9+scap10
 gen =~ tech+atti+comm+phys+serv+scap
'

cfaDiagram <- '
 tech =~ tech1
 atti =~ atti1
 comm =~ comm1
 phys =~ phys1
 serv =~ serv1
 scap =~ scap1
 gen =~ tech+atti+comm+phys+serv+scap
'

aaa <- cfa(cfaDiagram,sb)

twoLev3 <- cfa(cfa2lev3,sb)#subset(survBinOrd,!big3))

library(MplusAutomation)
prepareMplusData(subset(survBin,!big3),'pod.dat',dropCols=c('big3','id'))

runModels()
fit <- readModels()

res <- fit$parameters$unstandardized
res2 <- res%>%filter(paramHeader=='GEN.BY')

vcv <- fit$tech3$paramCov#[order2num,order2num]
order2num <- which(round(sqrt(diag(vcv)),3)==0.177)+(0:4)
round(sqrt(diag(vcv)[order2num]),3)-order2results$se[-1]
vcv2 <- vcv[order2num,order2num]
vcv2 <- vcv2%>%rbind(0,.)%>%cbind(0,.)

diff <- matrix(NA,6,6)
for(i in 2:6) for(j in 1:(i-1)) diff[i,j] <- res2$est[i]-res2$est[j]
diffSE <- matrix(NA,6,6)
for(i in 2:6) for(j in 1:(i-1)) diffSE[i,j] <- sqrt(vcv2[i,i]+vcv2[j,j]+2*vcv2[i,j])

