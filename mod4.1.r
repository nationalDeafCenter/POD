library(tidyverse)

param <- parameterEstimates(fit4.1)

## any coefs very different from 1?
diffFrom1 <- with(subset(param,op=='=~'&!is.na(z)),abs(est-1))

thresh <- subset(param,op=='|')%>%select(lhs,rhs,est)
for(lll in unique(thresh$lhs))
  thresh <- thresh%>%add_row(lhs=lll,rhs='t0',est=-2,.before=min(which(thresh$lhs==lll)))%>%
    add_row(lhs=lll,rhs='t4',est=1.5,.after=max(which(thresh$lhs==lll)))

thresh <- thresh%>%group_by(lhs)%>%
  mutate(x=lag(est),xend=est,yend=lhs,resp=factor(c(NA,names(levs)),levels=names(levs)))%>%
  filter(rhs!='t0')%>%mutate(t2=est[rhs=='t2'])%>%ungroup()

thresh$class <- substr(thresh$lhs,1,nchar(thresh$lhs)-1)
thresh$class[thresh$class=='scap1'] <- 'scap'
thresh <- thresh%>%group_by(class)%>%arrange(class,t2)%>%ungroup()%>%
  mutate(lhs=factor(lhs,levels=unique(lhs)))


#ggplot(thresh,aes(est,lhs,group=lhs))+geom_point()+geom_line()
ggplot(thresh,aes(x,lhs,xend=xend,yend=yend,color=resp))+geom_segment(size=2)+
  scale_color_manual(values=subwayPalette[1:4])+theme_bw()
ggsave('thresholds.jpg')

### compare to binary sums
survBin <- surv
varbs <- parameterEstimates(fit4)
varbs <- unique(varbs$rhs[varbs$op=='=~'])
for(vv in varbs)
  survBin[[vv]] <- as.numeric(survBin[[vv]]>2)

binSum <- round(rowMeans(survBin[,varbs],na.rm=TRUE)*length(varbs))

ggplot(as.data.frame(xtabs(~binSum)),aes(x=binSum,y=Freq))+geom_col()+geom_smooth()

factorScores <- predict(fit4.1)

factorScoreRank <- apply(factorScores,2,rank)


factorScores <- sweep(factorScores,2,apply(factorScores,2,IQR),'/')

binScores <- sapply(unique(thresh$class),function(cc) rowMeans(survBin[,grep(cc,names(survBin))],na.rm=TRUE))

binScores <- binScores[!survBin$big3,]

binSum <- binSum[!survBin$big3]

fsr <- as.data.frame(factorScoreRank)%>%gather('factor','rank')
bs <- as.data.frame(binScores)%>%gather('factor','binScore')


pdf('factorVsBin.pdf')
par(mfrow=c(2,3))
for(nn in colnames(binScores))
  plot(factorScoreRank[,nn],binScores[,nn],main=paste(nn,'cor=',round(cor(factorScoreRank[,nn],binScores[,nn],use='pairwise'),2)))
dev.off()


pdf('factorVsBinOverall.pdf')
plot(rowSums(factorScoreRank),binSum,main=paste('cor=',round(cor(factorScore[,nn],binSum[,nn],use='pairwise'),2)))
dev.off()

lavPredict(fit4.1)
