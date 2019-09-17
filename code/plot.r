source('crossTabs.r')

respDat <- cbind(dat[,grep(paste(names(summ),collapse='|'),names(dat),invert=TRUE)],resp,summ)

respPlot <- gather(respDat,resp,avg,grep('.',names(respDat),fixed=TRUE))
rrr <- strsplit(respPlot$resp,'.',fixed=TRUE)
respPlot$dim <- sapply(rrr,function(x) x[1])
respPlot$resp <- sapply(rrr,function(x) x[2])

respPlot$avg[respPlot$resp%in%c('NL','SL')] <- -respPlot$avg[respPlot$resp%in%c('NL','SL')]

respPlot$resp <- factor(respPlot$resp,levels=c('EL','L','NL','SL'))

respPlot$dim <- factor(respPlot$dim,names(sort(colMeans(summ,na.rm=TRUE))))

for(dd in grep('demo',names(respPlot))) respPlot[is.na(respPlot[,dd]),dd] <- 'NA'

p <- ggplot(respPlot,aes(dim,avg,fill=resp))+
    geom_bar(stat="summary",fun.y="mean")+geom_hline(yintercept=0,size=2)+
    scale_y_continuous(labels=scales::percent)+
        scale_fill_discrete(breaks=rev(c('NL','SL','L','EL')),labels=rev(levs))+
            labs(x=NULL,y=NULL,fill=NULL)+
                coord_flip()
ggsave('overall.jpg',p)

plotBy <- function(nn){
    ttt <- table(dat[[nn]])
    NAME <- ifelse(nn%in%rownames(varInf),varInf[nn,'desc'],nn)
    p2 <- p+facet_wrap(as.formula(paste('~',nn)),labeller=as_labeller(function(x) paste0(x,' (n=',ttt[x],')')))+ggtitle(NAME)
    ggsave(paste0(NAME,'.jpg'))
    p2
}

plotBy('demo68')
plotBy('demo67')
plotBy('demo65')
plotBy('Ethnicity')
plotBy('Disability')
plotBy('deafDisabled')

