library(Hmisc)
library(data.table)
library(plyr)

setwd('~/Documents/bertplot/data/bmi extract/')

states<-read.table('state-map.csv',header=F,sep=',')
colnames(states)<-c('state.num','state')

x <- sasxport.get('LLCP2013.XPT')
x.names<-names(x)
trim<-x[,grepl('bmi',x.names)|colnames(x)%in%c("x.state")]
colnames(trim)<-c('state.num','bmi','bmi5cat','rfbmi5')
head(trim)

# Make calculations
trim$obese = trim$bmi >= 3000
trim$gt.overweight = trim$bmi >= 2500
trim$overweight = trim$bmi >= 2500& trim$bmi< 3000
trim$normalweight = trim$bmi >= 1850& trim$bmi < 2500
trim$underweight = trim$bmi < 1850

trim=trim[,!grepl("NA",colnames(trim))]

withState  = merge( trim,states,by='state.num')
head(withState)

trim.dt = data.table(withState,key='state')
head(trim.dt)
pct.obese = as.data.frame(trim.dt[, mean(c(obese), na.rm = TRUE),by = state])
pct.gtoverweight = as.data.frame(trim.dt[, mean(c(gt.overweight), na.rm = TRUE),by = state])
pct.overweight = as.data.frame(trim.dt[, mean(c(overweight), na.rm = TRUE),by = state])
pct.normalweight = as.data.frame(trim.dt[, mean(c(normalweight), na.rm = TRUE),by = state])
pct.underweight = as.data.frame(trim.dt[, mean(c(underweight), na.rm = TRUE),by = state])

pct.obese$V1<-round(pct.obese$V1*100,2)
pct.gtoverweight$V1<-round(pct.gtoverweight$V1*100,2)
pct.overweight$V1<-round(pct.overweight$V1*100,2)
pct.normalweight$V1<-round(pct.normalweight$V1*100,2)
pct.underweight$V1<-round(pct.underweight$V1*100,2)

head(pct.obese)
head(pct.gtoverweight)

colnames(pct.obese)<-c('state','pct.obese2013')
colnames(pct.gtoverweight)<-c('state','pct.gtoverweight2013')
colnames(pct.overweight)<-c('state','Overweight.2013')
colnames(pct.normalweight)<-c('state','Normal.Weight.2013')
colnames(pct.underweight)<-c('state','Underweight.2013')

d2013<- Reduce(function(...) merge(...,by='state', all=T), list(pct.obese,pct.gtoverweight,pct.overweight,pct.normalweight,pct.underweight))
d2013$Obese.2013 = d2013$pct.obese2013
head(d2013)
#d2013<-merge(pct.obese,pct.gtoverweight,by='state')

# Load backdata
d2008<-read.table('2008.csv',header=T,sep=',')
d2009<-read.table('2009.csv',header=T,sep=',')
d2010<-read.table('2010.csv',header=T,sep=',')
d2011<-read.table('2011.csv',header=T,sep=',')
d2012<-read.table('2012.csv',header=T,sep=',')
d2013<-read.table('2013.csv',header=T,sep=',')

# Generate relevant info
d2008$pct.obese2008 = d2008$Obese
d2008$pct.gtoverweight2008 = d2008$Obese + d2008$Overweight

d2009$pct.obese2009 = d2009$Obese
d2009$pct.gtoverweight2009 = d2009$Obese + d2009$Overweight

d2010$pct.obese2010 = d2010$Obese
d2010$pct.gtoverweight2010 = d2010$Obese + d2010$Overweight

d2011$pct.obese2011 = d2011$Obese
d2011$pct.gtoverweight2011 = d2011$Obese + d2011$Overweight

d2012$pct.obese2012 = d2012$Obese
d2012$pct.gtoverweight2012 = d2012$Obese + d2012$Overweight

d2013$pct.obese2013 = d2013$Obese.2013
d2013$pct.gtoverweight2013 = d2013$Obese + d2013$Overweight


# 2011 + 2012
#tmp.old.data<-merge(d2011,d2012,by='state')
#head(tmp.old.data)

#next.old.data=merge(d2010,tmp.old.data,by='state')
#last.old.data = merge(d2009,next.old.data,by='state')
#old.data = merge(d2008,last.old.data,by='state')
#old.data<-old.data[,grepl('pct',colnames(old.data))|grepl('state',colnames(old.data))]
head(old.data)

# final merge
three.yr <- join_all(list(d2008,d2009,d2010,d2011,d2012,d2013),by='state')
head(three.yr)
#merge(old.data,d2013,by='state')

# 3-yr calculations
three.yr$avg.obesity.2013<-with(three.yr,round(rowMeans(cbind(pct.obese2013,pct.obese2012,pct.obese2011)),1))
three.yr$avg.obesity.2012<-with(three.yr,round(rowMeans(cbind(pct.obese2012,pct.obese2011,pct.obese2010)),1))
three.yr$avg.obesity.2011<-with(three.yr,round(rowMeans(cbind(pct.obese2011,pct.obese2010,pct.obese2009)),1))
three.yr$avg.obesity.2010<-with(three.yr,round(rowMeans(cbind(pct.obese2010,pct.obese2009,pct.obese2008)),1))

three.yr$avg.gtoverweight.2013<-with(three.yr,round(rowMeans(cbind(pct.gtoverweight2013,pct.gtoverweight2012,pct.gtoverweight2011)),1))
three.yr$avg.gtoverweight.2012<-with(three.yr,round(rowMeans(cbind(pct.gtoverweight2012,pct.gtoverweight2011,pct.gtoverweight2010)),1))
three.yr$avg.gtoverweight.2011<-with(three.yr,round(rowMeans(cbind(pct.gtoverweight2011,pct.gtoverweight2010,pct.gtoverweight2009)),1))
three.yr$avg.gtoverweight.2010<-with(three.yr,round(rowMeans(cbind(pct.gtoverweight2010,pct.gtoverweight2009,pct.gtoverweight2008)),1))

head(three.yr)

three.yr<-three.yr[order(three.yr$avg.obesity.2013,decreasing = T),]
three.yr<-three.yr[!(three.yr$state %in% c('Guam','Puerto Rico')),]   # exclude territories

# Rank in Descending Order.  50 states + DC
three.yr$rank2013<-52-round(rank(three.yr$avg.obesity.2013,),0)
three.yr$rank2012<-52-round(rank(three.yr$avg.obesity.2012),0)
three.yr$rank2011<-52-round(rank(three.yr$avg.obesity.2011),0)
three.yr$rank2010<-52-round(rank(three.yr$avg.obesity.2010),0) ## Use 2008?

three.yr$rank.change2013<-round(three.yr$rank2012-three.yr$rank2013,0)
three.yr$rank.change2012<-round(three.yr$rank2011-three.yr$rank2012,0)
three.yr$rank.change2011<-round(three.yr$rank2010-three.yr$rank2011,0)

three.yr$pct.diff2013<-with(three.yr,round(avg.obesity.2012-avg.obesity.2013,1))
three.yr$pct.diff2012<-with(three.yr,round(avg.obesity.2011-avg.obesity.2012,1))
three.yr$pct.diff2011<-with(three.yr,round(avg.obesity.2010-avg.obesity.2011,1))

#check what columns to export for python
head(three.yr)
three.yr[three.yr$state == 'Mississippi',grepl('Obese',colnames(three.yr))]

# Export for python
#2013
three.yr<-three.yr[order(three.yr$avg.obesity.2013,decreasing = T),]
output2013<-three.yr[,c("rank2012","rank2013","state","avg.obesity.2012","pct.gtoverweight2012", "avg.obesity.2013","pct.diff2013","rank.change2013")]
write.table(output2013,file = "output-2013.csv",quote=F,sep=',',row.names=F)
# Export for direct-transfer
transfer2013<-three.yr[,c("state","Underweight.2013","Normal.Weight.2013", "Overweight.2013","Obese.2013")]
write.table(transfer2013,file = "transfer-2013.csv",quote=F,sep=',',row.names=F)

#2012
three.yr<-three.yr[order(three.yr$avg.obesity.2012,decreasing = T),]
output2012<-three.yr[,c("rank2011","rank2012","state","avg.obesity.2011","pct.gtoverweight2011", "avg.obesity.2012","pct.diff2012","rank.change2012")]
write.table(output2012,file = "output-2012.csv",quote=F,sep=',',row.names=F)

transfer2012<-three.yr[,c("state","Underweight.2012","Normal.Weight.2012", "Overweight.2012","Obese.2012")]
write.table(transfer2012,file = "transfer-2012.csv",quote=F,sep=',',row.names=F)

#2011
three.yr<-three.yr[order(three.yr$avg.obesity.2011,decreasing = T),]
output2011<-three.yr[,c("rank2010","rank2011","state","avg.obesity.2010","pct.gtoverweight2010", "avg.obesity.2011","pct.diff2011","rank.change2011")]
write.table(output2011,file = "output-2011.csv",quote=F,sep=',',row.names=F)

transfer2011<-three.yr[,c("state","Underweight.2011","Normal.Weight.2011", "Overweight.2011","Obese.2011")]
write.table(transfer2011,file = "transfer-2011.csv",quote=F,sep=',',row.names=F)


.
