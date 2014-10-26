library(Hmisc)
library(data.table)
library(plyr)

setwd('~/Documents/bertplot/data/bmi extract/')

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

# final merge
three.yr <- join_all(list(d2008,d2009,d2010,d2011,d2012,d2013),by='state')

head(three.yr)
#merge(old.data,d2013,by='state')

# 3-yr calculations
three.yr$avg.obesity.2014<-with(three.yr,round(rowMeans(cbind(pct.obese2013,pct.obese2012,pct.obese2011)),1))
three.yr$avg.obesity.2013<-with(three.yr,round(rowMeans(cbind(pct.obese2012,pct.obese2011,pct.obese2010)),1))
three.yr$avg.obesity.2012<-with(three.yr,round(rowMeans(cbind(pct.obese2011,pct.obese2010,pct.obese2009)),1))
three.yr$avg.obesity.2011<-with(three.yr,round(rowMeans(cbind(pct.obese2010,pct.obese2009,pct.obese2008)),1))

three.yr<-three.yr[order(three.yr$avg.obesity.2013,decreasing = T),]
three.yr<-three.yr[!(three.yr$state %in% c('Guam','Puerto Rico','Virgin Islands')),]   # exclude territories

# Rank in Descending Order.  50 states + DC
three.yr$rank2014<-52-round(rank(three.yr$avg.obesity.2014,ties.method = 'max'),0)
three.yr$rank2013<-52-round(rank(three.yr$avg.obesity.2013,ties.method = 'max'),0)
three.yr$rank2012<-52-round(rank(three.yr$avg.obesity.2012,ties.method = 'max'),0)
three.yr$rank2011<-52-round(rank(three.yr$avg.obesity.2011,ties.method = 'max'),0) ## Use 2008?

three.yr$rank.change2014<-round(three.yr$rank2013-three.yr$rank2014,0)
three.yr$rank.change2013<-round(three.yr$rank2012-three.yr$rank2013,0)
three.yr$rank.change2012<-round(three.yr$rank2011-three.yr$rank2012,0)

three.yr$pct.diff2014<-with(three.yr,format(avg.obesity.2014-avg.obesity.2013,digits=2))
three.yr$pct.diff2013<-with(three.yr,format(avg.obesity.2013-avg.obesity.2012,digits=2))
three.yr$pct.diff2012<-with(three.yr,format(avg.obesity.2012-avg.obesity.2011,digits=2))

three.yr$avg.obesity.2014<-format(three.yr$avg.obesity.2014,digits=3)
three.yr$avg.obesity.2013<-format(three.yr$avg.obesity.2013,digits=3)
three.yr$avg.obesity.2012<-format(three.yr$avg.obesity.2012,digits=3)
three.yr$avg.obesity.2011<-format(three.yr$avg.obesity.2011,digits=3)

three.yr$Obese.2008<-format(three.yr$Obese.2008,digits=3)
three.yr$Obese.2009<-format(three.yr$Obese.2009,digits=3)
three.yr$Obese.2010<-format(three.yr$Obese.2010,digits=3)
three.yr$Obese.2011<-format(three.yr$Obese.2011,digits=3)
three.yr$Obese.2012<-format(three.yr$Obese.2012,digits=3)
three.yr$Obese.2013<-format(three.yr$Obese.2013,digits=3)

three.yr$pct.gtoverweight2009<-format(three.yr$pct.gtoverweight2008,digits=3)
three.yr$pct.gtoverweight2009<-format(three.yr$pct.gtoverweight2009,digits=3)
three.yr$pct.gtoverweight2010<-format(three.yr$pct.gtoverweight2010,digits=3)
three.yr$pct.gtoverweight2011<-format(three.yr$pct.gtoverweight2011,digits=3)
three.yr$pct.gtoverweight2012<-format(three.yr$pct.gtoverweight2012,digits=3)
three.yr$pct.gtoverweight2013<-format(three.yr$pct.gtoverweight2013,digits=3)


#check what columns to export for python
head(three.yr)
three.yr[three.yr$state == 'Mississippi',grepl('Obese',colnames(three.yr))]
three.yr[three.yr$state == 'West Virginia',grepl('Obese',colnames(three.yr))]

three.yr[three.yr$state == 'Mississippi',grepl('obesity',colnames(three.yr))]
three.yr[three.yr$state == 'Louisiana',grepl('ank',colnames(three.yr))]

# Export for python
#2014
three.yr<-three.yr[order(three.yr$rank2014,decreasing = F),]
output2013<-three.yr[,c("rank2013","rank2014","state","Obese.2013","pct.gtoverweight2013", "avg.obesity.2014","pct.diff2014","rank.change2014")]
write.table(output2013,file = "output-2013.csv",quote=F,sep=',',row.names=F)
# Export for direct-transfer
transfer2013<-three.yr[,c("state","Underweight.2013","Normal.Weight.2013", "Overweight.2013","Obese.2013")]
write.table(format(transfer2013,digits=3),file = "transfer-2013.csv",quote=F,sep=',',row.names=F)

#2013
three.yr<-three.yr[order(three.yr$rank2013,decreasing = F),]
output2012<-three.yr[,c("rank2012","rank2013","state","Obese.2012","pct.gtoverweight2012", "avg.obesity.2013","pct.diff2013","rank.change2013")]
write.table(output2012,file = "output-2012.csv",quote=F,sep=',',row.names=F)

transfer2012<-three.yr[,c("state","Underweight.2012","Normal.Weight.2012", "Overweight.2012","Obese.2012")]
write.table(format(transfer2012,digits=3),file = "transfer-2012.csv",quote=F,sep=',',row.names=F)

#2012
three.yr<-three.yr[order(three.yr$rank2012,decreasing = F),]
output2011<-three.yr[,c("rank2011","rank2012","state","Obese.2011","pct.gtoverweight2011", "avg.obesity.2012","pct.diff2012","rank.change2012")]
write.table(output2011,file = "output-2011.csv",quote=F,sep=',',row.names=F)

transfer2011<-three.yr[,c("state","Underweight.2011","Normal.Weight.2011", "Overweight.2011","Obese.2011")]
write.table(format(transfer2011,digits=3),file = "transfer-2011.csv",quote=F,sep=',',row.names=F)

# 2011
#three.yr<-three.yr[order(three.yr$rank2011,decreasing = F),]
output2010<-three.yr[,c("rank2011","state","Obese.2010","pct.gtoverweight2010","avg.obesity.2011")]
output2010
#head(three.yr[,c("state","Obese.2008","Obese.2009","Obese.2010","Obese.2011", "avg.obesity.2010","pct.diff2011")])
.

test<-three.yr[,c("pct.diff2012","rank.change2012")]
