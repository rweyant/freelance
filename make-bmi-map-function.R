library(ggplot2)
library(reshape2)
library(akima)
library(rgdal)
library(raster)
library(rgeos)
library(plyr)
library(maptools)
library(ggmap)
library(mapproj)
library(gstat)
library(sp)
library(spatstat)
library(rworldmap)

# Clip Shape file with bbox
# Robin Lovelace
# http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  proj4string(b_poly) <- proj4string(shp)
  gIntersection(shp, b_poly, byid = T)
}


# 
# Load SpatialPolygonsDataFrame of US State borders
#
setwd('/home/roberto/Documents/bertplot/data/US shapefile/files/')

states50 <-readOGR(dsn='cb_2013_us_state_20m',layer='cb_2013_us_state_20m')

b <- bbox(states50)
b[1, ] <- c(-124,-66)
b[2, ] <- c(24,50)
continental <-gClip(states50,b)
plot(continental)


#
# Combine Census Data with Shapefile
#

setwd('~/Documents/bertplot/data/bmi extract/export/')

make.year.plot<-function(year){
  # Load Data
  dat<-read.table(paste('output-',year,'.csv',sep=''),sep=',',header=T,na.strings = 'N')
  dat$state = tolower(dat$state)
  obese.col<-paste("avg.obesity.",year+1,sep='')
  print(obese.col)
  dat$obese<-dat[,c(obese.col)]
  head(dat)
  
  states50@data$state<-tolower(states50@data$NAME)
  states50@data<-merge(states50@data,
                       dat,
                       by='state',
                       sort = F,
                       all.x=T)   # Keep all coordinate lines
  
  head(states50@data)
  lnd_f<-fortify(states50,region='state')
  lnd_f<-merge(lnd_f,states50@data,by.x='id',by.y='state')
  lnd_f<-lnd_f[!lnd_f$NAME %in% c('Alaska','Hawaii','Puerto Rico'),]
  lnd_f$obeseRank=findInterval(lnd_f$obese,seq(22,35,2))+1
  lnd_f$obeseColor = sapply(findInterval(lnd_f$obese,seq(22,35,2))+1,switch,
                            '#399E39',
                            '#81E681',
                            '#A2F5A2',
                            '#E7EB9D',
                            '#EBCB8F',
                            '#E3905D',
                            '#CC533B',
                            'A60000')
  
  # Leanest
  lnd_f$obeseRank[lnd_f$obese==min(lnd_f$obese)]=min(lnd_f$obeseRank)-1
  # Fattest
  lnd_f$obeseRank[lnd_f$obese==max(lnd_f$obese)]=max(lnd_f$obeseRank)+1
  
  lnd_f$obeseFactor<-factor(lnd_f$obeseRank)
  head(lnd_f)
  table(lnd_f$obeseRank)
  
  colors=c('#399E39','#0BE300','#81E681','#A2F5A2','#E7EB9D','#EBCB8F','#E3905D','#CC533B','#A60000','#FF0000')
  
  trueCentroids = gCentroid(states50,byid=TRUE)
  text.labels<-as.data.frame(trueCentroids@coords)
  text.labels$id <- states50@data$STUSPS
  text.labels$obesity <- format(states50@data$obese,digits=3)
  text.labels<-text.labels[!text.labels$id%in%c('AK','HI','PR'),]
  # add Obesity percent
  text.labels$lab<-paste(text.labels$id,text.labels$obesity,sep='\n')
  head(text.labels)
  text.labels[,c('orig.x','orig.y')]=text.labels[,c('x','y')]
  # Manually fix some states
  text.labels[text.labels$id == 'CA',c('x')]<-c(-120.35)
  text.labels[text.labels$id == 'LA',c('x')]<-c(-92.5)
  text.labels[text.labels$id == 'FL',c('x')]<-c(-81.55)
  text.labels[text.labels$id == 'KY',c('x')]<-c(-85.)
  
  text.labels[text.labels$id == 'TN',c('lab')]<-"TN 32.6"
  text.labels[text.labels$id == 'SC',c('lab')]<-"SC  \n   31.9"
  text.labels[text.labels$id == 'NC',c('lab')]<-"NC 30.7"
  
  text.labels[text.labels$id == 'MI',c('x','y')]<-c(-84.8,43.5)
  
  text.labels[text.labels$id == 'NH',c('x','y','lab')]<-c(-74,47,'NH 27.2')
  text.labels[text.labels$id == 'VT',c('x','y','lab')]<-c(-75,46,'VT 24.7')
  
  text.labels[text.labels$id == 'MA',c('x','y')]<-c(-67,43)
  text.labels[text.labels$id == 'RI',c('x','y')]<-c(-68,40)
  text.labels[text.labels$id == 'CT',c('x','y')]<-c(-69,37)
  text.labels[text.labels$id == 'NJ',c('x','y')]<-c(-70,34)
  text.labels[text.labels$id == 'DE',c('x','y')]<-c(-70.8,31)
  text.labels[text.labels$id == 'MD',c('x','y')]<-c(-71.5,28)
  text.labels[text.labels$id == 'DC',c('x','y')]<-c(-72.3,25)
  
  text.labels$x<-as.numeric(text.labels$x)
  text.labels$y<-as.numeric(text.labels$y)
  
  text.labels$outside=text.labels$id %in% c('MA','RI','CT','NJ','DE','MD','DC')
  
  text.labels$init.horiz.x.start=text.labels$x
  text.labels$init.horiz.y.start=text.labels$y
  text.labels$init.horiz.x.end=text.labels$x-1
  text.labels$init.horiz.y.end=text.labels$y
  text.labels$second.horiz.x.start=text.labels$x-1
  text.labels$second.horiz.y.start=text.labels$y
  text.labels$second.horiz.x.end=text.labels$orig.x
  text.labels$second.horiz.y.end=text.labels$orig.y
  
  map.labels<-as.character(c(min(round(lnd_f$obese,1)),seq(22,35,2),max(round(lnd_f$obese,1))))
  
  map<-ggplot(lnd_f)+
    theme_bw()+
    geom_polygon(aes(x = long, y = lat,group = group, fill = obeseFactor))+ 
    geom_polygon(aes(x = long, y = lat,group = group), fill=NA,colour='white', size=.8)+
    #scale_fill_continuous(name='Obesity',low='#aaffaa',high='#ffaaaa')+
    scale_fill_manual(values=colors,
                      guide = guide_legend(title=paste('Percentage of Obese Adult Population ',year+1),
                                           title.position='top',
                                           label.position='bottom',
                                           label.hjust=0,
                                           keywidth=6,
                                           keyheight=2),
                      labels=map.labels)+
    #scale_fill_continuous(guide = guide_legend())+
    coord_map(project="conic", lat0 = 30)+
    geom_text(data = text.labels,aes(x = x,y=y,label=lab),colour='black',size=6) + 
    # lines
    #geom_segment(data=text.labels[text.labels$outside,],aes(x = init.horiz.x.start, y = init.horiz.y.start, xend = init.horiz.x.end, yend = init.horiz.y.end))+
    geom_segment(data=text.labels[text.labels$outside,],aes(x = second.horiz.x.start, y = second.horiz.y.start, xend = second.horiz.x.end, yend = second.horiz.y.end))+
    theme(axis.ticks = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x= element_blank(),
          axis.title.y= element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position="top",
          legend.title.align=.5,
          legend.title=element_text(size=20),
          legend.text=element_text(size=18))
  png(paste('/home/roberto/Documents/freelance/oDesk/obesity-remake-',year+1,'.png',sep=''),height=1200,width=1200)
  print(map)
  dev.off()
}

make.year.plot(2013)
make.year.plot(2012)
make.year.plot(2011)

