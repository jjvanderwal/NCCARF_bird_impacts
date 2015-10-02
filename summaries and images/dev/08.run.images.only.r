#Create image
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
#should have read in e.g.,
#spp='u1a'
#spp='u2'
#spp='u11a'
#spp='u214'

library(SDMTools); library(maptools); library(plotrix) #load the necessary libraries

poly.dir="/home/jc148322/Bird_NARP/raw.data/"
image.dir = "/home/jc148322/Bird_NARP/images2/";dir.create(image.dir)
states=readShapePoly(paste('/home/jc148322/AP02/climate.summaries/region_layers/Shapefile/STE11aAust.shp',sep=''))

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files
cellarea = grid.area(base.asc) #get the area of individual cells
pos$area = cellarea[cbind(pos$row,pos$col)] #append the cell area
aust.area=sum(pos$area)#area of terrestrial australia


bird.data=read.csv('/home/jc148322/Bird_NARP/raw.data/ClimModels.csv')#get bird spp
common.name=as.character(bird.data$ClimName[which(bird.data$ClimID==spp)])

###################################

spp.dir = paste('/home/jc165798/working/NARP_birds/models/',spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory

###################################
#Bring in all the necessary information
threshold = read.csv('output/maxentResults.csv'); threshold = threshold[which(threshold$Species==spp),] #read in teh maxent data and only keep info for the species of interest


###################################
#determine which threshold to use, and apply to pot.mat

if (threshold$Minimum.training.presence.area>0.8){
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1]/2
}else {
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1]#extract the species threshold value
}
spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory

files=list.files(pattern='tpoly')
if (length(files)==0){tpolys=NULL
}else{
tpolys = readShapePoly('tpoly.shp') }#read in the polygon files

#load asciis
potcurasc=read.asc.gz(paste(spp,'.current.asc.gz',sep=''))
RCP3PD.asc=read.asc.gz(paste(spp, '.RCP3PD.asc.gz',sep=''))
RCP6.asc=read.asc.gz(paste(spp, '.RCP6.asc.gz',sep=''))
RCP85.asc=read.asc.gz(paste(spp, '.RCP85.asc.gz',sep=''))
load('futpos.Rdata')
load('curpos.Rdata')

min.lat=min(min(curpos$lat[which(curpos[,6]>0)],na.rm=TRUE),min(futpos$lat[which(futpos[,6]>0)],na.rm=TRUE),min(futpos$lat[which(futpos[,7]>0)],na.rm=TRUE),min(futpos$lat[which(futpos[,8]>0)],na.rm=TRUE))

max.lat=max(max(curpos$lat[which(curpos[,6]>0)],na.rm=TRUE),max(futpos$lat[which(futpos[,6]>0)],na.rm=TRUE),max(futpos$lat[which(futpos[,7]>0)],na.rm=TRUE),max(futpos$lat[which(futpos[,8]>0)],na.rm=TRUE))

min.lon=min(min(curpos$lon[which(curpos[,6]>0)],na.rm=TRUE),min(futpos$lon[which(futpos[,6]>0)],na.rm=TRUE),min(futpos$lon[which(futpos[,7]>0)],na.rm=TRUE),min(futpos$lon[which(futpos[,8]>0)],na.rm=TRUE))

max.lon=max(max(curpos$lon[which(curpos[,6]>0)],na.rm=TRUE),max(futpos$lon[which(futpos[,6]>0)],na.rm=TRUE),max(futpos$lon[which(futpos[,7]>0)],na.rm=TRUE),max(futpos$lon[which(futpos[,8]>0)],na.rm=TRUE))
summary.table=read.csv(paste(spp,'.summary.csv',sep=''),as.is=TRUE)
area.total=as.numeric(summary.table[1,2])

setwd(image.dir)

bins = seq(0,1,length=101); bins = cut(threshold,bins,labels=FALSE) # get the threshold bin for cols
cols = c(rep('#E5E5E5',bins),colorRampPalette(c("tan","forestgreen"))(100)[bins:100]) #set colour ramp
#pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5)) #define corner points of legend
pnts=cbind(x=c(3,10,10,3), y=c(45,45,25,25))
zlim=c(0,1) #set z limits between 0 and 1
min.lon=min.lon-1;max.lon=max.lon+1;min.lat=min.lat-1;max.lat=max.lat+1

if (max.lat>=-18 & min.lat<=-34 |
	max.lon>=148 & min.lon<=120 | 
	(area.total/aust.area)>0.2) { 
	xlim=c(min(pos$lon),max(pos$lon)); 
	ylim=c(min(pos$lat),max(pos$lat))
	clip=base.asc
}else{ 
	xlim=c(min.lon,max.lon); 
	ylim=c(min.lat,max.lat)

	clippos=curpos; clippos[which(is.finite(clippos[,6])),6]=1
	clippos[which(clippos$lat<min.lat),6]=NA
	clippos[which(clippos$lat>max.lat),6]=NA
	clippos[which(clippos$lon<min.lon),6]=NA
	clippos[which(clippos$lon>max.lon),6]=NA
	clip=base.asc; clip[cbind(pos$row,pos$col)]=clippos[,6]}


	
png(paste(spp,'.png',sep=''),width=dim(base.asc)[1]*2+20, height=dim(base.asc)[2]*1+350, units='px', pointsize=20, bg='white')
	par(mar=c(3,3,3,2),mfrow=c(1,2),cex=1,oma=c(3,0,1,0))
	
	mat = matrix(c(4,3,3,3,3,3,
				1,1,1,2,2,2,
				1,1,1,2,2,2,
				1,1,1,2,2,2),nr=4,nc=6,byrow=TRUE) #create a layout matrix for images
	layout(mat) #call layout as defined above

	image(base.asc, ann=FALSE,axes=FALSE,col='white', zlim=zlim, xlim=xlim,ylim=ylim)
	clip(xlim[1],xlim[2],ylim[1],ylim[2])
	image(potcurasc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
    if (length(tpolys)>0) {plot(tpolys, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)}
	plot(states, lwd=0.5, ann=FALSE,axes=FALSE, add=TRUE,border="grey60")
	mtext('Current Climate Space', line=2,  side=1, cex=1.5)

	image(base.asc, ann=FALSE,axes=FALSE,col='white', zlim=zlim, xlim=xlim,ylim=ylim)
	clip(xlim[1],xlim[2],ylim[1],ylim[2])
	image(RCP85.asc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
    plot(states, lwd=0.5, ann=FALSE,axes=FALSE, add=TRUE,border="grey60")
	if (length(tpolys)>0) {plot(tpolys, lwd=2, ann=FALSE,axes=FALSE, add=TRUE)}
	mtext ('Current Emissions Trajectory - 2085', line=2,side=1,cex=1.5)
		
	
	par(mar=c(2,2,2,2))
	plot(1:20, axes=FALSE, ann=FALSE,type = "n")
	text (0.5, 18,common.name, cex=3,pos=4)
	color.legend(0.7,6,2.8,10,c('',''),cols,cex=1); text (2.9, 8,'Most suitable', cex=1.7,pos=4)
    legend(0.5,6, 'Boundary of core habitat',lwd=2, cex=1.7,bty='n',xjust=0)
	
	
	image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=zlim)
	image(clip,ann=FALSE,axes=FALSE, col="black",add=TRUE)
	plot(states, lwd=1, ann=FALSE,axes=FALSE, add=TRUE,border="grey60")

dev.off()
