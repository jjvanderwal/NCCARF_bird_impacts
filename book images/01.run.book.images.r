#Create image
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
#should have read in e.g.,


library(SDMTools); library(maptools); library(plotrix) #load the necessary libraries

image.dir = "/home/jc148322/Bird_NARP/book_images/";dir.create(image.dir)
states=readShapePoly(paste('/home/jc148322/AP02/climate.summaries/region_layers/Shapefile/STE11aAust.shp',sep=''))

base.asc = read.asc.gz(paste('/home/jc165798/Climate/CIAS/Australia/',resolution,'/baseline.76to05/base.asc.gz',sep='')) #read in the base asc file
pos = read.csv(paste('/home/jc165798/Climate/CIAS/Australia/',resolution,'/baseline.76to05/base.positions.csv',sep=''),as.is=TRUE) #read in the position files
cellarea = grid.area(base.asc) #get the area of individual cells
pos$area = cellarea[cbind(pos$row,pos$col)] #append the cell area
aust.area=sum(pos$area)#area of terrestrial australia

bird.data=read.csv('/home/jc148322/Bird_NARP/raw.data/ClimModels.csv')#get bird spp
common.name=as.character(bird.data$ClimName[which(bird.data$ClimID==spp)])

###################################
#read in threshold
spp.dir = paste(model.dir,'/',spp,'/',sep=''); setwd(spp.dir) 
threshold = read.csv('output/maxentResults.csv'); threshold = threshold[which(threshold$Species==spp),] #read in teh maxent data and only keep info for the species of interest

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
current=read.asc.gz(paste(spp,'.current.asc.gz',sep=''))

RCP85.asc=read.asc.gz(paste(spp, '.RCP85.asc.gz',sep=''))
load('futpos.Rdata')
load('curpos.Rdata')

min.lat=min(min(curpos$lat[which(curpos[,ncol(curpos)]>0)],na.rm=TRUE),min(futpos$lat[which(futpos[,7]>0)],na.rm=TRUE))

max.lat=max(max(curpos$lat[which(curpos[,ncol(curpos)]>0)],na.rm=TRUE),max(futpos$lat[which(futpos[,7]>0)],na.rm=TRUE))

min.lon=min(min(curpos$lon[which(curpos[,ncol(curpos)]>0)],na.rm=TRUE),min(futpos$lon[which(futpos[,7]>0)],na.rm=TRUE))

max.lon=max(max(curpos$lon[which(curpos[,ncol(curpos)]>0)],na.rm=TRUE),max(futpos$lon[which(futpos[,7]>0)],na.rm=TRUE))
summary.table=read.csv('area.summary.csv',as.is=TRUE)
area.total=as.numeric(summary.table[1,1])

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

#image parameters
cex=3
m=1 #margin
state.lwd=0.5
poly.lwd=1

	
tiff(paste(spp,'.current.tiff',sep=''),width=11.6, height=12.11, units='cm', pointsize=5, res=1200, bg='white')
		
	mat = matrix(c(2,2,2,3,3,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1,
					1,1,1,1,1),nr=5,nc=5,byrow=TRUE) #create a layout matrix for images
	layout(mat) #call layout as defined above
	
	par(mar=c(m*3,m,0,m))
	
	image(base.asc, ann=FALSE,axes=FALSE,col='white', zlim=zlim, xlim=xlim,ylim=ylim)
	clip(xlim[1],xlim[2],ylim[1],ylim[2])
	image(current, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim,xpd=TRUE,add=TRUE)
	plot(states, lwd=state.lwd, ann=FALSE,axes=FALSE, add=TRUE,border="grey40")
    if (length(tpolys)>0) {plot(tpolys, lwd=poly.lwd, ann=FALSE,axes=FALSE, add=TRUE)}
	mtext('Current Climate Space', line=1,side=1, cex=0.8*cex)
	
	par(mar=c(0,m,0,m))
	plot(1:10, axes=FALSE, ann=FALSE,type = "n")
	color.legend(0.7,7,4,9,c('',''),cols); text (4.5, 8,'Most suitable',pos=4, cex=cex)
    legend(0.5,7, 'Boundary of core habitat',lwd=poly.lwd,bty='n',xjust=0, cex=cex)

	if (max.lat>=-18 & min.lat<=-34 |
	max.lon>=148 & min.lon<=120 | 
	(area.total/aust.area)>0.2) { 
	}else{ 
	par(mar=c(0,m,m,m*2))
	image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=zlim)
	image(clip,ann=FALSE,axes=FALSE, col="black",add=TRUE)
	plot(states, lwd=state.lwd, ann=FALSE,axes=FALSE, add=TRUE,border="grey60")
	}

dev.off()


tiff(paste(spp,'.2085.tiff',sep=''),width=11.6, height=10, units='cm', pointsize=5, res=1200, bg='white')
	par(mar=c(m*3,m,m,m))

	image(base.asc, ann=FALSE,axes=FALSE,col='white', zlim=zlim, xlim=xlim,ylim=ylim)
	clip(xlim[1],xlim[2],ylim[1],ylim[2])
	image(RCP85.asc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim,xpd=FALSE,add=TRUE)
    plot(states, lwd=state.lwd, ann=FALSE,axes=FALSE, add=TRUE,border="grey40")
	if (length(tpolys)>0) {plot(tpolys, lwd=poly.lwd, ann=FALSE,axes=FALSE, add=TRUE)}
	mtext ('Current Emissions Trajectory - 2085', line=1, side=1, cex=0.8*cex)
		
dev.off()













