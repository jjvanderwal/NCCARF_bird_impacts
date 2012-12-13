#drafted by Lauren Hodgson <lhodgson86@gmail.com>
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################

#get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
#should have read in e.g.,
#spp='u1b'
#spp='u2'
#spp='u11a'
#spp='u214'
#spp='250a'
library(SDMTools); library(maptools) #load the necessary libraries

poly.dir="/home/jc148322/Bird_NARP/raw.data/"
image.dir = "/home/jc148322/Bird_NARP/images/"

polys = readShapePoly(paste(poly.dir,'ClimPolys.shp',sep='')) #read in the polygon files
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files
cellarea = grid.area(base.asc) #get the area of individual cells
pos$area = cellarea[cbind(pos$row,pos$col)] #append the cell area
aust.area=sum(pos$area)#area of terrestrial australia


bird.data=read.csv('/home/jc165798/working/NARP_birds/raw.data/data dumps_Jul26/ClimModels.csv')#get bird spp
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
spp.dir = paste('/home/jc148322/Bird_NARP/',spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory
load(file=paste(spp.dir,spp,'.potential.dist.mat.Rdata',sep='')) #load the potential matrix
pot.mat[which(pot.mat<threshold)] = 0 # change anything < threshold to 0

###################################
#overlay polygons
tpolys = polys[which(polys@data$CLIMID==spp),]


###################################
#Create asciis
#RCP85
cois=grep('RCP85', colnames(pot.mat), value=T) #determine columns of interest - RCP85
pot.mat2=pot.mat[,cois]#subset columns of interest

RCP85.quant=t(apply(pot.mat2[,],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) })) #get the percentiles

RCP85.asc=base.asc; RCP85.asc[cbind(pos$row,pos$col)]=RCP85.quant #create the asciis

#RCP45
cois=grep('RCP6', colnames(pot.mat), value=T) #determine columns of interest - RCP85
pot.mat2=pot.mat[,cois]#subset columns of interest

RCP6.quant=t(apply(pot.mat2[,],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) })) #get the percentiles

RCP6.asc=base.asc; RCP6.asc[cbind(pos$row,pos$col)]=RCP6.quant #create the asciis


#RCP3PD
cois=grep('RCP3PD', colnames(pot.mat), value=T) #determine columns of interest - RCP85
pot.mat2=pot.mat[,cois]#subset columns of interest

RCP3PD.quant=t(apply(pot.mat2[,],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) })) #get the percentiles

RCP3PD.asc=base.asc; RCP3PD.asc[cbind(pos$row,pos$col)]=RCP3PD.quant #create the asciis

#
potasc.quant=cbind(as.vector(RCP3PD.quant),as.vector(RCP6.quant),as.vector(RCP85.quant))
potcurasc=base.asc; potcurasc[cbind(pos$row,pos$col)]=pot.mat[,1] #create the asciis

###################################
#find min/max lats and lons
curpos=cbind(pos,pot.mat[,1])
futpos=cbind(pos,potasc.quant)
min.lat=min(min(curpos$lat[which(curpos[,6]>0)],na.rm=TRUE),min(futpos$lat[which(futpos[,6:8]>0)],na.rm=TRUE))
max.lat=max(max(curpos$lat[which(curpos[,6]>0)],na.rm=TRUE),max(futpos$lat[which(futpos[,6:8]>0)],na.rm=TRUE))
min.lon=min(min(curpos$lon[which(curpos[,6]>0)],na.rm=TRUE),min(futpos$lon[which(futpos[,6:8]>0)],na.rm=TRUE))
max.lon=max(max(curpos$lon[which(curpos[,6]>0)],na.rm=TRUE),max(futpos$lon[which(futpos[,6:8]>0)],na.rm=TRUE))
###################################
#Generate polygon-based stats
tdata=cbind(pos[,1:2],pot.mat[,1]);coordinates(tdata) = ~lon+lat
tout = overlay(tdata,tpolys);tout[which(is.na(tout))]=0

in.poly=pot.mat*tout #make a copy of pot.mat and multiply by ploygon to determine if inside polygon.

tout2=tout; tout2[which(tout==1)]=2 #set areas outside polygon to 1
tout2[which(tout2==0)]=1
tout2[which(tout2==2)]=0

out.poly=pot.mat*tout2 #make a copy of pot.mat and multiply by ploygon to determine if outside polygon.

in.poly.quant = potasc.quant*tout
out.poly.quant = potasc.quant*tout2

###################################
#determine areas
source('/home/jc148322/scripts/NARP_birds/06.run.summary.RCPs.r')

###################################
#Create image

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

png(paste(spp,'.png',sep=''),width=dim(base.asc)[1]*3+30, height=dim(base.asc)[2]*2+80, units='px', pointsize=20, bg='white')
	par(mar=c(2,2,2,0),mfrow=c(3,2),cex=1,oma=c(3,3,3,0))
	
	mat = matrix(c(1,1,1,6,2,2,2,2,2,
				1,1,1,7,7,2,2,2,2,
				1,1,1,7,7,2,2,2,2,
				3,3,3,4,4,4,5,5,5,
				3,3,3,4,4,4,5,5,5,
				3,3,3,4,4,4,5,5,5),nr=6,nc=9,byrow=TRUE) #create a layout matrix for images
	layout(mat) #call layout as defined above

	image(potcurasc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim)
    plot(tpolys, ann=FALSE,axes=FALSE, add=TRUE)
	mtext('Current', line=1,  side=2, cex=2)

	plot(1:60, axes=FALSE, ann=FALSE,type = "n")
	text (6, 60,common.name, cex=3,pos=4)
    legend(5, 55, legend = summary.table[,1], ncol = 1,cex=1.7,bty='n',xjust=0)
    legend(22, 55, legend = summary.table[,2:5], ncol = 4,cex=1.7,bty='n',xjust=0)
	
	image(RCP3PD.asc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim)
    plot(tpolys, ann=FALSE,axes=FALSE, add=TRUE)
    mtext('2085', line=1,  side=2, cex=2)
	mtext ('RCP3PD - low', line=2,side=1,cex=1.5)
	
    image(RCP6.asc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim)
    plot(tpolys, ann=FALSE,axes=FALSE, add=TRUE)
	mtext ('RCP6 - med', line=2,side=1,cex=1.5)
	
    image(RCP85.asc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim, xlim=xlim,ylim=ylim)
    plot(tpolys, ann=FALSE,axes=FALSE, add=TRUE)
	mtext ('RCP85 - high', line=2,side=1,cex=1.5)
    
	image(base.asc,ann=FALSE,axes=FALSE,col='#E5E5E5', zlim=zlim)
	image(clip,ann=FALSE,axes=FALSE, col="black",add=TRUE)

	plot(1:60, axes=FALSE, ann=FALSE,type = "n", xlim=c(1,60),ylim=c(1,60))
	legend.gradient(pnts,cols=cols,limits=round(zlim,digits=2), title='Suitability', cex=1.7)
	
	legend(1,17, 'Polygon of core habitat',lwd=2, cex=1.7,bty='n',xjust=0)
    	
dev.off()
