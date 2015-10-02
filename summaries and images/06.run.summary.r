#drafted by Lauren Hodgson <lhodgson86@gmail.com>
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
#get the command line arguments
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
library(SDMTools); library(maptools) #load the necessary libraries

#define directories
spp.dir = paste(model.dir,spp,'/',sep=''); setwd(spp.dir) #define the input species data directory

##----------------------------------------------------------------
#Bring in all the necessary information
base.asc = read.asc.gz(paste('/home/jc165798/Climate/CIAS/Australia/',resolution,'/baseline.76to05/base.asc.gz',sep='')) #read in the base asc file
pos = read.csv(paste('/home/jc165798/Climate/CIAS/Australia/',resolution,'/baseline.76to05/base.positions.csv',sep=''),as.is=TRUE) #read in the position files
cellarea = grid.area(base.asc) #get the area of individual cells
pos$area = cellarea[cbind(pos$row,pos$col)] #append the cell area
bird.data=read.csv('/home/jc148322/Bird_NARP/raw.data/ClimModels.csv')#get bird spp
common.name=as.character(bird.data$ClimName[which(bird.data$ClimID==spp)])

##----------------------------------------------------------------
#determine which threshold to use, and apply to pot.mat
threshold = read.csv('output/maxentResults.csv'); threshold = threshold[which(threshold$Species==spp),] #read in teh maxent data and only keep info for the species of interest
if (threshold$Minimum.training.presence.area>0.8){
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1]/2
}else {
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1]#extract the species threshold value
}
spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory
load(file=paste(spp.dir,spp,'.potential.dist.mat.Rdata',sep='')) #load the potential matrix
pot.mat[which(pot.mat<threshold)] = 0 # change anything < threshold to 0

##----------------------------------------------------------------
#Create asciis, from which images will be generated
current=base.asc; current[cbind(pos$row,pos$col)]=pot.mat[,1] #create the current ascii

#RCP85
cois=grep('RCP85', colnames(pot.mat), value=T) #determine columns of interest - RCP85

outquant=t(apply(pot.mat[,cois],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the 50th percentile
tenth=base.asc; tenth[cbind(pos$row,pos$col)]=outquant[,1] #create future median ascii
fiftieth=base.asc; fiftieth[cbind(pos$row,pos$col)]=outquant[,2] #create future median ascii
ninetieth=base.asc; ninetieth[cbind(pos$row,pos$col)]=outquant[,3] #create future median ascii

write.asc.gz(current,paste(spp.dir, spp, '.current',sep='')) #write out the ascii
write.asc.gz(fiftieth,paste(spp.dir, spp, '.RCP85',sep='')) #write out the ascii

curpos=cbind(pos,pot.mat[,1]); save(x=curpos, file=paste(spp.dir, 'curpos.Rdata',sep='')) #to be used in images
futpos=cbind(pos,outquant); save(futpos,file=paste(spp.dir, 'futpos.Rdata',sep='')) #to be used in images

##----------------------------------------------------------------
#read in polygon
files=list.files(pattern='tpoly')
if (length(files)==0){tpolys=NULL
}else{
tpolys = readShapePoly('tpoly.shp') }#read in the polygon files

#overlay polygon
if (is.null(tpolys)) { tout=pot.mat[,1]*0
}else{
tdata=cbind(pos[,c('lat','lon')],pot.mat[,1]);coordinates(tdata) = ~lon+lat
tout = overlay(tdata,tpolys);tout[which(is.na(tout))]=0 }

quantiles=cbind(pot.mat[,1],outquant)

in.poly=quantiles*tout #make a copy of pot.mat and multiply by ploygon to determine if inside polygon.

tout2=tout; tout2[which(tout==1)]=2 #set areas outside polygon to 1
tout2[which(tout2==0)]=1
tout2[which(tout2==2)]=0

out.poly=quantiles*tout2 #make a copy of pot.mat and multiply by ploygon to determine if outside polygon.

##----------------------------------------------------------------
#determine areas inside and outside polygons

#find and summarise areas and suitability: total; inside polygon, outside polygon
vois=c('area','suit')
inputs=c('quantiles','in.poly','out.poly')
varnames=c('total','in.poly','out.poly')
statnames=c('current','10th','50th','90th')

for (tvar in vois) {
	full.summary=NULL;i=0
	for (input in inputs) { i=i+1
		tdata=get(input);
		if (tvar=='area') tdata[which(tdata>0)]=1; 
		tdata=(tdata*pos$area) #find the areas of each cell above the threshold
		tsummary=apply(tdata,2,sum) #get the raw areas for each gcm
		tsummary=as.data.frame(t(tsummary)) #turn it into a one line data frame
		for (ii in 1:ncol(tsummary)){
			colnames(tsummary)[ii]=paste(varnames[i],statnames[ii],sep='.')} #name the columns appropriately
		if (input==inputs[1])full.summary=tsummary else full.summary=cbind(full.summary,tsummary)
	}
	write.csv(full.summary, paste(spp.dir,tvar,'.summary.csv',sep=''),row.names=FALSE)
}


##----------------------------------------------------------------
#Summarise bioclim variables within core polygon range
load(paste('/home/jc148322/Bird_NARP/bioclim_',resolution,'.Rdata',sep=''))
pos.bc=pos.bc[,5:8]*tout #set cells outside polygon to zero
pos.bc=pos.bc[which(pos.bc$bc01>0),]
bioclim.means=apply(pos.bc,2,mean)
bioclim.sd=apply(pos.bc,2,sd)
tt=c(bioclim.means,bioclim.sd)
tt=tt[c(1,5,2,6,3,7,4,8)]
tt=c(common.name,spp,tt)
bioclim.summary=matrix(NA,nr=1,nc=length(tt));bioclim.summary[1,]=tt
colnames(bioclim.summary)=c('common.name','ClimID','bc01.mean','bc01.sd','bc04.mean','bc04.sd','bc12.mean','bc12.sd','bc15.mean','bc15.sd')
write.csv(bioclim.summary, paste(spp.dir, spp,'.bioclim.csv',sep=''),row.names=FALSE)

##----------------------------------------------------------------


