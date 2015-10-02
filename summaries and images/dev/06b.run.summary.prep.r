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


image.dir = "/home/jc148322/Bird_NARP/images/"


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
load(file=paste(spp.dir,spp,'.potential.dist.mat.Rdata',sep='')) #load the potential matrix
pot.mat[which(pot.mat<threshold)] = 0 # change anything < threshold to 0

###################################
#overlay polygons
files=list.files(pattern='tpoly')
if (length(files)==0){tpolys=NULL
}else{
tpolys = readShapePoly('tpoly.shp') }#read in the polygon files


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
curpos=cbind(pos,pot.mat[,1]); save(x=curpos, file=paste(spp.dir, 'curpos.Rdata',sep=''))
futpos=cbind(pos,potasc.quant); save(futpos,file=paste(spp.dir, 'futpos.Rdata',sep=''))
min.lat=min(min(curpos$lat[which(curpos[,6]>0)],na.rm=TRUE),min(futpos$lat[which(futpos[,6:8]>0)],na.rm=TRUE))
max.lat=max(max(curpos$lat[which(curpos[,6]>0)],na.rm=TRUE),max(futpos$lat[which(futpos[,6:8]>0)],na.rm=TRUE))
min.lon=min(min(curpos$lon[which(curpos[,6]>0)],na.rm=TRUE),min(futpos$lon[which(futpos[,6:8]>0)],na.rm=TRUE))
max.lon=max(max(curpos$lon[which(curpos[,6]>0)],na.rm=TRUE),max(futpos$lon[which(futpos[,6:8]>0)],na.rm=TRUE))
###################################
#Generate polygon-based stats
if (is.null(tpolys)) { tout=pot.mat[,1]*0
}else{
tdata=cbind(pos[,1:2],pot.mat[,1]);coordinates(tdata) = ~lon+lat
tout = overlay(tdata,tpolys);tout[which(is.na(tout))]=0 }

in.poly=pot.mat*tout #make a copy of pot.mat and multiply by ploygon to determine if inside polygon.

tout2=tout; tout2[which(tout==1)]=2 #set areas outside polygon to 1
tout2[which(tout2==0)]=1
tout2[which(tout2==2)]=0

out.poly=pot.mat*tout2 #make a copy of pot.mat and multiply by ploygon to determine if outside polygon.

in.poly.quant = potasc.quant*tout
out.poly.quant = potasc.quant*tout2

###################################
#determine areas
source('/home/jc148322/scripts/NARP_birds/06b.run.summary.r')

write.csv(summary.tidy, paste(spp.dir, spp,'.summary2.csv',sep=''))
write.csv(oneline.summary, paste(spp.dir, spp,'.one.line.summary2.csv',sep=''))
