library(SDMTools);library(maptools)

indata=read.dbf('/home/jc148322/Bird_NARP/raw.data/ClimPolys.dbf')
files=list.files('/home/jc165798/working/NARP_birds/models/')

not.modelled=setdiff(indata$CLIMID,files)


poly.dir="/home/jc148322/Bird_NARP/raw.data/"

polys = readShapePoly(paste(poly.dir,'ClimPolys.shp',sep='')) #read in the polygon files

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files

cellarea = grid.area(base.asc) #get the area of individual cells
pos$area = cellarea[cbind(pos$row,pos$col)] #append the cell area
bird.data=read.csv('/home/jc148322/Bird_NARP/raw.data/ClimModels.csv')#get bird spp
oneline.summary=read.csv('/home/jc148322/Bird_NARP/species.outputs/1/1.one.line.summary.csv')

for (spp in not.modelled) { cat(spp,'\n')
	oneline.summary[1,]=NA
	common.name=as.character(bird.data$ClimName[which(bird.data$ClimID==spp)])
	if (length(common.name)==0) {common.name='none in ClimPolys.dbf'}
	spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); dir.create(spp.dir); setwd(spp.dir) #define the overarching species directory
	######separate polys
	tpolys = polys[which(polys@data$CLIMID==spp),]
	
	writePolyShape(tpolys,'tpoly')
	######calculate poly area
	pos.base=cbind(pos$row,pos$col,1)
	if (is.null(tpolys)) { tout=pos.base[,3]*0
	}else{
	tdata=cbind(pos[,1:2],pos.base[,3]);coordinates(tdata) = ~lon+lat ####WHY IS THIS NOT WORKING!!!???
	tout = overlay(tdata,tpolys);tout[which(is.na(tout))]=0 }
	total.area.of.poly=sum(tout*pos$area)/1000000000
	oneline.summary$common.name=common.name
	oneline.summary$CLIMID=spp
	oneline.summary$total.area.of.poly=total.area.of.poly
	write.csv(oneline.summary, paste(spp.dir, spp,'.one.line.summary.csv',sep=''))
	########bioclim
	load('/home/jc148322/Bird_NARP/pos.bc.Rdata')
	pos.bc=pos.bc[,5:8]*tout #set cells outside polygon to zero
	pos.bc=pos.bc[which(pos.bc$bc01>0),]
	bioclim.means=apply(pos.bc,2,mean)
	bioclim.sd=apply(pos.bc,2,sd)
	tt=c(bioclim.means,bioclim.sd)
	tt=tt[c(1,5,2,6,3,7,4,8)]
	tt=c(common.name,spp,tt)
	bioclim.summary=matrix(NA,nr=1,nc=length(tt));bioclim.summary[1,]=tt
	colnames(bioclim.summary)=c('common.name','ClimID','bc01.mean','bc01.sd','bc04.mean','bc04.sd','bc12.mean','bc12.sd','bc15.mean','bc15.sd')
	write.csv(bioclim.summary, paste(spp.dir, spp,'.one.line.bioclim.csv',sep=''))

}
	