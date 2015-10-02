args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
library(SDMTools); library(maptools)


base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files

cellarea = grid.area(base.asc) #get the area of individual cells
pos$area = cellarea[cbind(pos$row,pos$col)] #append the cell area

spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory

#overlay polygons
files=list.files(pattern='tpoly')
if (length(files)==0){tpolys=NULL
}else{
tpolys = readShapePoly('tpoly.shp') }#read in the polygon files


pos.base=cbind(pos$row,pos$col,1)
if (is.null(tpolys)) { tout=pos.base[,3]*0
}else{
tdata=cbind(pos[,1:2],pos.base[,3]);coordinates(tdata) = ~lon+lat
tout = overlay(tdata,tpolys);tout[which(is.na(tout))]=0 }
total.area.of.poly=sum(tout*pos$area)/1000000000

oneline.summary=read.csv(paste(spp,'.one.line.summary.csv',sep=''))
oneline.summary=cbind(oneline.summary,total.area.of.poly)

write.csv(oneline.summary, paste(spp.dir, spp,'.one.line.summary.csv',sep=''))

