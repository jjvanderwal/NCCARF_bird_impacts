
library(SDMTools); library(maptools) #load the necessary libraries

wd = "/home/jc165798/working/NARP_birds/models/"; setwd(wd) #define and set the working directory

species=list.files()
species=species[10:length(species)]
poly.dir="/home/jc148322/Bird_NARP/raw.data/"

polys = readShapePoly(paste(poly.dir,'ClimPolys.shp',sep='')) #read in the polygon files

species=species[5:length(species)]
for (spp in species){cat(spp,'\n')
	tpolys = polys[which(polys@data$CLIMID==spp),]
	spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); setwd(spp.dir)
	writePolyShape(tpolys,'tpoly')
	}
	