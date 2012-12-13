##save clim polys individually for faster processing times
library(SDMTools); library(maptools) #load the necessary libraries

wd = "/home/jc165798/working/NARP_birds/models_1km/"; setwd(wd) #define and set the working directory

species=list.files()
poly.dir="/home/jc148322/Bird_NARP/raw.data/"

polys = readShapePoly(paste(poly.dir,'ClimPolys.shp',sep='')) #read in the polygon files

for (spp in species){cat(spp,'\n')
	tpolys = polys[which(polys@data$CLIMID==spp),]
	spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); setwd(spp.dir)
	writePolyShape(tpolys,'tpoly')
	}
	
#Then, to update polys to latest version

poly.dir="/home/jc148322/Bird_NARP/raw.data/"

polys = readShapePoly(paste(poly.dir,'Outstanding_TaxonPolys_Clip.shp',sep='')) #read in the polygon files
species=polys@data$CLIMID

for (spp in species){cat(spp,'\n')
	tpolys = polys[which(polys@data$CLIMID==spp),]
	spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); setwd(spp.dir)
	writePolyShape(tpolys,'tpoly')
	}

	