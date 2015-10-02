library(SDMTools)

out.dir='/home/jc148322/Bird_NARP/summary.data/'
wd='/home/jc148322/Bird_NARP/species.outputs/';setwd(wd)

species=list.files()

summary.table = NULL
bioclim.table = NULL
for (spp in species) { cat(spp,'\n'); 
	oneline.summary=read.csv(paste(spp,'/',spp,'.one.line.summary.csv',sep=''),as.is=TRUE)
	oneline.summary=oneline.summary[,c(grep('common.name',colnames(oneline.summary)):grep('total.area.of.poly',colnames(oneline.summary))[1])]

	summary.table = rbind(summary.table,oneline.summary) 
	bioclim.table = rbind(bioclim.table,read.csv(paste(spp,'/',spp,'.one.line.bioclim.csv',sep=''),as.is=TRUE)) 
	
	} #merge all summary.table files and bioclim table files

setwd(out.dir)
write.csv(summary.table,'Area.summary.csv',row.names=FALSE)
write.csv(bioclim.table,'Bioclim.summary.csv',row.names=FALSE)
