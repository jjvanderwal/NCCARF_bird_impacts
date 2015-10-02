library(SDMTools)

out.dir='/home/jc148322/Bird_NARP/summary.data/'
wd='/home/jc148322/Bird_NARP/species.outputs/';setwd(wd)
bird.data=read.csv('/home/jc148322/Bird_NARP/raw.data/ClimModels.csv')#get bird spp
bird.data$ClimName=as.character(bird.data$ClimName)
species=list.files()

area.summary = suit.summary = bioclim.summary = NULL
for (spp in species) { cat(spp,'\n'); 
	common.name=bird.data$ClimName[which(bird.data$ClimID==spp)]
	common.name=gsub(',',' -',common.name)
	area=read.csv(paste(spp,'/area.summary.csv',sep=''),as.is=TRUE)
	suit=read.csv(paste(spp,'/suit.summary.csv',sep=''),as.is=TRUE)
	bioclim=read.csv(paste(spp,'/',spp,'.bioclim.csv',sep=''),as.is=TRUE)

	area=c(common.name,spp,area)
	suit=c(common.name,spp,suit)
	bioclim=c(common.name,spp,bioclim)
	
	area.summary = rbind(area.summary,area) 
	suit.summary = rbind(suit.summary,suit)
	bioclim.summary = rbind(bioclim.summary,bioclim)
	
	} #append all area, suitability, and bioclim tables together

setwd(out.dir)
write.csv(area.summary,'Area.summary.csv',row.names=FALSE)
write.csv(suit.summary,'Suitability.summary.csv',row.names=FALSE)
write.csv(bioclim.summary,'Bioclim.summary.csv',row.names=FALSE)

