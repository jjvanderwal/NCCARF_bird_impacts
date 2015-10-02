cp -r /home/jc148322/Bird_NARP/obsolete/species.outputs/* /home/jc148322/Bird_NARP/copy

#remove species now modelled to 1k
species=list.files('/home/jc165798/working/NARP_birds/models_1km/')

setwd('/home/jc148322/Bird_NARP/copy/')

for (spp in species) {cat(spp,'\n'); unlink(spp,recursive=TRUE) }

#remove all species not modelled
species=list.files('/home/jc148322/Bird_NARP/copy/')
modelled=list.files('/home/jc165798/working/NARP_birds/models/')
species=setdiff(species,modelled)

for (spp in species) {cat(spp,'\n'); unlink(spp,recursive=TRUE) }

#remove old files in spp directories, keep useful files
species=list.files('/home/jc148322/Bird_NARP/copy/')

for (spp in species) {cat(spp,'\n');
	setwd(paste('/home/jc148322/Bird_NARP/copy/',spp,sep=''))
	
	unlink(paste(spp,'.one.line.bioclim.csv',sep=''))
	unlink(paste(spp,'.one.line.summary.csv',sep=''))
	unlink(paste(spp,'.one.line.summary2.csv',sep=''))
	unlink(paste(spp,'.summary.csv',sep=''))
	unlink(paste(spp,'.summary2.csv',sep=''))

}

mv /home/jc148322/Bird_NARP/copy/* /home/jc148322/Bird_NARP/species.outputs/
