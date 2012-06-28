#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
wd = '~/working/NARP_birds/models/'; setwd(wd) #define and set the working directory
species = list.files(wd,pattern='lambdas',recursive=TRUE,full.names=TRUE) #get a list of species that have been modelled
tt = species
for (ii in 0:9) { tt = tt[-grep(paste('_',ii,'.lambdas',sep=''),tt)] }
species = tt

#cycle through each of the species
for (tspp in species) {
	spp = gsub('.lambdas','',basename(tspp)) #get the species name
	spp.dir = gsub('output','',dirname(tspp)); setwd(spp.dir) #get the species directory
	##create the sh file
	zz = file(paste('05.',spp,'.classstats.sh',sep=''),'w')
		cat('#!/bin/bash\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"~/working/NARP_birds/\" spp.dir=",basename(spp.dir)," spp=\"",spp,"\" ' ~/SCRIPTS/sdmcode/SDM/BirdsAustralia/NCCARF_2011_project/05.script2run.R 05.",spp,'.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	#submit the script
	system(paste('qsub -m n 05.',spp,'.classstats.sh',sep=''))
}
