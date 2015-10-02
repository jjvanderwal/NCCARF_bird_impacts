#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
wd = '~/working/NARP_birds/models/'; setwd(wd) #define and set the working directory
species = list.files() #get a list of species that have been modelled

#cycle through each of the species
for (spp in species) {
	spp.dir = paste(wd,spp,'/',sep=''); setwd(spp.dir) #set the working directory to the species directory
	##create the sh file
	zz = file('99.image.models.sh','w')
		cat('#!/bin/bash\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat("R CMD BATCH --no-save --no-restore '--args wd=",'"',spp.dir,'"',"' ~/SCRIPTS/sdmcode/SDM/BirdsAustralia/NCCARF_2011_project/99.script2run.R ",spp,'.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	#submit the script
	system(paste('qsub -m n -N ',spp,' 99.image.models.sh',sep=''))
}
