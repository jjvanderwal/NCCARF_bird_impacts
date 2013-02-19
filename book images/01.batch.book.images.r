#drafted by Lauren Hodgson ( lhodgson86@gmail.com ...  )
#GNU General Public License .. feel free to use / distribute ... no warranties
#alternative resolutions commented out
################################################################################
library(SDMTools)#load the necessary libraries
# model.dir='/home/jc148322/Bird_NARP/models_1km/'; setwd(model.dir) #1km directory
model.dir='/home/jc165798/working/NARP_birds/models/'; setwd(model.dir) #5km directory
# resolution='1km' 
resolution='5km'

species=list.files()

if (resolution=='5km') {
	spp1k=list.files('/home/jc165798/working/NARP_birds/models_1km/')
	species=setdiff(species,spp1k)
} #do not run summary on 5k models for species that have been modelled at 1km

sh.dir='/home/jc148322/scripts/NARP_birds/images.sh/';dir.create(sh.dir) #dir to write sh scripts to
for (spp in species){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste('01.',spp,'.images.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" model.dir=\"",model.dir,"\" resolution=\"",resolution,"\" ' ~/scripts/NARP_birds/01.run.book.images.r 01.",spp,'.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -l nodes=1:ppn=1 01.',spp,'.images.sh',sep=''))
}
