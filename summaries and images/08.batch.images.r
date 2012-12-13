
#drafted by Lauren Hodgson ( lhodgson86@gmail.com ...  )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools); #load the necessary libraries
model.dir='/home/jc148322/Bird_NARP/models_1km/'; setwd(model.dir) #define and set the working directory
#model.dir='/home/jc165798/working/NARP_birds/models/'; setwd(model.dir) #5km directory
species=list.files()

sh.dir='/home/jc148322/scripts/NARP_birds/images.sh/';dir.create(sh.dir) #dir to write sh scripts to
for (spp in species){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste('08.',spp,'.images.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" model.dir=\"",model.dir,"\" ' ~/scripts/NARP_birds/08.run.images.r 08.",spp,'.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -m n 08.',spp,'.images.sh',sep=''))
}
