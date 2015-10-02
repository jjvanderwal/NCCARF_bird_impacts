library(SDMTools); library(maptools) #load the necessary libraries
wd = "/home/jc165798/working/NARP_birds/models/"; setwd(wd) #define and set the working directory

species=list.files()

sh.dir='/home/jc148322/scripts/NARP_birds/patch.sh/';dir.create(sh.dir) #dir to write sh scripts to
for (spp in species){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste(spp,'.patch.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" ' ~/scripts/NARP_birds/patch.r ",spp,'.patch.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -m n ',spp,'.patch.sh',sep=''))
}
