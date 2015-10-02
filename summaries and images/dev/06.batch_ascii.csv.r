
#drafted by Lauren Hodgson ( lhodgson86@gmail.com ...  )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools); library(maptools) #load the necessary libraries
wd = "/home/jc165798/working/NARP_birds/models_1km/"; setwd(wd) #define and set the working directory

species=list.files()

sh.dir='/home/jc148322/scripts/NARP_birds/ascii_csv.sh/' #dir to write sh scripts to
for (spp in species[1:2]){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste('06.',spp,'.csv.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" ' ~/scripts/NARP_birds/06.run.ascii.csv.r 06.",spp,'.csv.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -m n 06.',spp,'.csv.sh',sep=''))
}
