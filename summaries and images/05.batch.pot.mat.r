
library(SDMTools)

species=list.files('/home/jc148322/Bird_NARP/models_1km/')

sh.dir='/home/jc148322/scripts/NARP_birds/pot_mat/';dir.create(sh.dir) #dir to write sh scripts to
for (spp in species[31:length(species)]){ cat(spp, '\n')
	setwd(sh.dir)
	##create the sh file
	 zz = file(paste('05.',spp,'.pot.mat.sh',sep=''),'w')
		 cat('#!/bin/bash\n',file=zz)
		 cat('cd $PBS_O_WORKDIR\n',file=zz)
		 cat("R CMD BATCH --no-save --no-restore '--args spp=\"",spp,"\" ' ~/scripts/NARP_birds/05.run.pot.mat.r 05.",spp,'.pot.mat.Rout \n',sep='',file=zz) #run the R script in the background
	close(zz) 

	##submit the script
	system(paste('qsub -l nodes=1:ppn=2 05.',spp,'.pot.mat.sh',sep=''))
}
