#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################

work.dir = '/home/jc165798/working/NARP_birds/models/'; setwd(work.dir) #set the working directory
script.file = '~/SCRIPTS/sdmcode/SDM/BirdsAustralia/NCCARF_2011_project/03.script2run.R' #define the script to be run
species = list.files() #get a list of all the species

for (spp in species) { #cycle through each of the species
	spp.folder = paste(work.dir,spp,"/",sep=""); setwd(spp.folder)#create a folder for the species
	spp.arg = paste('spp="',spp,'" ',sep='') #define the species argument
	base.dir.arg = paste('base.dir="',work.dir,'" ',sep='') #define the base directory argument
	zz = file(paste('02.summarize.',spp,'.sh',sep=''),'w')##create the sh file
		cat('#!/bin/bash\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat("R CMD BATCH --no-save --no-restore '--args ",spp.arg,base.dir.arg,"' ",script.file,' 03.summarize.Rout --no-save \n',sep='',file=zz)
	close(zz)
			
	#submit the job
	system(paste('qsub -m n -l nodes=1:ppn=2 -l pmem=4gb 02.summarize.',spp,'.sh',sep=''))
}
	