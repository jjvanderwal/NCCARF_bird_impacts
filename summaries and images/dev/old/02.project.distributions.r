#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
#define directories
work.dir = '/home/jc165798/working/NARP_birds/models/'
mxe.dir = '/home/jc165798/Climate/CIAS/Australia/5km/bioclim_mxe/'
maxent.jar = "/home/jc165798/working/NARP_birds/maxent.jar"

################################################################################
#list the projections, cycle thorugh them and project the models onto them
proj.list = list.files(pattern='RCP',mxe.dir) #list the projections

species = list.files(work.dir) #get a list of species

#cycle through each of the species
for (spp in species) {
	spp.dir = paste(work.dir,spp,'/',sep=''); setwd(spp.dir) #set the working directory to the species directory
	spplist = read.csv('output/maxentResults.csv',as.is=TRUE)$Species #get a full listing of species and subspecies run
	
	##create the sh file
	zz = file('02.project.models.sh','w')
		cat('#!/bin/bash\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat('module load java\n',file=zz)
		for (subspp in spplist) { #cycle through each of the subspecies
			dir.create(paste('output/ascii/',subspp,'/',sep=''),recursive=TRUE) #create the output directory for all maps
			#cycle through the projections
			for (tproj in proj.list) cat('java -mx1024m -cp ',maxent.jar,' density.Project ',spp.dir,'output/',subspp,'.lambdas ',mxe.dir,tproj,' ',spp.dir,'output/ascii/',subspp,'/',tproj,'.asc fadebyclamping nowriteclampgrid\n',sep="",file=zz)
			cat('gzip ',spp.dir,'output/ascii/',subspp,'/*asc\n',sep='',file=zz)
		}
	close(zz) 

	#submit the script
	system('qsub -m n 02.project.models.sh',wait = FALSE)
}
