#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
#define directories
sh.dir = '/home/jc148322/scripts/NARP_birds/birds_tmp_sh/';dir.create(sh.dir) #create a directory within which to put your sh scripts.
work.dir = '/home/jc148322/Bird_NARP/models_1km/';setwd(work.dir)
mxe.dir = '/home/jc165798/Climate/CIAS/Australia/1km/bioclim_mxe/'
maxent.jar = "/home/jc165798/working/NARP_birds/maxent.jar"
climate.dir = '/home/ctbccr/Ramona/Climate_Data/bioclim_mxe/1990/' #define the climate directory
################################################################################
#list the projections, cycle thorugh them and project the models onto them
proj.list = list.files(mxe.dir) #list the projections
proj.list = proj.list[intersect(grep('2085',proj.list),grep('RCP',proj.list))] #subset it to simply current and 2080 data

species = list.files() #get a list of species

#cycle through each of the species
for (spp in species) {
	spp.dir = paste(work.dir,spp,'/',sep=''); 
	spp.sh= paste(sh.dir,spp,'/',sep='');dir.create(spp.sh);setwd(spp.sh) #set the working directory to the species shell script directory
	zz = file('02.project.models.sh','w') ##create the sh file
		cat('#!/bin/bash\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat('source /etc/profile.d/modules.sh\n',file=zz) #necessary for tsch users?
		cat('module load java\n',file=zz)
		dir.create(paste(spp.dir,'output/ascii/',sep=''),recursive=TRUE) #create the output directory for all maps
		#cycle through the projections
		#for (tproj in proj.list) cat('java -mx1024m -cp ',maxent.jar,' density.Project ',spp.dir,'output/',spp,'.lambdas ',mxe.dir,tproj,' ',spp.dir,'output/ascii/',tproj,'.asc fadebyclamping nowriteclampgrid\n',sep="",file=zz)

		cat('java -mx2048m -cp ',maxent.jar,' density.Project ',spp.dir,'output/',spp,'.lambdas ',climate.dir,' ',spp.dir,'output/ascii/1990.asc fadebyclamping nowriteclampgrid\n',sep="",file=zz)
		
		cat('gzip ',spp.dir,'output/ascii/*asc\n',sep='',file=zz)
	close(zz) 


}

























