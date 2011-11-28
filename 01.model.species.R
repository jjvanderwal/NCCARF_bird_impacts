#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(RMySQL); library(SDMTools) #load the necessary libraries
wd = '~/working/NCCARF_2011_BIRD_project/'; setwd(wd) #define and set the working directory
climate.dir = '/home/31/jc165798/Climate/PCMDI/01.Oz.5km.61.90/bioclim/1975/' #define the climate directory
current.mxe.dir = '/home/31/jc165798/Climate/PCMDI/01.Oz.5km.61.90/mxe/1975/' #define the mxe directory for current climate to get map of predicted distribution

### extract species and background data from the database
conn = dbConnect("MySQL",host='spatialecology.jcu.edu.au',dbname='BirdsAustralia',username='mapusr',password='mapusr') #define the connection to the database
species = fetch(dbSendQuery(conn, statement="SELECT SpNo FROM TaxonList GROUP BY SpNo;"),n=-1); species = as.vector(species$SpNo)# get list of species
bkgd = fetch(dbSendQuery(conn, statement="SELECT surveyID, Latitude, Longitude, Positional_Accuracy FROM surveys;"),n=-1)#get the target group background
bkgd$lat = round(bkgd$Latitude/0.05)*0.05; bkgd$lon = round(bkgd$Longitude/0.05)*0.05; #round to nearest 5km grid cell... only need unique 5km grids for models
clim.vars = paste('bioclim_',sprintf('%02i',c(1,4,5,6,12,15,16,17)),sep='') #define the climate variables of interest
for (clim.var in clim.vars) { cat(clim.var,'\n') #cycle through each of hte climate variables
	bkgd[,clim.var] = extract.data(cbind(bkgd$Longitude,bkgd$Latitude), read.asc.gz(paste(climate.dir,clim.var,'.asc.gz',sep=''))) #append the climate data
}
out = data.frame(spp='bkgd',bkgd[,c('lat','lon',clim.vars)]); out = unique(na.omit(out)) #define the bkgd output file
write.csv(out,'target_group_bkgd.csv',row.names=FALSE); rm(out) #write out the file and remove it from memory

### cycle through each of the species
for (spp in species) { cat(spp,'... ') #cycle through each of the species
	occur = fetch(dbSendQuery(conn, statement=paste("
		SELECT surveyID, col
		FROM observations 
		WHERE SpNo=",spp,"
		GROUP BY surveyID, col;
		",sep='')),n=-1) #get the occurrence data
	if (length(occur)<1) { #if no occurrences... move to next species
		cat('No data ... nothing to run \n')
	} else { #run this as long as there are occurrences
		occur = merge(occur,bkgd) #merge with bkgd data
		out = data.frame(spp=sprintf('%04i',spp),occur[,c('lat','lon',clim.vars)]); out = unique(na.omit(out)) #define the bkgd output file
		spp.dir = paste(wd,'models/',sprintf('%04i',spp),'/',sep='')
		if (file.exists(paste(spp.dir,'occur.csv',sep=''))) { #check if data already exists
			cat('done \n')
		} else { #if not done... run the species if num of occur >= 10
			if (nrow(occur)<10) {
				cat('new but insufficient occurences ... nothing to run \n')
			} else { 
				cat('new ... submitting work \n')
				dir.create(paste(spp.dir,'output',sep=''),recursive=TRUE) #create the species directory
				write.csv(out,paste(spp.dir,'occur.csv',sep=''),row.names=FALSE); rm(out) #write out the file and remove it from memory
				zz = file(paste(spp.dir,'01.create.model.sh',sep=''),'w') #create the shell script to run the maxent model
					cat('#!/bin/bash\n',file=zz)
					cat('cd $PBS_O_WORKDIR\n',file=zz)
					cat('java -mx1024m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd.csv -s occur.csv -o output nothreshold nowarnings novisible replicates=10 nooutputgrids -r -a \n',sep="",file=zz)
					cat('cp -af output/maxentResults.csv output/maxentResults.crossvalide.csv\n',file=zz)
					cat('java -mx1024m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd.csv -s occur.csv -o output -j ',current.mxe.dir,' nothreshold nowarnings novisible nowriteclampgrid nowritemess writeplotdata -P -J -r -a \n',sep="",file=zz)
					cat('gzip output/*asc\n',file=zz)
				close(zz) 
				setwd(spp.dir); system(paste('qsub -m n -N ',sprintf('%04i',spp),' 01.create.model.sh',sep='')); setwd(wd) #submit the script			
			}
		}
	}
}

