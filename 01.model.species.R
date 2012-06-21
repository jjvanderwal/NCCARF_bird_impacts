#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools); library(maptools) #load the necessary libraries

wd = '~/working/NARP_birds/'; setwd(wd) #define and set theworking directory
climate.dir = '~/Climate/CIAS/Australia/5km/baseline.76to05/bioclim/' #define the climate directory

polys = readShapePoly('raw.data/unzipped/TaxonPolys_Dissolve.shp') #read in the polygon files
occur = read.csv('raw.data/observations.csv.gz',as.is=TRUE) #read in the observations
occur = occur[which(occur$Positional_Accuracy<=20000),] #keep only things with <20km accuracy
occur = occur[which(occur$rating %in% c(0,1,5)),] #keep only ratings of 0,1,5

clim.vars = paste('bioclim_',sprintf('%02i',c(1,4,5,6,12,15,16,17)),sep='') #define the climate variables of interest
for (clim.var in clim.vars) { cat(clim.var,'\n') #cycle through each of hte climate variables
	occur[,clim.var] = extract.data(cbind(occur$Lon,occur$Lat), read.asc.gz(paste(climate.dir,clim.var,'.asc.gz',sep=''))) #append the climate data
}
occur = na.omit(occur[,c('SpNo','Lat','Lon',colnames(occur)[grep('bioclim',colnames(occur))])]) #reorder the occur
bkgd = occur; bkgd$SpNo = 'bkgd'; bkgd = unique(bkgd) #define the background
write.csv(bkgd,'target_group_bkgd.csv',row.names=FALSE); rm(bkgd) #write out the file and remove it from memory

SpNos = sort(na.omit(as.numeric(unique(occur$SpNo)))) #get the unique species numbers

for (SpNo in SpNos) {  #cycle through each of the species numbers
	if (SpNo %in% polys@data$SPNO) { cat(SpNo,'\n')
		toccur = occur[occur$SpNo==SpNo,]; coordinates(toccur) = ~Lon+Lat #get a subset of the data
		tpolys = polys[which(polys@data$SPNO==SpNo),] #get the associated polygons
		tout = overlay(toccur,tpolys) #get what polygons a point falls within
		tdata = na.omit(data.frame(toccur,tpolys@data[tout,])) #append the associated polygon data with the occurence, omitting anything that fell outside a polygon
		tdata = tdata[which(tdata$RANGE_T %in% c('historic','core')),] #remove suspect, vagrant, escaped, irruptive and introduced records
		if (nrow(tdata)>0) {
			out = data.frame(spp = paste('u',SpNo,sep=''),tdata[,c('Lon','Lat',clim.vars)]) #create the FUll species model
			if (length(which(tdata$BR_RNGE_T=='breeding'))>0) out = rbind(out,data.frame(spp = paste('u',SpNo,'_breed',sep=''),tdata[which(tdata$BR_RNGE_T=='breeding'),c('Lon','Lat',clim.vars)])) #append the breeding range
			if (length(which(tdata$BR_RNGE_T=='non-breeding'))>0) out = rbind(out,data.frame(spp = paste('u',SpNo,'_nonbreed',sep=''),tdata[which(tdata$BR_RNGE_T=='non-breeding'),c('Lon','Lat',clim.vars)])) #append the breeding range

			subspps = as.vector(tdata$TAXONID); subspps = gsub('u','',subspps); subspps = gsub(SpNo,'',subspps) #get teh subspecies designations
			
			for (subspp in unique(subspps)) { # cycle through each of the subspecies
				if (nchar(subspp)>0) {
					rois = grep(subspp,subspps) #get a list of the subspecies and hybrid zones
					if (length(rois)>0) out = rbind(out,data.frame(spp = paste('u',SpNo,subspp,sep=''),tdata[rois,c('Lon','Lat',clim.vars)])) #get the core of the species
				}
			}
			out$spp = as.character(out$spp) #convert from factor to character

			print(unique(out$spp))
			
			spp.dir = paste(wd,'models/',SpNo,'/',sep='') #define the species directory
			dir.create(paste(spp.dir,'output',sep=''),recursive=TRUE) #create the species directory
			write.csv(out,paste(spp.dir,'occur.csv',sep=''),row.names=FALSE) #write out the file and remove it from memory
			zz = file(paste(spp.dir,'01.create.model.sh',sep=''),'w') #create the shell script to run the maxent model
				cat('#!/bin/bash\n',file=zz)
				cat('cd $PBS_O_WORKDIR\n',file=zz)
				cat('module load java\n',file=zz)
				cat('java -mx1024m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd.csv -s occur.csv -o output nothreshold nowarnings novisible replicates=10 nooutputgrids -r -a \n',sep="",file=zz)
				cat('cp -af output/maxentResults.csv output/maxentResults.crossvalide.csv\n',file=zz)
				cat('java -mx1024m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd.csv -s occur.csv -o output nothreshold nowarnings novisible nowriteclampgrid nowritemess writeplotdata -P -J -r -a \n',sep="",file=zz)
			close(zz) 
			setwd(spp.dir); system(paste('qsub -m n -N ',SpNo,' 01.create.model.sh',sep='')); setwd(wd) #submit the script	
		}
	}
}

