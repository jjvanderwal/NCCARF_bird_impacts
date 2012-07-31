#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools) #load the necessary libraries

wd = '~/working/NARP_birds/'; setwd(wd) #define and set theworking directory
climate.dir = '~/Climate/CIAS/Australia/5km/baseline.76to05/bioclim/' #define the climate directory

occur = read.csv('raw.data/ClimData.csv.gz',as.is=TRUE,header=FALSE) #read in the observations
colnames(occur) = c('ClimID','Latitude','Longitude','Positional_Accuracy','Start_Date','Year') #define the column names
occur = occur[which(occur$Positional_Accuracy<=25000),] #keep only things with <20km accuracy
bkgd = read.csv('raw.data/ClimSurveys.csv.gz',as.is=TRUE) #read in the background data

occur$Latitude = round(occur$Latitude/0.05)*0.05; bkgd$Latitude = round(bkgd$Latitude/0.05)*0.05 #round latitude to 0.05 degrees
occur$Longitude = round(occur$Longitude/0.05)*0.05; bkgd$Longitude = round(bkgd$Longitude/0.05)*0.05 #round Longitude to 0.05 degrees

occur = occur[,c('ClimID','Latitude','Longitude')] #keep only columns of interest
bkgd$ClimID='bkgd'; bkgd = bkgd[,c('ClimID','Latitude','Longitude')] #keep only columns of interest
nrow(occur); occur = unique(occur); nrow(occur) #keep only unique info
nrow(bkgd); bkgd = unique(bkgd); nrow(bkgd) #keep only unique info

clim.vars = paste('bioclim_',sprintf('%02i',c(1,4,5,6,12,15,16,17)),sep='') #define the climate variables of interest
for (clim.var in clim.vars) { cat(clim.var,'\n') #cycle through each of hte climate variables
	occur[,clim.var] = extract.data(cbind(occur$Longitude,occur$Latitude), read.asc.gz(paste(climate.dir,clim.var,'.asc.gz',sep=''))) #append the climate data
	bkgd[,clim.var] = extract.data(cbind(bkgd$Longitude,bkgd$Latitude), read.asc.gz(paste(climate.dir,clim.var,'.asc.gz',sep=''))) #append the climate data
}
bkgd = na.omit(bkgd); occur = na.omit(occur) #remove missing data
write.csv(bkgd,'target_group_bkgd.csv',row.names=FALSE); rm(bkgd) #write out the file and remove it from memory

occur$ClimID = gsub('br&nb','br_nb',occur$ClimID)
SpNos = unique(occur$ClimID) #get the unique species numbers

for (SpNo in SpNos) {  cat(SpNo,'\n')#cycle through each of the species numbers
	toccur = occur[occur$ClimID==SpNo,] #get the observations for the species
	spp.dir = paste(wd,'models/',SpNo,'/',sep='') #define the species directory
	dir.create(paste(spp.dir,'output',sep=''),recursive=TRUE) #create the species directory
	write.csv(toccur,paste(spp.dir,'occur.csv',sep=''),row.names=FALSE) #write out the file and remove it from memory
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

