#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools) #load the necessary libraries

wd = '~/working/NARP_birds/'; setwd(wd) #define and set theworking directory
climate.dir = '~/Climate/CIAS/Australia/1km/baseline.76to05/bioclim/' #define the climate directory
mxe.dir = '~/Climate/CIAS/Australia/1km/bioclim_mxe/'#define the projection directories
proj.list = list.files(mxe.dir) #list the projections
proj.list = proj.list[intersect(grep('2085',proj.list),grep('RCP8',proj.list))] #subset it to simply current and 2080 data
spp.oi = c("355","370","435","u242e","u253a","u25b","u261b","u281a","u282e","u29c","u333a","u350a",
	"u351","u355a","u355b","u361e","u368b","u370a","u370b","u375c","u389","u405d","u413b","u414",
	"u421a","u435a","u435b","u454a","u454b","u471a","u474","u493a","u494d","u496","u506a","u558b",
	"u558e","u565a","u565a_nb","u565a_br&nb","u591a","u605a","u606a","u611","u612a","u614a","u615","u648a",
	"u660","u672c","u677b","u678","u679a","u685","u686","u694f","u779a","u834","u946") #define the species here of interest
	
occur = read.csv('raw.data/ClimData.csv.gz',as.is=TRUE,header=FALSE) #read in the observations
colnames(occur) = c('ClimID','Latitude','Longitude','Positional_Accuracy','Start_Date','Year') #define the column names
occur = occur[which(occur$ClimID %in% spp.oi),] #keep on the spp of interest
occur = occur[which(occur$Positional_Accuracy<=25000),] #keep only things with <20km accuracy
bkgd = read.csv('raw.data/ClimSurveys.csv.gz',as.is=TRUE) #read in the background data
tasc = read.asc(paste(climate.dir,'bioclim_01.asc',sep='')) #read in a single asc file

round.latlon = function(x,pts) { #define a function to round lat & lon to midpoints of ascii file
    xy <- getXYcoords(x)
    xy$x <- xy$x + attr(x, "cellsize")/2
    xy$x <- c(xy$x[1] - attr(x, "cellsize"), xy$x)
    xy$y <- xy$y + attr(x, "cellsize")/2
    xy$y <- c(xy$y[1] - attr(x, "cellsize"), xy$y)
    xf <- xy$x[as.numeric(cut(pts[, 1], xy$x))]
    yf <- xy$y[as.numeric(cut(pts[, 2], xy$y))]
    return(cbind(xf, yf))
}
tt = round.latlon(tasc,cbind(occur$Longitude,occur$Latitude)) #round data to accuracy of climate data
occur$Latitude = tt[,2]; occur$Longitude = tt[,1] #round data to accuracy of climate data
tt = round.latlon(tasc,cbind(bkgd$Longitude,bkgd$Latitude)) #round data to accuracy of climate data
bkgd$Latitude = tt[,2]; bkgd$Longitude = tt[,1] #round data to accuracy of climate data

occur = occur[,c('ClimID','Latitude','Longitude')] #keep only columns of interest
bkgd$ClimID='bkgd'; bkgd = bkgd[,c('ClimID','Latitude','Longitude')] #keep only columns of interest
nrow(occur); occur = na.omit(occur); occur = unique(occur); nrow(occur) #keep only unique info
nrow(bkgd); bkgd = na.omit(bkgd); bkgd = unique(bkgd); nrow(bkgd) #keep only unique info

clim.vars = paste('bioclim_',sprintf('%02i',c(1,4,5,6,12,15,16,17)),sep='') #define the climate variables of interest
for (clim.var in clim.vars) { cat(clim.var,'\n') #cycle through each of hte climate variables
	tasc = read.asc(paste(climate.dir,clim.var,'.asc',sep=''))
	occur[,clim.var] = extract.data(cbind(occur$Longitude,occur$Latitude), tasc) #append the climate data
	bkgd[,clim.var] = extract.data(cbind(bkgd$Longitude,bkgd$Latitude), tasc) #append the climate data
}
bkgd = na.omit(bkgd); occur = na.omit(occur) #remove missing data
write.csv(bkgd,'target_group_bkgd_1km.csv',row.names=FALSE); rm(bkgd) #write out the file and remove it from memory

occur$ClimID = gsub('br&nb','br_nb',occur$ClimID) #replace the damn &
SpNos = unique(occur$ClimID) #get the unique species numbers

for (SpNo in SpNos) {  cat(SpNo,'\n')#cycle through each of the species numbers
	toccur = occur[occur$ClimID==SpNo,] #get the observations for the species
	spp.dir = paste(wd,'models_1km/',SpNo,'/',sep='') #define the species directory
	dir.create(paste(spp.dir,'output/ascii/',sep=''),recursive=TRUE) #create the species directory
	write.csv(toccur,paste(spp.dir,'occur.csv',sep=''),row.names=FALSE) #write out the file and remove it from memory
	zz = file(paste(spp.dir,'01.create.model.sh',sep=''),'w') #create the shell script to run the maxent model
		cat('#!/bin/bash\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat('module load java\n',file=zz)
		cat('java -mx2048m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd_1km.csv -s occur.csv -o output nothreshold nowarnings novisible replicates=10 nooutputgrids -r -a \n',sep="",file=zz)
		cat('cp -af output/maxentResults.csv output/maxentResults.crossvalide.csv\n',file=zz)
		cat('java -mx2048m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd_1km.csv -s occur.csv -o output nothreshold nowarnings novisible nowriteclampgrid nowritemess writeplotdata -P -J -r -a \n\n',sep="",file=zz)
		#cycle through the projections
		for (tproj in proj.list) {
			cat('java -mx2048m -cp ',wd,'maxent.jar density.Project ',spp.dir,'output/',SpNo,'.lambdas ',mxe.dir,tproj,' ',spp.dir,'output/ascii/',tproj,'.asc fadebyclamping nowriteclampgrid\n',sep="",file=zz)
		}
		cat('java -mx2048m -cp ',wd,'maxent.jar density.Project ',spp.dir,'output/',SpNo,'.lambdas ',climate.dir,' ',spp.dir,'output/ascii/1990.asc fadebyclamping nowriteclampgrid\n',sep="",file=zz)
		cat('gzip ',spp.dir,'output/ascii/*asc\n\n',sep='',file=zz)
	close(zz) 
	setwd(spp.dir); system(paste('qsub -m n -N ',SpNo,' 01.create.model.sh',sep='')); setwd(wd) #submit the script	
}
