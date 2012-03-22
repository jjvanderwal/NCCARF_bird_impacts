#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(RMySQL); library(SDMTools) #load the necessary libraries
wd = '~/working/NARP_birds/'; setwd(wd) #define and set the working directory
climate.dir = '~/Climate/PCMDI/01.Oz.5km.61.90/bioclim/1975/' #define the climate directory

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

###work with species glenn sent... not final datasets... not to be trusted...
occur = read.csv('RAW/DataToRun.csv',as.is=TRUE) #read in the data
occur = occur[which(occur$Positional_Accuracy<=20000),] #remove any location with < 20 km accuracy
occur$lat = round(occur$Latitude/0.05)*0.05; occur$lon = round(occur$Longitude/0.05)*0.05; #round to nearest 5km grid cell... only need unique 5km grids for models
occur$TID = paste('u',occur$SpNo,sep='') #get the species ID
occur$subspp = apply(cbind(occur$TID,occur$TaxonID),1,function(x) {gsub(x[1],'',x[2])}) #extract the subspecies information
occur = unique(occur[,c('lat','lon','TID','subspp','rnge_t','brRnge_t')]) #only keep a subset of hte data
for (clim.var in clim.vars) { cat(clim.var,'\n') #cycle through each of hte climate variables
	occur[,clim.var] = extract.data(cbind(occur$lon,occur$lat), read.asc.gz(paste(climate.dir,clim.var,'.asc.gz',sep=''))) #append the climate data
}
occur = na.omit(occur) #remove NA data


for (spp in unique(occur$TID)) { cat(spp,'\n') #cycle through each of the species
	tdata = occur[which(occur$TID==spp),] #get the subset that has the species of interest
	out = data.frame(spp = paste(spp,'FULL',sep='_'),tdata[,c('lon','lat',clim.vars)]) #create the FUll species model
	out = rbind(out,data.frame(spp = paste(spp,'core',sep='_'),tdata[which(tdata$rnge_t=='core'),c('lon','lat',clim.vars)])) #get the core of the species
	if (length(grep("historic",tdata$rnge_t))>0) out = rbind(out,data.frame(spp = paste(spp,'core',"historic",sep='_'),tdata[which(tdata$rnge_t %in% c('core',"historic")),c('lon','lat',clim.vars)])) #add historic info
	if (length(grep("irruptive",tdata$rnge_t))>0) out = rbind(out,data.frame(spp = paste(spp,"irruptive",sep='_'),tdata[which(tdata$rnge_t=="irruptive"),c('lon','lat',clim.vars)])) #add introduced info
	if (length(grep("introduced",tdata$rnge_t))>0) out = rbind(out,data.frame(spp = paste(spp,"introduced",sep='_'),tdata[which(tdata$rnge_t=="introduced"),c('lon','lat',clim.vars)])) #add irruptive info
	if (length(which(tdata$brRnge_t=='b'))>0) out = rbind(out,data.frame(spp = paste(spp,'breed',sep='_'),tdata[which(tdata$brRnge_t=='b'),c('lon','lat',clim.vars)])) #append the breeding range
	if (length(which(tdata$brRnge_t=='nb'))>0) out = rbind(out,data.frame(spp = paste(spp,'nonbreed',sep='_'),tdata[which(tdata$brRnge_t=='nb'),c('lon','lat',clim.vars)])) #append the breeding range
	for (subspp in unique(tdata$subspp)) { # cycle through each of the subspecies
		if (nchar(subspp)>0) {
			rois = which(tdata$subspp==subspp & tdata$rnge_t=='core') #define rows of interest that are core for the subspecies
			if (length(rois)>0) out = rbind(out,data.frame(spp = paste(spp,subspp,'core',sep='_'),tdata[rois,c('lon','lat',clim.vars)])) #get the core of the species
			rois.hist = which(tdata$subspp==subspp & tdata$rnge_t %in% c('core',"historic")) #define rows of interest that are core & historic for the subspecies
			if (length(rois.hist)>0 & length(rois)!=length(rois.hist)) out = rbind(out,data.frame(spp = paste(spp,subspp,'core',"historic",sep='_'),tdata[rois.hist,c('lon','lat',clim.vars)])) #get the core of the species
			if (nchar(subspp)==1) { #if dealing with a subspecies
				if (length(grep(subspp,tdata$subspp)) != length(which(tdata$subspp==subspp))) { #continue if there is hybrid zone information
					tdata2 = tdata[grep(subspp,tdata$subspp),] #keep subspecies and hybrid zone info
					rois = which(tdata2$rnge_t=='core') #define rows of interest that are core for the subspecies
					if (length(rois)>0) out = rbind(out,data.frame(spp = paste(spp,subspp,'core','hybridzone',sep='_'),tdata2[rois,c('lon','lat',clim.vars)])) #get the core of the species
					rois.hist = which(tdata2$rnge_t %in% c('core',"historic")) #define rows of interest that are core & historic for the subspecies
					if (length(rois.hist)>0 & length(rois)!=length(rois.hist)) out = rbind(out,data.frame(spp = paste(spp,subspp,'core','hybridzone',"historic",sep='_'),tdata2[rois.hist,c('lon','lat',clim.vars)])) #get the core of the species
				}
			}		
		}
	
	}
	out$spp = as.character(out$spp) #convert from factor to character

	spp.dir = paste(wd,'models/',spp,'/',sep='') #define the species directory
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
	setwd(spp.dir); system(paste('qsub -m n -N ',spp,' 01.create.model.sh',sep='')); setwd(wd) #submit the script			
}


### this is data in database... more trustworthy than glenns data dump
### cycle through each of the species
# for (spp in species) { cat(spp,'... ') #cycle through each of the species
	# tquery = paste("SELECT rawdata.surveyID, IF( ISNULL(rating), IF( RI=3 OR PA=3, 3, IF( RI=2 OR PA=2, 2, 1 ) ), rating ) as col ",
		# "FROM ( ",
			# "SELECT Latitude, Longitude, surveys.surveyID, MAX(OBS.rating) as RI, IF( MIN(Positional_Accuracy) < 20001 AND Year < 2013 AND Year > 1949, 1, IF( MIN(Positional_Accuracy) < 50001, 2, 3 ) ) as PA ",
			# "FROM surveys ",
			# "JOIN ( ",
				# "SELECT surveyID, IF( rating < 2, 1, IF( rating = 3, 3, 2 ) ) as rating ",
				# "FROM observations ",
				# "WHERE SpNo = ",spp," ",
				# "GROUP BY surveyID ",
			# ") as OBS ",
			# "ON surveys.surveyID = OBS.surveyID ",
			# "GROUP BY surveyID ",
		# ") as rawdata ",
		# "LEFT JOIN ( ",
			# "SELECT Latitude, Longitude, ROUND(AVG(rating)) as rating ",
			# "FROM feedback ",
			# "WHERE feedback.SpNo = ",spp," ",
			# "GROUP BY Latitude, Longitude ",
		# ") as userdata ",
		# "ON ROUND(userdata.Latitude,8)=ROUND(rawdata.Latitude,8) AND ROUND(userdata.Longitude,8)=ROUND(rawdata.Longitude,8);",sep='')
	# occur = fetch(dbSendQuery(conn, statement=tquery),n=-1) #get the occurrence data
	# occur = occur[which(occur$col==1),] #keep only good data
	# if (nrow(occur)<1) { #if no occurrences... move to next species
		# cat('No data ... nothing to run \n')
	# } else { #run this as long as there are occurrences
		# occur = merge(occur,bkgd) #merge with bkgd data
		# out = data.frame(spp=sprintf('%04i',spp),occur[,c('lat','lon',clim.vars)]); out = unique(na.omit(out)) #define the bkgd output file
		# spp.dir = paste(wd,'models/',sprintf('%04i',spp),'/',sep='')
		# if (file.exists(paste(spp.dir,'occur.csv',sep=''))) { #check if data already exists
			# cat('done \n')
		# } else { #if not done... run the species if num of occur >= 10
			# if (nrow(occur)<10) {
				# cat('new but insufficient occurences ... nothing to run \n')
			# } else { 
				# cat('new ... submitting work \n')
				# dir.create(paste(spp.dir,'output',sep=''),recursive=TRUE) #create the species directory
				# write.csv(out,paste(spp.dir,'occur.csv',sep=''),row.names=FALSE); rm(out) #write out the file and remove it from memory
				# zz = file(paste(spp.dir,'01.create.model.sh',sep=''),'w') #create the shell script to run the maxent model
					# cat('#!/bin/bash\n',file=zz)
					# cat('cd $PBS_O_WORKDIR\n',file=zz)
					# cat('java -mx1024m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd.csv -s occur.csv -o output nothreshold nowarnings novisible replicates=10 nooutputgrids -r -a \n',sep="",file=zz)
					# cat('cp -af output/maxentResults.csv output/maxentResults.crossvalide.csv\n',file=zz)
					# cat('java -mx1024m -jar ',wd,'maxent.jar -e ',wd,'target_group_bkgd.csv -s occur.csv -o output -j ',current.mxe.dir,' nothreshold nowarnings novisible nowriteclampgrid nowritemess writeplotdata -P -J -r -a \n',sep="",file=zz)
					# cat('gzip output/*asc\n',file=zz)
				# close(zz) 
				# setwd(spp.dir); system(paste('qsub -m n -N ',sprintf('%04i',spp),' 01.create.model.sh',sep='')); setwd(wd) #submit the script			
			# }
		# }
	# }
# }

