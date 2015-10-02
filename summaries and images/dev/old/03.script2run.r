#script was written by Jeremy VanDerWal (jjvanderwal@gmail.com)
#modified by Lauren Hodgson - for NCCARF Birds project 2012

################################################################################
###evaluate the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }

#need to have read in
# base.dir='/home/jc165798/working/NARP_birds/models/'
# spp='u1'


################################################################################
### define several functions

#define the function to summarize distdata
sum.data = function() { #this is a function to summarize the distribution data
	outdata1 = data.frame(ES=rep(NA,length(vois)),GCM=NA,year=NA) #define the basic information
	for (ii in 1:length(vois)) { 
		tt = strsplit(vois[ii],'_')[[1]] 
		if (length(tt)==1) { outdata1[ii,1:3] = c(NA,NA,1990) } else { outdata1[ii,1:3] = c(tt[1],tt[2],tt[3]) }
	}
	outdata1$sum.suitability = colSums(distdata*pos[,'area'],na.rm=TRUE) #get the sum of the environmental suitability
	outdata1$prop.abund = outdata1$sum.suitability / outdata1$sum.suitability[outdata1$year==1990] #calculate proportionate change in abundance
	#calculate the Class-based statistics and Istat, then append to the columns
	cur.asc = base.asc; cur.asc[cbind(pos$row,pos$col)] = distdata[,which(colnames(distdata)=="current.76to05")] #define the current data surface for estimating Istat
	for (ii in 1:nrow(outdata1)) { 
		tasc = base.asc; tasc[cbind(pos$row,pos$col)] = distdata[,ii] #put the data back into a matrix
		Ival = Istat(cur.asc,tasc) #calculate the Istatistic
		tasc[which(tasc>0)] = 1 #now convert binary data
		CS = ClassStat(tasc,latlon=TRUE) #get the class stats
		if (1%in%CS$class) { CS = CS[which(CS$class==1),] } else { CS = CS[1,]; CS[1,] = NA } #only keep info on distriubtion... if no distriubtion, set everything to 0
		if (ii == 1) { cois = NULL; for (jj in c('Istat',names(CS)[-1])) {outdata1[jj] = NA; cois = c(cois,which(names(outdata1)==jj)) } } #if the first summary, create columns to store data and define the column numbers for this data
		outdata1[ii,cois] = c(Ival,CS[,-1])
	}
	return(outdata1)#return the output
}
#define the function to summarize distdata removing small fragments
sum.data.remove.small.patches = function() { #this is a function to summarize the distribution data
	outdata1 = data.frame(ES=rep(NA,length(vois)),GCM=NA,year=NA,sum.suitability=NA,prop.abund=NA) #define the basic information
	for (ii in 1:length(vois)) { 
		tt = strsplit(vois[ii],'_')[[1]] 
		if (length(tt)==1) { outdata1[ii,1:3] = c(NA,NA,1990) } else { outdata1[ii,1:3] = c(tt[1],tt[2],tt[3]) }
	}
	#calculate the Class-based statistics and Istat, then append to the columns
	for (ii in 1:nrow(outdata1)) { 
		tasc = base.asc; tasc[cbind(pos$row,pos$col)] = distdata[,ii] #put the data back into a matrix
		patches = tasc; patches[which(patches>0)] = 1; patches = ConnCompLabel(patches)[cbind(pos$row,pos$col)] #get the unique patches 
		patches.ag = aggregate(distdata[,ii]*pos$area,by=list(patch=patches),sum); patches.ag$prop = patches.ag$x/sum(patches.ag$x) #get the patch ES weighted by area and then get proportion
		patches[which(patches %in% patches.ag$patch[which(patches.ag$prop<0.0001)])] = 0; patches[which(patches>0)] = 1 #make patches binary, keeping only patches that contribute more than 0.01% of the total ES
		tasc[cbind(pos$row,pos$col)] = tasc[cbind(pos$row,pos$col)] * patches #filter out small patches
		outdata1$sum.suitability[ii] = sum(distdata[,ii]*pos$area*patches,na.rm=TRUE) #get the sum of the environmental suitability
		outdata1$prop.abund[ii] = outdata1$sum.suitability[ii] / outdata1$sum.suitability[outdata1$year==1990] #calculate proportionate change in abundance
			
		Ival = NA #calculate the Istatistic
		tasc[which(tasc>0)] = 1 #now convert binary data
		CS = ClassStat(tasc,latlon=TRUE) #get the class stats
		if (1%in%CS$class) { CS = CS[which(CS$class==1),] } else { CS = CS[1,]; CS[1,] = NA } #only keep info on distriubtion... if no distriubtion, set everything to 0
		if (ii == 1) { cois = NULL; for (jj in c('Istat',names(CS)[-1])) {outdata1[jj] = NA; cois = c(cois,which(names(outdata1)==jj)) } } #if the first summary, create columns to store data and define the column numbers for this data
		outdata1[ii,cois] = c(Ival,CS[,-1])
	}
	return(outdata1)#return the output
}

################################################################################
### do the work
library(SDMTools) #load the necessary libraries

setwd(paste(base.dir,spp,sep='')) #set the working directory
out.dir = paste(base.dir,spp,'/summary/',sep=''); dir.create(out.dir,recursive=TRUE) #define the output directory & create it

base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base ascii grid file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the base positions file

futs.dir = '/home/jc165798/Climate/CIAS/Australia/5km/bioclim_mxe/'

futs = list.files(pattern='RCP',futs.dir); futs=gsub('//','/',futs) #list the future projections
varnames = futs #define the variable names

#extract ES, GCM, year information
ESs = GCMs = YEARs = current = NULL
for (ii in 1:length(varnames)) { 
	tt = strsplit(varnames[ii],'_')[[1]] #string split the file name
	if (length(tt)==1) { current = tt[1] } else { ESs = c(ESs,tt[1]); GCMs = c(GCMs,tt[2]); YEARs = c(YEARs,tt[3]) } #Assign the split string to the proper object
}
ESs = unique(ESs); GCMs = unique(GCMs); YEARs = unique(YEARs) #keep only unique values

threshold = read.csv("output/maxentResults.csv",as.is=TRUE)$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[1] # get the threshold

#read in and append subregions & dem... convert subregion to a mask of realized distribution matrix
distmat = read.csv(paste(gsub('models','',base.dir),'RealizedDistMatrix.csv',sep=''),as.is=TRUE) #read in the distribution matrix
distmat = distmat[which(distmat$spp_code_raster==spp),]; distmat = names(distmat)[which(distmat[1,]==1)]; distmat = gsub('\\.','-',distmat) #subset the information
subregionID = read.csv(paste(gsub('models','base.data',base.dir),'subregions_new07.csv',sep=''),as.is=TRUE) #correct for subregion id numbers
subregionID = subregionID$Value[which(subregionID$Abbreviation %in% distmat)] #correct for subregion id numbers
pos$subregion = extract.data(cbind(pos$lon,pos$lat),read.asc.gz(paste(gsub('models','base.data',base.dir),'subregions_new07.asc.gz',sep=''))) #extract the subregion data
pos$subregion[-which(pos$subregion %in% c(subregionID,-99))] = 0; pos$subregion[which(pos$subregion > 0)] = 1 #make the subregion column binary
pos$area = (grid.area(base.asc)[cbind(pos$row,pos$col)]) / 10000 #get out the area in ha of each cell of data
pos$dem = extract.data(cbind(pos$lon,pos$lat),read.asc.gz(paste(gsub('models','base.data',base.dir),'dem250.asc.gz',sep=''))) #read and append DEM

#write out the basic mask & pos
write.asc.gz(base.asc,paste(out.dir,'base.asc',sep=''))
write.csv(pos,paste(out.dir,'base.pos.csv',sep=''),row.names=FALSE)
tasc = base.asc; tasc[cbind(pos$row,pos$col)] = pos$subregion
write.asc.gz(tasc,paste(out.dir,'clip.realized.asc',sep='')) #wrie out the clipping grid

#cycle through each of the emmissiton scenarios & create a matrix of data representing the full sets of data
for (ES in ESs) { cat(ES,'...')
	vois = c('current.76to05',varnames[grep(ES,varnames)]) #define the variables of interest for this ES
	#create or load distdata from the model outputs
	if (file.exists(paste(out.dir,ES,'.distribution.matrix.Rdata',sep=''))) { cat('loading distribution data\n') #if file exists load the data
		load(paste(out.dir,ES,'.distribution.matrix.Rdata',sep='')) #load the data
	} else { cat('reading all distribution data\n') #read and create the distribution data matrix
		distdata = matrix(0.0,nr=nrow(pos),nc=length(vois)) #create a matrix of data that contains all of the projections
		colnames(distdata) = vois #set the column names
		for (ii in 1:length(vois)) { if (ii%%5==0) { cat(round(ii/length(vois)*100,1),'% ... ') }
			distdata[,ii] = read.asc.gz(paste('output/',vois[ii],'.asc.gz',sep=''))[cbind(pos$row,pos$col)] #read / append data to the dataframe
		}; cat('\n')
		save(distdata,file=paste(out.dir,ES,'.distribution.matrix.Rdata',sep='')) #write out the distribution matrix
	}
	#create or load the percentiles of the distributions
	if (file.exists(paste(out.dir,ES,'.distribution.percentile.matrix.Rdata',sep=''))) { cat('loading distribution data\n') #if file exists load the data
		load(paste(out.dir,ES,'.distribution.percentile.matrix.Rdata',sep='')) #load the data
	} else { cat('calculating percentiles for distribution data\n') #read and create the distribution percentile data matrix
		percentdata = matrix(NA,ncol=1+length(YEARs)*3,nrow=nrow(pos)) #create an empty matrix for storing percentile data
		tt = expand.grid(YEARs,c('_10th','_50th','_90th')); tt = c(1990,sort(paste(ES,'_',tt[,1],tt[,2],sep=''))); colnames(percentdata)=tt #assign the column names
		percentdata[,'1990'] = distdata[,which(colnames(distdata)=="current.76to05")] #put in the data for current
		for (YEAR in YEARs) { cat(YEAR,'...',round(which(YEARs==YEAR)/length(YEARs)*100,1),'% \n') #cycling through each year
			outquant = t(apply(distdata[,grep(YEAR,vois)],1,function(x) { return(quantile(x,c(0.1,0.5,0.9),na.rm=TRUE,type=8)) })) #get the percentiles
			percentdata[,grep(YEAR,colnames(percentdata))] = outquant[,] #append the data to the percentdata
		}; cat('\n')
		save(percentdata,file=paste(out.dir,ES,'.distribution.percentile.matrix.Rdata',sep='')) #write out the distribution matrix
	}	
	
	###first the potential distribution
	distdata[which(distdata<threshold)] = 0 #set anything less than threshold to be 0
	outdata = data.frame(dist.type='potential',sum.data())#extract the output summary data
	outdata = rbind(outdata,data.frame(dist.type='potential.NO.small.patches',sum.data.remove.small.patches()))#extract the output summary data after removing small patches
	
	###now assess the realized distribution
	distdata = distdata * pos[,'subregion'] #set anything less than threshold to be 0
	outdata = rbind(outdata,data.frame(dist.type='realized',sum.data()))
	outdata = rbind(outdata,data.frame(dist.type='realized.NO.small.patches',sum.data.remove.small.patches()))
	write.csv(outdata,paste(out.dir,ES,'.summary.data.csv',sep=''),row.names=FALSE) #write out the summary output data
		
}
