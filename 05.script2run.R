#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################
#get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
#should have read in e.g.,
# wd="~/working/NARP_birds/"
# spp.dir=1
# spp='u1a'

################################################################################
#prep work and checks
library(SDMTools) #load the library
work.dir = paste(wd,'models/',spp.dir,'/',sep=''); setwd(work.dir) #set the directories and constants
tfiles = list.files(paste('output/ascii/',spp,'/',sep=''),pattern='asc.gz')#list the projections
tfiles = tfiles[c(1,grep('RCP',tfiles))] #keep only RCP data

threshold = read.csv('output/maxentResults.csv'); threshold = threshold[which(threshold$Species==spp),]
spp.AUC = threshold$Training.AUC[1] #get the AUC
threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1] #extract the species threshold value
dir.create(paste('summary/images/',spp,'/potential/',sep=''),recursive=TRUE); dir.create(paste('summary/images/',spp,'/realized/',sep='')) #create the output directories

#extract ES, GCM, year information
ESs = GCMs = YEARs = current = NULL
for (ii in 1:length(tfiles)) { 
	varname = gsub('.asc.gz','',tfiles[ii])
	tt = strsplit(varname,'_')[[1]] #string split the file name
	if (length(tt)==1) { current = tt[1] } else { ESs = c(ESs,tt[1]); GCMs = c(GCMs,tt[2]); YEARs = c(YEARs,tt[3]) } #Assign the split string to the proper object
}
ESs = unique(ESs); GCMs = unique(GCMs); YEARs = unique(YEARs) #keep only unique values


################################################################################
# prepare the realized distribution clip ...
clipasc = read.asc.gz(paste(wd,'GIS/aust_subre.asc.gz',sep='')) #read in the bioregion asc
occur = read.csv("occur.csv",as.is=T)[1:3]; occur = occur[which(occur$spp==spp),] #read in the occurrence records
regions = extract.data(occur[,2:3],clipasc) #extract the regions occurrences fall within
regions = unique(regions) #identify the unique regions
pos = which(clipasc %in% regions) #identify locations within these regions
clipasc[which(is.finite(clipasc))] = 0; clipasc[pos] = 1 #change everything to 0 and only locations in bioregions of interest to 1
write.asc.gz(clipasc,paste(spp,'.bioregion.clip.asc',sep='')) #write out the clipping ascii
base.asc = read.asc.gz('~/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('~/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files
pos$clip = extract.data(cbind(pos$lon,pos$lat),clipasc) #extract the clip information
cellarea = grid.area(base.asc) #get the area of individual cells
pos$area = cellarea[cbind(pos$row,pos$col)] #append the cell area
rm(clipasc); rm(occur); rm(regions) #clean up memory

################################################################################
# bring the data into memory
pot.mat = matrix(0,nr=nrow(pos),nc=length(tfiles)) #create matrix to store information
colnames(pot.mat) = gsub('.asc.gz','',tfiles) #add column names
for (tfile in tfiles) { cat(tfile,'\n'); pot.mat[,gsub('.asc.gz','',tfile)] = read.asc.gz(paste('output/ascii/',spp,'/',tfile,sep=''))[cbind(pos$row,pos$col)] } #read in all the projection
save(pot.mat,file=paste('output/',spp,'.potential.dist.mat.Rdata',sep='')) #save the potential matrix
pot.mat[which(pot.mat<threshold)] = 0 # change anything < threshold to 0
real.mat = pot.mat; real.mat = real.mat[,] * pos[,'clip'] #create the realized distributions using the clip

################################################################################
# get some summary stats
sum.data = function() { #this is a function to summarize the distribution data
	outdata1 = data.frame(ES=rep(NA,length(vois)),GCM=NA,year=NA) #define the basic information
	for (ii in 1:length(vois)) { 
		tt = strsplit(vois[ii],'_')[[1]] 
		if (length(tt)==1) { outdata1[ii,1:3] = c(NA,NA,1990) } else { outdata1[ii,1:3] = c(tt[1],tt[2],tt[3]) }
	}
	outdata1$sum.suitability = colSums(distdata*pos[,'area'],na.rm=TRUE) #get the sum of the environmental suitability
	outdata1$prop.abund = outdata1$sum.suitability / outdata1$sum.suitability[outdata1$year==1990] #calculate proportionate change in abundance
	#calculate the Class-based statistics and Istat, then append to the columns
	cur.asc = base.asc; cur.asc[cbind(pos$row,pos$col)] = distdata[,which(colnames(distdata)=="1990")] #define the current data surface for estimating Istat
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
vois = colnames(pot.mat)
distdata = pot.mat
outdata = data.frame(dist.type='potential',sum.data())#extract the output summary data
distdata = real.mat
outdata = rbind(outdata,data.frame(dist.type='realized',sum.data())) #get the realized summary data
write.csv(outdata,paste('summary/',spp,'.classstats.csv',sep=''),row.names=FALSE) #write out the data

