args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
library(SDMTools); library(maptools) #load the necessary libraries

spp.dir = paste('/home/jc165798/working/NARP_birds/',model.dir,'/',spp,'/',sep=''); setwd(spp.dir) #define the input species data directory
pos = read.csv(paste('/home/jc165798/Climate/CIAS/Australia/',resolution,'/baseline.76to05/base.positions.csv',sep=''),as.is=TRUE) #read in the position files
##----------------------------------------------------------------
#determine which threshold to use, and apply to pot.mat
threshold = read.csv('output/maxentResults.csv'); threshold = threshold[which(threshold$Species==spp),] #read in teh maxent data and only keep info for the species of interest
if (threshold$Minimum.training.presence.area>0.8){
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1]/2
}else {
	threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1]#extract the species threshold value
}


spp.dir = paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep=''); setwd(spp.dir) #define the overarching species directory
load(file=paste(spp.dir,spp,'.potential.dist.mat.Rdata',sep='')) #load the potential matrix
pot.mat[which(pot.mat<threshold)] = 0 # change anything < threshold to 0

##----------------------------------------------------------------

#RCP85
cois=grep('RCP85', colnames(pot.mat), value=T) #determine columns of interest - RCP85

outquant=t(apply(pot.mat[,cois],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) })) #get the 50th percentile

futpos=cbind(pos,as.vector(outquant)); save(futpos,file=paste(spp.dir, 'futpos.Rdata',sep='')) #to be used in images
