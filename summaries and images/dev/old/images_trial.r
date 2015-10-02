#drafted by Lauren Hodgson <lhodgson86@gmail.com>
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################
library(SDMTools)

#get the command line arguements
args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) }
#should have read in e.g.,
spp='u1a'

###################################
wd = "/home/jc165798/working/NARP_birds/" #basic working directory setwd(image.dir)
image.dir = "/home/jc165798/working/NARP_birds/summary.data/images/" #directory to write images to
spp.dir = paste(wd,'models/', gsub('([a-z,\\.])','',spp),'/',sep=''); setwd(spp.dir) #define the overarching species directory

###################################
#Bring in all the necessary information
threshold = read.csv('output/maxentResults.csv'); threshold = threshold[which(threshold$Species==spp),] #read in teh maxent data and only keep info for the species of interest
threshold = threshold$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold[1] #extract the species threshold value

clipasc = read.asc.gz(paste(spp,'.bioregion.clip.asc.gz',sep='')) #read in the clipping ascii
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files
pos$clip = extract.data(cbind(pos$lon,pos$lat),clipasc) #extract the clip information

load(file=paste(spp.dir,'output/',spp,'.potential.dist.mat.Rdata',sep='')) #load the potential matrix
pot.mat[which(pot.mat<threshold)] = 0 # change anything < threshold to 0
real.mat = pot.mat; real.mat = real.mat[,] * pos[,'clip'] #create the realized distributions using the clip

ClassStatData = read.csv(paste(spp.dir,'summary/',spp,'.classstats.csv',sep=''),as.is=TRUE) #read in the class stats data

###################################
#subset the data; create summaries

pot.CSD=ClassStatData[which(ClassStatData$dist.type=='potential' & ClassStatData$ES=='RCP85' & ClassStatData$year=='2085'),]#subset to potential, RCP85, year 2085
real.CSD=ClassStatData[which(ClassStatData$dist.type=='realized' & ClassStatData$ES=='RCP85' & ClassStatData$year=='2085'),]#subset to potential, RCP85, year 2085

pot.CSD=pot.CSD[,c('sum.suitability', 'n.patches', 'total.area', 'prop.abund')] #subset columns of interest
real.CSD=real.CSD[,c('sum.suitability', 'n.patches', 'total.area', 'prop.abund')] #subset columns of interest

pot.quant = t(apply(pot.CSD[,],2,function(x) { return(quantile(x,c(0.025,0.5,0.975),na.rm=TRUE,type=8)) })) #get the percentiles
real.quant = t(apply(real.CSD[,],2,function(x) { return(quantile(x,c(0.025,0.5,0.975),na.rm=TRUE,type=8)) })) #get the percentiles

###################################
#Create maps

cois=grep('RCP85', colnames(pot.mat), value=T) #determine columns of interest - RCP85
pot.mat2=pot.mat[,cois];real.mat2=real.mat[,cois] #subset columns of interest

potasc.quant=t(apply(pot.mat2[,],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) })) #get the percentiles
realasc.quant=t(apply(real.mat2[,],1,function(x) { return(quantile(x,0.5,na.rm=TRUE,type=8)) })) #get the percentiles

potasc=base.asc; potasc[cbind(pos$row,pos$col)]=potasc.quant #create the asciis
realasc=base.asc; realasc[cbind(pos$row,pos$col)]=realasc.quant #create the asciis

potcurasc=base.asc; potcurasc[cbind(pos$row,pos$col)]=pot.mat[,1] #create the asciis
realcurasc=base.asc; realcurasc[cbind(pos$row,pos$col)]=real.mat[,1] #create the asciis

###################################
#Create image

setwd(image.dir)

bins = seq(0,1,length=101); bins = cut(threshold,bins,labels=FALSE) # get the threshold bin for cols
cols = c(rep('#E5E5E5',bins),colorRampPalette(c("tan","forestgreen"))(100)[bins:100]) #set colour ramp

pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5)) #define corner points of legend
zlim=c(0,1) #set z limits between 0 and 1

png(paste(spp,'.test.png',sep=''),width=dim(base.asc)[1]*2.5+30, height=dim(base.asc)[2]*2+60, units='px', pointsize=20, bg='white')
	par(mar=c(0,2,2,0),mfrow=c(2,3),cex=1,oma=c(3,3,3,0))
	
	mat = matrix(c(1,1,1,1,2,2,2,2,3,3,3,
				1,1,1,1,2,2,2,2,3,3,3,
				1,1,1,1,2,2,2,2,3,3,3,
				1,1,1,1,2,2,2,2,3,3,3,
				4,4,4,4,5,5,5,5,6,6,6,
				4,4,4,4,5,5,5,5,6,6,6,
				4,4,4,4,5,5,5,5,6,6,6,
				4,4,4,4,5,5,5,5,6,6,6),nr=8,nc=11,byrow=TRUE) #create a layout matrix for images
	layout(mat) #call layout as defined above

	image(potcurasc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim)
	mtext('Current', line=1,  side=3, cex=2)
	mtext('Potential', line=1,  side=2, cex=2)
	legend.gradient(pnts,cols=cols,limits=round(zlim,digits=2), title='Suitability', cex=1.7)
	
	image(potasc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim)
	mtext('2085 - RCP85 median', line=1,  side=3, cex=2)
	
	plot(0,0, ylim=c(0,15), xlim=c(0,1), axes=FALSE, ann=FALSE, type='n',col='gray86')
	text(0,14,paste('Total Area Current: ', round(ClassStatData[1,9]/1000000000,digits=2),' (1000s km^2)',sep=''),pos=4, cex=2)
	text(0,13,paste('Proportion Area Remaining: ', round(pot.quant[3,2]/ClassStatData[1,9],digits=2),' (',round(pot.quant[3,1]/ClassStatData[1,9],digits=2),' - ', round(pot.quant[3,3]/ClassStatData[1,9],digits=2),')',sep=''),pos=4, cex=2)
	text(0,12,paste('Proportion Abundance: ', round(pot.quant[1,2]/ClassStatData[1,5],digits=2),' (',round(pot.quant[1,1]/ClassStatData[1,5],digits=2),' - ', round(pot.quant[1,3]/ClassStatData[1,5],digits=2),')',sep=''),pos=4, cex=2)
	text(0,11,paste('# Patches, Current: ', round(ClassStatData[1,8],digits=1),sep=''),pos=4, cex=2)
	text(0,10,paste('# Patches, Future: ', round(pot.quant[2,2],digits=1),' (',round(pot.quant[2,1],digits=1),' - ', round(pot.quant[2,3],digits=1),')',sep=''),pos=4, cex=2)
	text(0,9,'Median (95% CI)',pos=4, cex=1.5)
	
	image(realcurasc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim)
	mtext('Realized', line=1,  side=2, cex=2)
	
	image(realasc, ann=FALSE,axes=FALSE,col=cols, zlim=zlim)

	plot(0,0, ylim=c(0,15), xlim=c(0,1), axes=FALSE, ann=FALSE, type='n',col='gray86')
	text(0,14,paste('Total Area Current: ', round(ClassStatData[74,9]/1000000000,digits=2),' (1000s km^2)',sep=''),pos=4, cex=2)
	text(0,13,paste('Proportion Area Remaining: ', round(real.quant[3,2]/ClassStatData[74,9],digits=2),' (',round(real.quant[3,1]/ClassStatData[1,9],digits=2),' - ', round(real.quant[3,3]/ClassStatData[1,9],digits=2),')',sep=''),pos=4, cex=2)
	text(0,12,paste('Proportion Abundance: ', round(real.quant[1,2]/ClassStatData[74,5],digits=2),' (',round(real.quant[1,1]/ClassStatData[1,5],digits=2),' - ', round(real.quant[1,3]/ClassStatData[1,5],digits=2),')',sep=''),pos=4, cex=2)
	text(0,11,paste('# Patches, Current: ', round(ClassStatData[74,8],digits=1),sep=''),pos=4, cex=2)
	text(0,10,paste('# Patches, Future: ', round(real.quant[2,2],digits=1),' (',round(real.quant[2,1],digits=1),' - ', round(real.quant[2,3],digits=1),')',sep=''),pos=4, cex=2)
	text(0,9,'Median (95% CI)',pos=4, cex=1.5)
	
dev.off()
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	