#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools) #load the necessary libraries
wd = '~/working/NCCARF_2011_BIRD_project/'; setwd(wd) #define and set the working directory
species = list.files('models/') #get a list of species that have been modelled
for (spp in species) { cat(spp,'\n') #cycle through each of hte species
	if (file.exists(paste('models/',spp,'/output/',spp,'_1975.asc.gz',sep=''))) { #create an image if the prediction exists
		tasc = read.asc.gz(paste('models/',spp,'/output/',spp,'_1975.asc.gz',sep='')) #read in the mapped prediction
		maxent = read.csv(paste('models/',spp,'/output/maxentResults.csv',sep=''),as.is=TRUE) #read in the accuracy & threshold values
		threshold = maxent$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[1] #define the threshold
		tasc[which(tasc<threshold)] = NA #set everything less than threshold to NA
		bins = seq(0,1,length=101); bins = cut(threshold,bins,labels=FALSE) # get the threshold bin for cols
		cols = c(rep('#E5E5E5',bins),colorRampPalette(c("yellow","red"))(100)[bins:100]) #define the color ramp
		cols = paste(cols,'90',sep='')
		png(paste('images/1975/',spp,'.png',sep=''),width=nrow(tasc),height=ncol(tasc),bg='transparent') #start creating the image
			par(mar=c(0,0,0,0))
			image(tasc,zlim=c(0,1),ann=FALSE,axes=FALSE,col=cols)
		dev.off()
	}
}
