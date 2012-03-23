#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################

args=(commandArgs(TRUE)); for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #get the command line arguements

#wd='~/working/NARP_birds/models/u1/'

################################################################################
library(SDMTools) #load the library
setwd(wd) #set theworking directory

baseasc = read.asc.gz('~/Climate/PCMDI/01.Oz.5km.61.90/base.asc.gz') #read in the base ascii
pos = read.csv('~/Climate/PCMDI/01.Oz.5km.61.90/base.pos.csv',as.is=TRUE) #read in the base positions information
pos$area = grid.area(baseasc)[cbind(pos$row,pos$col)]/1000000 #append the grid area in km2
ESs = c('sresa1b','sresa2','sresb1') # define the emission scenarios

maxentResults = read.csv('output/maxentResults.csv') #read in the maxent results
subspecies = list.files('output/ascii') #get a list of subspecies
for (subspp in subspecies) {cat(subspp,'\n') #cycle through each of the subspecies
	dir.create(paste('summary/',subspp,sep=''),recursive=TRUE)
	threshold = maxentResults$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[which(maxentResults$Species==subspp)] #define the species threshold
	asciis = list.files(paste('output/ascii/',subspp,'/',sep=''),pattern='asc.gz') #get a list of the asci file
	mat = matrix(NA,nr=nrow(pos),nc=length(asciis)); colnames(mat) = gsub('.asc.gz','',asciis) #create a matrix for storing the data
	for (asc in asciis) { cat('.') #read in all the data
		mat[,gsub('.asc.gz','',asc)] = read.asc.gz(paste('output/ascii/',subspp,'/',asc,sep=''))[cbind(pos$row,pos$col)] #append the data
	}; cat('    data read in\n')
	save(mat,file=paste('summary/',subspp,'/potential_distribution_matrix.RData',sep='')) #write out the data
	mat[which(mat<threshold)] = 0 #threshold the mat
	binmat = mat; binmat[which(binmat>0)] = 1 #create a binary matrix
	outarea = data.frame(run=colnames(mat),area.km2 = colSums(binmat[,] * pos$area)) #summarize the area change
	curasc = baseasc; curasc[cbind(pos$row,pos$col)] = mat[,'1975'] #get out the current suitability
	write.asc.gz(curasc,paste('summary/',subspp,'/current.suitability.asc',sep='')) #write out the data
	a1basc = baseasc; a1basc[cbind(pos$row,pos$col)] = rowMeans(binmat[,grep('sresa1b',colnames(mat))]) #get out the current suitability
	write.asc.gz(a1basc,paste('summary/',subspp,'/sresa1b.2080.certainty.asc',sep='')) #write out the data
	a2asc = baseasc; a2asc[cbind(pos$row,pos$col)] = rowMeans(binmat[,grep('sresa2',colnames(mat))]) #get out the current suitability
	write.asc.gz(a2asc,paste('summary/',subspp,'/sresa2.2080.certainty.asc',sep='')) #write out the data
	b1asc = baseasc; b1asc[cbind(pos$row,pos$col)] = rowMeans(binmat[,grep('sresb1',colnames(mat))]) #get out the current suitability
	write.asc.gz(b1asc,paste('summary/',subspp,'/sresb1.2080.certainty.asc',sep='')) #write out the data

	bins = seq(0,1,length=101); bins = cut(threshold,bins,labels=FALSE) # get the threshold bin for cols
	col.suit = c(rep('gray',bins),colorRampPalette(c('brown','yellow','forestgreen'))(100)[bins:100]) #define the colors for plots
	col.certain = colorRampPalette(c('brown','yellow','forestgreen'))(101) #define the colors for plots
	legend.pnts = cbind(c(113,114.5,114.5,113),c(-44,-44,-38,-38)) #define the location of the legend
	bitmap(paste('summary/',subspp,'.png',sep=''), width=(4*dim(curasc)[1])/100, height=dim(curasc)[2]/100, units='cm', res=300, pointsize=5, bg='white')
		par(mfrow=c(1,4),mar=c(0,0,0,0),cex=0.6)
		image(curasc,zlim=c(0,1),col=col.suit,ann=FALSE,axes=FALSE)
		legend('topleft',legend=paste(subspp,'\nCurrent -- 1975'),bty='n',cex=2)
		legend.gradient(legend.pnts,cols=col.suit,cex=2,title='suitability',font=2)
		image(b1asc,zlim=c(0,1),col=col.certain,ann=FALSE,axes=FALSE)
		legend('topleft',legend='2080 SRES B1\nLow emmisions',bty='n',cex=2)
		legend.gradient(legend.pnts,cols=col.certain,cex=2,title='Certainty (prop GCMs agree)',font=2)
		image(a1basc,zlim=c(0,1),col=col.certain,ann=FALSE,axes=FALSE)
		legend('topleft',legend='2080 SRES A1B\n"business as usual"',bty='n',cex=2)
		image(a2asc,zlim=c(0,1),col=col.certain,ann=FALSE,axes=FALSE)
		legend('topleft',legend='2080 SRES A2\nHigh emmisions',bty='n',cex=2)
	dev.off()
}
