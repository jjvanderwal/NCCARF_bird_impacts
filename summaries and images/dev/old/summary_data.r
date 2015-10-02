#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################

wd = "/home/jc165798/working/NARP_birds/" #basic working directory
outdir = paste(wd,"summary.data",sep=''); setwd(outdir) #directory to write output to
species = list.files(paste(wd,'models/',sep='')) #get a list of species

out = NULL #define the output
for (spp in species) { cat(spp,'\n') #cycle through each of the species
	mr = read.csv(paste(wd,'models/',spp,'/output/maxentResults.csv',sep=''),as.is=TRUE) #read in the maxent results
	mrcv = read.csv(paste(wd,'models/',spp,'/output/maxentResults.crossvalide.csv',sep=''),as.is=TRUE) #read in the maxent results for the cross validated runs
	mrcv = mrcv[,c('Species','Test.AUC')] #keep only columns of interest
	mrcv = mrcv[-grep('average',mrcv$Species),] #remove the average row
	for (ii in 0:9) mrcv$Species = gsub(paste('_',ii,sep=''),'',mrcv$Species) #remove the iteration values
	tt = aggregate(mrcv[,'Test.AUC'],by=list(Species=mrcv$Species),function(x) { return(c(mean(x,na.rm=TRUE),sd(x,na.rm=T))) } ) #get the mean & SD AUC stats
	tt = data.frame('Species'=tt$Species,'AUC.mean'=tt$x[,1],'AUC.sd'=tt$x[,1],stringsAsFactors=FALSE) #get the key information
	tout = merge(tt,mr[,c(1,grep('contribution',colnames(mr)))]) #merge the important data
	out = rbind(out,tout) #append to output
}
###save out in summary.data
write.csv(out,paste(outdir,'/Maxent.summary.data.csv', sep=''),row.names=FALSE)


################################################################################
##append all classstats together

out=tdata=NULL
for (spp in species) { cat(spp,'\n')
	sppdir=paste(wd, 'models/',spp, '/summary/', sep='')
	files=list.files(sppdir,pattern='.csv')
	
	for (tfile in files) { cat(tfile, '\n')
		tdata=read.csv(paste(sppdir, tfile, sep=''))
		sppcode=strsplit(tfile,'.classstats')[[1]][1]
		tdata=cbind(sppcode, tdata)
		out=rbind(out,tdata)
		}

}

write.csv(out,paste(outdir,'/Classstats.summary.data.csv',sep=''),row.names=FALSE)


################################################################################

cs=read.csv(paste(outdir,'/Classstats.summary.data.csv',sep=''))
cs$prop.area = NA
for (ii in unique(cs$sppcode)) {cat(ii,'\n'); 
	tdata = cs[which(cs$sppcode==ii),c('year','total.area')]
	tdata$prop.area = tdata$total.area / tdata$total.area[which(tdata$year==1990)]
	cs$prop.area[which(cs$sppcode==ii)] = tdata$prop.area
}
cs$spp = cs$sppcode

out = NULL
#get the 2.5 percentile
out = data.frame(percentile=0.025,aggregate(cs[,c('prop.abund','Istat','n.patches','total.area','prop.area','mean.perim.area.ratio')],by=list(spp=cs$sppcode,dist.type=cs$dist.type,ES=cs$ES,year=cs$year),function(x) {return(quantile(x,c(0.025),type=8,na.rm=TRUE))}))
out = rbind(out,data.frame(percentile=0.5,aggregate(cs[,c('prop.abund','Istat','n.patches','total.area','prop.area','mean.perim.area.ratio')],by=list(spp=cs$sppcode,dist.type=cs$dist.type,ES=cs$ES,year=cs$year),function(x) {return(quantile(x,c(0.5),type=8,na.rm=TRUE))})))
out = rbind(out,data.frame(percentile=0.975,aggregate(cs[,c('prop.abund','Istat','n.patches','total.area','prop.area','mean.perim.area.ratio')],by=list(spp=cs$sppcode,dist.type=cs$dist.type,ES=cs$ES,year=cs$year),function(x) {return(quantile(x,c(0.975),type=8,na.rm=TRUE))})))
out = rbind(out,data.frame(percentile=NA,cs[which(cs$year==1990),c('spp','dist.type','ES','year','prop.abund','Istat','n.patches','total.area','prop.area','mean.perim.area.ratio')]))
out = out[order(out$dist.type,out$spp,out$year,out$ES),]
#write out data
write.csv(out,paste(outdir,'/summary.of.Classstats.csv',sep=''),row.names=FALSE)
