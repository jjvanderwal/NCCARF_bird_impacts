library(SDMTools)
out.dir='/home/jc148322/Bird_NARP/'
species=list.files('/home/jc148322/Bird_NARP/species.outputs/')
base.asc = read.asc.gz('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.asc.gz') #read in the base asc file
pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in the position files


cur.richness=NULL
fut.richness=NULL
stability=NULL
for (spp in species) { cat(spp,'\n')
	wd=paste('/home/jc148322/Bird_NARP/species.outputs/',spp,'/',sep='');setwd(wd)
	files=list.files(pattern='pos.Rdata')
	if (length(files)==2){
	load('curpos.Rdata')
	load('futpos.Rdata')

	current=curpos[,5]; current[which(current>0)]=1
	future=futpos[,7];future[which(future>0)]=1

	overlap=current+future; overlap[which(overlap<2)]=0;overlap[which(overlap==2)]=1

	overlap.asc=base.asc; overlap.asc[cbind(pos$row,pos$col)]=overlap
	current.asc=base.asc; current.asc[cbind(pos$row,pos$col)]=current
	future.asc=base.asc; future.asc[cbind(pos$row,pos$col)]=future
	
	if (is.null(stability)) {stability = overlap.asc;  } #define the output & set everything = 0
	stability = stability + overlap.asc
	if (is.null(cur.richness)) {cur.richness = current.asc;  } #define the output & set everything = 0
	cur.richness = cur.richness + current.asc
	if (is.null(fut.richness)) {fut.richness = future.asc;  } #define the output & set everything = 0
	fut.richness = fut.richness + future.asc
}
}
zlim=c(0,max(cur.richness,na.rm=T))
prop.cur=stability/cur.richness;
prop.cur[which(prop.cur>1.4)]=1.4;prop.cur[which(prop.cur<0.6)]=0.6

pnts=cbind(x=c(113,116,116,113), y=c(-13,-13,-18.5,-18.5)) #define corner points of legend
stability.cols = colorRampPalette(c('red',"orange","yellow",'forestgreen'))(max(stability,na.rm=T)) #get the colors for the plot

png(paste(out.dir,'future_overlap_richness.png',sep=''), width=dim(richness)[1]*2+80, height=dim(richness)[2]*2+80, units='px', pointsize=20, bg='white') #start the plot
	par(mfrow=c(2,2),mar=c(2,2,2,2)) #set any plot margins
	image(cur.richness, ann=FALSE,axes=FALSE,col=cols, zlim=zlim) #plot the richness
	legend.gradient(pnts,cols=cols,limits=range(richness,na.rm=T), title='Richness', cex=1)
	mtext('Current',side=1)
	image(fut.richness, ann=FALSE,axes=FALSE,col=cols, zlim=zlim)
	mtext('Future 2085 RCP8.5',side=1)
	image(stability, ann=FALSE,axes=FALSE,col=stability.cols, zlim=range(stability,na.rm=T))
	legend.gradient(pnts,cols=stability.cols,limits=range(stability,na.rm=T), title='Richness', cex=1)
	mtext('Stability',side=1)
	image(prop.cur, ann=FALSE,axes=FALSE,col=prop.cols, zlim=c(0.6,1.4))
	legend.gradient(pnts,cols=stability.cols,limits=c(0.6,1.4), title='Proportion', cex=1)
	mtext('Proportion of current richness',side=1)
dev.off()

