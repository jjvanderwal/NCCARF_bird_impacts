
#drafted by Lauren Hodgson ( lhodgson86@gmail.com ...  )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
###################################
#Determine area and suitability stats, and make a summary table.
#for future

areas=area.in=area.out=area.total=area.future=NULL

tdata=potasc.quant; tdata[which(tdata>0)]=1
for (tt in 1:ncol(tdata)){ cat(tt, '\n')
area.total=sum(tdata[,tt]*pos$area)
areas=c(areas,area.total)
}

area.in=NULL
tdata=in.poly.quant;tdata[which(tdata>0)]=1
for (tt in 1:ncol(tdata)){ cat(tt, '\n')
area.in.poly=sum(tdata[,tt]*pos$area)
area.in=c(area.in,area.in.poly)
}
area.out=NULL
tdata=out.poly.quant; tdata[which(tdata>0)]=1
for (tt in 1:ncol(tdata)){ cat(tt, '\n')
area.out.poly=sum(tdata[,tt]*pos$area)
area.out=c(area.out,area.out.poly)
}

area.future=rbind(areas,area.in,area.out)
colnames(area.future)=c('RCP3PD','RCP6','RCP85')

#for current
area.total=pot.mat[,1];area.total[which(area.total>0)]=1
area.total=sum(area.total*pos$area)

tdata=in.poly[,1]; tdata[which(tdata>0)]=1
area.in=sum(tdata*pos$area)


tdata=out.poly[,1]; tdata[which(tdata>0)]=1
area.out=sum(tdata*pos$area)

area.current=c(area.total,area.in,area.out)

area.table=cbind(area.current,area.future)
#determine proportional areas
prop.area.table=NULL
prop.area.table=area.table/area.total

#determine summed suitability

suits=suit.in=suit.out=suit.total=suit.future=NULL

tdata=potasc.quant;
for (tt in 1:ncol(tdata)){ cat(tt, '\n')
suit.total=sum(tdata[,tt]*pos$area)
suits=c(suits,suit.total)
}

suit.in=NULL
tdata=in.poly.quant;
for (tt in 1:ncol(tdata)){ cat(tt, '\n')
suit.in.poly=sum(tdata[,tt]*pos$area)
suit.in=c(suit.in,suit.in.poly)
}
suit.out=NULL
tdata=out.poly.quant; 
for (tt in 1:ncol(tdata)){ cat(tt, '\n')
suit.out.poly=sum(tdata[,tt]*pos$area)
suit.out=c(suit.out,suit.out.poly)
}

suit.future=rbind(suits,suit.in,suit.out)
colnames(suit.future)=c('RCP3PD','RCP6','RCP85')

#for current
suit.total=sum(pot.mat[,1]*pos$area)

tdata=in.poly[,1]
suit.in=sum(tdata*pos$area)


tdata=out.poly[,1]
suit.out=sum(tdata*pos$area)

suit.current=c(suit.total,suit.in,suit.out)

suit.table=cbind(suit.current,suit.future)
prop.suit.table=suit.table/suit.total

#tidy up the tables, round things
area.table=round(area.table/1000000000,digits=2) #round the data
prop.area.table=round(prop.area.table,digits=2)
prop.suit.table=round(prop.suit.table,digits=2)

area.table=cbind(c('Total Area (1000s m^2)','Suitable area in polygon', 'Suitable area outside polygon'),area.table)
area.table=rbind(c('','Current', 'RCP3PD', 'RCP6', 'RCP85'),area.table)
prop.area.table=cbind(c('Proportion area remaining', 'Proportion area inside polygon', 'Proportion area outside polygon'),prop.area.table)
prop.suit.table=cbind(c('Proportion abundance','Proportion abund. in polygon', 'Proportion abund. outside polygon'),prop.suit.table)

#summary table
summary.table=rbind(area.table,prop.area.table, prop.suit.table)

###################################
#Generate novel vs. lost area info

#Total novel and lost
total.novel=c('Total novel area', '0')
total.lost=c('Current area lost','0')
for (tt in 1:3){
    tdata=potasc.quant[,tt]
    tdata[which(tdata>0)]=1
    current=pot.mat[,1]
    current[which(current>0)]=1
    difference=tdata-current
	novel.area=difference; lost.area=difference
    novel.area[which(novel.area<0)]=0; lost.area[which(lost.area>0)]=0; lost.area[which(lost.area==-1)]=1
    novel.area=sum(novel.area*pos$area);lost.area=sum(lost.area*pos$area)
    novel.area=round(novel.area/area.total,digits=2);lost.area=round(lost.area/area.total,digits=2)
    total.novel=c(total.novel,novel.area);total.lost=c(total.lost,lost.area)
}

#In.poly.novel
poly.novel=c('Novel area in polygon', '0')
in.poly.lost=c('Current area lost in polygon','0')
for (tt in 1:3){
    tdata=in.poly.quant[,tt]
    tdata[which(tdata>0)]=1
    current=in.poly[,1]
    current[which(current>0)]=1
    difference=tdata-current
	novel.area=difference; lost.area=difference
    novel.area[which(novel.area<0)]=0; lost.area[which(lost.area>0)]=0; lost.area[which(lost.area==-1)]=1
    novel.area=sum(novel.area*pos$area);lost.area=sum(lost.area*pos$area)
    novel.area=round(novel.area/area.total,digits=2);lost.area=round(lost.area/area.total,digits=2)
    poly.novel=c(poly.novel,novel.area);in.poly.lost=c(in.poly.lost,lost.area)
}

summary.table=rbind(summary.table,total.lost)
summary.table=rbind(summary.table,in.poly.lost)
summary.table=rbind(summary.table, total.novel,poly.novel) #this summary table is set up to be displayed as a legend in an image

summary.tidy=summary.table[-1,-1]
rownames(summary.tidy)=c('total.area','area.in','area.out', 'prop.area','prop.area.in','prop.area.out','prop.abund','prop.abund.in','prop.abund.out','prop.area.lost','prop.in.poly.lost','prop.total.novel','prop.in.poly.novel')
oneline.summary=c(common.name,spp,as.vector(summary.tidy))
oneline.colnames=c('common.name','CLIMID','CURRENT.total.area','CURRENT.area.in','CURRENT.area.out', 'CURRENT.prop.area','CURRENT.prop.area.in','CURRENT.prop.area.out','CURRENT.prop.abund','CURRENT.prop.abund.in','CURRENT.prop.abund.out','CURRENT.prop.area.lost','CURRENT.prop.in.poly.lost','CURRENT.prop.total.novel','CURRENT.prop.in.poly.novel',
'RCP3PD.total.area','RCP3PD.area.in','RCP3PD.area.out', 'RCP3PD.prop.area','RCP3PD.prop.area.in','RCP3PD.prop.area.out','RCP3PD.prop.abund','RCP3PD.prop.abund.in','RCP3PD.prop.abund.out','RCP3PD.prop.area.lost','RCP3PD.prop.in.poly.lost','RCP3PD.prop.total.novel','RCP3PD.prop.in.poly.novel',
'RCP6.total.area','RCP6.area.in','RCP6.area.out', 'RCP6.prop.area','RCP6.prop.area.in','RCP6.prop.area.out','RCP6.prop.abund','RCP6.prop.abund.in','RCP6.prop.abund.out','RCP6.prop.area.lost','RCP6.prop.in.poly.lost','RCP6.prop.total.novel','RCP6.prop.in.poly.novel',
'RCP85.total.area','RCP85.area.in','RCP85.area.out', 'RCP85.prop.area','RCP85.prop.area.in','RCP85.prop.area.out','RCP85.prop.abund','RCP85.prop.abund.in','RCP85.prop.abund.out','RCP85.prop.area.lost','RCP85.prop.in.poly.lost','RCP85.prop.total.novel','RCP85.prop.in.poly.novel')
tt=matrix(NA,nr=1,nc=length(oneline.summary))
tt[1,]=oneline.summary;colnames(tt)=oneline.colnames
oneline.summary=tt



































