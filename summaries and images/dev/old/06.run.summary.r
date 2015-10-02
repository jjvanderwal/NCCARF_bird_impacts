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
colnames(area.future)=c(10,50,90)

#for current
area.total=ClassStatData[1,9]

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
colnames(suit.future)=c(10,50,90)

#for current
suit.total=ClassStatData[1,5]

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
area.table=rbind(c('','Current', '10th percentile', '50th percentile', '90th percentile'),area.table)
prop.area.table=cbind(c('Proportion area remaining', 'Proportion area inside polygon', 'Proportion area outside polygon'),prop.area.table)
prop.suit.table=cbind(c('Proportion abundance','Proportion abund. in polygon', 'Proportion abund. outside polygon'),prop.suit.table)

#summary table
blank=c(' ',' ',' ',' ',' ')
summary.table=rbind(area.table,blank,prop.area.table, blank,prop.suit.table,blank)

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
summary.table=rbind(summary.table,in.poly.lost,blank)
summary.table=rbind(summary.table, total.novel,poly.novel)






