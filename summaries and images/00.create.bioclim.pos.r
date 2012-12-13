#create bioclim for 5km
library(SDMTools)

pos = read.csv('/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in 

bioclim.dir='/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/bioclim/';setwd(bioclim.dir)

bioclim01 = read.asc.gz("bioclim_01.asc.gz")
bioclim04 = read.asc.gz("bioclim_04.asc.gz")
bioclim12 = read.asc.gz("bioclim_12.asc.gz")
bioclim15 = read.asc.gz("bioclim_15.asc.gz")

pos.bc=pos
pos.bc$bc01=bioclim01[which(is.finite(bioclim01))]
pos.bc$bc04=bioclim04[which(is.finite(bioclim04))]
pos.bc$bc12=bioclim12[which(is.finite(bioclim12))]
pos.bc$bc15=bioclim15[which(is.finite(bioclim15))]

save(pos.bc,file='/home/jc148322/Bird_NARP/bioclim_5km.Rdata')

#create bioclim for 1km
library(SDMTools)

pos = read.csv('/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/base.positions.csv',as.is=TRUE) #read in 

bioclim.dir='/home/jc165798/Climate/CIAS/Australia/1km/baseline.76to05/bioclim/';setwd(bioclim.dir)

bioclim01 = read.asc("bioclim_01.asc")
bioclim04 = read.asc("bioclim_04.asc")
bioclim12 = read.asc("bioclim_12.asc")
bioclim15 = read.asc("bioclim_15.asc")

pos.bc=pos
pos.bc$bc01=bioclim01[which(is.finite(bioclim01))]
pos.bc$bc04=bioclim04[which(is.finite(bioclim04))]
pos.bc$bc12=bioclim12[which(is.finite(bioclim12))]
pos.bc$bc15=bioclim15[which(is.finite(bioclim15))]

save(pos.bc,file='/home/jc148322/Bird_NARP/bioclim_1km.Rdata')

