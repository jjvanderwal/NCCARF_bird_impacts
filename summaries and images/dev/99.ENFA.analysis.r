#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
#Script created for the purposes of running ENFA (Hirzel et al 2002) analysis on many species.
#reference:
#  Hirzel, A.H., Hausser, J., Chessel, D. and Perrin, N., 2002. Ecological-Niche Factor Analysis: 
#  How to Compute Habitat-Suitability Maps without Absence Data? Ecology, 83:2027-2036.
################################################################################
library(SDMTools); library(adehabitat); library(car) #load the necessary libraries

###create a Kasc file  from list of asc files
climate.dir = '/home/jc165798/Climate/CIAS/Australia/5km/baseline.76to05/bioclim/'; setwd(climate.dir) #define the climate directory
tkasc = as.kasc(list(bioclim01 = read.asc.gz("bioclim_01.asc.gz"),
	bioclim04 = read.asc.gz("bioclim_04.asc.gz"),
	bioclim05 = read.asc.gz("bioclim_05.asc.gz"),
	bioclim06 = read.asc.gz("bioclim_06.asc.gz"),
	bioclim12 = read.asc.gz("bioclim_12.asc.gz"),
	bioclim15 = read.asc.gz("bioclim_15.asc.gz"),
	bioclim16 = read.asc.gz("bioclim_16.asc.gz"),
	bioclim17 = read.asc.gz("bioclim_17.asc.gz")))

###transformation and standarization of environmental data
for (i in 1:length(tkasc)) { cat(i,'of',length(tkasc),'\n') #cycle through each layer in the kasc stack
	if (min(tkasc[,i],na.rm=TRUE)<0) tkasc[,i] = tkasc[,i]+ abs(min(tkasc[,i],na.rm=TRUE)) #ensure all is positive
	tt = powerTransform(na.omit(tkasc[,i])+1)  #estimate the power needed to normalize the environmental layer
	tkasc[,i] = bcPower(tkasc[,i]+1,tt$start) #normalize the environmental layer  
	tmean = mean(na.omit(tkasc[,i])) #get the mean of the environmental data
	tsd = sd(na.omit(tkasc[,i])) #get the standard deviation of the environmental data
	tkasc[,i] = (tkasc[,i] - tmean) / tsd #now standardize the variable as a z-score
}
rm(tt,tmean,tsd); gc()  #free up memory

###wok with the bird data
wd = '/home/jc165798/working/NARP_birds/'; setwd(wd) 
out.dir='/home/jc148322/Bird_NARP/summary.data/' #define and set theworking directory
occur.files = list.files('models') #get all the occur files
occur = NULL
for (tfile in occur.files) { cat(tfile,'\n'); occur = rbind(occur,read.csv(paste('models/',tfile,'/occur.csv',sep=''),as.is=TRUE)[1:3]) } #merge all occur files
species = unique(occur$ClimID) #get list of the unique spp names

#setup the data storage data frame
outdata = data.frame(species=species, marginality=NA, specialization=NA, bc01_mar=NA, bc04_mar=NA, bc05_mar=NA, bc06_mar=NA, bc12_mar=NA, bc15_mar=NA, bc16_mar=NA, bc17_mar=NA)

#cycle through each of the species
for (spp in species) { cat(spp,'\n') #if crashes change this number to the number of spp that have worked (by checking the outdata)
	tt = which(occur$ClimID == spp); pnts = cbind(occur$Lon[tt],occur$Lat[tt]) #select the points for the single species only
	if (length(pnts[,1]) > 5) { #run enfa if sufficient number of data points ... greater than 5 records
		enfadata1 = data2enfa(tkasc,pnts) #extract the location environmental data
		pc <- dudi.pca(enfadata1$tab, scannf = FALSE) #run initial PCA
		enfa1 <- enfa(pc, enfadata1$pr, scannf = FALSE) #run ENFA
		rm(enfadata1,pc); gc()  #free up memory
		###store the output data
		ii = which(outdata$species==spp)
		outdata$marginality[ii] = sqrt(enfa1$m)/1.96
		outdata$specialization[ii] = sqrt(sum(enfa1$s)) / 7
		outdata$bc01_mar[ii] = enfa1$mar[1]
		outdata$bc04_mar[ii] = enfa1$mar[2]
		outdata$bc05_mar[ii] = enfa1$mar[3]
		outdata$bc06_mar[ii] = enfa1$mar[4]
		outdata$bc12_mar[ii] = enfa1$mar[5]
		outdata$bc15_mar[ii] = enfa1$mar[6]
		outdata$bc16_mar[ii] = enfa1$mar[7]
		outdata$bc17_mar[ii] = enfa1$mar[8]

		rm(enfa1); gc() #free up memory
	}
	#now write out the data table
	write.csv(x=outdata,file=paste(out.dir,"ENFA.summary.data.csv",sep=''),row.names = FALSE)
}

#now write out the data table
write.csv(x=outdata,file=paste(out.dir,"ENFA.summary.data.csv",sep=''),row.names = FALSE)