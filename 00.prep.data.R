#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools); library(maptools) #load the necessary libraries

wd = '~/working/NARP_birds/'; setwd(wd) #define and set theworking directory

polys = readShapePoly('raw.data/unzipped/TaxonPolys_Dissolve.shp') #read in the polygon files


