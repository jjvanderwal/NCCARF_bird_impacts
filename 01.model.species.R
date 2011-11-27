library(RMySQL)	#load the necessary libraries
wd = '~/working/NCCARF_2011_project/'; setwd(wd)	#define and set the working directory

### extract data from the database
conn = dbConnect("MySQL",host='spatialecology.jcu.edu.au',dbname='BirdsAustralia',username='mapusr',password='mapusr') #define the connection to the database
occur = fetch(dbSendQuery(conn, statement="
	SELECT surveyID, SpNo, col, usr_val
	FROM observations 
	GROUP BY surveyID, SpNo, col, usr_val;
"),n=-1)	# get the occurrence data
bkgd = fetch(dbSendQuery(conn, statement="
	SELECT surveyID, Latitude, Longitude, Positional_Accuracy 
	FROM surveys;
"),n=-1)	#get the target group background

### append all the 

occur = na.omit(occur); bkgd = na.omit(occur)	#remove NA data

write.csv(bkgd,'target_group_bkgd.csv',row.names=FALSE)
save
### get the target group backgrounds

for (spp in species) { #cycle through each of the species

	
}
