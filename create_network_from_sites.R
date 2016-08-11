################################
#
#
# Distance cost of urban matrix 
# Project
#
# Written by Travis Gallo and Mason Fidino
# 8/8/2016
# Last Updated: 8/10/16
################################

# Step 1: Creating a network of lines between sites 

################################
### User specified functions ###
################################

# Load appropriate packages

package_load<-function(packages = NULL, quiet=TRUE, verbose=FALSE, warn.conflicts=FALSE){
  
  # download required packages if they're not already
  
  pkgsToDownload<- packages[!(packages  %in% installed.packages()[,"Package"])]
  if(length(pkgsToDownload)>0)
    install.packages(pkgsToDownload, repos="http://cran.us.r-project.org", quiet=quiet, verbose=verbose)
  
  # then load them
  for(i in 1:length(packages))
    require(packages[i], character.only=T, quietly=quiet, warn.conflicts=warn.conflicts)
}


######################
### Working script ###
######################

# load the functions from create_network_from_sites_source_functions.R
source("create_network_from_sites_utility_functions.R")

# Two functions in the above script...

# package_load: Give it the packages you want to load as a character vector
# and it will load them / download them if you do not have them. Only has one
# argument, "packages"

# make_lines: If you give this the sites as a SpatialPointsDateFrame
# it will return a SpatialLines object. Only has one arguement. "sites".


# Packages to load
to_load <- c("sp", "maptools", "raster", "rgdal", "rgeos")

# load the packages
package_load(to_load)

#read in sites and transform them from UTM to Lat-Long
sites <- readShapePoints("T:/PEOPLE/TravisGallo/GIS/meta_pop_sites.shp")
plot(sites)

# get the station ID's from the data in the sites s4 object
station_id <- sites@data$Sites

# make every unique combination of sites for the network
# Note: this returns a 2 by # of combination matrix, we 
# transpose it because this data.frame needs to have a 
# number of rows equal to the number of lines for
# the SpatialLinesDataFrame function
all_station_combos <- data.frame(t(combn(as.character(station_id), 2)))

# change the column names to something that makes sense
colnames(all_station_combos) <- c("site1", "site2")

# make the SpatialLines object
all_features <- make_lines(sites)

# Convert SpatialLines to a SpatialLinesDataFrame and tack on
# the all_station_combos data.frame so we know where the lines
# originate from and go to.  These are ordered correctly so we
# do not have to match.ID
all_features_df <- SpatialLinesDataFrame(all_features, all_station_combos, match.ID = FALSE)

# Write all_features_df as a shape file for GIS
writeOGR(all_features_df,"T:/PEOPLE/TravisGallo/GIS", "meta_pop_network",driver = "ESRI Shapefile")


######################################################
# Step 2: Calculate the length of each line that intersects with land use polygons
######################################################

# Creating segments of lines that overlap with land cover types was done in ArcGIS
# Measuring those segments was done in ArgGIS

#####################################################
# Step 3: Create distance matrix for each land cover type
#####################################################

# Pull .csv files created in ArcGIS and sum distances across line segments

line_distance= function (x){
	# Read in *** REMEMBER THESE DATA ARE IN CENTIMETERS****
	segs=read.csv(paste("T:/PEOPLE/TravisGallo/MetaPopProject/Data/",x,".csv", sep=""))

	# Sum across little segments for each line
	dist=aggregate(seg_dist~site1*site2, data=segs, FUN=sum)

	# Convert to meters
	dist[,3]=dist[,3] / 100
	as.data.frame(dist)
}

land_covers=c("developed_openspace","developed_high", developed_mid", "greenspace", "water", "agriculture")

# Currently set up to just work with developed open space
line_dist=list()
for (i in 1:length(land_covers)){
	line_dist[[i]]=line_distance("developed_openspace")
} 


### Developed Open Space Segments ###

# Read in *** REMEMBER THESE DATA ARE IN CENTIMETERS****
dev_openspace_segs=read.csv("T:/PEOPLE/TravisGallo/MetaPopProject/Data/developed_openspace.csv")

# Sum across little segments for each line
dev_openspace_dist=aggregate(seg_dist~site1*site2, data=dev_openspace_segs, FUN=sum)

# Convert to meters
dev_openspace_dist[,3]=dev_openspace_dist[,3] / 100


### END OF CODE