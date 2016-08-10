################################
#
#
# Distance cost of urban matrix 
# Project
#
# Written by Travis Gallo and Mason Fidino
# 8/8/2016
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
sites <- readShapePoints("T:/CENTERS/Urban Wildlife/UWI_GIS/ActiveStations/XYActiveStationsWI16.shp")


# get the station ID's from the data in the sites s4 object
station_id <- sites@data$StationID

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
writeOGR(all_features_df,"T:/PEOPLE/TravisGallo/GIS/network", "all_site_lines_final",driver = "ESRI Shapefile")


######################################################
# Step 2: Calculate the length of each line that intersects with land use polygons
######################################################

#######THIS IS A WORK IN PROGRESS#############

# Read in land use polygons
# These take a while to load, so if you are just testing use "low_use"
#high_use=readShapePoly("T:/PEOPLE/TravisGallo/GIS/landuse_high", proj4string=CRS("+proj=longlat +datum=WGS84"))
#med_use=readShapePoly("T:/PEOPLE/TravisGallo/GIS/landuse_mid", proj4string=CRS("+proj=longlat +datum=WGS84"))
low_use=readShapePoly("T:/PEOPLE/TravisGallo/GIS/landuse_low", proj4string=CRS("+proj=longlat +datum=WGS84"))
ag_use=readShapePoly("T:/PEOPLE/TravisGallo/GIS/landuse_ag", proj4string=CRS("+proj=longlat +datum=WGS84"))

x=readOGR("T:/PEOPLE/TravisGallo/GIS", "landuse_low")

#plot land use categories to test
#plot(high_use, border="red")
plot(ag_use, border="brown")
#plot(med_use, border="pink")
plot(low_use, border="grey")
lines(network, add=TRUE)

#read in lines shapefile from ArcGIS with appropriate projection
network=readOGR("T:/PEOPLE/TravisGallo/GIS/network", "all_site_lines")

crs(all_features_df)=crs(x)
y=intersect(x,network)


### END OF CODE