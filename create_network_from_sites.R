################################
#
#
# Creating a network of lines for metapopulation 
# Project
#
# Written by Travis Gallo and Mason Fidino
# 8/8/2016
################################


######################
### Working script ###
######################

# load the necessary functions, these are
# package_load, which loads our packages and/or downloads
# them if necessary and the make_lines function. Which
# will make the SpatialLines objects if we supply it
# a SpatialPointsDataFrame
source("create_network_from_sites_source_functions.R")

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

# use the make_lines function to create the SpatialLines object
all_features <- make_lines(sites)

# Convert SpatialLines to a SpatialLinesDataFrame and tack on
# the all_station_combos data.frame so we know where the lines
# originate from and go to.  These are ordered correctly so we
# do not have to match.ID
all_features_df <- SpatialLinesDataFrame(all_features, all_station_combos, match.ID = FALSE)


# Write all_features_df as a shape file for GIS
writeOGR(all_features_df,"T:/PEOPLE/TravisGallo/GIS/network", "all_site_lines_final",driver = "ESRI Shapefile")


### END OF CODE