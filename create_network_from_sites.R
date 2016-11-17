################################
#
#
# Distance cost of urban matrix 
# Project
#
# Written by Travis Gallo and Mason Fidino
# 8/8/2016
# Last Updated: 11/14/2016
################################

# Step 1: Creating a network of lines between sites 


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

# write the data from all_features_df so we can easily access it in the future

write.table(all_features_df@data, "T:/PEOPLE/TravisGallo/MetaPopProject/Data/all_lines_names_from_step_1.txt", 
            row.names = FALSE, sep = "\t")

######################################################
# Step 2: Calculate the length of each line that intersects with land use polygons
######################################################

# Creating segments of lines that overlap with land cover types was done in ArcGIS
# Measuring those segments was done in ArgGIS

#####################################################
# Step 3: Create distance matrix for each land cover type
#####################################################



land_covers=c("developed_openspace","developed_high", "developed_mid", "greenspace", "water", "agriculture", "total_seg_dist")

# Currently set up to just work with developed open space
line_dist=vector("list", length = length(land_covers))
for (i in 1:length(land_covers)){
	line_dist[[i]]=line_distance(land_covers[i])
} 

# Make the distance matrices with line_dist

# read in all potential lines
all_lines <- read.table("T:/PEOPLE/TravisGallo/MetaPopProject/Data/all_lines_names_from_step_1.txt",
                        header = TRUE)

# lapply over all the different distances
all_d_mats <- lapply(line_dist, FUN = f_into_m, all_lines = all_lines)

# save the distance matrices


for(i in 1:length(all_d_mats)){
  write.table(all_d_mats[[i]], 
              paste0("T:/PEOPLE/TravisGallo/MetaPopProject/Data/", land_covers[i], "_dmat.txt"),
              sep = "\t")
}

# list of sites that fall within species-specific dispersal distances

# natal dispersal distance from literature rounded to nearest "5"
raccoon=15
coyote=65
fox=20
deer=10
opossum=5
rabbit=5
skunk=5
# total distance between points
# Check directory
dm=read.csv("T:/PEOPLE/Travis Gallo/MetaPopProject/Data/total_seg_dist_dmat.txt",sep = "\t")
# sites that are within the natal dispersal distance
dm_raccoon=apply(dm,2,function (x) which(x<=raccoon))
dm_coyote=apply(dm,2,function (x) which(x<=coyote))
dm_fox=apply(dm,2,function (x) which(x<=fox))
dm_deer=apply(dm,2,function (x) which(x<=deer))
dm_opossum=apply(dm,2,function (x) which(x<=opossum))
dm_rabbit=apply(dm,2,function (x) which(x<=rabbit))
dm_skunk=apply(dm,2,function (x) which(x<=skunk))
# matrix of the number of sites that are kept
s.w.ndd=cbind(
  len_raccoon=sapply(dm_raccoon,FUN=length),
  len_coyote=sapply(dm_coyote,FUN=length),
  len_fox=sapply(dm_fox,FUN=length),
  len_deer=sapply(dm_deer,FUN=length),
  len_opossum=sapply(dm_opossum,FUN=length),
  len_rabbit=sapply(dm_rabbit,FUN=length),
  len_skunk=sapply(dm_skunk,FUN=length)
)

### Now that these are saved we can start working on the JAGS code


### END OF CODE
