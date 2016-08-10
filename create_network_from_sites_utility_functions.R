###########################################
#
#
#
# Source functions to create lines between all sites
#
# Written by Travis Gallo and Mason Fidino
#
# 8/10/2016
#
#
#
#




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

# Create the line data.frame

make_lines <- function(sites = NULL){
  # make a new copy of sites so that we can
  # start removing a row from the data
  # with every iteration of the first
  # for loop
  points <- sites
  
  # determine the total number of sites
  n_sites <- length(sites)
  
  # start creating lines between sites
  for(i in 1:(n_sites - 1)) {
    
    # At the first iteration
    if(i == 1){ 
      
      # create the j vector which is used to give lines a unique name.
      # set to NULL here so that we can alter flow with an
      # ifelse loop later.
      j <- NULL
      
      # create all_lines list to store all of the lines
      all_lines <- list()
      
      # end if
    }
    
    # At the last iteration all lines have been made
    # so we can go ahead and stop here.
    if(i == n_sites){
      stop("lines created")
    }
    
    # The apply function create lines from the coords in the first
    # row of points to all other points.  We always calculate from
    # the first row because we remove that specific row at the end
    # of the i loop. However, we cannot use the apply function
    # when we only have two rows sites left in points so we 
    # use this if else statement.
    if(i < (n_sites - 1)){
      l1 <- apply(points@coords[-1,], 1, function(x) Line(rbind(x, points@coords[1,])))
    }else{
      l1 <- list(Line(rbind(points@coords[-1,], points@coords[1,])))
    }
    
    # During the first iteration when j = NULL
    if(length(j)==0){
      
      # Determine the number of lines in the l1 list
      n_lines <- length(l1)
      
      # Give the j vector some unique values
      j <- 1:n_lines
      
      # Create the list long enough to hold these lines
      loop_list <- vector("list", length = n_lines)
      
      # If j already has values (when i > 1)
    }else{
      
      # Determine the number of lines in the l1 list
      n_lines <- length(l1)
      
      # Give the j vector n_lines unique values based on the last number
      # of j from the previous iteration.
      j <- seq(from = c(last_number+1), by = 1, length.out = n_lines)
      
      # Create a list long enough to store these values
      loop_list <- vector("list", length = n_lines)
      
      # end else statement
    }
    
    # For the number of lines in a particular iteration
    for(k in 1:n_lines){
      
      # Change them to Line class object
      # and give them a unique name based
      # off of the vector j.
      loop_list[[k]] <- Lines(l1[[k]], ID = paste0("line_", j[k]))
      
      # end k loop
    }
    
    # append loop_list to the end of all_lines
    all_lines <- c(all_lines, loop_list)
    
    # remove the first row from points, we don't 
    # need to create lines to that point anymore
    # because we have them to all other points
    points <- points[-1,]
    
    # get the last number of vector j
    last_number <- tail(j, 1)
    
    # end i loop
  }
  
  # Turn all of these lines into a SpatialLines object
  all_features <- SpatialLines(LinesList = all_lines)
  
  
  return(all_features)
}

