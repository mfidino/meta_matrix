
n_site <- 100

dist_mat <- matrix(NA, nrow = n_site, ncol = n_site)

# fill diagonal with zeros

diag(dist_mat) <- 0

# figure out where to start the row fill

row_fill <- which(dist_mat == 0, arr.ind = TRUE)
# add 1 to the column vector

row_fill[,2] <- row_fill[,2] + 1

# drop the last row in row fill

row_fill <- row_fill[-100,]

