# Remove previous workspace
rm(list = ls())

# Create a example data.frame
data0 <- data.frame(tree = c(1, 2, 3),
                    d = c(20, 25, 19),
                    d1 = c(19, 23, 18),
                    d2 = c(16, 19, 16),
                    d3 = c(13, 14, 13),
                    d4 = c(10, 9, 9),
                    d5 = c(7, 5, 6))

# Alternative 1 (lapply, by tree) ----
# Function to transpose variables that are in columns 
merge.i <- function(i, data, varv){
  # i: position of each tree in data
  # data: data.frame
  # varv: variables we want to transpose
  data.i <- data[i,] # Select tree in position i
  varf <- colnames(data)[!colnames(data) %in% varv] # Variables we don't want to transpose
  dat <- data.i[,varf] # Select variables we don´t want to transpose in data
  values <- as.numeric(data.i[,varv]) # Select variables we want to transpose
  return(merge(dat, values)) # Make a merge to join dat and values
}

# Test of function merge.i
merge.i (1, data0, c("d1", "d2", "d3", "d4", "d5")) 

# Function to apply merge.i to all trees
merge.var <- function(data, varv){
  # data: data.frame
  # varv: variables we want to transpose
  i <- 1:dim(data)[1] # Sequence of positions of trees
  def <- do.call(rbind, lapply(i, merge.i, data, varv)) # Apply function merge.i by tree and join results in a single data.frame
  return(def)
}

# Test of function merge.var
merge.var(data0, c("d1", "d2", "d3", "d4", "d5"))

# Alternative 2 (for loop, by variable)  ----
merge.var2 <- function(data, varv){
  # data: data.frame
  # varv: variables we want to transpose
  varf <- colnames(data)[!colnames(data) %in% varv] # Variables we don't want to transpose
  dat <- data[,varf] # Select variables we don´t want to transpose in data
  dat.i <- list() # Create an empty list to save results of for loop
  for (i in 1:length(varv)){ # For each variable of varv
    dat.i[[i]] <- data.frame(dat, dn = as.numeric(data[,varv[i]])) # Join dat with variable i
  }
  def1 <- do.call(rbind, dat.i) # Join results of for loop, what were saved in dat.i
  def <- def1[order(def1$tree, def1$d),] # Order by varf
  return(def)
}

# Test of function merge.var2
merge.var2(data0, c("d1", "d2", "d3", "d4", "d5"))

# Comparing computing time (faster function with for loop)
system.time(merge.var2(data0, c("d1", "d2", "d3", "d4", "d5")))
system.time(merge.var(data0, c("d1", "d2", "d3", "d4", "d5")))

# Both functions should be proved in terms of computing time, for cases of data.frames with bigger dimensions 