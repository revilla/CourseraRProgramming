pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  mydata <- NULL
  
  for (numfile in id) {
    mydata <- rbind(mydata, read.csv(fnb(directory, numfile)))
  }
  #measure_mean <- colMeans(mydata[pollutant], na.rm=TRUE)
  measure_mean <- lapply(mydata[pollutant], mean, na.rm=TRUE)
  measure_mean
}

fnb <- function(directory, n) {
  nz <- 2 - floor(log10(n))
  zs <- ""
  i <- 0L
  while (i < nz) {
    zs <- paste(zs, "0", sep = "")
    i <- i + 1L
  }
  fn <- paste(directory, "/", zs, as.character(n), ".csv", sep = "")
}

pollutantmean("specdata", "nitrate", 2)
