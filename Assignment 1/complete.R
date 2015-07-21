complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  mydata <- NULL
  result <- data.frame(id=integer(), nobs=integer())
  
  for (numfile in id) {
    filename <- fnb(directory, numfile)
    mydata <- read.csv(filename)
    cc <- mydata[complete.cases(mydata),]
    result <- rbind(result, c(numfile, nrow(cc)))
  }
  colnames(result) <- c("id", "nobs")
  result
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