corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  result <- vector()
  
  files <- list.files(directory)
  filenumbers <- as.integer(strtrim(files,3))
  cc <- complete(directory, filenumbers)
  ccsub <- subset(cc, nobs > threshold)
  ccsub_ids <- ccsub$id
  for (id in ccsub_ids) {
    data <- read.csv(fnb(directory, id))
    result <- c(result, cor(data[,c(2)], data[,c(3)], use="complete.obs"))
  }
  result
}