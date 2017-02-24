library()
#Under any circumstances, you cannot have more than (2^31)-1 = 2,147,483,647 rows or columns

subsetWithRelease <- function(nrow = 100, bigdata){    
  subset <- bigdata[1:nrows, 1:ncol(bigdata)]
  rm(bigdata)
  return(subset)
}

naHandler <- function(yData){
  naVec <- is.na(yData)
  yData[naVec] <- mean(yData)
  return(yData)
}

handleNaData <- function(data, ncol = 1, fn = naHandler){
  y <- data[ncol]
  return(naHandler(y))
}

findClasses <- function(data){
  classes <- sapply(data, class)
  return(classes)
}

#Principal Component Analysis



