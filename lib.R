#Interesting Operations:
#substr(string, first, last) -> can return a string or a vector of string depends on the type of input
#gsub(",","",vector) -> can substitute char ',' to ''
#as.Date("2017-3-12", format="%Y-%m-%d"), with Date datatype, weekdays(date_vector), months(date_vector), days(date_vector), quarters(date_vector) can be applied
#other Date format: %y (year 2 digits), %b (Month abbrev.), %B (Month Full name), %m (Month decimal)

#This function reads data from SPSS, XLS, CSV.
readData <- function(filePath, fileType, header=TRUE){
  switch(fileType,
         spss = {
           library(foreign)
           data <- read.spss(filePath)
         },xlsx = {
           library(xlsx)
           data <- read.xlsx(filePath)
         },csv = {
           data <- read.csv(filePath, header = header)
         },print("No specification")
         )
  return(data)
}

#This function writes data in CSV format.
writeData <- function(data, filePath, fileType, header=TRUE){
  
}

splitData <- function(data, numberOfSplits){
  rep.num <- nrow(data)/numberOfSplits
  chunks <- split(data, sample(rep(1:numberOfSplits,rep.num)))
  return(chunks)
}

#This function splits data and store into a bunch of different files
splitFile <- function(data, fileName, numberOfSplits){
  rep.num <- nrow(data)/numberOfSplits
  chunks <- split(data, sample(rep(1:numberOfSplits,rep.num)))
  currentDir <- getwd()
  setwd(currentDir)
  index <- 1
  for(chunk in chunks){
    writeData(chunk,paste(fileName,index,collapse = "-"), "csv")
    index <- index + 1
  }
}

#Under any circumstances, you cannot have more than (2^31)-1 = 2,147,483,647 rows or columns
#This function takes big data and subset it into the first n rows
#Input: first number of rows
#Output: Subset of Data
subsetWithRelease <- function(nrow = 1000, bigdata){    
  subset <- bigdata[1:nrows, 1:ncol(bigdata)]
  rm(bigdata)
  return(subset)
}

subsetWithLevels <- function(data, col, colName){
  for (name in col){
    tmp <- subset(data, colName == name)
    #Do something here
  }
}

#This function takes a table and a columnName 
#get rid of Commas in the data table and change it into numeric value
#Input: Data table, Target column name
#Output: Data table
getRidOfComma <- function(table, colName){
  #as.numeric(gsub(",","",colName))
  return(table)
}

#This function takes a table and a columnName, calculate na value
#Input: Data table, Target column name
#Output: Data table
#Intrinsic Value: $percentage: percentage of Na Values within the column
naReader <- function(table, colName){
  #naVec <- is.na(table[colName])
  percentages <- apply(table, 2, function(x){
    return(length(is.na(x))/length(x))
    })
}

#This function change na value into column mean
#Input: Data table
#Output: Data table
naMean <- function(data){
  apply(data, 2, function(x){
    x[is.na(x)] <- mean(x)
  })
  return(data)
}

#This function change na value into column mode
#Input: Data table
#Output: Data table
naMode <- function(data){
  apply(data, 2, function(x){ 
    x[is.na(x)] <- names(which.max(table(x)))
  })
  return(data)
}

#This function takes a table and give a summary of all the classes it contains
#Input: Data table
#Output: A vector of all sorts of classes
findClasses <- function(data){
  classes <- apply(data, 2, class)
  #classes <- sapply(data, class)
  #Can also use unlist(lapply(data, class))
  return(classes)
}

#This function takes two tables, provide merging columns and return merged database
#Input: Data tables and merging columns (vectors)
#Output: Merged table
mergeDatabases <- function(data1, data2, col1, col2){
  #Can be multiple columns, if so, pass column vectors
  data <- merge(data1, data2, by.x = c(col1),by.y = c(col2))
  return(data)
}

#This function takes subsetData, partitioned based on list of column names
#Input: Data table and partition col (needs to be factors)
#Output: Summary
sumByConditions <- function(subset, listObjects){
  exposures <- aggregate(x = subset, by = listObjects, FUN = function(x){
    #sum(pmax(x,0)), sum every value in x that is larger than 0
    #pmax is parallel comparison
    sum(x)
  })
  return(exposures)
}

#This function transpose data matrix
#Input: Data table and parameters to include colnames or rownames
#Output: Transposed matrix
transposeData <- function(data, colHeader = FALSE, rowHeader = FALSE){
  library(reshape2)
  if (colHeader) {
    #column array is first row
    col_array <- data[,1]
    if (rowHeader){
      #row array is first column
      row_array <- data[1,]
      data.mod <- t(data[-1,-1])
      rownames(data.mod) <- row_array
    }else{
      data.mod <- t(data[,-1])
    }
    colnames(data.mod) <- col_array
  }else{
    if (rowHeader){
      #row array is first column
      row_array <- data[1,]
      data.mod <- t(data[-1,])
      rownames(data.mod) <- row_array
    }else{
      data.mod <- t(data)
    }
  }
}

#This function adds a percentage change column in the dataframe
#Input: Data, Column
#Output: Data.MOD
percentageChangeByOne <- function(data, column){
  x <- data[,column]
  x.diff <- c(NA, diff(x)/x*100)
  cbind(x.diff, data)
  return(data)
}

#Below are reshape2 functions

#This function takes wide-format data and converts it into long-format data
meltData <- function(data, ids, varName = "variable", valName = "value"){
  library(reshape2)
  return(melt(data, id.vars = ids, variable.names = varName, value.name = valName))
}

#This function takes long-format data and converts it into wide-format data
summarizeData <- function(data, ids, variable){
  library(reshape2)
  id.string <- paste(ids, collapse = "+")
  constrain.string <- paste(id.string, variable, collapse = "~")
  data.summary <- dcast(data, constrain.string, fun.aggregate = mean, na.rm = TRUE)
  return(data.summary)
}

#Resample functions: Bootstrapping
#Resample with timeseries

#Principal Component Analysis
example <- function(){
  library(devtools)
  install_github("ggbiplot", "vqv")
  library(ggbiplot)
  data(iris)
  head(iris,10)
  log.ir <- log(iris[,1:4])
  ir.species <- iris[,5]
  ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
  print(ir.pca)
  plot(ir.pca, type = "l")
  predict(ir.pca, newdata = tail(log.ir, 2))
  g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                groups = ir.species, ellipse = TRUE, 
                circle = TRUE)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  print(g)
}

#KNN Analysis:
#https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
knn <- function(){
  
}
