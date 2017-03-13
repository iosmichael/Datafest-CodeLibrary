library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
#Interesting Operations:
#substr(string, first, last) -> can return a string or a vector of string depends on the type of input
#gsub(",","",vector) -> can substitute char ',' to ''
#as.Date("2017-3-12", format="%Y-%m-%d"), with Date datatype, weekdays(date_vector), months(date_vector), days(date_vector), quarters(date_vector) can be applied
#other Date format: %y (year 2 digits), %b (Month abbrev.), %B (Month Full name), %m (Month decimal)

#This function reads data from SPSS, XLS, CSV etc.
readData <- function(filePath, fileType, header=TRUE){
  
}

#Under any circumstances, you cannot have more than (2^31)-1 = 2,147,483,647 rows or columns
#This function takes big data and subset it into the first n rows
#Input: first number of rows
#Output: Subset of Data
subsetWithRelease <- function(nrow = 100, bigdata){    
  subset <- bigdata[1:nrows, 1:ncol(bigdata)]
  rm(bigdata)
  return(subset)
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

#This function takes subsetData, partitian based on list of column names
#Input: Data table and partition col
#Output: Summary
sumByConditions <- function(subset, listObjects){
  exposures <- aggregate(x = subset, by = listObjects, FUN = function(x){
    sum(pmax(x,0))
  })
  return(exposures)
}

transposeData <- function(data, colHeader = FALSE, rowHeader = FALSE){
  if (colHeader) {
    if (rowHeader){
      
    }else{
      
    }
  }else{
    if (rowHeader){
      
    }else{
      
    }
  }
}
#Principal Component Analysis


example <- function(){
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

example()

