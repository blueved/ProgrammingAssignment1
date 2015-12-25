## Utilities-
## generate the file name to read
buildFileName <- function (id){
  a<-"00"
  if (id > 99){
    a<-""
  } else if (id > 9){
    a<-"0"
  }
  fileName<-file.path(paste(a, toString(id),".csv",sep=""))
}

###################################################
## 
## testinga and learning purpose 
##
##################################################
runTest1 <- function(){
  result <- c(4.064128 , 1.706047 , 1.280833, 1.702932)
  test <- c( pollutantmean("specdata", "sulfate", 1:10),
             pollutantmean("specdata", "nitrate", 70:72),
             pollutantmean("specdata", "nitrate", 23),
             pollutantmean("specdata", "nitrate"))
  
  rbind(result, test)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
stepByStep <- function(fileRead, pollutant){
  ## create a list of bools [TRUE: non na / FALSE: na]
  booList <- !is.na(fileRead[,c(pollutant)])  
  
  ## get the line numbers of the TRUE
  x<-which(booList)  
  
  message("Found ", length(x),  " items")
  
  ## apply the indexes to the file and get only the pollutant values
  tmpList <- fileRead[x,pollutant]            
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
