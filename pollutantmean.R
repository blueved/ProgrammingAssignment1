## 
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  if(pollutant != "nitrate" && pollutant != "sulfate"){
    message ("Error invalid pollutant: ",pollutant)
  }
  else{ 
    currrentDir<-getwd()
    setwd(directory)
    ## build a list of the pollutant
    pList <- c()
    for (i in id){
      fileName <- buildFileName(i)
      fileRead <- read.csv(fileName)
      ## pList<-c(pList,stepByStep(fileRead, pollutant))
      
      pList<-c(pList,fileRead[which(!is.na(fileRead[,c(pollutant)])),pollutant])
    }
    setwd(currrentDir)
    mean(pList)
  }
}
## unitest
pollutantTest <- function(){
	testRes <- c(4.065, 1.706, 1.281)
	a <- pollutantmean("specdata", "sulfate", 1:10)  ##  4.064)
	b <- pollutantmean("specdata", "nitrate", 70:72) ##, "==", 1.706)
	c <- pollutantmean("specdata", "nitrate", 23)    ##"==", 1.281)
	delta <- testRes - c(a,b,c)
	res <- matrix(c(a,b,c,testRes, delta), 3,3)
	colnames (res) <-  c("Calculated","Expected", "Error")
	res
}

