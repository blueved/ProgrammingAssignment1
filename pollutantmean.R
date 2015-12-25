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

