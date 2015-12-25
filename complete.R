complete <- function(directory, id = 1:332) {
	currrentDir<-getwd()
	setwd(directory)	
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases.
	message('test the git')
	
	index <- 1
	df <- data.frame(matrix(ncol = 2))
    for (i in id){
      fileName <- buildFileName(i)
      fileRead <- read.csv(fileName)
      ## number of sulfate completed
	  isSulfate <- !is.na(fileRead[,'sulfate'])
	  ## number of nitrate completed
	  isNitrate <- !is.na(fileRead[,'nitrate'])
	  x <- length(which(isNitrate & isSulfate))
	  df[index,]<- c( i,x)
	  index<- index + 1
    }
		
	setwd(currrentDir)
	colnames(df)<- c("id","nobs")
	df
}
