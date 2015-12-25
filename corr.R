corr <- function(directory, threshold = 0) {
        allCasesCompleted<-complete(directory)
		allCasesCompletedAboveThreshold<-allCasesCompleted[which(allCasesCompleted[,'nobs'] > threshold),]
		
		xx <-c()
		if(length (allCasesCompletedAboveThreshold) == 0){
			xx <- c(0)
		}else{
			currrentDir<-getwd()
			setwd(directory)
			
			for (i in allCasesCompletedAboveThreshold[,'id']){
			  fileName <- buildFileName(i)
			  fileRead <- read.csv(fileName)
			  xx<- c(xx,cor( fileRead[,'sulfate'],fileRead[,'nitrate'], use="complete.obs") )
			}
		}
		setwd(currrentDir)
		xx
}