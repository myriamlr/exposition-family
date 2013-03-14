boot.samples <- function(DATA,DESIGN=NULL,constrained=FALSE){
	if(constrained){
		boot.index <- vector()
		for(i in 1:ncol(DESIGN)){
			boot.index <- c(boot.index,sample(which(DESIGN[,i]==1),replace=TRUE))
		}
	}else{
		boot.index <- sample(nrow(DATA),nrow(DATA),TRUE)
	}
	
	return(boot.index)
}