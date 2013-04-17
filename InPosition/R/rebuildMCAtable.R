rebuildMCAtable<-function(DATA){
	orig.cols <- sum(cumsum(colSums(DATA)) %% nrow(DATA)==0)
	colSums.data <- colSums(DATA)
	DATA <- apply(matrix(unlist(mapply(rep,times=colSums.data,x=as.matrix(1:length(colSums.data)))),nrow(DATA),orig.cols),2,sample)
	return(DATA)
}