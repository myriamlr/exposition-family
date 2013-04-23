rebuildMCAtable<-function(DATA){
	##private function to compensate
	# na.rep.check <- function(times,x){
		# ret.vec <- rep(x=x,times=times)
		# if(length(ret.vec) < round(times)){
			# ret.vec <- c(ret.vec,NA)
		# }
		# return(ret.vec)
	# }
	
	####
	#I need to know num columns and %% because that tells me how many items per column in original data...
	####
	
	na.locs <- which(DATA < 1 & DATA > 0,arr.ind=TRUE)
	orig.cols <- sum(cumsum(colSums(DATA)) %% nrow(DATA)==0)
	colSums.data <- colSums(DATA)
	#print(mapply(rep,times=colSums.data,x=as.matrix(1:length(colSums.data))))
	#print(mapply(na.rep.check,times=colSums.data,x=as.matrix(1:length(colSums.data))))
	#DATA <- apply(matrix(unlist(mapply(na.rep.check,times=colSums.data,x=as.matrix(1:length(colSums.data)))),nrow(DATA),orig.cols),2,sample)
	DATA <- apply(matrix(unlist(mapply(rep,times=colSums.data,x=as.matrix(1:length(colSums.data)))),nrow(DATA),orig.cols),2,sample)	
	return(DATA)
}