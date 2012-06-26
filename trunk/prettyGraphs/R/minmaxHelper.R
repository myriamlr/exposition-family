minmaxHelper <-
function(mat1,mat2=NULL,axis1=1,axis2=2){

	if(!is.matrix(mat1) && !is.data.frame(mat1)){
		print("mat1 was not a matrix or a data frame.")
		return(0)
	}
	if(is.null(mat2)){
		mat2=mat1
	}
	#trackMinMaxMatrix <- rbind(mat1,mat2)
	#minMaxList <- list(minx=min(c(trackMinMaxMatrix[,axis1],0)),miny=min(c(trackMinMaxMatrix[,axis2],0)),maxx=max(c(trackMinMaxMatrix[,axis1],0)),maxy=max(c(trackMinMaxMatrix[,axis2],0)))
	#minMaxList <- list(minx=min(trackMinMaxMatrix[,axis1]),miny=min(trackMinMaxMatrix[,axis2]),maxx=max(trackMinMaxMatrix[,axis1]),maxy=max(trackMinMaxMatrix[,axis2]))
	original_min_max_list <- findRealMinMax(mat1,mat2,axis1,axis2)
	minx<-original_min_max_list$minx * 1.1
	maxx<-original_min_max_list$maxx * 1.1	
	miny<-original_min_max_list$miny * 1.1	
	maxy<-original_min_max_list$maxy * 1.1
	if(minx == 0){
		minx = -abs(maxx * 0.1)
	}
	if(miny == 0){
		miny = -abs(maxy * 0.1)
	}	
	if(maxx == 0){
		maxx = abs(minx * 0.1)
	}		
	if(maxy == 0){
		maxy = abs(miny * 0.1)
	}	
	minMaxList <- list(minx=minx,miny=miny,maxx=maxx,maxy=maxy)
	return(minMaxList)
}
