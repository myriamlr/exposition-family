texpoDesignCheck <-
function(DATA,DESIGN=NULL,make_design_nominal=TRUE){
	DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)
	if(ncol(DESIGN) >= nrow(DESIGN)){
		stop('You have too many groups, try a method in ExPosition.')
	}
	if(ncol(DESIGN)==1){
		stop('You have too few groups, try a method in ExPosition.')		
	}
	if(!sum(colSums(DESIGN)>1)==ncol(DESIGN)){
		#this might be too stringent...
		stop('You have at least 1 group made up of only 1 observation, try a method in ExPosition.')
	}
	return(DESIGN)
}
