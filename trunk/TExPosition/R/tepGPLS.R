##for a generalized (mixed-modality) PLS. This is for later development.
tepGPLS <-
function(DATA1,DATA2,make_data1_nominal=FALSE,make_data2_nominal=FALSE,DESIGN=NULL,make_design_nominal=TRUE,weights1=NULL,weights2=NULL,symmetric=TRUE,graphs=TRUE,k=0){
	
	if(nrow(DATA1) != nrow(DATA2)){
		stop("DATA1 and DATA2 must have the same number of rows.")
	}
	
}