createSingleColorVector <-
function(matrix,exclude=NULL){
	if(is.null(exclude)){
		return( matrix(sample( 1:length( prettyGraphsColors() ),1,length(prettyGraphsColors()) ),dim(matrix)[1],1) )
	}else{
		return( matrix(sample( 1:length( prettyGraphsColors()[-exclude] ),1,length(prettyGraphsColors()[-exclude]) ),dim(matrix)[1],1) )
	}
}
