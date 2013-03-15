makeDistancesAndWeights <- function(DATA,method="euclidean",masses=NULL){
	if(method=="chi2"){
		chi2res <- chi2Dist(DATA)
		D <- chi2res$D
		MW <- list(M=chi2res$M)
	}else{
		D <- as.matrix(dist(DATA,method=method,diag=TRUE,upper=TRUE))
		MW <- computeMW(D,masses=masses)		
	}
	return(list(D=D,MW=MW))
}