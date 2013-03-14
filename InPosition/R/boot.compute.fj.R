

####The DATA that comes in is dependent on method (e.g., PCA vs. CA)
	#In the case of PCA, send in the already normalized matrix?
	#In the case of CA, just let it do its thing.
boot.compute.fj <- function(DATA,res,DESIGN=NULL,constrained=FALSE){
	boot.index <- boot.samples(DATA,DESIGN,constrained=constrained)
#	BootX <- t(DATA[boot.index,])
#	BootXProfiles <- BootX/((rowSums(BootX)) %*% matrix(1, 1, ncol(BootX)))
#	return(BootXProfiles %*% res$fi[boot.index,] %*% diag(res$pdq$Dv^-1) )
	
		#this should catch that things are NULL and skip them.
	supplementaryCols(DATA[boot.index,],res,center=res$center,scale=res$scale)		
		#this just skips center & scale and presumes DATA is proper
	supplementaryCols(DATA[boot.index,],res,center=FALSE,scale=FALSE)
}