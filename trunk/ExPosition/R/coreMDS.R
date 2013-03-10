coreMDS <-
function(DATA,m=NULL,decomp.approach='svd',k=0){

	DATA_dims <- dim(DATA)
	#DATA comes in already scaled & centered or not. That happens at the MDS or MDS-extension level.

	if(is.null(m)){
		m <- rep(1/DATA_dims[1],DATA_dims[1])
	}
	
	X <- DATA
	
	pdq_results <- basePDQ(X,is.mds=TRUE,decomp.approach=decomp.approach,k=k)
	
	fi <- matrix(1/sqrt(m),nrow=length(m),ncol=length(pdq_results$Dv)) * (pdq_results$p %*% diag(sqrt(pdq_results$Dv)))
	rownames(fi) <- rownames(DATA)		
	di <- rowSums(fi^2)
	ri <- repmat((1/di),1,pdq_results$ng) * (fi^2)
	ri <- replace(ri,is.nan(ri),0)	
	ci <- repmat(m,1,pdq_results$ng) * (fi^2)/repmat(t(pdq_results$Dv),DATA_dims[1],1)
	di <- as.matrix(di)		

	#I can append the masses & weights if necessary in the appropriate functions
	res <- list(fi=fi,di=di,ci=ci,ri=ri,t=pdq_results$tau,eigs=pdq_results$Dv,pdq=pdq_results,X=X)
}
