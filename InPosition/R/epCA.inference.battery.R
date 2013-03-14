###function to handle fixed & random (bootstrap) effects for epCA	
epCA.inference.battery <- function(DATA, DESIGN = NULL, make_design_nominal = TRUE, masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, graphs = TRUE, k = 0, test.iters=100, constrained=FALSE, critical.value=2){

	res <- epCA(DATA, DESIGN, make_design_nominal, masses, weights, hellinger, symmetric, graphs, k)
	fj.boot.array <- array(0,dim=c(ncol(DATA),res$ExPosition.Data$pdq$ng,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,res$ExPosition.Data$pdq$ng)
	ncomps <- res$ExPosition.Data$pdq$ng		

	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		fj.boot.array[,,i] <- boot.compute.fj(DATA,res$ExPosition.Data,DESIGN,constrained)
		perm.eigs <- permute.components(DATA,hellinger,symmetric,scaleFlag,centerFlag)
		eigs.perm.matrix[i,] <- perm.eigs[1:(min(length(perm.eigs),ncomps))]		
		setTxtProgressBar(pb,i)		
	}
	
	fj.boot.array <- replace(fj.boot.array,is.nan(fj.boot.array),0)
	rownames(fj.boot.array) <- colnames(DATA)
	boot.tests <- boot.ratio.test(fj.boot.array,critical.value=critical.value)	
	
	##need to return p-values where I can.
	return(list(fj.boot.array=fj.boot.array,fixed=res,boot.tests=boot.tests,perm.eigs.inertia=eigs.perm.matrix,perm.omni.inertia=rowSums(eigs.perm.matrix)))
	
}

#do something about this
#permute.components <- function(DATA,hellinger,symmetric,scaleFlag,centerFlag){
#	perm.DATA <- apply(DATA,2,sample)
#	return(epCA(perm.DATA,hellinger=hellinger,symmetric=symmetric,graphs=FALSE)$eigs)
#}