
epPCA.inference.battery.old <- function(DATA, scaleFlag = TRUE, centerFlag = TRUE, DESIGN = NULL, make_design_nominal = TRUE, graphs = TRUE, k = 0, test.iters=100, constrained=FALSE, critical.value=2){
    	
	res <- epPCA(DATA, scaleFlag, centerFlag, DESIGN, make_design_nominal, graphs=FALSE, k)
	fj.boot.array <- array(0,dim=c(ncol(DATA),res$ExPosition.Data$pdq$ng,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,res$ExPosition.Data$pdq$ng)
	ncomps <- res$ExPosition.Data$pdq$ng
	
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		fj.boot.array[,,i] <- boot.compute.fj(DATA,DESIGN,constrained)
		perm.eigs <- permute.components(DATA,scaleFlag,centerFlag)
		eigs.perm.matrix[i,] <- perm.eigs[1:(min(length(perm.eigs),ncomps))]
		setTxtProgressBar(pb,i)		
	}		
	fj.boot.array <- replace(fj.boot.array,is.nan(fj.boot.array),0)
	rownames(fj.boot.array) <- colnames(DATA)
	boot.tests <- boot.ratio.test(fj.boot.array,critical.value=critical.value)


	if(graphs){
		silence <- epGraphs(res)
		#inference graphs
	}

	##need to return p-values where I can.
	return(list(fj.boot.array=fj.boot.array,fixed=res,boot.tests=boot.tests,perm.eigs.inertia=eigs.perm.matrix))
}



##do somethign about this.
#permute.components <- function(DATA,scaleFlag,centerFlag){
#	perm.DATA <- apply(DATA,2,sample)
#	return(epPCA(perm.DATA,graphs=FALSE)$eigs)
#}
