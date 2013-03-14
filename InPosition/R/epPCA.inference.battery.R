epPCA.inference.battery <- function(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, graphs = TRUE, k = 0, test.iters=1000, constrained=FALSE,critical.value=2){
    	
	res <- epPCA(DATA, scale, center, DESIGN, make_design_nominal, graphs=FALSE, k)
#	if(graphs){ ##this is stupid and I need to change this.
		q.boot.array <- array(0,dim=c(ncol(DATA),res$ExPosition.Data$pdq$ng,test.iters))
		eigs.perm.matrix <- matrix(0,test.iters,res$ExPosition.Data$pdq$ng)
		ncomps <- res$ExPosition.Data$pdq$ng
#	}else{
#		q.boot.array <- array(0,dim=c(ncol(DATA),res$pdq$ng,test.iters))
#		eigs.perm.matrix <- matrix(0,test.iters,res$pdq$ng)		
#		ncomps <- res$pdq$ng		
#	}

	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
#		if(graphs){ ##this is stupid and I need to change this.
			q.boot.array[,,i] <- boot.compute.Q(res,DESIGN,constrained)
#		}else{
#			q.boot.array[,,i] <- boot.compute.Q(res,DESIGN,constrained)
#		}
		perm.eigs <- permute.components(DATA,scale,center)
		eigs.perm.matrix[i,] <- perm.eigs[1:(min(length(perm.eigs),ncomps))]
		setTxtProgressBar(pb,i)		
	}		
	q.boot.array <- replace(q.boot.array,is.nan(q.boot.array),0)
	rownames(q.boot.array) <- colnames(DATA)
	boot.tests <- boot.ratio.test(q.boot.array,critical.value=critical.value)
	return(list(q.boot.array=q.boot.array,fixed=res,boot.tests=boot.tests,perm.eigs.inertia=eigs.perm.matrix))
}


permute.components <- function(DATA,scale,center){
	perm.DATA <- apply(DATA,2,sample)
	return(epPCA(perm.DATA,graphs=FALSE,scale=scale,center=center)$ExPosition.Data$eigs)
}

boot.compute.Q <- function(res,DESIGN=NULL,constrained=FALSE){
	boot.index <- boot.samples(res$ExPosition.Data$X,DESIGN,constrained=constrained)
	#return(t(DATA[boot.index,]) %*% res$pdq$p[boot.index,] %*% diag(res$pdq$Dv^-1))
	return(t(res$ExPosition.Data$X[boot.index,]) %*% res$ExPosition.Data$pdq$p[boot.index,] %*% diag(res$ExPosition.Data$pdq$Dv^-1))	
}


# boot.samples <- function(DATA,DESIGN=NULL,constrained=FALSE){
	# if(constrained){
		# boot.index <- vector()
		# for(i in 1:ncol(DESIGN)){
			# boot.index <- c(boot.index,sample(which(DESIGN[,i]==1),replace=TRUE))
		# }
	# }else{
		# return(sample(nrow(DATA),nrow(DATA),TRUE))
	# }
# }

# boot.ratio.test <- function(boot.cube,critical.value=2){	
	# boot.cube.mean <- apply(boot.cube,c(1,2),mean)
	# boot.cube.mean_repeat <- array(boot.cube.mean,dim=c(dim(boot.cube)))
	# boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
	# s.boot<-(apply(boot.cube.dev,c(1,2),mean))^(1/2)
	# boot.ratios <- boot.cube.mean / s.boot
	# significant.boot.ratios <- (abs(boot.ratios) > critical.value)
	# rownames(boot.ratios) <- rownames(boot.cube)
	# rownames(significant.boot.ratios) <- rownames(boot.cube)	
	# return(list(sig.boot.ratios=significant.boot.ratios,boot.ratios=boot.ratios))
# }