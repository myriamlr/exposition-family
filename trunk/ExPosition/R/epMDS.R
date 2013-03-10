epMDS <-
function(DATA,DATA_is_dist=TRUE,method="euclidean",DESIGN=NULL,make_design_nominal=TRUE,masses=NULL,graphs=TRUE,k=0){
	main <- deparse(substitute(DATA))	
	DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)
	DATA <- as.matrix(DATA)
	DATA_dimensions = dim(DATA)
		
	if(DATA_is_dist && (nrow(DATA)==ncol(DATA))){
		D <- DATA
		MW <- computeMW(D,masses=masses)
	}else{
		#print('Creating distance matrix from DATA.')
		if(method=="chi2"){
			chi2res <- chi2Dist(DATA)
			D <- chi2res$D
			MW <- list(M=chi2res$M)
		}else{
			D <- as.matrix(dist(DATA,method=method,diag=TRUE,upper=TRUE))
			MW <- computeMW(D,masses=masses)		
		}
	}
	
	#do this every time.
	Mrepmat <- matrix(MW$M,nrow=nrow(D),ncol=ncol(D))
	if(is.null(dim(MW$M))){ # it is a vector; with new way, it is always a vector
		BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% MW$M)
	}else{#ths forces a matrix to be a vector this needs to be better.
		#BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% diag(masses))
		BigXi <- diag(DATA_dimensions[1]) - (matrix(1,DATA_dimensions[1],1) %*% diag(MW$M))
	}
#	S <- (-(0.5) * BigXi %*% D %*% t(BigXi))
	S <- -.5 * sqrt(Mrepmat) * BigXi %*% D %*% t(BigXi) * sqrt(t(Mrepmat))

	rownames(S) <- rownames(D)
	colnames(S) <- colnames(D)	
	
	res <- coreMDS(S,MW$M,k=k)

	#overwrite res with half of res because it's MDS and we don't care
	res <- list(fi=res$fi,di=res$di,ci=res$ci,ri=res$ri,t=res$t,eigs=res$eigs,pdq=res$pdq,M=MW$M,X=res$X,D=D)
	class(res) <- c("epMDS","list")

	epPlotInfo <- epGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs)
	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
#	return(res)
}
