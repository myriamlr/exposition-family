epMDS.inference.battery <- function(DATA,DATA_is_dist=TRUE,method="euclidean",DESIGN=NULL,make_design_nominal=TRUE,masses=NULL,graphs=TRUE,k=0, test.iters=1000, constrained=FALSE,critical.value=2){

###some private functions
permute.components.mds <- function(DATA,method="euclidean",masses=NULL,k=0){
	perm.DATA <- apply(DATA,2,sample)
	return(epMDS(DATA=DATA,DATA_is_dist=FALSE,method=method,masses=masses,k=k)$ExPosition.Data$eigs)
}
###end private functions
    	
	fixed.res <- epMDS(DATA=DATA,DATA_is_dist= DATA_is_dist,method=method,DESIGN=DESIGN,make_design_nominal= make_design_nominal,masses=masses,graphs=FALSE,k=k)
	if(DATA_is_dist){
		print("You must provide raw data (not distances) for inference tests with MDS. Returning descriptive (fixed effect) results.")
		return(fixed.res)
	}

	ncomps <- fixed.res$ExPosition.Data$pdq$ng
	#fj.boot.array <- array(0,dim=c(ncol(DATA),ncomps,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,min(dim(DATA)))
		
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		if(i==1){
			start.time <- proc.time()
		}
				
		#fj.boot.array[,,i] <- boot.compute.fj(DATA,res=fixed.res,DESIGN=DESIGN,constrained=constrained)
		perm.eigs <- permute.components.mds(DATA,method=methods,masses=masses,k=k)
		eigs.perm.matrix[i,1:length(perm.eigs)] <- perm.eigs
		
		if(i==1){
			cycle.time <- (proc.time() - start.time) #this is in seconds...
			if(!continueResampling(cycle.time,test.iters)){
				##exit strategy.
				return(fixed.res)
			}
		}
		setTxtProgressBar(pb,i)		
	}		

	#fj.boot.array <- replace(fj.boot.array,is.nan(fj.boot.array),0)
	#rownames(fj.boot.array) <- colnames(DATA)
	#fj.boot.data <- boot.ratio.test(fj.boot.array,critical.value=critical.value)
	
	eigs.perm.matrix <- eigs.perm.matrix[,1:ncomps]
	component.p.vals <- 1-(colSums(eigs.perm.matrix < matrix(fixed.res$ExPosition.Data$eigs,test.iters, ncomps,byrow=TRUE))/test.iters)
	component.p.vals[which(component.p.vals==0)] <- 1/test.iters
	components.data <- list(p.vals=component.p.vals, eigs.perm=eigs.perm.matrix)
	
	Inference.Data <- list(components=components.data)
	class(Inference.Data) <- c("epPCA.inference.battery","list")

	ret.data <- list(Fixed.Data=fixed.res,Inference.Data=Inference.Data)
	class(ret.data) <- c("inpoOutput","list")	

	#graphing needs to happen here.
	if(graphs){
		#epGraphs(fixed.res,graphs=graphs)
		inGraphs(ret.data)
	}

	return(ret.data)
}