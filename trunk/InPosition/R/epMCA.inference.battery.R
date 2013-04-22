###function to handle fixed & random (bootstrap) effects for epMCA	
epMCA.inference.battery <- function(DATA, make_data_nominal = TRUE, DESIGN = NULL, make_design_nominal = TRUE, masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, correction = c("b"), graphs = TRUE, k = 0, test.iters=100, constrained=FALSE, critical.value=2){

####private functions
permute.components.mca <- function(DATA,make_data_nominal=TRUE,hellinger=FALSE,symmetric=TRUE,masses=NULL,weights=NULL,correction=c("b"),k=0){
 	if(!make_data_nominal){
		DATA <- rebuildMCAtable(DATA)
		make_data_nominal <- !make_data_nominal
	}
	perm.DATA <- apply(DATA,2,sample)
	return(epMCA(perm.DATA,make_data_nominal=TRUE,hellinger=hellinger,symmetric=symmetric,graphs=FALSE,k=k,masses=masses,weights=weights,correction=correction)$ExPosition.Data$eigs)
}
####end private

	if(make_data_nominal){
		nom.DATA <- makeNominalData(DATA)
		#make_data_nominal <- !make_data_nominal
	}else{
		nom.DATA <- DATA	
	}

	fixed.res <- epMCA(nom.DATA, make_data_nominal=FALSE, DESIGN, make_design_nominal, masses, weights, hellinger, symmetric, correction, graphs=FALSE, k)
			
	ncomps <- fixed.res$ExPosition.Data$pdq$ng		
	fj.boot.array <- array(0,dim=c(ncol(nom.DATA),ncomps,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,min(dim(DATA)))

	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		if(i==1){
			start.time <- proc.time()
		}
		
		fj.boot.array[,,i] <- boot.compute.fj(nom.DATA,fixed.res$ExPosition.Data,DESIGN,constrained)
		perm.eigs <- permute.components.mca(DATA,make_data_nominal=make_data_nominal,hellinger=hellinger,symmetric=symmetric,masses=masses,weights=weights,correction=correction,k=k)
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
	
	rownames(fj.boot.array) <- colnames(nom.DATA)
	fj.boot.data <- boot.ratio.test(fj.boot.array,critical.value=critical.value)
	
	inertia.perm <- rowSums(eigs.perm.matrix)
	omni.p <- max(1-(sum(inertia.perm < sum(fixed.res$ExPosition.Data$eigs))/test.iters),1/test.iters)
	omni.data <- list(p.val=omni.p,inertia.perm=inertia.perm)
	
	eigs.perm.matrix <- eigs.perm.matrix[,1:ncomps]
	component.p.vals <- 1-(colSums(eigs.perm.matrix < matrix(fixed.res$ExPosition.Data$eigs,test.iters, ncomps,byrow=TRUE))/test.iters)
	component.p.vals[which(component.p.vals==0)] <- 1/test.iters
	components.data <- list(p.vals=component.p.vals, eigs.perm=eigs.perm.matrix)
	
 	Inference.Data <- list(components=components.data,fj.boots=fj.boot.data,omni=omni.data)
	class(Inference.Data) <- c("epMCA.inference.battery","list")

	ret.data <- list(Fixed.Data=fixed.res,Inference.Data=Inference.Data)
	class(ret.data) <- c("inpoOutput","list")	

	#graphing needs to happen here.
	if(graphs){
		inGraphs(ret.data)
	}

	return(ret.data)
	
}