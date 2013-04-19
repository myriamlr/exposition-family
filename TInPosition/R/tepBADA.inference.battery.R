tepBADA.inference.battery <- function(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2){
	
	############################
	###private functions for now
	loo.test <- function(DATA,DESIGN,scale=TRUE,center=TRUE,group.masses=group.masses,weights=weights,k=0,i){
		Xminus1 <- DATA[-i,]
		Yminus1 <- DESIGN[-i,]
		BADAminus1 <- tepBADA(DATA=Xminus1,DESIGN=Yminus1, make_design_nominal=FALSE,center=center, scale=scale,graphs=FALSE,group.masses=group.masses,weights=weights,k=k)
		supX <- supplementaryRows(SUP.DATA=t(DATA[i,]), res=BADAminus1)
		assignSup <- fii2fi(DESIGN=t(DESIGN[i,]), fii=supX$fii, fi=BADAminus1$TExPosition.Data$fi)
		return(list(assignSup=assignSup,supX=supX))
	}
	
	
	
	##private functions for now	
	permute.tests <- function(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, group.masses = NULL, weights = NULL, k = 0){
		
		PermDATA <- DATA[sample(nrow(DATA),nrow(DATA),FALSE),]
		perm.res <- tepBADA(DATA=PermDATA, scale=scale, center=center, DESIGN=DESIGN, make_design_nominal=FALSE, group.masses=group.masses, weights=weights, graphs = FALSE, k=k)
		
		perm.r2 <- perm.res$TExPosition.Data$assign$r2
		perm.eigs <- perm.res$TExPosition.Data$eigs		
		perm.inertia <- sum(perm.eigs)
		return(list(perm.r2=perm.r2,perm.eigs=perm.eigs,perm.inertia=perm.inertia))	
	}	
	############################	
	
	DATA <- as.matrix(DATA)
	DESIGN <- as.matrix(DESIGN)
	if(make_design_nominal){
		DESIGN <- makeNominalData(DESIGN)
	}
	
	fixed.res <- tepBADA(DATA = DATA, scale = scale, center = center, DESIGN = DESIGN, make_design_nominal = FALSE, group.masses = group.masses, ind.masses = ind.masses, weights = weights, graphs = FALSE, k = k)
	
	ncomps <- fixed.res$TExPosition.Data$pdq$ng
	FBY <- array(0,dim=c(nrow(fixed.res$TExPosition.Data$X),ncomps,test.iters))
	FBX <- array(0,dim=c(ncol(fixed.res$TExPosition.Data$X),ncomps,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,ncomps)
	r2.perm <- inertia.perm <- matrix(0,test.iters,1)
	
	#boot & perm test next
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		if(i==1){
			start.time <- proc.time()
		}
				
		boot.res <- boot.compute.fi.fj(DATA,DESIGN,fixed.res)
		FBX[,,i] <- boot.res$FBX
		FBY[,,i] <- boot.res$FBY
		permute.res <- permute.tests(DATA = DATA, scale = scale, center = center, DESIGN = DESIGN, group.masses = group.masses, weights = weights, k = k)
		eigs.perm.matrix[i,] <- permute.res$perm.eigs
		r2.perm[i,] <- permute.res$perm.r2
		inertia.perm[i,] <- permute.res$perm.inertia
		
		if(i==1){
			cycle.time <- (proc.time() - start.time) #this is in seconds...
			if(!continueResampling(cycle.time,test.iters+nrow(DESIGN))){
				##exit strategy.
				return(fixed.res)
			}
		}
		
		setTxtProgressBar(pb,i)		
	}
	
	loo.assign <- matrix(0,nrow(DESIGN),ncol(DESIGN))
	loo.fii <- matrix(0,nrow(DESIGN),ncomps)
	##loo test first
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:nrow(DATA)){
		loo.test.res <- loo.test(DATA=DATA,DESIGN=DESIGN,scale=scale,center=center,i=i,k=k,group.masses=group.masses,weights=weights)
		loo.assign[i,] <- loo.test.res$assignSup$assignments
		loo.fii[i,] <- loo.test.res$supX$fii
		setTxtProgressBar(pb,i)			
	}
	
	rownames(FBX) <- colnames(DATA)
	rownames(FBY) <- colnames(DESIGN)		
	fj.boot.data <- list(fj.tests=boot.ratio.test(FBX,critical.value),fj.boots=FBX)
	fi.boot.data <- list(fi.tests=boot.ratio.test(FBY,critical.value),fi.boots=FBY)
	boot.data <- list(fj.boot.data=fj.boot.data,fi.boot.data=fi.boot.data)
	
	component.p.vals <- 1-(colSums(eigs.perm.matrix < matrix(fixed.res$TExPosition.Data$eigs,test.iters, ncomps,byrow=TRUE))/test.iters)
	component.p.vals[which(component.p.vals==0)] <- 1/test.iters
	components.data <- list(p.vals=component.p.vals, eigs.perm=eigs.perm.matrix)
	
	omni.p <- max(1-(sum(inertia.perm < sum(fixed.res$TExPosition.Data$eigs))/test.iters),1/test.iters)
	omni.data <- list(p.val=omni.p,inertia.perm=inertia.perm)
	
	r2.p <- max(1-(sum(r2.perm < sum(fixed.res$TExPosition.Data$assign$r2))/test.iters),1/test.iters)
	r2.data <- list(p.val=r2.p,r2.perm=r2.perm)
	
 	Inference.Data <- list(omni=omni.data,r2=r2.data,components=components.data,boot.data=boot.data)
 	class(Inference.Data) <- c("tepBADA.inference.battery","list")
 	
 	ret.data <- list(Fixed.Data=fixed.res,Inference.Data=Inference.Data)
 	class(ret.data) <- c("tinpoOutput","list")
 	
	if(graphs){
		tinGraphs(ret.data)
	}
	
 	return(ret.data)
	
	#return(list(FbootX.array=FBX,FbootY.array=FBY,BootTests.X=boot.tests.x,BootTests.Y=boot.tests.y,fixed=res,r2.perm=r2.perm,inertia.perm=inertia.perm,eigs.perm=eigs.perm.matrix,loo.assign=loo.assign,loo.fii=loo.fii))	

}


# permute.tests <- function(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, k = 0){
	
	# PermDATA <- DATA[sample(nrow(DATA),nrow(DATA),FALSE),]
	# perm.res <- tepBADA(DATA=PermDATA, scale, center, DESIGN, make_design_nominal, group.masses, ind.masses, weights, graphs = FALSE, k)
	# perm.eigs <- perm.fixed.res$TExPosition.Data$eigs
	# perm.r2 <- perm.fixed.res$TExPosition.Data$assign$r2
	# perm.inertia <- sum(perm.eigs)
	# return(list(perm.r2=perm.r2,perm.eigs=perm.eigs,perm.inertia=perm.inertia))	
# }

# boot.compute.cubes <- function(DATA,DESIGN,res,scaleVals=TRUE,centerVals=TRUE){
	# massedDESIGN <- t(t(DESIGN) * (1/(colSums(DESIGN))))
	# boot.sample.vector <- boot.samples(DATA,DESIGN)
	# BootX <- DATA[boot.sample.vector,]
	# Rboot <- scale(t(massedDESIGN) %*% BootX,scale=scaleVals,center=centerVals)
	
	# if(is.null(dim(fixed.res$W))){
		# Fboot_ctr_Y <- Rboot %*% (matrix(fixed.res$W,length(fixed.res$W),length(fixed.res$pdq$Dv)) * (fixed.res$fj %*% diag(fixed.res$pdq$Dv^-1)))
	# }else{
		# Fboot_ctr_Y <- Rboot %*% fixed.res$W %*% fixed.res$pdq$q
	# }
	
	# if(is.null(dim(fixed.res$M))){
		# Fboot_ctr_X <- t(Rboot) %*% (matrix(fixed.res$W,length(fixed.res$M),length(fixed.res$pdq$Dv)) * (fixed.res$fi %*% diag(fixed.res$pdq$Dv^-1)))
	# }else{
		# Fboot_ctr_X <- t(Rboot) %*% fixed.res$M %*% fixed.res$pdq$p
	# }
	# Fboot_ctr_Y <- replace(Fboot_ctr_Y,is.nan(Fboot_ctr_Y),0)
	# Fboot_ctr_X <- replace(Fboot_ctr_X,is.nan(Fboot_ctr_X),0)			
	# return(list(FBX=Fboot_ctr_X,FBY=Fboot_ctr_Y))
# }