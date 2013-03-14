tepBADA.inference.battery <- function(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2){
	
	DATA <- as.matrix(DATA)
	DESIGN <- as.matrix(DESIGN)
	if(make_design_nominal){
		DESIGN <- makeNominalData(DESIGN)
	}
	
	res <- tepBADA(DATA = DATA, scale = scale, center = center, DESIGN = DESIGN, make_design_nominal = FALSE, group.masses = group.masses, ind.masses = ind.masses, weights = weights, graphs = FALSE, k = k)
	
	
	FBY <- array(0,dim=c(nrow(res$TExPosition.Data$X),res$TExPosition.Data$pdq$ng,test.iters))
	FBFUCK <- array(0,dim=c(nrow(res$TExPosition.Data$X),res$TExPosition.Data$pdq$ng,test.iters))	
	FBX <- array(0,dim=c(ncol(res$TExPosition.Data$X),res$TExPosition.Data$pdq$ng,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,res$TExPosition.Data$pdq$ng)
	r2.perm <- inertia.perm <- matrix(0,test.iters,1)
	ncomps <- res$TExPosition.Data$pdq$ng
	
	
	loo.assign <- matrix(0,nrow(DESIGN),ncol(DESIGN))
	loo.fii <- matrix(0,nrow(DESIGN),ncomps)
	##loo test first
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:nrow(DATA)){
		loo.test.res <- loo.test(DATA,DESIGN,scale,center,i)
		loo.assign[i,] <- loo.test.res$assignSup$assignments
		loo.fii[i,] <- loo.test.res$supX$fii
		setTxtProgressBar(pb,i)			
	}

	
	#boot & perm test next
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		boot.res <- boot.compute.cubes(DATA,DESIGN,res)

		FBX[,,i] <- boot.res$FBX
		FBY[,,i] <- boot.res$FBY
		FBFUCK[,,i] <- boot.res$FBtY
		permute.res <- permute.tests(DATA = DATA, scale = scale, center = center, DESIGN = DESIGN, make_design_nominal = make_design_nominal, group.masses = group.masses, ind.masses = ind.masses, weights = weights, k = k)
		eigs.perm.matrix[i,] <- permute.res$perm.eigs
		r2.perm[i,] <- permute.res$perm.r2
		inertia.perm[i,] <- permute.res$perm.inertia
		setTxtProgressBar(pb,i)		
	}		
	boot.tests.x <- boot.ratio.test(FBX,critical.value)
	boot.tests.y <- boot.ratio.test(FBY,critical.value)
	
	return(list(FBFUCK=FBFUCK,FbootX.array=FBX,FbootY.array=FBY,BootTests.X=boot.tests.x,BootTests.Y=boot.tests.y,fixed=res,r2.perm=r2.perm,inertia.perm=inertia.perm,eigs.perm=eigs.perm.matrix,loo.assign=loo.assign,loo.fii=loo.fii))	

}


loo.test <- function(DATA,DESIGN,scale=TRUE,center=TRUE,i){
	Xminus1 <- DATA[-i,]
	Yminus1 <- DESIGN[-i,]
	BADAminus1 <- tepBADA(DATA=Xminus1,DESIGN=Yminus1, make_design_nominal=FALSE,center=center, scale=scale,graphs=FALSE)
	supX <- supplementaryRows(SUP.DATA=t(DATA[i,]), res=BADAminus1)
	assignSup <- fii2fi(DESIGN=t(DESIGN[i,]), fii=supX$fii, fi=BADAminus1$TExPosition.Data$fi)
	return(list(assignSup=assignSup,supX=supX))
}

permute.tests <- function(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, k = 0){
	
	PermDATA <- DATA[sample(nrow(DATA),nrow(DATA),FALSE),]
	perm.res <- tepBADA(DATA=PermDATA, scale, center, DESIGN, make_design_nominal, group.masses, ind.masses, weights, graphs = FALSE, k)
	perm.eigs <- perm.res$TExPosition.Data$eigs
	perm.r2 <- perm.res$TExPosition.Data$assign$r2
	perm.inertia <- sum(perm.eigs)
	return(list(perm.r2=perm.r2,perm.eigs=perm.eigs,perm.inertia=perm.inertia))	
}

# boot.compute.cubes <- function(DATA,DESIGN,res,scaleVals=TRUE,centerVals=TRUE){
	# massedDESIGN <- t(t(DESIGN) * (1/(colSums(DESIGN))))
	# boot.sample.vector <- boot.samples(DATA,DESIGN)
	# BootX <- DATA[boot.sample.vector,]
	# Rboot <- scale(t(massedDESIGN) %*% BootX,scale=scaleVals,center=centerVals)
	
	# if(is.null(dim(res$W))){
		# Fboot_ctr_Y <- Rboot %*% (matrix(res$W,length(res$W),length(res$pdq$Dv)) * (res$fj %*% diag(res$pdq$Dv^-1)))
	# }else{
		# Fboot_ctr_Y <- Rboot %*% res$W %*% res$pdq$q
	# }
	
	# if(is.null(dim(res$M))){
		# Fboot_ctr_X <- t(Rboot) %*% (matrix(res$W,length(res$M),length(res$pdq$Dv)) * (res$fi %*% diag(res$pdq$Dv^-1)))
	# }else{
		# Fboot_ctr_X <- t(Rboot) %*% res$M %*% res$pdq$p
	# }
	# Fboot_ctr_Y <- replace(Fboot_ctr_Y,is.nan(Fboot_ctr_Y),0)
	# Fboot_ctr_X <- replace(Fboot_ctr_X,is.nan(Fboot_ctr_X),0)			
	# return(list(FBX=Fboot_ctr_X,FBY=Fboot_ctr_Y))
# }