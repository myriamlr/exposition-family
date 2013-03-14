tepDICA.inference.battery <- function(DATA, make_data_nominal = FALSE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2){
	
	DATA <- as.matrix(DATA)
	if(make_data_nominal){
		DATA <- makeNominalData(DATA)
	}
	
	DESIGN <- as.matrix(DESIGN)
	if(make_design_nominal){
		DESIGN <- makeNominalData(DESIGN)
	}
	
	res <- tepDICA(DATA=DATA, make_data_nominal = FALSE, DESIGN = DESIGN, make_design_nominal = FALSE, group.masses = group.masses, ind.masses = ind.masses, weights = weights, hellinger = hellinger, symmetric = symmetric, graphs = FALSE, k = k)

	FBY <- array(0,dim=c(nrow(res$TExPosition.Data$X),res$TExPosition.Data$pdq$ng,test.iters))
	FBX <- array(0,dim=c(ncol(res$TExPosition.Data$X),res$TExPosition.Data$pdq$ng,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,res$TExPosition.Data$pdq$ng)
	r2.perm <- inertia.perm <- matrix(0,test.iters,1)
	ncomps <- res$TExPosition.Data$pdq$ng
	loo.assign <- matrix(0,nrow(DESIGN),ncol(DESIGN))
	loo.fii <- matrix(0,nrow(DESIGN),ncomps)

	##loo test first
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:nrow(DATA)){
		loo.test.res <- loo.test(DATA=DATA, make_data_nominal = FALSE, DESIGN = DESIGN, make_design_nominal = FALSE, group.masses = group.masses, ind.masses = ind.masses, weights = weights, hellinger = hellinger, symmetric = symmetric, k = k, i)
		loo.assign[i,] <- loo.test.res$assignSup$assignments
		loo.fii[i,] <- loo.test.res$supX$fii
		setTxtProgressBar(pb,i)			
	}

	#boot & perm test next
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
#		if(graphs){ ##this is stupid and I need to change this.
			boot.res <- boot.compute.cubes.ca(DATA,DESIGN,res)
#		}else{
#			boot.res <- boot.compute.cubes(DATA,DESIGN,res)
#		}
		FBX[,,i] <- boot.res$FBX
		FBY[,,i] <- boot.res$FBY
		permute.res <- permute.tests(DATA=DATA, make_data_nominal = FALSE, DESIGN = DESIGN, make_design_nominal = make_design_nominal, group.masses = group.masses, ind.masses = ind.masses, weights = weights, hellinger = hellinger, symmetric = symmetric,k = k)
		eigs.perm.matrix[i,] <- permute.res$perm.eigs
		r2.perm[i,] <- permute.res$perm.r2
		inertia.perm[i,] <- permute.res$perm.inertia
		setTxtProgressBar(pb,i)		
	}		
	boot.tests.x <- boot.ratio.test(FBX,critical.value)
	boot.tests.y <- boot.ratio.test(FBY,critical.value)

	return(list(FbootX.array=FBX,FbootY.array=FBY,BootTests.X=boot.tests.x,BootTests.Y=boot.tests.y,fixed=res,r2.perm=r2.perm,inertia.perm=inertia.perm,eigs.perm=eigs.perm.matrix,loo.assign=loo.assign,loo.fii=loo.fii))	
}


# ###something needs to be done about these...
# loo.test <- function(DATA,make_data_nominal = FALSE,DESIGN,make_design_nominal = TRUE,group.masses = NULL, ind.masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE,k = k, i){
	# DICAminus1 <- tepDICA(DATA[-i,],DESIGN=DESIGN[-i,],make_design_nominal=make_design_nominal,make_data_nominal=make_data_nominal,hellinger=hellinger,symmetric=symmetric,weights=weights,ind.masses=ind.masses,group.masses=group.masses,graphs=FALSE,k=k)
	
	# supX <- supplementaryRows(SUP.DATA=t(DATA[i,]), res=DICAminus1)
	# assignSup <- fii2fi(DESIGN=t(DESIGN[i,]), fii=supX$fii, fi=DICAminus1$TExPosition.Data$fi)
	# return(list(supX=supX,assignSup=assignSup))
# }

# permute.tests <- function(DATA, make_data_nominal = FALSE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, k = 0){
	
	# perm.samps <- sample(nrow(DESIGN),nrow(DESIGN),FALSE)
	# res.perm <- tepDICA(DATA,DESIGN=DESIGN[perm.samps,],make_design_nominal=make_design_nominal,make_data_nominal=make_data_nominal,hellinger=hellinger,symmetric=symmetric,weights=weights,ind.masses=ind.masses,group.masses=group.masses,graphs=FALSE)
	# perm.r2 <- res.perm$TExPosition.Data$assign$r2
	# perm.eigs <- res.perm$TExPosition.Data$eigs	
	# perm.inertia <- sum(perm.eigs)
	# return(list(perm.r2=perm.r2,perm.eigs=perm.eigs,perm.inertia=perm.inertia))
# }

# boot.compute.cubes <- function(DATA,DESIGN,res){
	# boot.sample.vector <- boot.samples(DATA,DESIGN)
	# R <- t(DESIGN) %*% DATA[boot.sample.vector,]
	# Fboot_ctr_Y <- supplementaryRows(R,res)$fii
	# Fboot_ctr_X <- (t(R)/((rowSums(t(R))) %*% matrix(1, 1, ncol(t(R))))) %*% res$fi %*% diag(res$pdq$Dv^-1)	
	# Fboot_ctr_Y <- replace(Fboot_ctr_Y,is.nan(Fboot_ctr_Y),0)
	# Fboot_ctr_X <- replace(Fboot_ctr_X,is.nan(Fboot_ctr_X),0)	
	# return(list(FBX=Fboot_ctr_X,FBY=Fboot_ctr_Y))
# }
