###don't know yet...

# boot.compute.cubes <- function(DATA,DESIGN,res){
	# boot.sample.vector <- boot.samples(DATA,DESIGN)
	# R <- t(DESIGN) %*% DATA[boot.sample.vector,]
	# Fboot_ctr_Y <- supplementaryRows(R,res)$fii
	# Fboot_ctr_X <- (t(R)/((rowSums(t(R))) %*% matrix(1, 1, ncol(t(R))))) %*% res$fi %*% diag(res$pdq$Dv^-1)	
	# Fboot_ctr_Y <- replace(Fboot_ctr_Y,is.nan(Fboot_ctr_Y),0)
	# Fboot_ctr_X <- replace(Fboot_ctr_X,is.nan(Fboot_ctr_X),0)	
	# return(list(FBX=Fboot_ctr_X,FBY=Fboot_ctr_Y))
# }

boot.compute.cubes.ca <- function(DATA,DESIGN,res){
	 boot.sample.vector <- boot.samples(DATA,DESIGN,constrained=TRUE)
	 #boot.sample.vector <- 1:nrow(DATA)
	 R <- t(DESIGN[boot.sample.vector,]) %*% DATA[boot.sample.vector,]
	 Rpro<-caSupplementalElementsPreProcessing(R,res$TExPosition.Data$hellinger)
	 tRpro<-caSupplementalElementsPreProcessing(t(R),res$TExPosition.Data$hellinger)
	Fboot_ctr_Y <- Rpro %*% res$TExPosition.Data$fj %*% diag(res$TExPosition.Data$pdq$Dv^-1)		  
	Fboot_ctr_X <- tRpro %*% res$TExPosition.Data$fi %*% diag(res$TExPosition.Data$pdq$Dv^-1)		  
	# Fboot_ctr_Y <- supplementaryRows(R,res)$fii
	# Fboot_ctr_X <- (t(R)/((rowSums(t(R))) %*% matrix(1, 1, ncol(t(R))))) %*% res$fi %*% diag(res$pdq$Dv^-1)	
	 Fboot_ctr_Y <- replace(Fboot_ctr_Y,is.nan(Fboot_ctr_Y),0)
	 Fboot_ctr_X <- replace(Fboot_ctr_X,is.nan(Fboot_ctr_X),0)	
	 return(list(FBX=Fboot_ctr_X,FBY=Fboot_ctr_Y))
	
 }


boot.compute.cubes <- function(DATA,DESIGN,res){
	massedDESIGN <- t(t(DESIGN) * (1/(colSums(DESIGN))))
	boot.sample.vector <- boot.samples(DATA,DESIGN,constrained=TRUE)


#	BootX <- DATA[boot.sample.vector,]
	BootX <- DATA[boot.sample.vector,]
	BootY <- massedDESIGN[boot.sample.vector,]
	Rboot<-t(BootY) %*% BootX
	Rboot <- expo.scale(Rboot,scale=res$TExPosition.Data$scale,center=res$TExPosition.Data$center)
	
#	if(is.null(dim(res$W))){	
		Fboot_ctr_Y <- Rboot %*% (matrix(res$TExPosition.Data$W,length(res$TExPosition.Data$W),length(res$TExPosition.Data$pdq$Dv)) * (res$TExPosition.Data$fj %*% diag(res$TExPosition.Data$pdq$Dv^-1)))
		Fboot.Y.test <- supplementaryRows(t(massedDESIGN) %*% BootX,res)$fii
#	}else{
#		Fboot_ctr_Y <- Rboot %*% res$W %*% res$pdq$q
#	}
	
#	if(is.null(dim(res$M))){
		Fboot_ctr_X <- t(Rboot) %*% (matrix(res$TExPosition.Data$M,length(res$TExPosition.Data$M),length(res$TExPosition.Data$pdq$Dv)) * (res$TExPosition.Data$fi %*% diag(res$TExPosition.Data$pdq$Dv^-1)))
#	}else{
#		Fboot_ctr_X <- t(Rboot) %*% res$M %*% res$pdq$p
#	}
	Fboot_ctr_Y <- replace(Fboot_ctr_Y,is.nan(Fboot_ctr_Y),0)
	Fboot_ctr_X <- replace(Fboot_ctr_X,is.nan(Fboot_ctr_X),0)	
	FBtY<-replace(Fboot.Y.test,is.nan(Fboot.Y.test),0)
	return(list(FBX=Fboot_ctr_X,FBY=Fboot_ctr_Y,FBtY=FBtY))
}

