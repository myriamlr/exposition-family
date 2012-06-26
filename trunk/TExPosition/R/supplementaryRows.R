supplementaryRows <-
function(SUP.DATA,res,center=TRUE,scale=TRUE){
	#This only works with supplementary rows. Technically, it can work with a simple t(SUP.DATA) but I will not let it, yet...
	#I believe supplementary columns needs a bit more thought.
	SUP.DATA <- as.matrix(SUP.DATA)

	mds.types <- c('epMDS')#can add DiSTATIS to this.
	pca.types <- c('epPCA','epGPCA','tepBADA')
	ca.types <- c('epCA','epMCA','tepDICA')

	if((class(res)[1] %in% c(pca.types))){
		#this is too specific to BADA, I should pass in scale and center.
		#sup.transform <- scale(SUP.DATA,center=attributes(res$R)$`scaled:center`,scale=attributes(res$R)$`scaled:scale`)
		sup.transform <- scale(SUP.DATA,center=center,scale=scale)
		if(c('W') %in% names(res)){
			if(is.null(dim(res$W))){
				sup.transform <- sup.transform * t(repmat(res$W,1,nrow(sup.transform)))
			}else if(nrow(res$W)==ncol(res$W) && ncol(res$W)==nrow(res$fj)){
				sup.transform <- sup.transform %*% res$W
			}else{
				stop('Something is wrong with the weights matrix.')
			}
		}
		this.fj <- res$fj
	}else if((class(res)[1] %in% c(ca.types))){
		if(!res$hellinger){
			sup.transform <- SUP.DATA/((rowSums(SUP.DATA))%*%matrix(1,1,ncol(SUP.DATA)))
		}else{
			sup.transform <- (SUP.DATA/repmat(rowSums(SUP.DATA),1,ncol(SUP.DATA)))^(1/2)
		}
		this.fj <- res$fj		
	}else if((class(res)[1] %in% c(mds.types))){
		#MDS supplemental data _must_ be a distance matrix.
		if(nrow(SUP.DATA)==ncol(SUP.DATA) && sum(diag(SUP.DATA))==0){
			BigXi <- diag(nrow(res$X)) - (matrix(1,nrow(res$X),1) %*% diag(res$M))
			sup.transform <- t(-(0.5) * BigXi %*% (SUP.DATA - (res$D %*% diag(res$M) %*% matrix(1,1,length(diag(res$M))))))
			this.fj <- res$fi
		}else{
			stop("MDS supplemental data is not a distance matrix.")
		}
	}else{
		stop("Unknown class type. Supplementary projection computation must stop.")	
	}
	
	#CAN USE THIS FOR ALL SUPPLEMENTARY ROWS!!
	#mds_results_sup <- t(S_sup) %*% mds_results$f %*% diag(mds_results$Dv^(-1))
	fii <- sup.transform %*% this.fj %*% diag(res$pdq$Dv^-1)
	dii <- rowSums(fii^2)
	rii <- repmat((1/dii),1,length(res$pdq$Dv)) * (fii^2)
	rii <- replace(rii,is.nan(rii),0)
	dii <- as.matrix(dii)
	rownames(fii) <- rownames(dii) <- rownames(rii) <- rownames(SUP.DATA)
	
	return(list(fii=fii,dii=dii,rii=rii))
}
