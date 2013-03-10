pickSVD <-
function(datain,is.mds=FALSE,decomp.approach='svd',k=0){
	#check k
	if(k < 1){
		k <- min(nrow(datain),ncol(datain))
	}
	#find precision limit, fix what comes back.
	precisionLimit <- 2*.Machine$double.eps	
	#check decomp.approach
	if(is.null(decomp.approach)){
		decomp.approach <- 'svd'
	}
	#get size
	#if there are more than 1 million elements or you want to go fast, do the eigen decomp.approach!
	num.el <- (dim(datain)[1] * dim(datain)[2])
	if(num.el > 1000000){
		decomp.approach <- 'eigen'
	}
	
	
	if( tolower(decomp.approach)=='eigen' ){
		print('eigen method')
		dataDims <- dim(datain)
		I <- dataDims[1]
		J <- dataDims[2]

		m <- min(c(I,J))
		flip <- FALSE
		
		if (I < J){
			datain <- t(datain)
			flip <- TRUE
		}

		eigOut <- eigen(t(datain) %*% datain)	
		Q <- eigOut$vectors
		d <- sqrt(eigOut$values)
		P <- datain %*% Q %*% diag(d^-1)
		if(flip){
			temp<-Q
			Q<-P
			P<-temp
		}
	}else{ ##the default method.
		print('svd method')
		svd.out <- svd(datain,nu=k,nv=k)
		P <- svd.out$u
		Q <- svd.out$v
		d <- svd.out$d
	}
	
	if(is.mds){
		indToKeep <- which(d > precisionLimit)	
		tau <- d[indToKeep]/sum(d[indToKeep])	##value could be small due to error.
	}else{
		indToKeep <- which(d^2 > precisionLimit)		
		tau <- d[indToKeep]^2/sum(d[indToKeep]^2)	##value could be small due to error.
	}
	indToKeep <- indToKeep[1:min(c(length(indToKeep),k))]		
	
	return(list(u=P[,indToKeep],v=Q[,indToKeep],d=d[indToKeep],tau=tau*100))
}
