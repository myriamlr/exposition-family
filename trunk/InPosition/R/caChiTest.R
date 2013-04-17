caChiTest <- function(DATA,res,critical.value=2){
	
	vals.to.test <- res$ExPosition.Data$cj * matrix(res$ExPosition.Data$eigs,nrow(res$ExPosition.Data$cj),ncol(res$ExPosition.Data$cj),byrow=TRUE) * sum(DATA)
	df <- nrow(DATA) - 1
	#p.vals
	p.vals <- 1-pchisq(vals.to.test,df)
	
	signed.vals <- sign(res$ExPosition.Data$fj) * sqrt(vals.to.test)
	rownames(signed.vals) <- colnames(DATA)
	significant.vals <- p.vals < (2*(1-pnorm(critical.value)))
	rownames(significant.vals) <- rownames(signed.vals)
	return(list(sig.vals=significant.vals, signed.vals=signed.vals,critical.value=critical.value))
	
}