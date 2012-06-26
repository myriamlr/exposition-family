tepBADA <-
function(DATA,scaleFlag=TRUE,centerFlag=TRUE,DESIGN=NULL,make_design_nominal=TRUE,group.masses=NULL,ind.masses=NULL,weights=NULL,graphs=TRUE,k=0){
		
	DESIGN <- texpoDesignCheck(DATA,DESIGN,make_design_nominal)
	colDESIGN <- colnames(DESIGN)
	massedDESIGN<-t(t(DESIGN) * (1/(colSums(DESIGN))))
	colnames(massedDESIGN) <- colDESIGN	
	
	main <- deparse(substitute(DATA))		
	DATA <- as.matrix(DATA)
	XMW <- computeMW(DATA,masses=ind.masses,weights=weights)

	R <- scale(t(massedDESIGN) %*% DATA,scale=scaleFlag,center=centerFlag)
	RMW <- computeMW(R,masses=group.masses,weights=weights)

	colnames(R) <- colnames(DATA)
	rownames(R) <- colnames(DESIGN)	
	Rdesign <- diag(nrow(R))
	rownames(Rdesign) <- rownames(R)	
	
	#res <- corePCA(R,M=RMW$M,W=RMW$W,k=k)
	res <- epGPCA(R, DESIGN=Rdesign, make_design_nominal=FALSE, scaleFlag = FALSE, centerFlag = FALSE, masses = RMW$M, weights = RMW$W, graphs = FALSE, k = k)

	centerSup <- FALSE
	if(centerFlag){
		centerSup <- attributes(R)$`scaled:center`
	}
	scaleSup <- FALSE
	if(scaleFlag){
		scaleSup <- attributes(R)$`scaled:scale`
	}

	supplementaryRes <- supplementaryRows(DATA,res,center=centerSup,scale=scaleSup)
	#supplementaryRes <- supplementaryRows(DATA,res,center=centerFlag,scale=scaleFlag)
	
	res$fii <- supplementaryRes$fii
	res$dii <- supplementaryRes$dii
	res$rii <- supplementaryRes$rii

	assignments <- fii2fi(DESIGN,res$fii,res$fi)
	assignments$r2 <- R2(RMW$M,res$di,XMW$M,res$dii)
	class(assignments) <- c("tepAssign","list")
	res$assign <- assignments
	
	#new res here
	class(res) <- c("tepBADA","list")	
	if(graphs){
		tepPlotInfo <- tepGraphHandler(res,DATA,DESIGN,main)
	}
	return(tepOutputHandler(res=res,tepPlotInfo=tepPlotInfo))
}
