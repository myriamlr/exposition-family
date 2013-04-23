tepGraphs <-
function(res,DESIGN=NULL,x_axis=1,y_axis=2,fi.col=NULL, fi.pch=NULL, fii.col=NULL, fii.pch = NULL, fj.col=NULL, fj.pch = NULL,col.offset=NULL,constraints=NULL,xlab=NULL,ylab=NULL,main=NULL,contributionPlots=TRUE,correlationPlotter=TRUE,showHulls=1,biplots=FALSE,graphs=TRUE){

	pca.types <- c('tepBADA')
	ca.types <- c('tepDICA')
	
	#A simple override/check. If someone puts in texpoOutput class data, tepGraphs will recognize it.
	tepPlotInfo <- NULL
	if(class(res)[1] == "texpoOutput"){
		if(length(res)==2){
			tepPlotInfo <- res$Plotting.Data
		}
		res <- res$TExPosition.Data
	}
	
	#perhaps make this stuff a function, or have TExPosition call all of tepGraphs.
	if(!(class(res)[1] %in% c(pca.types,ca.types))){
		stop("Unknown TExPosition class. Plotting has stopped.")
	}else{
		if(is.null(main)){
			main <- deparse(substitute(res))
		}
		if(length(unlist(strsplit(main,"")))>40){
			main <- "Results"
		}				
		if(is.null(xlab)){
			xlab <- paste("Component ",x_axis," variance: ", round(res$t[x_axis],3), "%",sep="")
		}
		if(is.null(ylab)){
			ylab <- paste("Component ",y_axis," variance: ", round(res$t[y_axis],3), "%",sep="")
		}
		#epPlotInfo check will look for proper colors & constraints, mostly.
		if(!is.null(tepPlotInfo)){
			if(!(nrow(res$fi)==nrow(tepPlotInfo$fi.col)) || !(nrow(res$fj)==nrow(tepPlotInfo$fj.col)) || !(nrow(res$fii)==nrow(tepPlotInfo$fii.col))){
				
				print('Dimension mismatch. tepPlotInfo will be reset, no hulls can be shown.')
				tepPlotInfo$fii.col <- NULL
				tepPlotInfo$fi.col <- NULL
				tepPlotInfo$fj.col <- NULL
				tepPlotInfo$constraints <- NULL
			}		
		}else{
			tepPlotInfo <- list(fii.col=NULL,fi.col=NULL,fj.col=NULL,constraints=NULL)
		}
		
		#fii.col, fi.col, fj.col, and constraints take precedence over tepPlotInfo. This is because epPlotInfo only exists via expoOutput.			
		if(is.null(fii.col) || is.null(fi.col) || nrow(fi.col)!=nrow(res$fi) || nrow(fii.col)!=nrow(res$fi)){
			if(is.null(tepPlotInfo$fii.col) || is.null(tepPlotInfo$fi.col)){
				if(is.null(DESIGN)){
					stop("fii.col and DESIGN are NULL. You must provide one or the other.")
				}else{
					#this will catch failures and stop.
					DESIGN <- texpoDesignCheck(DATA=NULL,DESIGN=DESIGN,make_design_nominal=FALSE)
					obs.cols <- createColorVectorsByDesign(DESIGN,offset=col.offset)
					fii.col <- obs.cols$oc
					fi.col <- obs.cols$gc
				}
			}else{
				fii.col <- tepPlotInfo$fii.col
				fi.col <- tepPlotInfo$fi.col
			}
		}
		if(is.null(fj.col) || nrow(fj.col)!=nrow(res$fj)){
			if(is.null(tepPlotInfo$fj.col)){
				fj.col <- createColorVectorsByDesign(matrix(1,nrow(res$fj),1),hsv=FALSE,offset=col.offset)$oc
			}else{
				fj.col <- tepPlotInfo$fj.col	
			}
		}

		if(is.null(fi.pch) || nrow(fi.pch)!=nrow(res$fi)){
			if(is.null(tepPlotInfo$fi.pch)){
				fi.pch <- as.matrix(rep(21,nrow(res$fi)))
			}else{
				fi.pch <- tepPlotInfo$fi.pch
			}
		}
		
		if(is.null(fii.pch) || nrow(fii.pch)!=nrow(res$fii)){
			if(is.null(tepPlotInfo$fii.pch)){
				fii.pch <- as.matrix(rep(21,nrow(res$fii)))
			}else{
				fii.pch <- tepPlotInfo$fii.pch
			}
		}
				
		if(is.null(fj.pch) || nrow(fj.pch)!=nrow(res$fj)){
			if(is.null(tepPlotInfo$fj.pch)){
				fj.pch <- as.matrix(rep(21,nrow(res$fj)))
			}else{
				fj.pch <- tepPlotInfo$fj.pch
			}
		}
		
		if(is.null(constraints)){
			if(!is.null(tepPlotInfo$constraints)){
				constraints <- tepPlotInfo$constraints
			}
			#this is needed because if we switch axes, it could be different constraints.
			constraints <- calculateConstraints(results=res,x_axis=x_axis,y_axis=y_axis,constraints=constraints)			
		}		
		#by the time I get here, I should be guaranteed to have a fii.col, fi.col, fj.col, and constraints.		
		
		if(graphs){
			fii.plot.info <- prettyPlot(res$fii,x_axis=x_axis,y_axis=y_axis,col=fii.col,axes=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,pch=fii.pch,contributionCircles=FALSE,dev.new=TRUE)
			fi.plot.info <- prettyPlot(res$fi,x_axis=x_axis,y_axis=y_axis,col=fi.col,axes=FALSE,contributionCircles=TRUE,contributions=res$ci,pch=fi.pch,dev.new=FALSE,new.plot=FALSE)
			if(showHulls > 0 && showHulls <= 1){
				colorDesign <- makeNominalData(fii.col)
				for(i in 1:nrow(res$fi)){
					peeledHull(res$fii[which(fii.col[, 1] == fi.col[i,1]), ], x_axis = x_axis, y_axis = y_axis, percentage = showHulls, col = "black", lwd = 3)
					peeledHull(res$fii[which(fii.col[, 1] == fi.col[i,1]), ], x_axis = x_axis, y_axis = y_axis, percentage = showHulls, col = fi.col[i, ], lwd = 1)					
				}
			}
			if(biplots){
				fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,axes=FALSE,contributionCircles=TRUE,contributions=res$cj,pch=fj.pch,dev.new=FALSE)
			}else{
				fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,axes=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,pch=fj.pch,contributionCircles=TRUE,contributions=res$cj,dev.new=TRUE)	
			}	
			if(contributionPlots){
				contributionBars(res$fi,res$ci,x_axis=x_axis,y_axis=y_axis,main=main,col=fi.plot.info$col)
				contributionBars(res$fj,res$cj,x_axis=x_axis,y_axis=y_axis,main=main,col=fj.plot.info$col)			
			}		
			if(correlationPlotter && class(res)[1]%in%pca.types){
				correlationPlotter(res$X,res$fi,col=fj.col,pch=fj.pch,x_axis=1,y_axis=2,xlab=xlab,ylab=ylab,main=main) 
			}									
		}
	}
	tepPlotInfo <- list(fii.col=fii.col, fii.pch=fii.pch,fi.col=fi.col, fi.pch=fi.pch,fj.col=fj.col,fj.pch=fj.pch,constraints=constraints)
	class(tepPlotInfo) <- c("tepGraphs", "list")
	return(tepPlotInfo)	
}
