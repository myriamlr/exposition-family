tepGraphs <-
function(res,tepPlotInfo=NULL,x_axis=1,y_axis=2,xlab=NULL,ylab=NULL,main=NULL,contributionPlots=TRUE,correlationPlotter=TRUE,showHulls=1,biplots=FALSE){

	if(is.null(main)){
		main <- deparse(substitute(res))
		if(length(unlist(strsplit(main,"")))>40){
			main <- "Results"
		}
	}
	
	
	pca.types <- c('tepBADA')
	ca.types <- c('tepDICA')
	#A simple override/check. If someone puts in texpoOutput class data, tepGraphs will recognize it.
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
		if(is.null(xlab)){
			xlab <- paste("Component ",x_axis," variance: ", round(res$t[x_axis],3), "%",sep="")
		}
		if(is.null(ylab)){
			ylab <- paste("Component ",y_axis," variance: ", round(res$t[y_axis],3), "%",sep="")
		}
		if(is.null(tepPlotInfo)){
			tepPlotInfo <- list(fii.col=NULL,fi.col=NULL,fj.col=NULL,constraints=NULL)
		}else{
			if(!(nrow(res$fi)==nrow(tepPlotInfo$fi.col)) || !(nrow(res$fj)==nrow(tepPlotInfo$fj.col)) || !(nrow(res$fii)==nrow(tepPlotInfo$fii.col))){
				print('Dimension mismatch. tepPlotInfo will be reset, no hulls can be shown.')
				tepPlotInfo$fii.col <- NULL
				tepPlotInfo$fi.col <- NULL
				tepPlotInfo$fj.col <- NULL
				tepPlotInfo$constraints <- NULL
			}
		}
		#I have to do color corrections for NULL on fii and fi.
		fii.col <- tepPlotInfo$fii.col		
		fi.col <- tepPlotInfo$fi.col
		fj.col <- tepPlotInfo$fj.col
		constraints <- calculateConstraints(results=res,x_axis=x_axis,y_axis=y_axis,constraints=tepPlotInfo$constraints)

		if(is.null(fii.col) || is.null(fi.col)){
			obs.cols <- createColorVectorsByDesign(matrix(1,nrow(res$fii),1))
			fii.col <- as.matrix(prettyGraphsColors()[obs.cols$oc])
			fi.col <- as.matrix(fii.col[1:nrow(res$fi),])
			showHulls <- -1			
		}
#no, not quite!
		fii.plot.info <- prettyPlot(res$fii,x_axis=x_axis,y_axis=y_axis,col=fii.col,new_window=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,contributionCircles=FALSE)
		fi.plot.info <- prettyPlot(res$fi,x_axis=x_axis,y_axis=y_axis,col=fi.col,new_window=FALSE,contributionCircles=TRUE,contributions=res$ci)
		if(showHulls > 0 && showHulls <= 1){
			for(i in 1:nrow(res$fi)){
				peeledHull(res$fii[which(fii.col[,1]==fi.col[i,1]),],x_axis=x_axis,y_axis=y_axis,percentage=showHulls,col="black",lwd=3)
				peeledHull(res$fii[which(fii.col[,1]==fi.col[i,1]),],x_axis=x_axis,y_axis=y_axis,percentage=showHulls,col=fi.col[i,],lwd=1)
			}
		}
		if(biplots){
			fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,new_window=FALSE,contributionCircles=TRUE,contributions=res$cj)
		}else{
			fj.plot.info <- prettyPlot(res$fj,x_axis=x_axis,y_axis=y_axis,col=fj.col,new_window=TRUE,xlab=xlab,ylab=ylab,main=main,constraints=constraints,contributionCircles=TRUE,contributions=res$cj)	
		}	
		if(contributionPlots){
			contributionBars(res$fi,res$ci,x_axis=x_axis,y_axis=y_axis,main=main,col=fi.plot.info$col)
			contributionBars(res$fj,res$cj,x_axis=x_axis,y_axis=y_axis,main=main,col=fj.plot.info$col)			
		}		
		if(correlationPlotter && class(res)[1]%in%pca.types){
			correlationPlotter(res$X,res$fi,col=fj.plot.info$col,x_axis=1,y_axis=2,xlab=xlab,ylab=ylab,main=main) 
		}									
	}
	
	tepPlotInfo <- list(fii.col=fii.plot.info$col,fi.col=fi.plot.info$col,fj.col=fj.plot.info$col,constraints=constraints)
	class(tepPlotInfo) <- c("tepGraphs", "list")
	return(tepPlotInfo)	
}
