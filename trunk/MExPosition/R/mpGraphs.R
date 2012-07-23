mpGraphs <- function(res, mpPlotInfo=NULL, x_axis=1, y_axis =2, xlab = NULL, ylab = NULL, main = NULL) 
{ 
	if(is.null(main))
	{	main <- deparse(substitute(res))
		if(length(unlist(strsplit(main,"")))>40)
		{	main <- "Results"	
		}
	}
	
	if(!(class(res)[1] %in% c('mpSTATIS','mpDISTATIS','mpMFA')))
	{	stop('Unknown MExPosition class. Plotting has stopped.')
		
	} 
		
	if(is.null(xlab))
	{	xlab.innerproduct <- paste("Component ",x_axis, " variance:", round(res$InnerProduct$t[x_axis],3),"%",sep="")
		xlab.table <- paste("Component ",x_axis, " variance:", round(res$Table$t[x_axis],3),"%",sep="")
	}
	if(is.null(ylab))
	{	ylab.innerproduct <- paste("Component ",y_axis, " variance:", round(res$InnerProduct$t[y_axis],3),"%",sep="")
		ylab.table <- paste("Component ",y_axis, " variance:", round(res$Table$t[y_axis],3),"%",sep="")
	}
	
	if(!class(res)[1] %in% c('mpSTATIS','mpDISTATIS','mpMFA'))
	{	stop("Unknown MExPosition class. Plotting has stopped.")
	}
	else 
	{	if(is.null(mpPlotInfo))
		{	mpPlotInfo <- list(fi.col = NULL, fj.col = NULL, table.col = NULL)
		}	
	}
	
	fi.col <- mpPlotInfo$fi.col
	fj.col <- mpPlotInfo$fj.col
	table.col <- mpPlotInfo$table.col
	if((class(res)[1] %in% c('mpSTATIS','mpMFA')))
	{	innerproduct.fi.plot.info <- prettyPlot(res$InnerProduct$fi, x_axis=x_axis, y_axis=y_axis, col=table.col, xlab=xlab.innerproduct, ylab=ylab.innerproduct, 
												main = paste("Inner Product",main), contributionCircles=TRUE, contributions = res$InnerProduct$ci)
		
		compromise.fi.plot.info <- prettyPlot(res$Table$fi, x_axis=x_axis, y_axis=y_axis, col=fi.col, xlab=xlab.table, ylab=ylab.table, 
												main = paste("Compromise",main), contributionCircles=TRUE, contributions = res$Table$ci)
		
		compromise.partial.plot.info <- prettyPlot(res$Table$partial.fi, x_axis=x_axis, y_axis=y_axis, col=rep(fi.col,res$Overview$num.groups), 
												xlab=xlab.table, ylab=ylab.table, main = paste("Partial Scores",main))	

		compromise.fi.plot.info <- prettyPlot(res$Table$fi, x_axis=x_axis, y_axis=y_axis, col=fi.col,contributionCircles=TRUE, contributions = res$Table$ci, new_window=FALSE)												
																	
		loading.plot.info <- prettyPlot(res$Table$Q, x_axis=x_axis, y_axis=y_axis, col=fj.col, xlab=xlab.table, ylab=ylab.table, 
												main = paste("Loadings",main),contributions = res$Table$cj, contributionCircles=TRUE)
		
		
	}
	if(class(res)[1] %in% c('mpDISTATIS'))
	{	innerproduct.fi.plot.info <- prettyPlot(res$InnerProduct$fi, x_axis=x_axis, y_axis=y_axis, col=table.col, xlab=xlab.innerproduct, ylab=ylab.innerproduct, 
												main = paste("Inner Product",main), contributionCircles=TRUE, contributions = res$InnerProduct$ci)
		
		compromise.fi.plot.info <- prettyPlot(res$Table$fi, x_axis=x_axis, y_axis=y_axis, col=fi.col, xlab=xlab.table, ylab=ylab.table, 
												main = paste("Compromise",main), contributionCircles=TRUE, contributions = res$Table$ci)
																		
		compromise.partial.plot.info <- prettyPlot(res$Table$partial.fi, x_axis=x_axis, y_axis=y_axis, col=rep(fi.col,res$Overview$num.groups), 
												xlab=xlab.table, ylab=ylab.table, main = paste("Partial Scores",main), new_window=FALSE)
												
		#compromise.fi.plot.info <-prettyPlot(book.sort$mexPosition.Data$Compromise$fi, contributionCircles=TRUE, contributions = book.sort$mexPosition.Data$Compromise$ci)																										
	}
		
	mpPlotInfo <- list(fi.col = fi.col, fj.col = fj.col,table.col=table.col)
	class(mpPlotInfo) <- c("mpGraphs","list")
	return(mpPlotInfo)
}
