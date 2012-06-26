correlationPlotter <-
function(data_matrix,factor_scores,x_axis=1,y_axis=2,col=NULL,xlab=NULL,ylab=NULL,main=""){
	if(nrow(data_matrix)==nrow(factor_scores)){
		loadings <- cor(data_matrix,factor_scores)
	}
	else if(ncol(data_matrix)==nrow(factor_scores)){
		loadings <- cor(t(data_matrix),factor_scores)
	}else{
		print("Dimension mismatch. Please check data_matrix and factor_scores.")
		return(NULL)	
	}
	loadings <- replace(loadings,is.na(loadings),0)

#	if(is.null(taus)){
		#plotCircle(xl=paste("Component ",x_axis,sep=""),yl=paste("Component ",y_axis,sep=""),m=main)
		if(!is.null(xlab) && !is.null(ylab)){
			plotCircle(xlab=xlab,ylab=ylab,main=main)	
		}else{
			plotCircle(xlab=paste("Component ",x_axis,sep=""),ylab=paste("Component ",y_axis,sep=""),main=main)
		}
#	}else{
#		plotCircle(xl=paste("Component ",x_axis," explained variance: ",round(taus[x_axis]),"%",sep=""),yl=paste("Component ",y_axis," explained variance: ",round(taus[y_axis]),"%",sep=""),m=main)
#	}
	
	if(is.null(col)){
		col <- prettyGraphsColors()[colorVectorIsNull(loadings)$oc]
	}

#I can bring these back later.	
#	if(arrows){
		for(i in 1:dim(loadings)[1]){
			points(c(0,loadings[i,x_axis]),c(0,loadings[i,y_axis]),col="black",type="l")
		}
#		prettyGraphs(loadings,col=col,display_names=TRUE,display_points=FALSE,x_axis=1,y_axis=2,new_window=FALSE)
#	}else{
		prettyPlot(loadings,col=col,display_names=TRUE,display_points=TRUE,x_axis=1,y_axis=2,new_window=FALSE)
#	}
	#prettyGraphs(loadings,col=col,display_names=FALSE,x_axis=x_axis,y_axis=y_axis,new_window=FALSE)
}
