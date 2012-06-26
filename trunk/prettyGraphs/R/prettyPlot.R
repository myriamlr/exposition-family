prettyPlot <-
function(data_matrix,x_axis=1,y_axis=2,col=NULL,xlab="",ylab="",main="",display_names=TRUE,display_points=TRUE,new_window=TRUE,constraints=NULL,axis_line_width=3,pos=3,pch=NULL,cex=1,textSizes=0.8,contributionCircles=FALSE,contributions=NULL,flip=FALSE){
	
	#I need a different type of checker here...
	#Also, I need to use apply instead of my silly matrix nonsense below. See how FactoMineR does it.
	if(is.null(col)){
		col <- as.matrix(prettyGraphsColors()[colorVectorIsNull(data_matrix)$oc])
	}
	if(	(display_names==FALSE && display_points==FALSE) ){
		#For your health!
		print("Sorry, but you cannot have display_points and display_names set to FALSE. Nothing would have been plotted!")
		display_points<-TRUE
	}
	
	#always do?
	min_max_list <- minmaxHelper(data_matrix,data_matrix,x_axis,y_axis)	
	if(!is.null(constraints)){
	#if it is not null
		if(("minx" %in% names(constraints)) && ("maxx" %in% names(constraints)) && ("miny" %in% names(constraints)) && ("maxy" %in% names(constraints))){
			#and if it meets criteria, use it. This way, if FALSE, it should also go here.
			min_max_list <- list(minx=constraints$minx,miny=constraints$miny,maxx=constraints$maxx,maxy=constraints$maxy)
		}
	}
	
	#determine axis points
	axis_list <- determineAxesPosition(min_max_list)
	
	#make new window
	if(new_window){
		makeNewPlotWindow(axis_list,min_max_list,xlab,ylab,main,axis_line_width)
	}
	
	#am I displaying names?
	if(display_names){
		plotText(data_matrix,col,x_axis,y_axis,pos=pos,textSizes=textSizes,contributionCircles=contributionCircles,contributions=contributions)
	}
	
	#am I displaying points?
	if(display_points){
		if(is.null(pch)){
			plotPoints(data_matrix,col,x_axis,y_axis,cex=cex,contributionCircles=contributionCircles,contributions=contributions,flip=flip)
		}else{
			plotPoints(data_matrix,col,x_axis,y_axis,cex=cex,pch=pch,contributionCircles=contributionCircles,contributions=contributions,flip=flip)
		}
	}
	return(list(col=col,constraints=constraints))
}
