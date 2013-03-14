prettyPlot <-
function(data_matrix,x_axis=1,y_axis=2,col=NULL,xlab="",ylab="",main="",display_names=TRUE,display_points=TRUE,plot_axes=TRUE,constraints=NULL,axis_line_width=3,pos=3,pch=21,cex=1,textSizes=0.8,contributionCircles=FALSE,contributions=NULL,flip=FALSE,asp=1,findBounds=TRUE,dev.new=TRUE,clean_plot=TRUE){
	
	#I want to always send back colors and constraints.
	#I need a different type of checker here...
	#Also, I need to use apply instead of my silly matrix nonsense below. See how FactoMineR does it.
	if(is.null(col)){
		col <- colorVectorIsNull(data_matrix)$oc
	}
	#I only need constraints if I am making a new window.
	check.constraints <- minmaxHelper(data_matrix,data_matrix,x_axis,y_axis,findBounds=findBounds)	
	if(!is.null(constraints)){
	#if it is not null
		if(("minx" %in% names(constraints)) && ("maxx" %in% names(constraints)) && ("miny" %in% names(constraints)) && ("maxy" %in% names(constraints))){
			#and if it meets criteria, use it. This way, if FALSE, it should also go here.
			check.constraints <- list(minx=constraints$minx,miny=constraints$miny,maxx=constraints$maxx,maxy=constraints$maxy)
		}
	}
	constraints <- check.constraints	
	
	
	if(	(display_names==FALSE && display_points==FALSE) ){
		#For your health!
		print("Sorry, but you cannot have display_points and display_names set to FALSE. Nothing would have been plotted!")
		display_points<-TRUE
	}
	
	#this assumes you already have a device ready to go.
	if(dev.new){
		dev.new()	
	}
	
	if(clean_plot){
		plot(c(0,0),c(0,0),type="n",col="white",axes=FALSE,xlab=xlab,ylab=ylab,ylim=c(constraints$miny,constraints$maxy),xlim=c(constraints$minx,constraints$maxx),main=main,asp=asp)
	}
			
	#make a new plot on a device.
	if(plot_axes){		
		#determine axis points
		axis_list <- determineAxesPosition(constraints)		
		#plot the axes
		makeNewPlotWindow(axis_list,axis_line_width)
	}	
	#am I displaying points?
	if(display_points){
		plotPoints(data_matrix,col,x_axis,y_axis,cex=cex,pch=pch,contributionCircles=contributionCircles,contributions=contributions,flip=flip)
	}
	#am I displaying names?
	if(display_names){
		plotText(data_matrix,col,x_axis,y_axis,pos=pos,textSizes=textSizes,contributionCircles=contributionCircles,contributions=contributions)
	}		
	
	return(list(col=col,constraints=constraints))
}
