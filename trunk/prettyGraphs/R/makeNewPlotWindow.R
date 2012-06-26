makeNewPlotWindow <-
function(axis_position_list,min_max_list,xlab="",ylab="",main="",axis_line_width=3){
	#This should make a blank plot window with custom placed axes and labels.
	dev.new()
	#not sure yet...
	plot(c(0,0),c(0,0),type="n",col="white",axes=FALSE,xlab=xlab,ylab=ylab,ylim=c(min_max_list$miny,min_max_list$maxy),xlim=c(min_max_list$minx,min_max_list$maxx),main=main)
	points(axis_position_list$xx,axis_position_list$xy,type="l",col="black",lwd=axis_line_width)
	points(axis_position_list$yx,axis_position_list$yy,type="l",col="black",lwd=axis_line_width)	
}
