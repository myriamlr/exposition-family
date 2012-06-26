plotCircle <-
function(xlab="",ylab="",main="",axis_line_width=3){

	j = seq(pi/128,2*pi,by=pi/128)
	coords = cbind(cos(j),sin(j))
	coords = rbind(coords,coords[1,])
	#an override because I could not trace the real problem...
	min_max_list <- list(minx=-1.1,miny=-1.1,maxx=1.1,maxy=1.1)
	axis_list <- determineAxesPosition(min_max_list)
	makeNewPlotWindow(axis_list,min_max_list,xlab,ylab,main,axis_line_width)
	points(coords,col="black",type='l')
	points(0,0,col="black",pch=20)
}
