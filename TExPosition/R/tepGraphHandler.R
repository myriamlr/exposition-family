tepGraphHandler <-
function(res,DATA,DESIGN,main){
	obs.cols <- createColorVectorsByDesign(DESIGN)
	measure.cols <- createColorVectorsByDesign(matrix(1,ncol(DATA),1))
	
	fii.col <- as.matrix(prettyGraphsColors()[obs.cols$oc])
	fi.col <- as.matrix(prettyGraphsColors()[obs.cols$gc])
	fj.col <- as.matrix(prettyGraphsColors()[measure.cols$oc])
	
	tepPlotInfo <- list(fii.col=fii.col,fi.col=fi.col,fj.col=fj.col,constraints=NULL)
	tepPlotInfo <- tepGraphs(res,main=main,tepPlotInfo=tepPlotInfo)	
	return(tepPlotInfo)
}
