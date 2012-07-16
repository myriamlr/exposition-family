mpGraphHandler <-
function(res, data, DESIGN, table, main)
{  
	obs.cols <- createColorVectorsByDesign(DESIGN)
	measure.cols <- createColorVectorsByDesign(t(table))
	fi.col <- as.matrix(prettyGraphsColors()[obs.cols$oc])	
	fj.col <- as.matrix(prettyGraphsColors()[measure.cols$oc])
	table.col <- as.matrix(prettyGraphsColors()[measure.cols$gc])
	mpPlotInfo <- list(table.col = table.col, fi.col = fi.col, fj.col = fj.col)
	mpPlotInfo <- mpGraphs(res, mpPlotInfo = mpPlotInfo, main = main)
	return(mpPlotInfo)
}