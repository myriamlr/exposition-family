createColorVectorsByDesign <-
function(design_matrix,hsv=TRUE){
#	cpsu <- colorPoints(design_matrix)	
#	group_colors <- matrix(cpsu$cps[,1],nrow=length(cpsu$cps[,1]),ncol=1,byrow=TRUE)
	if(hsv){
		group_colors <- prettyGraphsHSVColorSelection(n.colors=ncol(design_matrix))
	}else{
		group_colors <- prettyGraphsColorSelection(n.colors=ncol(design_matrix))	
	}
	rownames(group_colors)<-colnames(design_matrix)

	observation_colors <- as.matrix(group_colors[which(design_matrix==1,arr.ind=TRUE)[,2],1])
	rownames(observation_colors) <- rownames(design_matrix)
	return(list(oc=observation_colors,gc=group_colors))
}
