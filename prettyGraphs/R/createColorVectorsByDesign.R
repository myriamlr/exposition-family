createColorVectorsByDesign <-
function(design_matrix){
	cpsu <- colorPoints(design_matrix)	
	group_colors <- matrix(cpsu$cps[,1],nrow=length(cpsu$cps[,1]),ncol=1,byrow=TRUE)
	rownames(group_colors)<-colnames(design_matrix)
	group_colors <- as.numeric(group_colors)
	observation_colors <- design_matrix %*% group_colors
	pass_group_colors <- rowSums(t(design_matrix * repmat(observation_colors,1,dim(design_matrix)[2]))) / colSums(design_matrix)

	return(list(oc=observation_colors,gc=pass_group_colors))
}
