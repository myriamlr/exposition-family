colorVectorIsNull <-
function(data_matrix){
	#I need a different type of checker here...
	#Also, I need to use apply instead of my silly matrix nonsense below. See how FactoMineR does it.
	#design_matrix <- diag(dim(data_matrix)[1])
	design_matrix <- matrix(1,nrow(data_matrix),1)
	#colnames(design_matrix) <- rownames(data_matrix)
	rownames(design_matrix) <- rownames(data_matrix)
	color_vector <- createColorVectorsByDesign(design_matrix)
	return(color_vector)
}
