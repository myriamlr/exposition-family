plotText <-
function(data_matrix,col,x_axis=1,y_axis=2,pos=3,textSizes=0.8,contributionCircles=FALSE,contributions=NULL){
	thesecontributions <- 0
	if(contributionCircles){
		if(!is.null(contributions)){
			thesecontributions <- rowSums(contributions[,c(x_axis,y_axis)])
			thesecontributions <- (thesecontributions / ((max(thesecontributions) - min(thesecontributions)) / (1.2 - 0))) + 0
			thesecontributions[thesecontributions > 1.2] <- 1.2
		}
	}
	if(pos < 1 || pos > 4){
		text(data_matrix[,x_axis],data_matrix[,y_axis],col=col,labels=rownames(data_matrix),cex=textSizes+thesecontributions,pos=3)
	}else{
		text(data_matrix[,x_axis],data_matrix[,y_axis],col=col,labels=rownames(data_matrix),cex=textSizes+thesecontributions,pos=pos)
	}
}
