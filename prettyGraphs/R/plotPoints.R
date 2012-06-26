plotPoints <-
function(data_matrix,col,x_axis=1,y_axis=2,cex=1,pch=NULL,contributionCircles=FALSE,contributions=NULL,flip=FALSE){
	if(contributionCircles){
		if(is.null(contributions)){
			print("No contributions provided. Defaulting to standard size")
			if(flip){
				points(data_matrix[,x_axis],data_matrix[,y_axis],col=col,pch=21,bg="black",cex=cex)			
			}else{
				points(data_matrix[,x_axis],data_matrix[,y_axis],col="black",pch=21,bg=col,cex=cex)
			}
		}else{
			thesecontributions <- rowSums(contributions[,c(x_axis,y_axis)])
			thesecontributions <- (thesecontributions / ((max(thesecontributions) - min(thesecontributions)) / (2 - 0))) + 0
			thesecontributions[thesecontributions > 2] <- 2
			if(is.null(pch)){
				if(flip){
					points(data_matrix[,x_axis],data_matrix[,y_axis],col=col,pch=21,bg="black",cex=(cex+thesecontributions))
				}else{
					points(data_matrix[,x_axis],data_matrix[,y_axis],col="black",pch=21,bg=col,cex=(cex+thesecontributions))
				}
			}else{
				if(flip){
					points(data_matrix[,x_axis],data_matrix[,y_axis],pch=pch,col=col,bg="black",cex=(cex+thesecontributions))
				}else{
					points(data_matrix[,x_axis],data_matrix[,y_axis],pch=pch,col="black",bg=col,cex=(cex+thesecontributions))
				}
			}			
		}
	}else{
		if(is.null(pch)){
			if(flip){
				points(data_matrix[,x_axis],data_matrix[,y_axis],col=col,pch=21,bg="black",cex=cex)			
			}else{
				points(data_matrix[,x_axis],data_matrix[,y_axis],col="black",pch=21,bg=col,cex=cex)
			}
		}else{
			if(flip){
				points(data_matrix[,x_axis],data_matrix[,y_axis],pch=pch,col=col,bg="black",cex=cex)
			}else{
				points(data_matrix[,x_axis],data_matrix[,y_axis],pch=pch,col="black",bg=col,cex=cex)
			}		
		}
	}
}
