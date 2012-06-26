contributionBars <-
function(factor_scores,contributions,x_axis=1,y_axis=2,col=NULL,main="",upper='blue',lower='red',important=NULL,threshold=0,sortContributions=TRUE){

	factor_scores <- as.matrix(factor_scores)
	contributions <- as.matrix(contributions)
	if(sum(rownames(factor_scores)==rownames(contributions)) != nrow(factor_scores)){
		rownames(contributions) <- rownames(factor_scores)
	}

	if(!is.null(important)){
		#use this. i.e., from bootstrap
		#this will make a return later as a Bootstrap Ratio plot.
	}else{
		if(threshold == 0){
			threshold <- 1/dim(contributions)[1]
		} else if(threshold >= 1){
			threshold <- 1/dim(contributions)[1]
		}
	}
	if(is.null(col)){
		if(sortContributions){
			col <- c('blue','red','gray')
		}else{
			col <- c('gray')
		}
	}

	#multiply the sign of the data by the contributions
	contributions_with_signs <- sign(factor_scores) * contributions
	dev.new()
	par(mfrow=c(1,2))
	flipFlag<-TRUE
	axes <- c(x_axis,y_axis)
	
	for(i in 1:length(axes)){
	
		if(sortContributions){
			ordered_inds <- order(contributions_with_signs[,axes[i]],decreasing=FALSE)
		}else{
			ordered_inds <- 1:dim(contributions_with_signs)[1]
		}

		ordered <- contributions_with_signs[ordered_inds,axes[i]]
		
		if(is.null(col)){
			pos_above <- ordered >= threshold
			neg_above <- ordered <= (-threshold)			
			pos_above[pos_above == TRUE] <- col[1]
			neg_above[neg_above == TRUE] <- col[2]			
			pos_above[pos_above == FALSE] <- col[3]		
			neg_above[neg_above == FALSE] <- col[3]
			ordered_colors <- c(neg_above,pos_above)							
		}else{	
			ordered_colors <- col[ordered_inds]
		}

		#draw a line across the thresholds.
		if(!flipFlag){
			#horizontal cut lines
			barplot(ordered,col=ordered_colors,ylim=c(-1.1,1.1),axes=TRUE,horiz=flipFlag,sub=paste("Component ",axes[i],sep=""))				
			abline(h=0,col="black")				
			abline(h=threshold,col=upper,lty=2)
			abline(h=-threshold,col=lower,lty=2)
		}else{
			barplot(ordered,col=ordered_colors,xlim=c(-1.1,1.1),axes=TRUE,horiz=flipFlag,sub=paste("Component ",axes[i],sep=""))							
			#vertical cut lines
			abline(v=0,col="black")								
			abline(v=threshold,col=upper,lty=2)
			abline(v=-threshold,col=lower,lty=2)			
		}
		flipFlag<-!flipFlag
		
		#make a pie chart of contributions
		#later!
	}

	if(main==""){		
		mtext("Contributions to variance",side=3,outer=TRUE,line=-3)
	}else{
		mtext(main,side=3,outer=TRUE,line=-3)
	}

}
