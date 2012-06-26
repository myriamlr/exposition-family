colorPoints <-
function(classes){


# Initialization in a stupid way for now, but it works.
	possibles = createOriginalPointsAndColors()
	possibleColors = possibles$colors
	possiblePoints = possibles$points
	
#This requires that classes are input either as a vector, or a matrix, where
#the observations are column wise, and unique elements can be retrieved by the
#rows, and then counted!
	uniqueClasses = unique(as.matrix(classes))
	numberOfClasses = dim(uniqueClasses)[1]

	colorPointStruct <- matrix(list(),numberOfClasses,2)
		
	thisPoint = sample(length(possiblePoints),1,FALSE)
	possiblePoints = possiblePoints[-thisPoint]
	thisColor = sample(length(possibleColors),1,FALSE)
	possibleColors = possibleColors[-thisColor]
	
	for(i in 1:numberOfClasses){
		if(length(possibleColors) >= 1){
#			print('In the if!')
#			thisColor = sample(length(possibleColors),1,FALSE)
#			possibleColors = possibleColors[-thisColor]
#			thisColor = thisColor + 30
			thisColor = (thisColor + 30) %% length(possibleColors)
			possibleColors = possibleColors[-thisColor]
		}
		else if(length(possiblePoints) >= 1){
#			print('In the else-if!')
			thisPoint = sample(length(possiblePoints),1,FALSE)
			possiblePoints = possiblePoints[-thisPoint]
			possibleColors = prettyGraphsColors()
#			thisColor = sample(length(possibleColors),1,FALSE)
#			possibleColors = possibleColors[-thisColor]			
			thisColor = (thisColor + 30) %% length(possibleColors)
			possibleColors = possibleColors[-thisColor]
		}
		else{
#			print('In the else!')		
			possibles = createOriginalPointsAndColors()
			possibleColors = possibles$colors
			possiblePoints = possibles$points
			thisPoint = sample(length(possiblePoints),1,FALSE)
			possiblePoints = possiblePoints[-thisPoint]
#			thisColor = sample(length(possibleColors),1,FALSE)
#			possibleColors = possibleColors[-thisColor]			
			thisColor = (thisColor + 30) %% length(possibleColors)
			possibleColors = possibleColors[-thisColor]
		}
		colorPointStruct[i,] <- list(clr=thisColor,pt=thisPoint)
	}

	if(sum(unlist(colorPointStruct[,1]==0)) != 0){
		zero.colors <- unlist(colorPointStruct[,1])
		if(length(zero.colors)==1){
			colorPointStruct[1,1] <- sample(length(prettyGraphsColors()),1,FALSE)
		}else{
			range <- 1:max(zero.colors)
			excludes <- zero.colors[which(zero.colors>0)]
			theseColors <- sample(range[-c(excludes)],sum(zero.colors==0),FALSE)
			zero.colors[which(zero.colors==0)] <- theseColors
			colorPointStruct[,1] <- zero.colors
		}
	}
	return(list(cps=colorPointStruct,uc=uniqueClasses))
}
