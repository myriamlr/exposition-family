print.distatis.compromise <- function(x,...){
	
	res.distatis.compromise <- x
	if(!inherits(res.distatis.compromise,"distatis.compromise"))stop('no convenient data')
	cat('**Results for the Compromise via DISTATIS**\n\n')
	cat('The results are available for the following objects: \n\n')
	res <- array("",c(6,2),list(1:6,c("Name","Description")))
	
	res[1,] <- c("$Compromise", "S+, Compromise Matrix")
  	res[2,] <- c("$eigs.vector", "Eigen Vector of S+")
 	res[3,] <- c("$eigs","Eigen Value of S+")
  	res[4,] <- c("$t","Percent variance explained (tau)")
  	res[5,] <- c("$fi", "Factor Scores of S+")
  	res[6,] <- c("$ci","Contributions of the Rows of S+")

	
	print(res)
}