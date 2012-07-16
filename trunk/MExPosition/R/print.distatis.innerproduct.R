print.distatis.innerproduct <- function(x,...){
	
	res.distatis.innerproduct <- x
	if(!inherits(res.distatis.innerproduct,"distatis.innerproduct"))stop('no convenient data')
	cat("**Results for the Inner Product via DISTATIS**\n\n")
	cat("The results are available for the following objects:\n\n")
	res <- array("",c(6,2),list(1:6,c("Name","Description")))
	
	res[1,] <- c("$S","i X i X j array of S Matrices")
  	res[2,] <- c("$C","k X K, C Matrix")
  	res[3,] <- c("$eigs.vector","Eigen Vectors of C")
  	res[4,] <- c("$eigs", "Eigen Values of C")
  	res[5,] <- c("$t","Percent variance explained (tau)")
  	res[6,] <- c("$fi", "Factor Scores of C")

	print(res)
}