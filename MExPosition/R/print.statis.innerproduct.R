print.statis.innerproduct <- function(x,...){
	
	res.statis.innerproduct <- x
	if (!inherits(res.statis.innerproduct, "statis.innerproduct")) stop ("no convenient data")
  cat("**Results from the analysis of the between table structure via STATIS **\n")
  cat("*The results are available for the following objects:\n\n")
  res <- array("", c(8, 2), list(1:8, c("Name", "Description")))
	
	res[1,] <- c("$S", "i X i X j array of S Matrices")
	res[2,] <- c("$C","k X k, C Matrix")
	res[3,] <- c("$RVMatrix","RV Matrix")
	res[4,] <- c("$eigs.vector","Eigen Vectors of C")
	res[5,] <- c("$eigs","Eigen Values of C")
	res[6,] <- c("$fi","Factor Scores of C")
	res[7,] <- c("$ci","Contributions of the Rows of C")
	res[8,] <- c("$t","% Variance Explained of C (tau)")

	print(res)
}