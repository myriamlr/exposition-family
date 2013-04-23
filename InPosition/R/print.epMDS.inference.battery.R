print.epMDS.inference.battery <-
function (x,...) {

  res.epMDS.inference.battery <- x
  if (!inherits(res.epMDS.inference.battery, "epMDS.inference.battery")) stop ("no convenient data")
  cat("**Results for Multidimensional Scaling Inference Battery**\n")
  cat ("The analysis was performed on a square matrix of", "nope!",
       "items\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(1, 2), list(1:1, c("name", "description")))
  
  res[1,] <- c("NOPE","MDS tests are not functional at this time.")
  
  print(res)

}
