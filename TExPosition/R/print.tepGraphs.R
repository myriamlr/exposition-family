print.tepGraphs <-
function (x,...) {

#list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=taus,M=M,W=W,pdq=pdqFIN)

  res.tepGraphs <- x
  if (!inherits(res.tepGraphs, "tepGraphs")) stop ("no convenient data")
  cat("**TExPosition plotting data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(4, 2), list(1:4, c("name", "description")))
  
  res[1,] <- c("$fii.col","The colors for the individuals.")
  res[2,] <- c("$fi.col","The colors for the groups.")
  res[3,] <- c("$fj.col","The colors for the column items.")
  res[4,] <- c("$constraints","Plotting constraints for axes.")  
  
  print(res)

}
