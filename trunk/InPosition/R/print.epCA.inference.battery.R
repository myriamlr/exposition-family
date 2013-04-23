print.epCA.inference.battery <-
function (x,...) {

  res.epCA.inference.battery <- x
  if (!inherits(res.epCA.inference.battery, "epCA.inference.battery")) stop ("no convenient data")
  cat("**Results for Correspondence Analysis Inference Battery**\n")
  cat ("Permutation was performed on ", ncol(res.epCA.inference.battery$components$eigs.perm),
       "components and bootstrap performed on ", nrow(res.epCA.inference.battery$fj.boots$boot.ratios), " variables\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(3, 2), list(1:3, c("name", "description")))
  
  res[1,] <- c("$components","p-values ($p.vals) and permutations ($eigs.perm) for each component.")
  res[2,] <- c("$fj.boots","significant ($sig.boot.ratio), bootstrap ratios ($boot.ratios), and critical values ($critical.value) for boostrap tests on measures ($fj).")
  res[3,] <- c("$omni","Omnibus test of inertia (only works when MCA is corrected): p-value ($p.val) and permuted inertia ($inertia.perm).")     
  
  print(res)
}
