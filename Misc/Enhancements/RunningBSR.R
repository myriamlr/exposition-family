library(InPosition)
data(words)
pca.words.res <- epPCA.inference.battery(words$data,scale=FALSE,graphs=FALSE)


n <- 0
mean.run <- matrix(0,nrow(pca.words.res$Inference.Data$fj.boots$boots),ncol(pca.words.res$Inference.Data$fj.boots$boots))
M2 <- mean.run #not really, but just initialize to zero because I'm lazy.
x <- pca.words.res$Inference.Data$fj.boots$boots
for(i in 1:100){
	n <- n + 1 ##unnecessary because I know n; sort of.; also, isn't n just x?
	delta <- x[,,i] - mean.run
	mean.run <- mean.run + (delta/n)
	M2 <- M2 + delta*(x[,,i]-mean.run)
}
variance <- M2/(n-1)
###online BSR is 
mean.run/sqrt(variance) #this is faster than ^

boot.cube <- x
boot.cube.mean <- apply(boot.cube, c(1, 2), mean)
    boot.cube.mean_repeat <- array(boot.cube.mean, dim = c(dim(boot.cube)))
    boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
    s.boot <- (apply(boot.cube.dev, c(1, 2), mean))^(1/2)



for(x in 1:100){
	n <- n + 1 ##unnecessary because I know n; sort of.; also, isn't n just x?
	delta <- x - mean.run
	mean.run <- mean.run + (delta/n)
	M2 <- M2 + delta*(x-mean.run)
}

variance <- M2/(n)



