##A
rm(list=ls())

one.f.wide.format <- read.csv('S(A).csv')


##B
one.f.long.format <- stack(one.f.wide.format) ##there is an alternative approach with reshape. 

colnames(one.f.long.format) <- c('y','A')
one.f.long.format$S <- paste("Subj",1:nrow(one.f.long.format),sep=".")



##C

	###what does y, ~, and A mean?
aov.res <- aov(y~A,data= one.f.long.format)
	##summary is a ubiqitous function.
aov.table <- summary(aov.res)


##What about contrasts?
scores <- one.f.long.format$y

contrast.1 <- c(rep(-3,5),rep(5,5),rep(-1,5),rep(-1,5))
lin.model.1 <- lm(scores ~ contrast.1)
summary(lin.model.1)

contrast.2 <- c(rep(-1,5),rep(0,5),rep(1,5),rep(0,5))
lin.model.2 <- lm(scores ~ contrast.2)
summary(lin.model.2)






##psi 2 pg x
##psi 3 pg x
grp.means <- colMeans(one.f.wide.format)
contraz <- c(-1,0,1,0)

sum.grp.contraz <- sum(grp.means * contraz)
sum.contraz2 <- sum(contraz^2)

SS.constrast <- (5 * (sum.grp.contraz^2))/sum.contraz2
SSe <- 88.55
MSe <- 2.35




scp.2 <- sum((scores - mean(scores)) * contrast.2)^2
ss.scores <- sum((scores - mean(scores))^2)
ss.contrast.2 <- sum(contrast.2^2)


r2 <- scp.2/(ss.scores*ss.contrast.2)