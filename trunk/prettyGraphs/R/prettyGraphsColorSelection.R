#http://stackoverflow.com/questions/3789968/generate-a-list-of-primes-in-r-up-to-a-certain-number
prettyGraphsColorSelection <- function(n.colors=1,prime.offset=23,starting.color=184){
	##stolen
	primest <- function(n){
	    p <- 2:n
	    i <- 1
	    while (p[i] <= sqrt(n)) {
	        p <-  p[p %% p[i] != 0 | p==p[i]]
	        i <- i+1
	    }
	    return(p[length(p)])
	}	
	##stolen
	getPrime <- function(prime.offset){
		div <- 2:floor(sqrt(prime.offset))
		if(!any(prime.offset %% div == 0)) {return(prime.offset)}
		else{primest(prime.offset)}				
	}	
	prime.offset<-getPrime(prime.offset)
	the.colors <- prettyGraphsColors()
	if(round(starting.color) < 0 || round(starting.color) > length(the.colors)){
		starting.color <- 184
	}	
	#the.seq <- round(seq(starting.color,length(the.colors)*n.colors,prime.offset) %% length(the.colors))
	return(as.matrix(the.colors[(seq(1,n.colors*prime.offset,prime.offset) + (starting.color-1)) %% length(the.colors)]))
	
}