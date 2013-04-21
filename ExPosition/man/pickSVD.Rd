\name{pickSVD}
\alias{pickSVD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pickSVD(datain, is.mds = FALSE, decomp.approach = "svd", k = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datain}{
%%     ~~Describe \code{datain} here~~
}
  \item{is.mds}{
%%     ~~Describe \code{is.mds} here~~
}
  \item{decomp.approach}{
%%     ~~Describe \code{decomp.approach} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (datain, is.mds = FALSE, decomp.approach = "svd", k = 0) 
{
    dataDims <- dim(datain)
    I <- dataDims[1]
    J <- dataDims[2]
    m <- min(I, J)
    if (k < 1 || k > m) {
        k <- m
    }
    flip <- FALSE
    if (I < J) {
        datain <- t(datain)
        flip <- TRUE
    }
    if (is.null(decomp.approach)) {
        decomp.approach <- "svd"
    }
    num.el <- (I * J)
    if (num.el > 1e+06) {
        decomp.approach <- "eigen"
    }
    if (tolower(decomp.approach) == "eigen") {
        eigOut <- eigen(t(datain) \%*\% datain)
        Q <- eigOut$vectors
        d <- sqrt(eigOut$values)
        P <- datain \%*\% Q \%*\% diag(d^-1)
    }
    else {
        svd.out <- svd(datain, nu = k, nv = k)
        P <- svd.out$u
        Q <- svd.out$v
        d <- svd.out$d
    }
    if (flip) {
        temp <- Q
        Q <- P
        P <- temp
    }
    P <- P[, 1:min(length(d), ncol(P))]
    Q <- Q[, 1:min(length(d), ncol(Q))]
    precisionLimit <- 2 * .Machine$double.eps
    if (is.mds) {
        indToKeep <- which(d > precisionLimit)
        tau <- d[indToKeep]/sum(d[indToKeep])
    }
    else {
        indToKeep <- which(d^2 > precisionLimit)
        tau <- d[indToKeep]^2/sum(d[indToKeep]^2)
    }
    indToKeep <- indToKeep[1:min(c(length(indToKeep), k))]
    return(list(u = P[, indToKeep], v = Q[, indToKeep], d = d[indToKeep], 
        tau = tau * 100))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
