\name{makeDistancesAndWeights}
\alias{makeDistancesAndWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeDistancesAndWeights(DATA, method = "euclidean", masses = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{
%%     ~~Describe \code{DATA} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{masses}{
%%     ~~Describe \code{masses} here~~
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
function (DATA, method = "euclidean", masses = NULL) 
{
    if (method == "chi2") {
        chi2res <- chi2Dist(DATA)
        D <- chi2res$D
        MW <- list(M = chi2res$M)
    }
    else {
        D <- as.matrix(dist(DATA, method = method, diag = TRUE, 
            upper = TRUE))
        MW <- computeMW(D, masses = masses)
    }
    return(list(D = D, MW = MW))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
