\name{mdsSupplementalElementsPreProcessing}
\alias{mdsSupplementalElementsPreProcessing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mdsSupplementalElementsPreProcessing(SUP.DATA = NULL, D = NULL, M = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SUP.DATA}{
%%     ~~Describe \code{SUP.DATA} here~~
}
  \item{D}{
%%     ~~Describe \code{D} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
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
function (SUP.DATA = NULL, D = NULL, M = NULL) 
{
    if (is.null(SUP.DATA) || is.null(D)) {
        stop("You must provide supplemental and active data.")
    }
    if (nrow(D) != ncol(D)) {
        stop("D dims do not match.")
    }
    if (sum(diag(D)) != 0) {
        stop("D diag is not 0.")
    }
    if (nrow(SUP.DATA) != ncol(SUP.DATA)) {
        stop("SUP.DATA dims do not match.")
    }
    if (sum(diag(SUP.DATA)) != 0) {
        stop("SUP.DATA diag is not 0.")
    }
    if (nrow(SUP.DATA) != nrow(D)) {
        stop("SUP.DATA and D dims do not match.")
    }
    if (is.null(M)) {
        M <- rep(1/nrow(D), nrow(D))
    }
    BigXi <- diag(nrow(D)) - (matrix(1, nrow(D), 1) \%*\% M)
    Mrepmat <- matrix(M, nrow = nrow(D), ncol = ncol(D))
    sup.subtract <- (SUP.DATA - (D \%*\% as.matrix(M) \%*\% matrix(1, 
        1, length(M))))
    return(-0.5 * sqrt(Mrepmat) * BigXi \%*\% sup.subtract \%*\% 
        t(BigXi) * sqrt(t(Mrepmat)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
