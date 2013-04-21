\name{mdsTransform}
\alias{mdsTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mdsTransform(D, MW)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
%%     ~~Describe \code{D} here~~
}
  \item{MW}{
%%     ~~Describe \code{MW} here~~
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
function (D, MW) 
{
    DATA_dimensions <- dim(D)
    Mrepmat <- matrix(MW$M, nrow = nrow(D), ncol = ncol(D))
    if (is.null(dim(MW$M))) {
        BigXi <- diag(DATA_dimensions[1]) - (matrix(1, DATA_dimensions[1], 
            1) \%*\% MW$M)
    }
    else {
        BigXi <- diag(DATA_dimensions[1]) - (matrix(1, DATA_dimensions[1], 
            1) \%*\% diag(MW$M))
    }
    S <- -0.5 * sqrt(Mrepmat) * BigXi \%*\% D \%*\% t(BigXi) * sqrt(t(Mrepmat))
    rownames(S) <- rownames(D)
    colnames(S) <- colnames(D)
    return(S)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
