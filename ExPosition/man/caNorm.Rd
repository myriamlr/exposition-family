\name{caNorm}
\alias{caNorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
caNorm(X, X_dimensions, colTotal, rowTotal, grandTotal, weights = NULL, masses = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{X_dimensions}{
%%     ~~Describe \code{X_dimensions} here~~
}
  \item{colTotal}{
%%     ~~Describe \code{colTotal} here~~
}
  \item{rowTotal}{
%%     ~~Describe \code{rowTotal} here~~
}
  \item{grandTotal}{
%%     ~~Describe \code{grandTotal} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
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
function (X, X_dimensions, colTotal, rowTotal, grandTotal, weights = NULL, 
    masses = NULL) 
{
    rowCenter = colTotal/grandTotal
    if (is.null(masses)) {
        masses = rowTotal/grandTotal
    }
    if (is.null(weights)) {
        weights = rowCenter^-1
    }
    rowProfiles <- X/((rowSums(X)) \%*\% matrix(1, 1, ncol(X)))
    deviations = rowProfiles - (repmat(1, X_dimensions[1], 1) \%*\% 
        rowCenter)
    return(list(rowCenter = rowCenter, masses = masses, weights = weights, 
        rowProfiles = rowProfiles, deviations = deviations))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
