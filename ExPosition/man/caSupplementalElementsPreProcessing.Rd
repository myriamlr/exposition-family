\name{caSupplementalElementsPreProcessing}
\alias{caSupplementalElementsPreProcessing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
caSupplementalElementsPreProcessing(SUP.DATA, hellinger = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SUP.DATA}{
%%     ~~Describe \code{SUP.DATA} here~~
}
  \item{hellinger}{
%%     ~~Describe \code{hellinger} here~~
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
function (SUP.DATA, hellinger = FALSE) 
{
    if (!hellinger) {
        return(SUP.DATA/((rowSums(SUP.DATA)) \%*\% matrix(1, 1, 
            ncol(SUP.DATA))))
    }
    else {
        return((SUP.DATA/repmat(rowSums(SUP.DATA), 1, ncol(SUP.DATA)))^(1/2))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
