\name{rvCoeff}
\alias{rvCoeff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rvCoeff(S, T, type = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
%%     ~~Describe \code{S} here~~
}
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (S, T, type = -1) 
{
    rv = 0
    if (sum(dim(S) == dim(T)) != 2) {
        print("Dimensions do not match")
        return(NaN)
    }
    if (type == 0) {
        rv = matrixTrace(t(S) \%*\% T)/sqrt(matrixTrace(t(S) \%*\% 
            S) \%*\% matrixTrace(t(T) \%*\% T))
    }
    else if (type == 1) {
        rv = t(as.vector(S)) \%*\% as.vector(T)/sqrt(((as.vector(S)) \%*\% 
            as.vector(S)) \%*\% ((as.vector(T)) \%*\% as.vector(T)))
    }
    else {
        top = 0
        bottom_left = 0
        bottom_right = 0
        for (i in 1:dim(S)[1]) {
            for (j in 1:dim(T)[2]) {
                top = top + (S[i, j] * T[i, j])
                bottom_left = bottom_left + S[i, j]^2
                bottom_right = bottom_right + T[i, j]^2
            }
        }
        rv = top/sqrt(bottom_left * bottom_right)
    }
    return(as.numeric(rv))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
