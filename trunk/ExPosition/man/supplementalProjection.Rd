\name{supplementalProjection}
\alias{supplementalProjection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
supplementalProjection(sup.transform = NULL, f.scores = NULL, Dv = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sup.transform}{
%%     ~~Describe \code{sup.transform} here~~
}
  \item{f.scores}{
%%     ~~Describe \code{f.scores} here~~
}
  \item{Dv}{
%%     ~~Describe \code{Dv} here~~
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
function (sup.transform = NULL, f.scores = NULL, Dv = NULL) 
{
    if (is.null(sup.transform) || is.null(f.scores) || is.null(Dv)) {
        stop("No inputs can be NULL.")
    }
    if (ncol(sup.transform) != nrow(f.scores)) {
        stop("Column dim of sup.transform does not match row dim of f.scores")
    }
    if (ncol(f.scores) != length(Dv)) {
        stop("Column dim of f.scores does not match length of Dv")
    }
    f.out <- sup.transform \%*\% f.scores * matrix(Dv^-1, nrow(sup.transform), 
        ncol(f.scores), byrow = TRUE)
    d.out <- rowSums(f.out^2)
    r.out <- repmat((1/d.out), 1, length(Dv)) * (f.out^2)
    r.out <- replace(r.out, is.nan(r.out), 0)
    d.out <- as.matrix(d.out)
    return(list(f.out = f.out, d.out = d.out, r.out = r.out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
