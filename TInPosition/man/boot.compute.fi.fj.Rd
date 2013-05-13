\name{boot.compute.fi.fj}
\alias{boot.compute.fi.fj}
\title{
Bootstrap computations for TInPosition.
}
\description{
Provides bootstrap projections for $fi and $fj from TExPosition methods.
}
\usage{
boot.compute.fi.fj(DATA, DESIGN, res)
}

\arguments{
  \item{DATA}{
The original data matrix to be bootstrapped. Rows will be bootstrapped and are assumed to be observations.
}
  \item{DESIGN}{
A design matrix (in disjunctive coding). Only used if \code{constrained} is TRUE.
}
  \item{res}{
of class \code{expoOutput}. Results from one of the \code{\link{ExPosition}} methods (e.g., \code{\link{epPCA}}, \code{\link{epMCA}}),
}
}

\value{
\item{FBX}{a set of factor scores of the measures (columns, \code{fj}) for the bootstrapped data.}
\item{FBY}{a set of factor scores of the groups (\code{fi}) for the bootstrapped data.}
}
\author{
Derek Beaton
}
\keyword{ bootstrap }
