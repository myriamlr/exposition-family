\name{pickSVD}
\alias{pickSVD}
\title{
Pick which generalized SVD (or related) decomposition to use.
}
\description{
This function is an interface for the user to a general SVD or related decomposition. It provides direct access to \code{\link{svd}} and \code{\link{eigen}}. Future decompositions will be available.
}
\usage{
pickSVD(datain, is.mds = FALSE, decomp.approach = "svd", k = 0)
}
\arguments{
  \item{datain}{a data matrix to decompose.}
  \item{is.mds}{a boolean. TRUE for a MDS decomposition.}
  \item{decomp.approach}{a string. 'svd' for singular value decomposition, 'eigen' for an eigendecomposition. All approaches provide identical output. Some approaches are (in some cases) faster than others.}
  \item{k}{numeric. The number of components to return.}
}
\details{
}
\value{
}
\references{
}
\author{
Derek Beaton
}
\note{
}
\seealso{
}
\examples{

}
\keyword{ misc }
