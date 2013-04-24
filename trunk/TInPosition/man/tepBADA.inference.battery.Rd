\name{tepBADA.inference.battery}
\alias{tepBADA.inference.battery}
\encoding{UTF-8}
\title{
	Barycentric Discriminant Analysis Inference Battery
}
\description{
Barycentric Discriminant Analysis (BADA) Inference Battery via TInPosition.
}
\usage{
tepBADA.inference.battery(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2)
}
\arguments{
    \item{DATA}{original data to perform a PCA on.}
  \item{scale}{a boolean, vector, or string. See \code{\link{expo.scale}} for details.}
  \item{center}{a boolean, vector, or string. See \code{\link{expo.scale}} for details.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{group.masses}{a diagonal matrix or column-vector of masses for the groups.}
  \item{ind.masses}{a diagonal matrix or column-vector of masses for the individuals (rows).}
  \item{weights}{a diagonal matrix or column-vector of weights for the column items.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
  \item{test.iters}{number of iterations}
  \item{critical.value}{numeric. A value, analogous to a z- or t-score to be used to determine significance (via bootstrap ratio).}
}
\details{

}
\value{
	See \code{\link{tepBADA}} (and also \code{\link{corePCA}}) for details on what is returned. In addition to the values returned:\cr
}
\references{

}
\author{
Derek Beaton, Jenny Rieck, Hervé Abdi
}
\note{

}

\seealso{

}
\examples{
data(bada.wine)
bada.res <- tepBADA.inference.battery(bada.wine$data,scale=FALSE,DESIGN=bada.wine$design,make_design_nominal=FALSE)
}
\keyword{ multivariate }