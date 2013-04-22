\name{epMDS.inference.battery}
\alias{epMDS.inference.battery}
\encoding{UTF-8}
\title{
epMDS.inference.battery: Inference tests for Multidimensional Scaling (MDS) via InPosition. 
}
\description{
DO NOT USE THIS FUNCTION. It is still under development!\cr
Multidimensional Scaling (MDS) and a battery of inference tests via InPosition. The battery includes permutation and bootstrap tests.
}
\usage{
epMDS.inference.battery(DATA, DATA_is_dist = TRUE, method = "euclidean", DESIGN = NULL, make_design_nominal = TRUE, masses = NULL, graphs = TRUE, k = 0, test.iters = 1000, constrained = FALSE, critical.value = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{original data to perform a MDS on.}
  \item{DATA_is_dist}{a boolean. If TRUE (default) the DATA matrix should be a symmetric distance matrix. If FALSE, a Euclidean distance of row items will be computed and used.}
  \item{method}{which distance metric should be used. \code{method} matches \code{\link{dist}}; "chi2" also available (\code{\link{chi2Dist}}). Default is "euclidean".}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{masses}{a diagonal matrix (or vector) that contains the masses (for the row items).}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
  \item{test.iters}{number of iterations}
  \item{constrained}{a boolean. If a DESIGN matrix is used, this will constrain bootstrap resampling to be within groups.}
  \item{critical.value}{numeric. A value, analogous to a z- or t-score to be used to determine significance (via bootstrap ratio).}
}
\details{
Do not use this.
}
\value{
Returns two lists ($Fixed.Data and $Inference.Data). For $Fixed.Data, see \code{\link{epPCA}}, \code{\link{corePCA}} for details on the descriptive (fixed-effects) results.

$Inference.Data returns:
\item{components}{Permutation tests of components. p-values ($p.vals) and distributions of eigenvalues ($eigs.perm) for each component}
}
\author{
Derek Beaton and Hervé Abdi.
}
\seealso{
\code{\link{epMDS}}, \code{\link{coreMDS}}
}
\examples{
}
\keyword{ multivariate }
\keyword{ permutation }
\keyword{ bootstrap }