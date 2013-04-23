\name{tepDICA.inference.battery}
\alias{tepDICA.inference.battery}
\encoding{UTF-8}
\title{
Discriminant Correspondence Analysis Inference Battery
}
\description{
Discriminant Correspondence Analysis (DICA) Inference Battery via TInPosition
}
\usage{
tepDICA.inference.battery(DATA, make_data_nominal = FALSE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2)
}
\arguments{
\item{DATA}{original data to perform a MCA on. This data can be in original formatting (qualitative levels) or in dummy-coded variables.}
  \item{make_data_nominal}{a boolean. If TRUE (default), DATA is recoded as a dummy-coded matrix. If FALSE, DATA is a dummy-coded matrix.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{group.masses}{a diagonal matrix or column-vector of masses for the groups.}
  \item{ind.masses}{a diagonal matrix or column-vector of masses for the individuals (rows).}
  \item{weights}{a diagonal matrix or column-vector of weights for the column it}
  \item{hellinger}{a boolean. If FALSE (default), Chi-square distance will be used. If TRUE, Hellinger distance will be used.}
  \item{symmetric}{a boolean. If TRUE (default) symmetric factor scores for rows.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
  \item{test.iters}{number of iterations}
  \item{critical.value}{numeric. A value, analogous to a z- or t-score to be used to determine significance (via bootstrap ratio).}
}
\details{

}
\value{
	See \code{\link{tepDICA}} (and also \code{\link{coreCA}}) for details on what is returned. In addition to the values returned:\cr
}
\references{
}
\author{
Derek Beaton, Hervé Abdi
}
\seealso{

}
\examples{
data(dica.wine)
dica.res <- tepDICA.inference.battery(dica.wine$data,DESIGN=dica.wine$design,make_design_nominal=FALSE)
}
\keyword{ multivariate }