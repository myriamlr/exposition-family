\name{supplementalProjection}
\alias{supplementalProjection}
\title{
Supplemental projections.
}
\description{
Performs a supplementary projection across ExPosition (and related) techniques.
}
\usage{
supplementalProjection(sup.transform = NULL, f.scores = NULL, Dv = NULL)
}
\arguments{
  \item{sup.transform}{Data already transformed for supplementary projection. That is, the output from: \code{\link{caSupplementalElementsPreProcessing}}, \code{\link{mdsSupplementalElementsPreProcessing}}, \code{\link{pcaSupplementaryColsPreProcessing}}, or \code{\link{pcaSupplementaryRowsPreProcessing}}.}
  \item{f.scores}{Active factor scores, e.g., res$ExPosition.Data$fi}
  \item{Dv}{Active singular values, e.g., res$ExPosition.Data$pdq$Dv}
}
\value{
	A list with: \cr
	\item{f.out}{Supplementary factor scores.}
	\item{d.out}{Supplementary square distances.}
	\item{r.out}{Supplementary cosines.}
}
\author{
Derek Beaton
}
\examples{

}
\keyword{ misc }
\keyword{ multivariate }