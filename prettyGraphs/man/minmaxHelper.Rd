\name{minmaxHelper}
\alias{minmaxHelper}
\title{
minmaxHelper
}
\description{
Computes the x- and y- axis constraints for all prettyGraphs functions.
}
\usage{
minmaxHelper(mat1, mat2 = NULL, axis1 = 1, axis2 = 2)
}
\arguments{
  \item{mat1}{
Matrix that will be plotted. Must be at least 2 rows by 2 columns.
}
  \item{mat2}{
if NULL, mat2=mat2. Second matrix that will be plotted (if a biplot). Must be at least 2 rows by 2 columns.
}
  \item{axis1}{
Which column of the matrix will be the x-axis? Default = 1.
}
  \item{axis2}{
Which column of the matrix will be the y-axis? Default = 2.
}
}
\value{
\item{minMaxList}{
	A list with the following values: minx, miny, maxx, maxy
}
}
\author{
Derek Beaton
}
\examples{
	data(beavers)
	minMaxList <- minmaxHelper(as.matrix(beaver1),as.matrix(beaver1),1,2) 
}
\keyword{ misc }