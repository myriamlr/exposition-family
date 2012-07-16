\name{mpGraphs}
\alias{mpGraphs}
\title{mpGraphs: MExPosition plotting function}
\description{MExPosition plotting function which is an interface to \code{\link{prettyGraphs}}.
}
\usage{mpGraphs(res, mpPlotInfo = NULL, x_axis = 1, y_axis = 2, xlab = NULL, ylab = NULL, main = NULL)
}

\arguments{
  \item{res}{results from MExPosition (i.e., $mexPosition.Data)}
  \item{mpPlotInfo}{results from mpGraphs (i.e., $Plotting.Data)}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{main}{main label for the graph window}
}
\details{
mpGraphs is an interface between \code{\link{MExPosition}} and \code{\link{prettyGraphs}}. 
}
\value{
The following items are bundled inside of $Plotting.Data:\cr
\item{$fi.col}{the colors that are associated to the groups.}
\item{$fj.col}{the colors that are associated to the column items.}
}
\author{
Cherise R. Chin Fatt and Derek Beaton
}

\seealso{
 \code{\link{prettyGraphs}}
}
\keyword{multivariate}
\keyword{graphs}
\keyword{misc}
