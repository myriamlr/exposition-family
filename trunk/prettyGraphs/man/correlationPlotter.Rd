\name{correlationPlotter}
\alias{correlationPlotter}

\title{
correlationPlotter
}
\description{
Computes and plots a correlation circle (used in multivariate analyses). Correlation is computed between measured items and components (factors, dimensions, principal axes, etc...). 
}
\usage{
correlationPlotter(data_matrix, factor_scores, x_axis = 1, y_axis = 2, col = NULL, xlab = "", ylab = "", main = "")
}
\arguments{
  \item{data_matrix}{
	A set of data (i.e., original measures and observations)
}
  \item{factor_scores}{
	One set of factor scores that were computed from the original data matrix.
}
  \item{x_axis}{
Which axis is the x-axis? Default is 1.
}
  \item{y_axis}{
Which axis is the y-axis? Default is 2.
}
  \item{col}{
A single-column matrix of colors for each data point.
}
  \item{xlab}{
A label to be placed along the x-axis.
}
  \item{ylab}{
A label to be placed along the y-axis.
}
  \item{main}{
A title to be placed at the top of the graph.
}

 
}

\author{
Derek Beaton
}
\keyword{ graphs }
\keyword{ multivariate }