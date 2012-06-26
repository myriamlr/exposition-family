\name{contributionBars}
\alias{contributionBars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
contributionBars
}
\description{
Produces bar charts for multivariate analyses. Plots the contribution to the variance from each data point for upwards of two axes (components).
}
\usage{
contributionBars(factor_scores, contributions, x_axis = 1, y_axis = 2, col = NULL, main = "", upper = "blue", lower = "red", important = NULL, threshold = 0, sortContributions = TRUE)
}
\arguments{
  \item{factor_scores}{
The factor scores, or x- and y-axis points of a data set.
}
  \item{contributions}{
The amount of contribution to the variance (between 0-1) by each data point.
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
  \item{main}{
A title to be placed at the top of the graph.
}
  \item{upper}{
The color used to identify the upper bound items that contribute above average variance.
}
  \item{lower}{
The color used to identify the lower bound items that contribute above average variance.
}
  \item{important}{
NOT CURRENTLY USED. A place holder for future versions.
}
  \item{threshold}{
A threshold (between 0-1) to draw upper and lower bounds for important items. Default is 1/number of items.
}
  \item{sortContributions}{
boolean, if TRUE, items will be sorted by contribution. if FALSE, items will appear in their row order.
}
}

\author{
Derek Beaton
}
\keyword{ graphs }
\keyword{ multivariate }
