\name{prettyPlot}
\alias{prettyPlot}
\title{
prettyPlot
}
\description{
prettyPlot creates simple, crisp, publication-style quality graphics for multivariate analyses. 
}
\usage{
prettyPlot(data_matrix,x_axis=1,y_axis=2,col=NULL,xlab="",ylab="",main="",display_names=TRUE,display_points=TRUE,axes=TRUE,constraints=NULL,axis_line_width=3,pos=3,pch=21,cex=1,text.cex=0.8,contributionCircles=FALSE,contributions=NULL,flip=FALSE,asp=1,findBounds=TRUE,dev.new=TRUE,new.plot=TRUE)
}
\arguments{
  \item{data_matrix}{
	A set of data you would like to plot on 2 dimensions (e.g., a scatter plot). Can be original data or factor scores or anything with at least 2 columns.
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
  \item{display_names}{
boolean. If TRUE, the names of the points will be displayed on the plot.
}
  \item{display_points}{
boolean. If TRUE, the points will be displayed on the plot.
}
	\item{axes}{boolean. If TRUE, a new set of axes are plotted.}
  \item{new_window}{
boolean. If TRUE, a new graphics device is created. If FALSE, plotting will occur on current graphics device.
}
  \item{constraints}{
A list that contains the plot constraints. Default is NULL (constraints are computed each time).
}
  \item{axis_line_width}{
integer. Determines thickness of axis lines.
}
  \item{pos}{
integer between 1-4. Determines position of text for points. See \code{\link{par}}.
}
  \item{pch}{
an integer. Indicates which point style to use. See \code{\link{par}}.
}
  \item{cex}{
a continuous value. Indicates the size of the points. See \code{\link{par}. Used multiplicatively for points and additively for text with \code{contributions}.}.
}
  \item{text.cex}{
A replacement for the cex parameter in text() (to avoid collisions with cex). See \code{\link{par}} and \code{\link{text}}.
}
  \item{contributionCircles}{
boolean. If TRUE, items plotted will vary in size, dependent on amount of contribution to the variance.
}
  \item{contributions}{
A matrix of contribution values for data_matrix (should also be the same size & dimensionality as data_matrix)
}
  \item{flip}{
boolean. If TRUE, exchanges bg and col for all applicable pch values.
}
  \item{asp}{
numeric. Aspect ratio, see \code{\link{par}}.
}
  \item{findBounds}{
boolean. If TRUE, finds the min and max of each plotted axes. If FALSE, the largest value is used for all constraints.
}
  \item{dev.new}{
boolean. If TRUE, \code{\link{dev.new()}} is called internally to create new device. If FALSE, a device must already be open.
}
  \item{new.plot}{
boolean. If TRUE, \code{\link{plot()}} is called for a new plot on the current device. If FALSE, items will be overlayed much like \code{\link{points}}.
}

}
\value{
Returns a two item list:

	\item{col}{A matrix of colors where each element is the color for each item plotted.}
	\item{pch}{A matrix of pch values where each element is the pch number for each item plotted.}	
	\item{constraints}{A list (from \code{\link{minmaxHelper}}) of the plot constraints (i.e., min and max for axes)}

}
\author{
Derek Beaton
}
\keyword{ graphs }
\keyword{ multivariate }
