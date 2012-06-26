\name{tepGraphs}
\alias{tepGraphs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tepGraphs: TExPosition plotting function
}
\description{
	TExPosition plotting function which is an interface to \code{\link{prettyGraphs}}.
}

\usage{
tepGraphs(res, tepPlotInfo = NULL, x_axis = 1, y_axis = 2, xlab = NULL, ylab = NULL, main = NULL, contributionPlots = TRUE, correlationPlotter = TRUE, showHulls = 1, biplots = FALSE)
}

\arguments{
  \item{res}{results from TExPosition (i.e., $TExPosition.Data)}
  \item{tepPlotInfo}{results from tepGraphs (i.e., $Plotting.Data)}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{main}{main label for the graph window}
  \item{contributionPlots}{a boolean. If TRUE (default), contribution bar plots will be created.}
  \item{correlationPlotter}{a boolean. If TRUE (default), a correlation circle plot will be created. Applies to PCA family of methods (CA is excluded for now).}
  \item{showHulls}{a value between 0 and 1 to make a peeled hull at that percentage. All values outside of 0-1 will not plot any hulls.}  
  \item{biplots}{a boolean. If FALSE (default), separate plots are made for row items ($fii and $fi) and column items ($fj). If TRUE, row ($fii and $fi) and column ($fj) items will be on the same plot.}
}
\details{
tepGraphs is an interface between \code{\link{TExPosition}} and \code{\link{prettyGraphs}}. 
}
\value{
	The following items are bundled inside of $Plotting.Data:\cr
\item{$fii.col}{the colors that are associated to the individuals (row items; $fii).}	
\item{$fi.col}{the colors that are associated to the groups ($fi).}
\item{$fj.col}{the colors that are associated to the column items ($fj).}
\item{$constraints}{axis constraints for the plots (determines end points of the plots).}
}
\author{
Derek Beaton
}

\seealso{
 \code{\link{prettyGraphs}}
}
\examples{
	#this is for TExPosition's iris data
	data(ep.iris)
	bada.iris <- tepBADA(ep.iris$data,DESIGN=ep.iris$design,make_design_nominal=FALSE)
	#there are only 2 components, not 3.		
	bada.iris.plotting.data.2.and.1 <- tepGraphs(bada.iris,x_axis=2,y_axis=1)
}
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ misc }
