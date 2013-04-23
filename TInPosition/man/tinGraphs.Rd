\name{tinGraphs}
\alias{tinGraphs}
\title{
tinGraphs: TInPosition plotting function
}
\description{
	TInPosition plotting function which is an interface to \code{\link{prettyGraphs}}.
}
\usage{
tinGraphs(res, DESIGN = NULL, x_axis = NULL, y_axis = NULL, inference.info = NULL, color.by.boots = TRUE, boot.cols = c("orchid4", "olivedrab3", "firebrick2"), fi.col = NULL, fi.pch = NULL, fii.col = NULL, fii.pch = NULL, fj.col = NULL, fj.pch = NULL, col.offset = NULL, constraints = NULL, xlab = NULL, ylab = NULL, main = NULL, contributionPlots = TRUE, correlationPlotter = TRUE, showHulls = 0.95, biplots = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{results from TExPosition}
  \item{DESIGN}{A design matrix to apply colors (by pallete selection) to row items}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{inference.info}{
%%     ~~Describe \code{inference.info} here~~
}
  \item{color.by.boots}{
%%     ~~Describe \code{color.by.boots} here~~
}
  \item{boot.cols}{
%%     ~~Describe \code{boot.cols} here~~
}
  \item{fi.col}{A matrix of colors for the group items. If NULL, colors will be selected.}
  \item{fi.pch}{A matrix of pch values for the group items. If NULL, pch values are all 21.}    
  \item{fii.col}{A matrix of colors for the row items (observations). If NULL, colors will be selected.}
  \item{fii.pch}{A matrix of pch values for the row items (observations). If NULL, pch values are all 21.}  
  \item{fj.col}{A matrix of colors for the column items. If NULL, colors will be selected.}
  \item{fj.pch}{A matrix of pch values for the column items. If NULL, pch values are all 21.}
  \item{col.offset}{A numeric offset value. Is passed to \code{\link{createColorVectorsByDesign}}.}
  \item{constraints}{Plot constraints as returned from \code{\link{prettyPlot}}. If NULL, constraints are selected.}        
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{main}{main label for the graph window}
  \item{contributionPlots}{a boolean. If TRUE (default), contribution bar plots will be created.}
  \item{correlationPlotter}{a boolean. If TRUE (default), a correlation circle plot will be created. Applies to PCA family of methods (CA is excluded for now).}
  \item{showHulls}{a value between 0 and 1 to make a peeled hull at that percentage. All values outside of 0-1 will not plot any hulls.}  
  \item{biplots}{a boolean. If FALSE (default), separate plots are made for row items ($fii and $fi) and column items ($fj). If TRUE, row ($fii and $fi) and column ($fj) items will be on the same plot.}
}
\details{
tinGraphs is an interface between \code{\link{TInPosition}} and \code{\link{prettyGraphs}}. 
}
\author{
Derek Beaton
}
\seealso{
 \code{\link{prettyGraphs}},  \code{\link{inGraphs}}
}
\examples{
	#this is for TExPosition's iris data
	data(ep.iris)
	bada.iris <- tepBADA.inference.battery(ep.iris$data,DESIGN=ep.iris$design,make_design_nominal=FALSE)
	#there are only 2 components, not 3.		
	bada.iris.plotting.data.biplot <- tinGraphs(bada.iris,x_axis=1,y_axis=2,biplots=TRUE)
}
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ misc }