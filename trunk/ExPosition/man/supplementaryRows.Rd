\name{supplementaryRows}
\alias{supplementaryRows}
\title{
Supplementary rows
}
\description{
Computes factor scores for supplementary observations (rows).
}
\usage{
supplementaryRows(SUP.DATA, res, center = TRUE, scale = TRUE)
}
\arguments{
  \item{SUP.DATA}{a data matrix of supplementary observations (must have the same measures [columns] as active data)}
  \item{res}{ExPosition or TExPosition results}
  \item{center}{either a vector to center by, or a boolean to indicate if supplemental data should be centered by active data}
  \item{scale}{either a vector to scale by, or a boolean to indicate if supplemental data should be scaled by active data}
}
\details{
This function recognizes the class types of: \code{\link{epPCA}}, \code{\link{epGPCA}}, \code{\link{tepBADA}}, \code{\link{epMDS}}, \code{\link{epCA}}, \code{\link{epMCA}}, and \code{\link{tepDICA}}. Further, the function recognizes if Hellinger (as opposed to row profiles; in CA, MCA and DICA) were used. 
}
\value{
A list of values containing:\cr
\item{fii}{factor scores computed for supplemental observations}
\item{dii}{squared distances for supplemental observations}
\item{rii}{cosines for supplemental observations}
}
\author{
Derek Beaton
}
\examples{
	data(jocn.2005.fmri)
	mds.res <- epMDS(jocn.2005.fmri$scans$data)
	sup.info <- supplementaryRows(jocn.2005.fmri$images$data,mds.res$ExPosition.Data)
	prettyPlot(sup.info$fii)
	prettyPlot(mds.res$ExPosition.Data$fi,new_window=FALSE,col=mds.res$Plotting.Data$fi.col,contributionCircles=TRUE,contributions=mds.res$ExPosition.Data$ci)
}
\keyword{ misc }
