\name{createColorVectorsByDesign}
\alias{createColorVectorsByDesign}
\title{
createColorVectorsByDesign
}
\description{
Produces a color vector for items by using a design matrix.
}
\usage{
createColorVectorsByDesign(design_matrix)
}
\arguments{
  \item{design_matrix}{
A dummy coded design matrix.
}
}
\value{
	Produces a list with the following items\cr
	\item{oc}{The colors of the observations (based on group)}
	\item{gc}{The colors of the groups}	
}

\author{
Derek Beaton
}
\keyword{ misc }
