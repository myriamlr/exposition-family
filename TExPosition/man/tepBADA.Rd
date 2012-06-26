\name{tepBADA}
\alias{tepBADA}
\title{
	Barycentric Discriminant Analysis
}
\description{
Barycentric Discriminant Analysis (BADA) via TExPosition.
}
\usage{
tepBADA(DATA, scaleFlag = TRUE, centerFlag = TRUE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, graphs = TRUE, k = 0)
}
\arguments{
    \item{DATA}{original data to perform a PCA on.}
  \item{scaleFlag}{a boolean. If TRUE (default) the data will be scaled (z-scored) per column.}
  \item{centerFlag}{a boolean. If TRUE (default) the data will be centered at 0 per column.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{group.masses}{a diagonal matrix or column-vector of masses for the groups.}
  \item{ind.masses}{a diagonal matrix or column-vector of masses for the individuals (rows).}
  \item{weights}{a diagonal matrix or column-vector of weights for the column items.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
}
\value{
	See \code{\link{epGPCA}} (and also \code{\link{corePCA}}) for details on what is returned. In addition to the values returned:\cr
	\item{fii}{factor scores computed for supplemental observations}
	\item{dii}{squared distances for supplemental observations}
	\item{rii}{cosines for supplemental observations}
	\item{assign}{a list of assignment data. See \code{\link{fii2fi}} and \code{\link{R2}}}}
\references{
Abdi, H., and Williams, L.J. (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 2, 433-459.\cr
Abdi, H. and Williams, L.J. (2010). Correspondence analysis. In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
Abdi, H. & Williams, L.J. (2010). Barycentric discriminant analysis (BADIA). In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of Research Design}. Thousand Oaks (CA): Sage. pp. 64-75. \cr
Abdi, H., Williams, L.J., Beaton, D., Posamentier, M., Harris, T.S., Krishnan, A., & Devous, M.D. (in press, 2012). Analysis of regional cerebral blood flow data to discriminate among Alzheimer's disease, fronto-temporal dementia, and elderly controls: A multi-block barycentric discriminant analysis (MUBADA) methodology. \emph{Journal of Alzheimer Disease}, , -.
Abdi, H., Williams, L.J., Connolly, A.C., Gobbini, M.I., Dunlop, J.P., & Haxby, J.V. (2012). Multiple Subject Barycentric Discriminant Analysis (MUSUBADA): How to assign scans to categories without using spatial normalization. \emph{Computational and Mathematical Methods in Medicine}, 2012, 1-15. doi:10.1155/2012/634165.
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{corePCA}}, \code{\link{epPCA}}, \code{\link{epGPCA}},  \code{\link{epMDS}}\cr
	For MatLab code: http://utd.edu/~derekbeaton/attachments/Software/matlab/MuSuBADA_V3.zip
}
\examples{
data(bada.wine)
bada.res <- tepBADA(bada.wine$data,scaleFlag=FALSE,DESIGN=bada.wine$design,make_design_nominal=FALSE)
}
\keyword{ multivariate }
