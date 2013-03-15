\name{mpDISTATIS}
\alias{mpDISTATIS}
\title{mpDISTATIS: DISTATIS via MExPosition}
\description{All DISTATIS steps are combined in this function. It enables preparation of the data, processing and graphing.}
\usage{mpDISTATIS(data, sorting = 'No', normalization = 'None', masses = 'NULL', table=NULL, make.table.nominal = TRUE, DESIGN = NULL, make.design.nominal = TRUE, graphs = TRUE)}
\arguments{
  \item{data}{Data Matrix}
  \item{sorting}{a boolean. If YES, DISTATIS will by processed as a sorting task. Default is NO}
  \item{normalization}{Normaliztion string option: 'None' (default), 'Sum_PCA', or 'MFA'}
  \item{table}{Table which identifies the different tables.}
  \item{make.table.nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{masses}{Masses: if NULL, 1/num.obs would be set by default. For customized masses, enter the matrix of customized masses}
  \item{graphs}{a boolean. If TRUE (default), graphs are displayed}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make.design.nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  
}
\details{
 \code{mpDISTATIS} performs DISTATIS on a set of data matrices measured on the same set of observations. 
}
\value{
Returns a large list of items which are divided into three categories: 
  \item{$Overview}{Overview of Results}
  \item{$InnerProduct}{Results for the Inner Product}
  \item{$Compromise}{Results for the Compromise}
  \item{$Table}{Results for the Tables}
The results for Overview are bundled inside of $Overview. 
  \item{$Overview$data}{Data Matrix}
  \item{$Overview$normalization}{Type of Normalization used.}
  \item{$Overview$sorting}{Indicates if the task is a sorting task}
  \item{$Overview$table}{Table which indicates the tables}
  \item{$num.groups}{Number of groups}
The results for InnerProduct are bundled inside of $InnerProduct
  \item{$InnerProduct$S}{Inner Product: Scalar Product Matrices}
  \item{$norm.S}{Normalized Scalar Product Matrices}
  \item{$InnerProduct$C}{Inner Product: C Matrix}
  \item{$InnerProduct$eigs.vector}{Inner Product: Eigen Vectors}
  \item{$InnerProduct$eigs}{Inner Product: Eigen Values}
  \item{$InnerProduct$fi}{Inner Product: Factor Scores}
  \item{$InnerProduct$tau}{Inner Product: Percent Variance Explained (tau)}
  \item{$InnerProduct$alphaWeights}{Alpha Weights}
The results for the Tables are bundled inside of $Table.
  \item{$Table$m}{Table: Masses}
  \item{$Table$eigs}{Table: Eigen Values}
  \item{$Table$eigs.vector}{Table: Eigen Vectors}
  \item{$Table$Q}{Table: Loadings}
  \item{$Table$fi}{Table: Factor Scores}
  \item{$Table$partial.fi}{Table: Partial Factor Scores}
  \item{$Table$partial.fi.array}{Table: Array of Partial Factor Scores}
  \item{$Table$cj}{Table:Contribution for the rows}
  \item{$Table$cj}{Table: Contribution for the columns}
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167. \cr \cr
Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007). Analyzing assessors and products in sorting tasks: DISTATIS, theory and applications. \emph{Food Quality and Preference}, 18, 627-640.\cr \cr
Abdi, H., & Valentin, D. (2005). DISTATIS: the analysis of multiple distance matrices. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage. pp. 284-290. \cr
}
\author{Cherise R. Chin Fatt \email{cherise.chinfatt@utdallas.edu}
}
\seealso{
  \code{\link{mpSTATIS}}
}
\examples{
data('faces2005')
table = c('pixel','pixel','pixel','pixel','pixel','pixel', 'distance','distance','distance','distance','distance','distance', 'ratings','ratings','ratings','ratings','ratings','ratings', 'similarity','similarity','similarity','similarity','similarity','similarity')
demo.distatis.2005 <- mpDISTATIS(faces2005$data, sorting = 'No', normalization = 'MFA', table = table)
}
\keyword{ multivariate }
