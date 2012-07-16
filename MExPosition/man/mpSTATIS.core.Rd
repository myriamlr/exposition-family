\name{mpSTATIS.core}
\alias{mpSTATIS.core}
\encoding{UTF-8}
\title{mpSTATIS.core}
\description{mpSTATIS.core performed the core of STATIS.}
\usage{mpSTATIS.core(X, num.obs, table, num.groups, optimization.option = 'STATIS')}
\arguments{
  \item{X}{Matrix of preprocessed data}
  \item{num.obs}{Number of Observations}
  \item{table}{Table which identifies the tables.}
  \item{num.groups}{Number of Groups/Tables}
  \item{optimization.option}{String option with the following options: 'None', 'STATIS', 'RV Matrix' and 'STATIS Power 1'}
}

\value{
   \item{S}{Scalar Product Matrices}
  \item{RVMatrix}{RV Matrix}
  \item{C}{C Matrix}
  \item{eigenVector}{Inner Product: Eigen Vectors of S}
  \item{eigenValue}{Inner Product: Eigen Value}
  \item{factorScores}{Inner Product: Factor Scores}
  \item{percentVar}{Inner Product: Percent Variance Explained}
  \item{alphaWeights}{Alpha Weights}
  \item{Compromise}{Compromise Matrix}
  \item{Compromise.EigenVector}{Compromise: Eigen Vectors}
  \item{Compromise.EigenValues}{Compromise: Eigen Values}
  \item{Compromise.cumEigenValues}{Compromise: Cumulative Eigen Values}
  \item{Compromise.factorScores}{Compromise: Factor Scores}
  \item{Compromise.percentVar}{Compromise: Percent Variance Explained}
  \item{Compromise.cumPercentVar}{Compromise: Cumulative Percent Variance Explained}
  \item{table.EigenValues}{Table: Eigen Values}
  \item{table.EigenVectors}{Table: Eigen Vectors}
  \item{table.loadings}{Table: Loadings}
  \item{table.FactorScores}{Table: Factor Scores}
  \item{table.partialFactorScores}{Table: Partial Factor Scores}
  \item{table.contribution}{Table: Contribution}
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4. \cr \cr
Abdi, H., & Valentin, D. (2007). STATIS. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Sage. pp. 955-962. \cr
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS}}, \code{\link{mpDISTATIS.core}}, \code{\link{mpDISTATIS}}
}
\keyword{ multivariate}
