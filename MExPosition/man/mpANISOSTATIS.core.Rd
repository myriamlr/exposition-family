\name{mpANISOSTATIS.core}
\alias{mpANISOSTATIS.core}
\encoding{UTF-8}
\title{mpANISOSTATIS.core: ANISOSTATIS via MExPosition}
\description{Performs ANISOSTATIS on the data}
\usage{mpANISOSTATIS.core(X, num.obs, table, num.groups, optimization.option='ANISOSTATIS_Type1')}
\arguments{
  \item{X}{Matrix of preprocessed data}
  \item{num.obs}{Number of observations}
  \item{table}{Table Matrix- used to identifty the tables}
  \item{num.groups}{Number of groups}
  \item{optimization.option}{String option of either 'ANISOSTATIS_Type1' (DEFAULT), 'ANISOSTATIS_Type2' or 'ANISOSTATIS_Power1'}
}
\details{
Computation of Anisotropic STATIS (ANISOSTATIS), where the same weight is given to each variable of a given table.
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
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpDISTATIS}}, \code{\link{mpSTATIS}}, \code{\link{mpSTATIS.core}}
}
\keyword{ multivariate}
