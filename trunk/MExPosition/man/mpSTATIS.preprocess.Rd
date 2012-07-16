\name{mpSTATIS.preprocess}
\alias{mpSTATIS.preprocess}
\alias{mpSTATIS.preprocess}
\title{mpSTATIS.preprocess: Preprocessing for STATIS}
\description{Combines all preprocessing choices, and prepares the data for STAITS processing.}
\usage{mpSTATIS.preprocess(data, table, row.preprocess = 'None', column.preprocess = 'None', table.preprocess = 'None', make_table_nominal=TRUE)}
\arguments{
  \item{data}{Data Matrix}
  \item{table}{Table which identifies the tables.}
  \item{row.preprocess}{String option for row preprocessing with the following options: 'None' (default), 'Profile', 'Hellinger', 'Center' and 'Center_Hellinger'}
  \item{column.preprocess}{String option for column preprocessing with the following options: 'None' (default), 'Center', '1Norm', 'Center_1Norm' and 'Z_Score'}
  \item{table.preprocess}{String option for table preprocessing with the following options: 'None' (default), 'Num_Columns', 'Tucker', 'Sum_PCA', 'RV_Normalization' and 'MFA_Normalization'}
  \item{make_table_nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
}
\details{
This function calls all the preprocessing functions and consolidates the results. In addition it prepares the group matrix, and gets the data ready for processing. 
}
\value{
  \item{data.preprocessed}{Matrix of the Preprocessed Data}
  \item{num.obs}{Number of Observations}
  \item{col.groups}{Original matrix which was selected in the initial step}
  \item{groupMatrix}{Matrix which identifies the Tables}
  \item{numgroups}{Number of Tables}
  \item{table.ids}{Table IDs}
  \item{row.preprocess}{Option of row preprocessing selected}
  \item{column.preprocess}{Option of column preprocessing selected}
  \item{table.preprocess}{Option of table preprocessing selected}
}  
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt \email{cherise.chinfatt@utdallas.edu}
}
\seealso{
  \code{\link{mpSTATIS.rowPreproc}}, \code{\link{mpSTATIS.columnPreproc}}, \code{\link{mpSTATIS.tablePreproc}}
}
\examples{
X <- matrix(1:10,2)
Y<- c('g1','g1','g1','g2','g2')
row.preprocess='Center'
column.preprocess='Center'
table.preprocess='Sum_PCA'
preproc <-mpSTATIS.preprocess(X, t(Y), row.preprocess, column.preprocess, table.preprocess, make_table_nominal = TRUE)
}
\keyword{ multivariate }