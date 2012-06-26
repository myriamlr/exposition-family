\name{texpoDesignCheck}
\alias{texpoDesignCheck}
\title{
texpoDesignCheck
}
\description{
TExPosition's DESIGN matrix check function. Calls into ExPosition's \code{\link{designCheck}}.
}
\usage{
texpoDesignCheck(DATA, DESIGN = NULL, make_design_nominal = TRUE)
}
\arguments{
  \item{DATA}{original data that should be matched to a design matrix}
  \item{DESIGN}{a column vector with levels for observations or a dummy-coded matrix}
  \item{make_design_nominal}{a boolean. Will make DESIGN nominal if TRUE (default).}
}
\details{
	Execution stops if:\cr
	1. DESIGN has more columns (groups) than observations,
	2. DESIGN has only 1 column (group), or
	3. DESIGN has at least 1 occurence where an observation is the only observation in a group (i.e., colSums(DESIGN)==1 at least once).
}
\value{
\item{DESIGN}{dummy-coded design matrix}
}

\author{
Derek Beaton
}
\keyword{ misc }
