\name{continueResampling}
\alias{continueResampling}
\title{
A stopping mechanism if resampling will take too long.
}
\description{
This function asks the user if they want to continue with resampling if the total time for resampling takes more than 1 minute. It also provides an estimate of how long resampling takes. This function is required for \code{InPosition} and \code{TInPosition} and we do not recommend others use it.
}
\usage{
continueResampling(cycle.time, test.iters)
}
\arguments{
  \item{cycle.time}{
Is the subtraction of two calls to \code{proc.time}.
}
  \item{test.iters}{
The number of iterations (i.e., a loop) expected.
}
}
\author{
Derek Beaton
}
\note{
If computation time is expected to take more than 1 minute, this asks the user if they would like to continue. If 'Y', looping continues. If 'N', it stops. See inference battery functions for details
}