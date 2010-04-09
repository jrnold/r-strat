\name{stratStVal}
\alias{stratStVal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate Parameter Starting Values }
\description{
  Provides several options for generating the starting values for the
  parameters estimated in \code{\link{strat}}.
}

\usage{
stratStVal(n, method = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ Integer. Number of starting values to return, i.e. number of
    parameters.}
  \item{method}{ Integer. Method to use:
    \enumerate{
      \item 0 vector.
      \item Random numbers from a uniform distribution on the inteval ( 0, 0.5).
      \item Random numbers from a uniform distribution on the interval ( 0, 1).
      \item Random numbers from a standard normal distribution.
      }
  }
}
\details{
  The default method is to assume starting values of 0 for all parameters.
  The other other methods for generating starting values for parameters
  are taken from STRAT \url{http://www.rochester.edu/college/psc/signorino/}.
}

\value{
  Numerical vector of length \code{n}.
}

\seealso{ \code{\link{strat}}, \code{\link{mle}}, \code{\link{optim}} }

\examples{
nCoef <- 6

## default method - all 0's
stratStVal(nCoef)

## uniform distribution between 0 and 1
stratStVal(nCoef, method=3)

}
\keyword{utilities}