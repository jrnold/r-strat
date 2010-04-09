\name{unitTests}
\alias{unitTests.rino}
\title{ Unit tests for the package rino }

\description{
  Performs unit tests defined in this package by running
  \code{example(unitTests.svUnit)}. Tests are in \code{runit*.R} files
  Located in the '/unitTests' subdirectory or one of its subdirectories
  ('/inst/unitTests' and subdirectories in package sources).
}


\note{
  This method of integrating svUnit with  \code{R CMD CHECK} is stolen
  from the SciViews-K example written by Philippe Grosjean
  (\email{phgrosjean@sciviews.org}) and available from
  \url{http://www.sciviews.org/SciViews-K/SciViews-K_UnitManual_0.6.zip}.
}

\examples{
library(svUnit)
# Make sure to clear log of errors and failures first
clearLog()

# Run all test units defined in the 'rino package
(runTest(svSuite("package:rino"), "rino"))

# Check errors at the end of the process (needed to interrupt R CMD check)
errorLog()
}
\keyword{utilities}
