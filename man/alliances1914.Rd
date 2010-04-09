\name{alliances1914}
\docType{data}
\alias{alliances1914}
\title{Alliance Portfolios of Germany and Russia, 1914}
\description{
  This dataset contains Germany's and Russia's alliance portfolios with
  all the European nations in 1914, along with the nations' proportion of system capabilities. 
}
\usage{alliances1914}
\format{
  A data frame with 4 variables and 20 observations.  The alliance coding
  is: 0=no alliance, 1=entente,2=neutrality pact, 3=defense pact.
  \tabular{rlll}{
    [,1] \tab Nation  \tab factor \tab Country Code \cr
    [,2] \tab Cap    \tab numeric \tab System Capabilities \cr
    [,3] \tab GMY    \tab integer \tab Alliance with Germany (0-3) \cr
    [,4] \tab RUS    \tab integer \tab Alliance with Russia (0-3) \cr
  }

}
\source{Signorino and Ritter (1999), Table 6, p. 134}
\references{
    Signorino, C. S. and Ritter, J. M. (1999)
    Tau-b or Not Tau-b, \emph{International Studies Quarterly}, \bold{43}, 115--144.
}
\keyword{datasets}
