\name{UNVotes1947}
\docType{data}
\alias{UNVotes1947}
\title{UN Voting Portfolios of European Major Powers and U.S., 1947}
\description{
  This dataset contains UN voting portfolios of the U.S., United
  Kingdom, France and the Soviet Union in 1947.
}
\usage{UNVotes1947}
\format{
  A data frame with 5 variables and 32 observation. The vote coding is:
  1=no, 2=abstain, and 3=yes. UNCase does not refer to the actual
  resolution numbers.  Rather, the resolutions are simply listed sequentially as
  they appear in the data.
  \tabular{rlll}{
    [,1] \tab UNCase  \tab factor \tab UN Case \cr
    [,3] \tab USA    \tab integer \tab Vote by the  USA  \cr
    [,4] \tab UK    \tab integer \tab Vote by the United Kingdom  \cr
    [,5] \tab FRN   \tab integer \tab Vote by France \cr
    [,6] \tab RUS     \tab integer \tab Vote by the Soviet Union
  }
}
\source{Signorino and Ritter (1999), Table 7(b), p. 137}
\references{
  Signorino, C. S. and Ritter, J.M. (1999)
  Tau-b or Not Tau-b, \emph{International Studies Quarterly}, \bold{43}, 115--144.
}
\keyword{datasets}
