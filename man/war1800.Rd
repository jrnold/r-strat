\name{war1800}
\docType{data}
\alias{war1800}
\title{Nineteenth century militarized interstate disputes}
\description{
  This dataset is a sample nineteenth-century militarized interstate disputes.
}
\usage{war1800}
\format{
  A data frame with 20 variables and 313 observations.  
  \tabular{rll}{
    midnum \tab integer \tab MID number \cr
    year \tab integer \tab year \cr
    ccode1 \tab integer \tab state 1 country code \cr
    abbrev1 \tab factor \tab state 1 3-letter abbreviation \cr
    ccode2 \tab integer \tab state 2 country code \cr
    abbrev2 \tab factor \tab state 2 3-letter abbreviation \cr
    cap.1 \tab numeric \tab stat 1's percent of system capabilities \cr
    cap.2 \tab numeric \tab stat 2's percent of system capabilities \cr
    balanc \tab numeric \tab cap.1/(cap.1 + cap.2) \cr
    s.wt.re1 \tab numeric \tab S score weighted for 1's region \cr
    s.wt.re2 \tab numeric \tab S score weighted for 2's region \cr
    dem1 \tab integer \tab state 1's democracy score \cr
    dem2 \tab integer \tab state 2's democracy score \cr
    distance \tab integer \tab distance between states 1 and 2 \cr
    revis1 \tab logical \tab whether state 1 is revisionist \cr
    revis2 \tab logical \tab whether state 2 is revisionist \cr
    sq \tab logical \tab status quo outcome dummy \cr
    capit \tab logical \tab capitulation by 2 outcome dummy \cr
    war \tab logical \tab war outcome dummy
  }

}
\source{Curtis Signorino,
  \url{http://www.rochester.edu/college/psc/signorino/}, war1800.dta }
\keyword{datasets}
