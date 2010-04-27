library(Formula)
library(maxLik)
data(war1800)

strat( list(outcome ~ peaceyrs + s.wt.re1 + 0 | 1 | balanc * peaceyrs + 0, outcome ~ 0 | 0 | balanc), data=war1800)


foo <- function() {
    x <- 1
    bar <- function() {
        x
    }
    return(bar)
}
