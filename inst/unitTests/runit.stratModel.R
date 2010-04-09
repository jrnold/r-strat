#!/usr/bin/env Rscript
### runit.strat.R
### Title: Unit Tests for StratModel class and methods.
### Author: Author: Jeffrey Arnold <jarnold7@mail.rochester.edu>
### Date: 6/17/2008
### Copyright (C): 2008  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

if (FALSE) {
    library(svUnit)
}

formatMatrix <- function(X, f) {
    n <- ncol(X)
    rcNames <- list(rownames(X), colnames(X))
    return(matrix(as(X, f), ncol=n, dimnames=rcNames))
}
expandMatrix <- function(A, n) matrix(rep(A, n), nrow=(nrow(A) * n), byrow=TRUE)

randomLogicalSimplex <- function(n, m) {
    data.frame(t(apply(matrix(runif(n*m), ncol=m), 1, function(x) x == max(x))))
}

### ----------------------------------------------------------------------------

foo <- new("StratModel",
           X = list(matrix(c(1,2),
           dimnames=list(NULL, c("alpha", "bravo")), ncol=2)),
           Y = matrix(1, dimnames=list(NULL, "charlie.1" )),
           pinfo = list(charlie.1 = 1),
           oPlayers = c("1"),
           oResponses = c("charlie"),
           players = c("1"),
           outcomes = c("charlie", "delta"),
           alpha = list( a1 = 1),
           tNodes = c("charlie", "delta"),
           paths=list(1, 2))

b1 <- list(c(1,2))

## Nonsensical model, but probabilities all evaluate to 0.5 in this case.
barAlpha <- runif(2)

bar <- new("StratModel_1-2",
           X = list(
           matrix(barAlpha[1], dimnames=list(NULL, c("alpha"))),
           matrix(barAlpha[1], dimnames=list(NULL, c("foxtrot"))),
           matrix(barAlpha[1], dimnames=list(NULL, c("foxtrot"))),
           matrix(barAlpha[2], dimnames=list(NULL, c("bravo"))),
           matrix(barAlpha[2], dimnames=list(NULL, c("bravo")))
           ),
           Y = matrix(c(1, 0, 0), ncol=3, dimnames=list(NULL, c("charlie", "delta", "echo"))),
           pinfo = list(
           charlie.1 = 1,
           delta.1 = 1,
           echo.1 = 1,
           delta.2 = 1,
           echo.2 = 1
           ),
           oPlayers = c("1", "1", "1", "2", "2"),
           oResponses = c("charlie", "delta", "echo", "delta", "echo"),
           players = c("1", "2"),
           outcomes = c("charlie", "delta", "echo"),
           alpha = list( a1 = 1, a2 = 1, a3 =1,  a4 = 1),
           tNodes = c("charlie", "delta", "echo"))

baz <- new("StratModel_1-2",
           X = list(
           matrix(barAlpha[1], dimnames=list(NULL, c("alpha"))),
           matrix(barAlpha[1], dimnames=list(NULL, c("foxtrot"))),
           matrix(barAlpha[2], dimnames=list(NULL, c("bravo"))),
           matrix(barAlpha[2], dimnames=list(NULL, c("bravo")))
           ),
           Y = matrix(c(1, 0), ncol=2, dimnames=list(NULL, c("charlie", "delta"))),
           pinfo = list(
           charlie.1 = 1,
           delta.1 = 1,
           charlie.2 = 1,
           delta.2 = 1
           ),
           oPlayers = c("1", "1", "2", "2"),
           oResponses = c("charlie", "delta", "charlie", "delta"),
           players = c("1", "2"),
           outcomes = c("charlie", "delta"),
           alpha = list( a1 = 1, a2 = 1, a3 =1,  a4 = 1),
           tNodes = c("charlie", "delta", "charlie"))

b2 <- lapply(rep(runif(1), 5), function(x) x)
names(b2) <- c("charlie.1.alpha", "delta.1.foxtrot", "echo.1.foxtrot", "delta.2.bravo", "echo.2.bravo")

bBaz <- lapply(rep(runif(1), 4), function(x) x)
names(b2) <- c("charlie.1.alpha", "delta.1.foxtrot", "charlie.2.bravo", "delta.2.bravo")

test_.calcUtility <- function() {


    ## Check Regular execution
    checkIdentical(.calcUtility(foo, b1, expand=FALSE),
                       list(charlie.1 = 5))

    ## Check Expansion
    checkIdentical(.calcUtility(foo, b1, expand=TRUE),
                   list(charlie.1 = 5,
                        delta.1 = 0))

}

test_.getPaths <- function() {
    paths12 <- list("a1", c("a2", "a3"), c("a2", "a4"))
    checkIdentical(.getPaths(bar), paths12)
    checkIdentical(.getPaths(baz), paths12)
}

test_.getOut2Term <- function() {
    checkIdentical(.getOut2Term(bar),
                   list(charlie = as.integer(1),
                        delta = as.integer(2),
                        echo = as.integer(3)))

    checkIdentical(.getOut2Term(baz),
                   lapply(list(charlie = c(1, 3), delta = 2), as.integer))
}


test_.calcPrAction12 <- function() {
    checkIdentical(.calcPrAction12(bar, b2),
                    matrix( rep(0.5, 4), ncol=4,
                           dimnames=list(NULL, paste("a", 1:4, sep=""))))

    ## different outcomes shouldn't matter.
    checkIdentical(.calcPrAction12(baz, bBaz),
                   matrix( rep(0.5, 4), ncol=4,
                          dimnames=list(NULL, paste("a", 1:4, sep=""))))

}

test_prAction <- function() {
    checkIdentical(.calcPrAction12(bar, b2), prAction(bar, b2))
    checkIdentical(.calcPrAction12(bar, b2), prAction(baz, bBaz))
}

test_.stratPredict <- function() {
    ## Test that when given action argument it is identical to the action
    checkIdentical(prAction(bar, b2), .stratPredict(bar, b2, type="actions"))

    checkIdentical(.stratPredict(bar, b2, type="terminal"),
                   matrix(c(0.5, 0.25, 0.25), ncol=3))

    checkIdentical(.stratPredict(bar, b2, type="outcomes"),
                   matrix(c(0.5, 0.25, 0.25), ncol=3,
                          dimnames=list(NULL, c("charlie", "delta", "echo"))))

    checkIdentical(prAction(baz, bBaz), .stratPredict(baz, bBaz, type="actions"))

    checkIdentical(.stratPredict(baz, bBaz, type="terminal"),
                   matrix(c(0.5, 0.25, 0.25), ncol=3))

    checkIdentical(.stratPredict(baz, bBaz, type="outcomes"),
                   matrix(c(0.75, 0.25), ncol=2,
                          dimnames=list(NULL, c("charlie", "delta"))))

}

test_.stratPredict <- function() {
    ## Test that when given action argument it is identical to the action
    checkIdentical(prAction(bar, b2), .stratPredict(bar, b2, type="actions"))

    checkIdentical(.stratPredict(bar, b2, type="terminal"),
                   matrix(c(0.5, 0.25, 0.25), ncol=3))

    checkIdentical(.stratPredict(bar, b2, type="outcomes"),
                   matrix(c(0.5, 0.25, 0.25), ncol=3,
                          dimnames=list(NULL, c("charlie", "delta", "echo"))))

    checkIdentical(prAction(baz, bBaz), .stratPredict(baz, bBaz, type="actions"))

    checkIdentical(.stratPredict(baz, bBaz, type="terminal"),
                   matrix(c(0.5, 0.25, 0.25), ncol=3))

    checkIdentical(.stratPredict(baz, bBaz, type="outcomes"),
                   matrix(c(0.75, 0.25), ncol=2,
                          dimnames=list(NULL, c("charlie", "delta"))))

}

test_.stratLogLik <- function() {
    checkIdentical(.stratLogLik(bar, b2), log(0.50))
    checkIdentical(.stratLogLik(bar, b2, individual=TRUE), log(0.50))
    checkIdentical(.stratLogLik(baz, bBaz), log(0.75))
}


## Make all test functions svTests
fxns <- ls(pattern = "^test_")
for (f in fxns) {
    assign(f, as.svTest(get(f)))
}


