#!/usr/bin/env Rscript
### runit.SScore.R
###
### Title: Unit Tests for SScore.R
### Author: Jeffrey Arnold <jarnold7@mail.rochester.edu>
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Copyright (C): 2008  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

if (FALSE) {
    library(RUnit)
}

test.vectorNorm <- function() {

    foo <- c(-2, -1, 0, 1, 2)
    bar <- c(4, 3)
    baz <- c(-4, -3)
    qux <- c("foo", "bar")
    quux <- c(1, 0, NA)

    checkEqualsNumeric(vectorNorm(foo, p=0), 4)
    checkEqualsNumeric(vectorNorm(foo, p=1), 6)

    checkEqualsNumeric(vectorNorm(bar), 5)
    checkEqualsNumeric(vectorNorm(bar, p=2), 5)
    checkEqualsNumeric(vectorNorm(baz, p=2), 5)

    checkEqualsNumeric(vectorNorm(bar, p=3), 91^(1/3))
    checkEqualsNumeric(vectorNorm(baz, p=3), 91^(1/3))
    checkEqualsNumeric(vectorNorm(baz, p=Inf), 4)
    checkEqualsNumeric(vectorNorm(baz, p=Inf), 4)

    checkException(vectorNorm(foo, p=-1))
    checkException(vectorNorm(foo, p=1/4))
    checkException(vectorNorm(foo, p="foo"))

    checkEquals(vectorNorm(qux), as.numeric(NA))
    checkEqualsNumeric(vectorNorm(qux, na.rm=TRUE), 0)
    checkEqualsNumeric(vectorNorm(quux), as.numeric(NA))
    checkEqualsNumeric(vectorNorm(quux, na.rm=TRUE), 1)
    checkEqualsNumeric(vectorNorm(quux, p=0), NA)
    checkEqualsNumeric(vectorNorm(quux, p=0, na.rm=TRUE), 1)
}


test.SScore.ex1 <- function() {
    ## TODO - how to make it better?
    A <- c(3,2,3,0,3,3,1,2)
    B <- c(2,3,0,3,3,3,1,3)
    w <- c(0.5, 0.4, 0.05, 0.05, 0.25, 0.25, 0.25, 0.25)
    m <- cbind(rep(3,4), rep(2,4))

    checkEqualsNumeric(SScore(A, B, w=w, m=m), 0.475)
}

test.SScore.table5 <- function() {

    ## (a)
    a <- matrix( c(0, 3, 1, 2, 2, 1, 3, 0), ncol = 2, byrow=TRUE)
    checkEqualsNumeric(SScore(a[,1], a[,2], m=3), -1/3)

    ## (b)
    b <- matrix( c(1, 3, rep(c(2,2), 2), 3, 1), ncol = 2, byrow=TRUE)
    checkEqualsNumeric(SScore(b[,1], b[,2], m=3), 1/3)

    ## (c)
    c <- matrix( c(rep(c(2,3),2), rep(c(3,2),2)), ncol=2, byrow=TRUE)
    checkEqualsNumeric(SScore(c[,1], c[,2], m=3), 1/3)

    ## (d)
    d <- matrix( c(rep(c(3,3),4)), ncol=2, byrow=TRUE)
    checkEqualsNumeric(SScore(d[,1], d[,2], m=3), 1)

    ## (e)
    e <- matrix( c(rep(c(0,0), 10), 0, 3, 3, 0), ncol=2, byrow=TRUE)
    checkEqualsNumeric(SScore(e[,1], e[,2], m=3), 2/3)

    ## (f)
    f <- matrix( c(0, 3, rep(c(1,1), 10), 3, 0), ncol=2, byrow=TRUE)
    checkEqualsNumeric(SScore(f[,1], f[,2], m=3), 2/3)

    ## (g)
    g <- matrix( c(0, 3, rep(c(2,2),10), 3, 0), ncol=2, byrow=TRUE)
    checkEqualsNumeric(SScore(g[,1], g[,2], m=3), 2/3)

    ## (h)
    h <- matrix( c(0, 3, 3, 0, rep(c(3,3), 10)), ncol=2, byrow=TRUE)
    checkEqualsNumeric(SScore(h[,1], h[,2], m=3), 2/3)
}

## Table 6 of Signornino and Ritter (1999) p. 134
test.SScore.table6 <- function() {

    ## Table 6 says -0.45 but I get -0.46. This might be due to
    ## rounding errors in the System Cap scores presented in the table.
    ## Unweighted
    checkEqualsNumeric(SScore(alliances1914$GMY, alliances1914$RUS, m=3), 0.4)

    ## weighted
    checkEqualsNumeric(SScore(alliances1914$GMY, alliances1914$RUS,
                              w=alliances1914$Cap, m=3), -0.46)

}

## Tables 7 and 8 of Signorino and Ritter (1999) p. 136-138
## test.SScore.table8 <- function() {

##     colState <- c("USA", "UK", "FRN")
##     rowState <- c("UK", "FRN", "RUN")
##     allyOnly <- matrix(c(-0.59, -0.58, -0.70,
##                          -0.58, 0.99, 0.87,
##                          -0.70, 0.87, 0.88), byrow=TRUE, ncol=3,
##                        dimnames=list(rowState, colState))
##     withUNVoting <- matrix(c(-0.01, 0.10, -0.60,
##                              0.10, 0.73, 0.19,
##                              -0.60, 0.19, 0.30), byrow=TRUE, ncol=3,
##                        dimnames=list(rowState, colState))

##     w <- c(alliances1947$SysCap, rep(1, ncol(UNVoting1947)) / ncol(UNVoting1947))
##     m <- rep(3, ncol(alliances1947) + ncol(UNVoting1947))

##     checkEqualsNumeric(SScore(allyOnly[,"USA"], allyOnly[,"UK"], w=allyOnly$SysCap), -0.59)
##     checkEqualsNumeric(SScore(allyOnly[,"USA"], allyOnly[,"FRN"], w=allyOnly$SysCap), -0.59)
##     checkEqualsNumeric(SScore(allyOnly[,"USA"], allyOnly[,"RUN"], w=allyOnly$SysCap), -0.59)

## }




