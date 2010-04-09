#!/usr/bin/env Rscript
## SScore.R
## Signorino and Ritter's S-score
##

vectorNorm <- function(x, p=2, na.rm=FALSE) {

    x <- as.numeric(x)

    ## Arbitrary p-norm
    if ( p >= 1 && p < Inf ) {
        y <-  sum( abs(x)^p, na.rm=na.rm)^(1/p)
    }
    ## Not really a norm, but what the hell
    else if (p == 0) {
        y <- ifelse( any(is.na(x)) && !na.rm, NA,
                    length(x[ !(x == 0 | is.na(x))]))
    }
    ## Infinity Norm
    else if ( p == Inf) {
        y <- max(abs(x), na.rm=na.rm)
    }
    else {
        stop(paste(p, " is not a valid norm type."))
    }
    return(y)
}

SScore <- function(x, y, w=NULL, m=NULL, dist=1, na.rm=FALSE) {
    x <- as.numeric(x)
    y <- as.numeric(y)

    ## weights must be non-negative
    ## Null values still evaluate as TRUE.
    if ( any(w < 0) ) {
        stop("All elements in w must be >= 0")
    }

    ## max distance must be postive
    if ( any(m <= 0) ) {
        stop("All elements in m must be > 0")
    }

    ## Weights and max distance
    if ( is.null(w) || length(w) == 1) {
        w <- rep(1, length(x)) / length(x)
    }

    ## maximum distance is the norm of the weights
    dmax <- vectorNorm(w, p=dist)

    ## if Maximum distance not found, use empirical max
    ## Scalar doesn't matter, R will handle it
    if ( is.null(m)) {
        m <- max(cbind(x,y), na.rm=TRUE) - min(cbind(x,y), na.rm=TRUE)
    }

    return(1 - (2 / dmax) * vectorNorm( (w / m) * (x - y), p=dist, na.rm=TRUE))
}

## pr_dist2Sscore<- function(x, y=NULL, w, distance="Manhattan" )
## {
##     x <- x * w
##     if (! is.null(y))
##         y <- x * w

##     return(1 - 2 * (dist(x, y) / dist(w)))
## }
## Todo:

