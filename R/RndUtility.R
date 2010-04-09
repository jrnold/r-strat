### StratUtility.R
###
### Author:jeffrey.arnold@gmail.com
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description: Methods and functions for the StratUtility Class
###
### Copyright (C): 2009  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

setMethod("+", signature=c("RndUtilNum", "RndUtilNum"),
          function(e1, e2) {
              e1@U <- e1@U + e2@U
              e1@error <- e1@error + e2@error
              return(e1)
          })

setMethod("+", signature=c("RndUtilNum", "numeric"),
          function(e1, e2) {
              e1@U <- e1@U + e2
              return(e1)
          })

setMethod("+", signature=c("numeric", "RndUtilNum"),
          function(e1, e2) return(callGeneric(e2, e1)))


setMethod("*", signature=c("RndUtilNum", "numeric"),
          function(e1, e2) {
              e1@U <- e1@U * e2
              e1@error <- e1@error * e2^2
              return(e1)
          })
setMethod("*", signature=c("numeric", "RndUtilNum"),
          function(e1, e2) callGeneric(e2, e1))

ModelFrame4 <- function(terms, contrasts, xlevels, na.action, model.frame)
{
    return(new("ModelFrame4",
               terms = terms,
               contrasts = contrasts,
               xlevels = xlevels,
               na.action = na.action,
               model.frame = model.frame))
}


RndUtilFormula <- function(U, error) {
    new("RndUtilFormula", U=U, error=error)
}

RndUtilMatrix <- function(U, error) {
    new("RndUtilMatrix", U=U, error=error)
}

RndUtilNum <- function(U, error)
{
    new("RndUtilNum", U=U, error=error)
}

### Calculates the sum of a list of RndUtilNum objects
.sumRndUtilNum <- function(x) {
    U <- sum(sapply(x, function(i) slot(i, "U")))
    pinfo <- sum(sapply(x, function(i) slot(i, "error")))
    return(RndUtilNum(U, pinfo))
}
