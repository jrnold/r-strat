### StratModel.R
###
### Author: Jeffrey B. Arnold <jarnold7@mail.rochester.edu>
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description:
###
### Copyright (C): 2009  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

setMethod("getTermNames", "StratMatrix",
          function(object) {
              xTerms <- array(lapply(object, function(i) colnames(i@U)),
                              dim(object), dimnames(object))
              return(xTerms)
          })

setMethod("getCoefNames", "StratMatrix",
          function(object) {
              xTerms <- getTermNames(object)
              for (i in dimnames(xTerms)[[1]]) {
                  for ( j in dimnames(xTerms)[[2]]) {
                      if ( ! is.null(xTerms[[i, j]])) {
                          xTerms[[i, j ]] <- paste(i, j, xTerms[[i, j]], sep=".")
                      }
                  }
              }
              return(xTerms)
          })

