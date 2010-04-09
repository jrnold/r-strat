## TODO:
## * lots of clean-up
## * separate creation of model.frame, model.matrix, and Y variables
## * add xlevels, terms, contrasts to output
## * alter onemodelframe return a single model frame with multiple terms.

### S3 Class
## setOldClass("data.frame")
setOldClass("data.frame")
setOldClass("terms")

### Helper Classes
setClassUnion("CharacterOrNull", c("character","NULL"))
setClassUnion("NumericOrNull", c("numeric","NULL"))
setClassUnion("MatrixOrNull", c("matrix","NULL"))
setClassUnion("ListOrNull", c("list","NULL"))
setClassUnion("DataFrameOrNull", c("data.frame", "NULL"))

## Generics
setGeneric("getCoefNames",
           function(object, ...) standardGeneric("getCoefNames"))

## Model.Frame information
setClass("ModelFrame4",
         representation(terms = "terms",
                        na.action = "character",
                        model.frame = "DataFrameOrNull"))

## Strat Model
setClass("StratModel",
         contains=c("array"),
         representation(modelName="character",
                        sigmaA="numeric"))

## Strat Model
setClass("StratMf",
         contains=c("array"),
         representation(modelName="character",
                        sigmaA="numeric"))

## Strat Matrix
## I allow null Y's to make it easier to pass arguments to pr functions.
setClass("StratMatrix",
         contains=c("array"),
         representation(modelName="character",
                        sigmaA="numeric",
                        Y="MatrixOrNull"))

## Strat MLE class extends the stats-4 mle class
setClass("Strat",
         representation(
                        "mle",
                        model = "StratModel",
                        nobs = "integer"))


###' Starting values for the MLE
.stratStVal <- function(n) {
    runif(n) * 0.5
}

stratModelPath_1.2 <- function() {
    list(1, c(2,3), c(2,4))
}

.stratPrAction_1.2<- function(object, par)
{
    Beta <- relist(par, getCoefNames(object))
    mu <- mapply(function(x, y) as.matrix(x) %*% as.matrix(y),
                 object, Beta)

    sigmaA <- object@sigmaA
    sigmaP <- object@sigmaP

    U11 <- 0
    U12 <- mu[ , "u1"]
    U13 <- mu[ , "u2"]
    U22 <- 0
    U23 <- mu[ , "u3"]

    p3mu <- U22 - U23
    p3A <- 2 * sigmaA[2]
    p3P <- 2 * sigmaP[2]
    p3 <- pnorm(p3mu / sqrt( p3A + p3P))

    ## Probability for the choice of player 3
    p1mu <- U11 - p3 * U12  - (1 - p3) * U13
    p1P <- sigmaP[1] * ( 1 + p3^2 + (1 - p3)^2)
    p1A <- 2 * sigmaA[1]
    p1 <- pnorm(p1mu / sqrt(p1P + p1A))

    pMatrix <- cbind(p1, (1 - p1), p3, (1 - p3))
    colnames(pMatrix) <- paste("p", 1:4, sep="")
    return(pMatrix)
}

#' Invert vector, treating it as a linear transformation
#'
#' @param object StratModel-class
#'
#' @return List. Each element is named for an outcome and has
#' as its values the terminal nodes of that outcome.
.invertVector <- function(foo) {
    bar <- unique(foo)
    baz <- lapply(bar, function(x, y) which(x == y), foo)
    names(baz) <- bar
    return(baz)
}

.stratPrTerminal <- function(object, par)
{
    pMatrix <- .stratPrAction(object, par)
    pMatrix <- lapply(object@paths, function(x) as.matrix(pMatrix[ , x]))
    pMatrix <- sapply(pMatrix, function(x) apply(x, 1, prod))
    return(pMatrix)
}

.stratPrOutcome <- function(object, par)
{
    pMatrix <- .stratPrTerminal(object, par)
    outcomes <- object@outcomes
    if (length(outcomes) != length(unique(outcomes))) {
        tNodes <- .invertVector(outcomes)
        pMatrix <- lapply(tNodes, function(x) as.matrix(pMatrix[ , x]))
        pMatrix <- sapply(pMatrix, function(x) apply(x, 1, sum))
    }
    colnames(pMatrix) <- outcomes
    pMatrix <- pMatrix[ , unique(outcomes)]
    return(pMatrix)
}

.stratObsLogLik <- function(object, par)
{
    pMatrix <-  .stratPrOutcome(object, par)
    ll <-  rowSums(object@Y[ , unique(object@outcomes)] * log(pMatrix))
    return(ll)
}

.stratLogLik <- function(object, par)
{
    ll <-  sum(.stratObsLogLik(object, par))
    return(ll)
}

stratParser <- function(formulae,
                        data=parent.frame(),
                        subset=NULL,
                        weights=NULL,
                        na.action="na.omit",
                        model=FALSE,
                        outcomes,
                        sigmaA,
                        sigmaP)
{
    nEq <- length(formulae)

    mf <- formulasFrame(formulae,
                        data=data, subset=subset,
                        weights=weights, na.action=na.action,
                        one.frame=FALSE)

    ## Model Terms
    mt <- lapply(mf, function(i) attr(i, "terms"))

    ## Response variables - all should be the same
    Y <- data.frame(y = model.response(mf[[1]]))
    yMf <- model.frame( ~ y - 1, data=Y)
    yMt <- attr(yMf, "terms")
    yLevels <- .getXlevels(yMt, yMf)$y
    Y <- model.matrix( yMt, yMf, contrasts=FALSE)
    colnames(Y) <- yLevels
    attr(Y, "levels") <- yLevels

    if (is.null(outcomes)) {
        outcomes <- attr(Y, "levels")
    }

    ## Design Matrices
    X <- mapply(model.matrix, mt, mf, SIMPLIFY=FALSE)

    sModel <- new("StratModel",
              sigmaA=sigmaA,
              sigmaP=sigmaP,
              outcomes=outcomes,
              paths=list(1, c(2,3), c(2,4)),
              modelName="1-2"
              )
    mf <- new("StratMf", sModel, mf)
    XY <- new("StratMatrix", sModel, X, Y=Y)

    return(list(model=mf, matrix=XY))
}

setMethod("getCoefNames", "StratMatrix",
          function(object)
      {
          cNames <- lapply(object, colnames)
          cNames <- mapply(paste, names(object), cNames,
                           MoreArgs=list(sep=":"), SIMPLIFY=FALSE)
          return(cNames)
      })


strat <- function(formulae,   # includes utilities and private errors
                  sigmaA=c(0,0),
                  sigmaP=c(1,1),
                  outcomes=NULL,
                  ## model.frame arguments
                  data=parent.frame(),
                  subset=NULL,
                  weights=NULL,
                  na.action="na.omit",
                  model=FALSE,  ## return the model
                  ## optim/mle arguments
                  start=NULL,
                  method="BFGS",
                  ...)
{
    ## save call for returning it later.
    fcall <- match.call(expand.dots = TRUE)

    mf <- stratParser(formulae=formulae,
                      data=data,
                      subset=subset,
                      weights=weights,
                      na.action=na.action,
                      model=model,
                      sigmaA=sigmaA,
                      sigmaP=sigmaP,
                      outcomes=outcomes)

    object <- mf$matrix

    ## Get Names of Coefficients for the model
    coefNames <- unlist(getCoefNames(object))

    ## Log-likelihood Function
    .llik <- function() {

        ## The only arguments to this step are each of the coefficients initialized
        ## to their starting values.
        b <- sapply(names(formals()), function(i) get(i))
        ## b <- sapply(coefNames, function(x) get(x))
        names(b) <- coefNames

        ll <- -1 * .stratLogLik(object, b)

        return(ll)
    }

    ## sets the coefficient names as the arguments
    ## - initializing them with starting values
    nCoef <- length(coefNames)
    if (is.null(start)) {
        start <- .stratStVal(nCoef)
    }
    start <- as.list(start)
    names(start) <- coefNames
    formals(.llik) <- start

    ## The optimization occurs here
    results <- mle(.llik, method=method, ...)

    results@call <- fcall
    results <- new("Strat",
                   results,
                   model = mf$model,
                   nobs = nrow(object@Y))

    return(results)
}

.stratPredict <- function(object, newdata, type="outcomes", ...) {
    b <- object@fullcoef

    formulae <- lapply(foo@model, function(i) attr(i, "terms"))
    newObject <- stratParser(formulae,
                          outcomes=object@model@outcomes,
                          sigmaA=object@model@sigmaA,
                                    sigmaP=object@model@sigmaP,
                          data=newdata,
                          na.action="na.pass", ... )
    if (type == "outcomes") {
        p <- .stratPrOutcome(newObject$matrix, b)
    } else if (type == "terminal") {
        p <- .stratPrTerminal(newObject$matrix, b)
    } else if (type == "actions") {
        p <- .stratPrAction(newObject$matrix, b)
    } else {
        stop("type=", type, " is not a valid argument.")
    }

    return(p)
}
setMethod("predict", signature=c("Strat"), .stratPredict)

