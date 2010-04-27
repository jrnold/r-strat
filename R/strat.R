stratInitValues <- function(n=1) {
    rep(0, n)
}

## Return log probabilities
stratLinkInv <- function(q, ...) {
    q <- as.numeric(q)
    thresh <- -qnorm(.Machine$double.eps)
    q <- pmin(pmax(q, -thresh), thresh)
    pnorm(q, log.p=TRUE, ...)
}

##' <description>
##'
##' <details>
##' @title
##' @param formula
##' @param data
##' @param subset
##' @param na.action
##' @param agentError
##' @param privateInfo
##' @param outcomes numeric. Maps the terminal nodes to the levels of the dependent variable.
##' E.g., first element of the vector is the level of the dependent variable associated with the outcome at the first terminal node (the numbering is model dependent).  By default, the first terminal node is associated with the first level of the dependent variable, the second terminal node with the second level, etc.
##' @param formulae formula or List. Each element is a formula for the
##' player.        list(y ~ x11 | x12 | x13 , ~ 0 | 0 | x22)
##' specifies a formula where the dependent
##' @return
##' @author Jeffrey Arnold
##'
strat <- function(formula, data, subset, na.action,
                  agentError=0,
                  privateInfo=1,
                  outcomes=NULL,
                  ...
                  )
{
    ## save call for returning it later.
    fcall <- match.call(expand.dots = TRUE)

    ## Clean formula and convert to Formula
    if (is.list(formula)) {
        ## TODO: check that all elements are formula
        nPlayers <- length(formula)
        utilsPerPlayer <- sapply(formula, function(x) length(as.Formula(x))[2])

        ## Convert list to Formula
        f <- do.call(as.Formula, formula)

        ## take only the 1st dependent variable specified
        f <- Formula(formula(f, lhs=1))

    } else if (is(formula, "formula")) {
        nPlayers <- 1
        f <- Formula(formula)
        utilsPerPlayer <- length(f)[2]
    } else {
        stop("formula must either be of class formula or list")
    }

    ## Create Model Frame
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    mf$formula <- f
    mf <- eval(mf, parent.frame())
    paramLabels <- attr(attr(mf, "terms"), "term.labels")
    nParam <- length(paramLabels) + 1

    ## Outcome variable
    y <- model.response(mf)
    nOutcomes <- nlevels(y)
    outcomeLabels <- levels(y)
    y <- Matrix(y)

    ## Model Matrix
    formFull <- terms(formula(f, collapse=TRUE))
    ## ensure there is always an intercept in the matrix
    attr(formFull, "intercept") <- 1
    X <- Matrix(model.matrix(formFull, mf))
    nobs <- nrow(X)

    ## Create array indicating of non-zero parameters for each (player, outcome)
    activeParams <- array(as.logical(NA), dim=c(nParam, nOutcomes, nPlayers),
                          dimnames=list( c("(Intercept)", paramLabels), outcomeLabels, 1:nPlayers))
    for (i in seq_len(nPlayers)) {
        for (j in seq_len(nOutcomes)) {
            k <- (i - 1) + j
            mt <- terms(formula(f, rhs=k))
            hasIntercept <- as.logical(attr(mt, "intercept"))
            hasTerms <- paramLabels %in% attr(mt, "term.labels")
            activeParams[ , j, i] <- c(hasIntercept, hasTerms)
        }
    }

    ## Create initial values
    paramArray <- activeParams * stratInitValues(length(activeParams))

    ## Parameters as a vector (as passed to maxLik)
    params <- as.numeric(paramArray)

    ## For testing assume a 1-2 binary choice model with independent private info errors = N(0, 0.5)
    llik <- function(B, X, y) {
                     nEdges <- 4
                     players <- 2

                     y <- as.numeric(y)

                     B <- array(B, c(ncol(X), length(unique(y)), players))

                     ## Calculate probabilities for each action
                     pAct <- matrix(NA, ncol=nEdges, nrow=nobs)
                     pAct[ , 3] <- stratLinkInv(X %*% ( B[ , 2, 2] - B[ , 3, 2]), sd=1)
                     pAct[ , 4] <- log(1 - exp(pAct[ , 3]))
                     pAct[ , 1] <- stratLinkInv( pAct[, 3] * (X %*% B[ , 2, 1])
                                                + pAct[ , 4] * (X %*% B[ , 2, 2]),
                                                sd=sqrt(pAct[ , 3]^2 + pAct[ , 4]^2 + 1))
                     pAct[ , 2] <- log(1 - exp(pAct[ , 1]))

                     ## prob for each terminal node
                     paths <- list(c(1), c(2,3), c(2,4))
                     pNodes <- sapply( paths, function(idx) apply(pAct[ , idx, drop=FALSE],
                                                                  1, prod))

                     ## Outcome probs (assume 1-1 terminal nodes to outcomes for now)
                     pOut <- pNodes

                     ## Individual obs log-likelihoods
                     ll <- mapply(function(i, j) pOut[i, j], seq_len(nobs), y)

                     ## Total logLik
                     -1 * sum(ll)
                 }
    maxBFGS(llik, start=params, print.level=0, fixed=as.logical(activeParams),
          X=X, y=y)

    ## TODO: convert paramArray to vector,
    ## TODO: convert paramArray to vector with non-fixed variables
    ## TODO: should outcomes be the numeric value of the factor, or the level (string).

    ## Values are logical indicating whether the term is included
    ## of dimension #player * #outcome * #(union of all terms, intercept)

    ## TWO methods
    ## Evaluate all model matrices first (lots of data)
    ## Use a single model matrix

}

