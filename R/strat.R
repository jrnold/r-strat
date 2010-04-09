### strat.R
### Author: Jeffrey.Arnold <jarnold7@mail.rochester.edu>
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description:  Functions and methods related to the creation of StratMLE class
### objects, i.e. the estimation of the strategic mode.
###
### Copyright (C): 2009  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

#' Calculate utilities from StratModel object and coefficients
#'
#' @param object StratModel object
#' @param b numeric.  named  vector of coefficients.
#' @param expand if TRUE, then all (outcome, player) pairs are included in the list with
#' outcome player pairs not in slot X of object given a value of 0.
#'
#' @return list of numeric elements each named "outcome.player".
.calcUtility <- function(object, b) {

    ## Coefficients
    coefNames <- getCoefNames(object)

    pInfo <- lapply(object, function(i) slot(i, "error"))
    U <- lapply(object, function(i) slot(i, "U"))
    U[ sapply(U, length) == 0 ] <- as.matrix(0)

    ## coerce the coefficients into the right list structure
    bravo <- vector(length = length(U), mode="list")
    ## Column first
    k <- 1
    for ( i in seq_len(dim(object)[2])) {
        for (j in seq_len(dim(object)[1])) {
            nomen <- coefNames[[j, i]]
            if ( ! is.null(nomen)) {
                bravo[[k]] <- b[ nomen ]
            } else {
                bravo[[k]] <- 0
            }
            k <- k + 1
        }
    }

    U <- mapply(function(x, y) as.numeric(as.matrix(x) %*% as.matrix(y)),
                U, bravo,
                SIMPLIFY=FALSE)
    print(lapply(U, head))

    U <- array(mapply(function(u, p) RndUtilNum(u, p), U, pInfo),
               dim = dim(object),
               dimnames = dimnames(object))
    return(new("StratUtility",
             U, Y=object@Y, tree=object@tree))

}


#' Generate mle starting values
#'
#' The default option is uniformly distributed numbers on (0, 0.5)
#' These are the options allowed in STRAT, with the addition
#' of the 0 vector, which is used in Zelig.
#'
#' @param n integer. number of parameters to generate.
#' @method = 1. Predefined method to use for generating starting values.
#' @return numeric vector of length n.
stratStVal <- function(n, method=1)
{
    return(switch(method,
              { rep(0, n) },
              { runif(n) * 0.5 },
              { runif(n) },
              { rnorm(n) },
                  stop(paste(method, " is not suppored"))))
}


.stratPredict <- function(object, b, type="outcomes") {
    ## convert to StratUtility Object
    object <- .calcUtility(object, b)

    ## Calculate the probabilities for each action
    pMatrix <- .prAction(object)


    if (type %in% c("terminal", "outcomes")) {
        aPaths <- getPaths(object@tree)
        pMatrix <- do.call( "cbind",
                           lapply(aPaths,
                                  function(i)
                                  ## need to remove the root node from the path
                                  apply(subset(pMatrix, ,i[1:(length(i)-1)]),
                                        1, prod)))
        colnames(pMatrix) <- names(aPaths)
        if (type == "outcomes") {
            out2nodes <- .getOut2Term(object@tree)
            pMatrix <- do.call("cbind",
                               lapply(out2nodes,
                                      function(i)
                                      rowSums(subset(pMatrix, ,i))))
            colnames(pMatrix) <- names(out2nodes)
        }
    }
    return(pMatrix)
}

.stratLogLik <- function(object, b, individual=FALSE) {
    ll <- rowSums(log(.stratPredict(object, b)[ , colnames(object@Y)]) * object@Y)
    if (! individual) {
        ll <- sum(ll)
    }
    return(ll)
}


#' Calculate utilities and probabilities for all actions in a Game
#'
#' @param actions StratTree.
#' @param utilities list of StratUtilities
#' @param node Name of node to use as the root node to calculate proabilities.
#' Assumed to be the root node of the
#' @param p list of existing probaibilities.  Since the function is recursive,
#' this argument is used to build the list of probabilities for all children
#' nodes.
#'
#' @return list with elements
#' * p numeric vector with probability for each action.
#' * U array of StratUtility objects for each player and outcome
.walkGameTree <- function(object, node=getRootNode(actions))
{
    results <- vector(mode="list", length=2)
    names(results) <- c("U", "p")

    actions <- object@tree

    if (node %in% getActionNodes(actions) ) {
        children <- getChildren(actions)[[ node ]]

        ## name of player at the
        player <- actions[[ node ]]@player

        ## Over all children call .walkGameTree
        ## This calculates the probabilities and utilities
        ## for all children nodes.
        ## note - sapply makes the results into a 2D array.
        ## rows = (p, U)
        childUtil <- sapply(children,
                           function(i) {
                               .walkGameTree(object, node=i)
                           })

        ## extract probabilities from the results
        p <- childUtil[ "p", ]
        ## Needed so that names only are of children nodes
        names(p) <- NULL
        ## concatenates lists and removes empty probabilities
        p <- do.call("c", p)

        ## extract utilities from the results
        ## make children utilities into a 2D array
        ## rows are outcomes, columns are players
        ## This simplifies future computations
        ## elements of the array
        players <- dimnames(object)[[2]]
        iPlayer <- which(player == players)
        uDim <- c(length(children), length(players))
        childUtil <- array(unlist(childUtil[ "U", ]),
                        dim=uDim, dimnames=list(children, players))

        ## Only the player at the action node  matters
        ## take only his utilities
        playerU <- unlist(childUtil[ , iPlayer])

        ## extract agent errors from the StratTree object
        alpha <- lapply(actions, function(i) slot(i, "error"))
        ## only include the errors of children.
        alpha <- alpha[ children ]

        ## Calculate the Probabilities of choosing each action
        childP <- .stratChoice(playerU, alpha)
        names(childP) <- children

        ## The utility for the node is the sum of the children utilities
        ## weighted by the probability of choosing each child.
        ## Multiply each child utility by its probability
        childUtil <- apply(childUtil, 2, function(i) mapply("*", i, childP))

        ## Sum weighted utilities of each player over all child nodes.
        results$U <- lapply(childUtil, function(i) .sumRndUtilNum(i))

        results$p <- c(childP, p)
    } else {
        ## This is a terminal node.
        ## Grab the utilities outcome matched to that terminal node.
        results$p <- list()
        outcome <- actions[[ node ]]@outcome
        results$U <- object[ outcome,  ]
    }
    return(results)
}


#' Calculate the probabilities given utilities and agent errors
#'
#' Currently assumes two choices and normal distributions.
#'
#' @param U list of StratUtility objects.
#' @param alpha numeric list of agent errors.
#'
#' @result numeric vector of probabilities for each action.
.stratChoice <- function(U, alpha) {
    utils <- ( U[[1]]@U - U[[2]]@U )
    v <- sum(unlist(c(U[[1]]@error, U[[2]]@error, alpha)))
    p <- pnorm( utils / sqrt(v))
    p <- list(p, 1 - p)
    return( p)
}

#' Calculate probabilities for a strategic model with
#' an arbitrary tree.
.prAction  <- function(object) {
    p <- do.call("cbind", .walkGameTree(object)$p)
    return(p)
}

StratParser <- function(formulae,
                        tree,
                        data=parent.frame(),
                        subset=NULL,
                        weights=NULL,
                        na.action="na.omit",
                        model=FALSE)
{
    ## naming each top-level list for its dependent variable
    names(formulae) <- lapply(formulae,
                         function(i) {
                             as.character(i[[1]][[1]][2])
                         })

    pf <- parent.frame()
    StratParser

    outcomes <- unique(getOutcomes(tree))
    nOutcomes <- length(outcomes)

    players <- getPlayers(tree)
    nPlayers <- length(players)

    uOutcomes <- rep(outcomes, nPlayers)
    nDim <- nPlayers * nOutcomes

    .makePlayerOutcomeArray <- function(foo) {
        array(foo, dim=c(nOutcomes, nPlayers),
              dimnames=list(outcomes, players))
    }

    ## Create arrays of private information and outcomes
    ## This checks whether the player outcome combinations provided
    ## by the user are valid.  The array will also make it
    ## convenient in the next step to check and fill in missing
    ## variances and utilities.

    ## initialize pInfo and utility arrays
    pInfo <- array(dim=c(nOutcomes, nPlayers),
                   dimnames=list(outcomes, players))
    ## This needs to be initialized with expressions for formulae
    ## to be inserted. otherwise errors occur due to differing lengths.
    utilities <- array(list(expression()),
                       dim=c(nOutcomes, nPlayers),
                       dimnames=list(outcomes, players))

    for (i in names(formulae)) {
        for (j in names(formulae[[i]])) {
            if (i %in% outcomes & j %in% players) {
                utilities[[i, j]] <- formulae[[ c(i, j) ]][[1]]
                if (length(formulae[[ c(i, j) ]]) > 1) {
                    pInfo[[i, j]] <- formulae[[ c(i, j) ]][[2]]
                }
            } else {
                ## TODO: I could just ignore these
                stop( "Player ", i, " and outcome ", j,
                     "are not in this game.")
            }
        }
    }

    ## Complete the array by filling in any missing values
    for (i in outcomes) {
        for (j in players) {
            if (length(utilities[[ i, j]]) == 0) {
                utilities[[i, j]] <- as.formula( paste(i, 0, sep="~"))
            }
            if (is.na(pInfo[[ i, j]])) {
                pInfo[[i, j]] <- 0
            }
        }
    }


    ###  Model Frame #####################################
    mf <- formulasFrame(unlist(utilities),
                        data=data, subset=subset,
                        weights=weights, na.action=na.action,
                        one.frame=FALSE)

    ## Model Terms
    mt <- lapply(mf, function(i) attr(i, "terms"))
    # xlevels
    xlevels <- mapply(.getXlevels, mt, mf)

    ## Response variables
    Y <- do.call("cbind", lapply(mf[ ! duplicated(uOutcomes)],
                                 model.response))
    colnames(Y) <- unique(uOutcomes)

    ## number of observations
    nobs <- nrow(Y)

    ## Design Matrices
    X <- mapply(model.matrix, mt, mf, simplify=FALSE)

    ## Create Strat object
    sMat <- mapply(RndUtilMatrix, X, pInfo, SIMPLIFY=FALSE)
    sMat <- new("StratMatrix",
                  .makePlayerOutcomeArray(object),
                  Y = Y,
                  tree = tree)

    ## contrasts (saved for results)
    contrasts <- lapply(X, function(i) attr(i, "contrasts"))

    ## Return Description of the model
    modelList <- mapply(ModelFrame4,
                        mt, contrasts, xlevels, rep(na.action, nDim), mf)
    if (! model) {
        modelList <- lapply(modelList,
                            function(i) new("ModelFrame4", i, model.frame = NULL))
    }
    sModel <- mapply(RndUtilFormula,
                        modelList, pInfo)
    sModel <- .makePlayerOutcomeArray(modelList)
    sModel <- new("StratModel", .Data=modelList, tree=tree)

    return(list(StratMatrix=sMat, StratModel=sModel))
}

## Function:
strat <- function(formulae,   # includes utilities and private errors
                  tree,
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

    #### Preprocessing of the formulae

    ## TODO: Call StratParser

    ## TODO: I no longer drop empty utilities - I will need to handle
    ## this in the multiplication by coefficients part
    ## Create random utility object

    ## free up some memory - TODO (is this necessary?)
    rm(X, Y)

    ## Get Names of Coefficients for the model
    coefNames <- unlist(getCoefNames(object))

    ## Log-likelihood Function
    .llik <- function() {

        ## The only arguments to this step are each of the coefficients intialized
        ## to their starting values.
        b <- sapply(names(formals()), function(i) get(i))
        ## b <- sapply(coefNames, function(x) get(x))
        names(b) <- coefNames

        ll <- -1 * .stratLogLik(object, b)

        return(ll)
        ## remember to return negative log-likelihood!!!
    }

    ## sets the coefficient names as the arguments
    ## - initializing them with starting values
    nCoef <- length(coefNames)
    if (is.null(start)) {
        start <- stratStVal(nCoef)
    }
    start <- as.list(start)
    names(start) <- coefNames
    formals(.llik) <- start

    ## The optimization occurs here
    results <- mle(.llik, method=method)

    results@call <- fcall

    return(results)
}

## .stratPredict <- function(object, newdata, na.action="na.pass") {
##     mf <- formulasFrame(object@terms, newdata, na.action = na.action)
##     for (i in seq_len(length(mf))) {
##         if( ! is.null(cl <- attr(object@terms[i]))) .checkMFClasses(cl, mf[i])
##     }
##     ## TODO: account for xlevels

##     X <- mapply(model.matrix, mt, mf, object@contrasts, simplify=FALSE)
##     ## Drop model.matrices without columns, i.e. 0 utilities
##     not0 <- sapply(X, function(i) ncol(i) > 0)
##     X <- X[not0]
##     oResponses <- oResponses[not0]
##     oPlayers <- oPlayers[not0]

##     Y <- do.call("cbind", lapply(mf[ ! duplicated(oResponses)],
##                                  model.response))
##     colnames(Y) <- unique(oResponses)

##     object <- new(class(object),
##                   object,
##                   X = X,
##                   Y = Y)


## }


setMethod("coef", signature=c("StratMLE"),
          function(object) object@coef)
setMethod("formula", signature=c("StratMLE"),
          function(x) lapply(x@terms, formula))
## setMethod("model.frame") TODO
## setMethod("model.matrix") TODO
setMethod("terms", signature=c("StratMLE"),
          function(x) x@terms)

setMethod("logLik", signature=c("StratMLE"),
          function(object) {
              ll <- logLik(as(object, "mle"))
              ## by default mle log likelihood does not include
              ## the number of observations
              attr(ll, "nobs") <- attr(ll, "nall") <- object@nobs
              return(ll)
          })

## Methods to implement
## These are the methods suggested for all model fitting functions
## as well as a few implemented in lme4 that are relevant.
## fitted
## model.frame
## model.matrix
## print
## resid(uals)
## show
## update
## with
## weights


