### StratTree.R
###
### Author:jeffrey.arnold@gmail.com
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description: Methods and functions related to the StratActionNode,
### StratTerminalNode, and StratTree classes
###
### Copyright (C): 2009  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

StratActionNode <- function(player, error) {
    new("StratActionNode", player=player, error=error)
}
StratTerminalNode <- function(outcome, error) {
    new("StratTerminalNode", outcome=outcome, error=error)
}

StratTree <- function(nodes, parents) {
    new("StratTree",
        .Data=nodes,
        names = names(nodes),
        parents=parents)
}

#' Return a character vector of Terminal nodes in a StratTee.
#'
#' @param StratTree
#'
#' @return character vector of names of terminal nodes.
setMethod("getTerminalNodes",
          signature=(c("StratTree")),
          function(object) {
              return(object@label[sapply(object@children, is.null)])
          })

#' Return a character vector of the Action nodes in a StratTree.
#'
#' @param StratTree
#'
#' @return character vector of names of action nodes.
.getActionNodes <- function(object)
{
    isAction <- lapply(object, class) == "StratActionNode"
    return(names(object)[isAction])
}
setMethod("getActionNodes", signature=(c("StratTree")), .getActionNodes)

#' Return a character vector of the Terminal nodes in a StratTree.
#'
#' @param StratTree
#'
#' @return character vector of names of terminal nodes.
.getTerminalNodes <- function(object)
{
    isTerminal <- lapply(object, class) == "StratTerminalNode"
    return(names(object)[isTerminal])
}

setMethod("getTerminalNodes",
          signature=(c("StratTree")), .getTerminalNodes)

#' Return a list of the children for each node.
#'
#' @param StratTree
#'
#' @return list of character vectors of the children of each node.
.getChildren <- function(object)
{
    children <- lapply(names(object),
                       function(i) {
                           names(object)[ which(object@parents == i) ]
                       })
    names(children) <- names(object)
    return(children)
}
setMethod("getChildren",
          signature=(c("StratTree")), .getChildren)

#' List of Outcomes matched to each terminal Node.
#'
#' @param object StratTree-class
#'
#' @return character elements named for each terminal node.
.getOutcomes <- function(object)
{
    isTermNode <- names(object) %in% getTerminalNodes(object)
    outcomes <- sapply(object[isTermNode], function(i) slot(i, "outcome"))
    return(outcomes)
}
setMethod("getOutcomes", "StratTree", .getOutcomes)

#' List of unique players in the Game
#'
#' @param object StratTree-class
#'
#' @return character of the names of the players in the game.
.getPlayers <- function(object)
{
    isAction <- names(object) %in% getActionNodes(object)
    players <- sapply(object[isAction], function(i) slot(i, "player"))
    return(unique(players))
}
setMethod("getPlayers", "StratTree", .getPlayers)

#' List of Terminal Nodes matched to each output.
#'
#' @param object StratTree-class
#'
#' @return List. Each element is named for an outcome and has
#' as its values the terminal nodes of that outcome.
.getOut2Term <- function(object) {
    tNodes <- getOutcomes(object)
    outcomes <- unique(tNodes)
    out2nodes <- lapply(outcomes, function(i) names(tNodes)[tNodes == i])
    names(out2nodes) <- outcomes
    return(out2nodes)
}


#' Return the name of the root node of a StratTree
#'
#' @param StratTree
#'
#' @return list of character vectors of the children of each node.
setMethod("getRootNode",
          signature=(c("StratTree")),
          function(object) {
              isRoot <- sapply(object@parents, is.na)
              return(names(object)[isRoot])
          })

.areYouMyMommy <- function(object, node) {
    parent <- object@parents[ names(object) == node][[1]]
    isRoot <- is.na(parent)
    if (! isRoot) {
        return( c(node, .areYouMyMommy(object, parent)))
    } else {
        return(node)
    }
}

#' Return paths from an StratActions class object.
#'
#' @param object StratActions class object.
#'
#' @return a list of character vectors with the paths from the root node to each
#' terminal node.
setMethod("getPaths",
          signature=(c("StratTree")),
          function(object) {
              Qt <- getTerminalNodes(object)
              P <- lapply(Qt, function(i) .areYouMyMommy(object, i))
              names(P) <- Qt
              return(P)
          })


