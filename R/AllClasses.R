#!/usr/bin/env Rscript
### AllClasses.R
###
### Title: S4 Classes
### Author: Jeffrey Arnold <jarnold7@mail.rochester.edu>
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description: All S4 classes defined in package rino
###
### Copyright (C): 2008  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

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

### Strategic Models
## setClass("StratModel",
##          representation(
##                         ## Outcomes/Utilities
##                         Y="matrix",  ## List of responses for each (outcome, player)
##                         X="list",  ## List of design matrices for each (outcome, player)
##                         pinfo="list",  ## List of private information for each (outcome, player)
##                         oPlayers="character",  ## vector of players for
##                         oResponses="character", ## list of outcome variables
##                         ## Structure of the model
##                         tree="StratTree")) ## Game Tree
##                         ## players="character", ## names of unique players
##                         ## outcomes="character", ## names of unique outcomes
##                         ## tNodes="character",  ## the outcome for each terminal node
##                         ## paths="list"))


## Model.Frame information
setClass("ModelFrame4",
         representation(
                        terms = "terms",
                        contrasts = "ListOrNull",
                        xlevels = "ListOrNull",
                        na.action = "character",
                        model.frame = "DataFrameOrNull"))

### Random Utility Classes
setClass("RndUtility", "VIRTUAL")

## Utility formula and error
setClass("RndUtilFormula",
         contains="RndUtility",
         representation(U="ModelFrame4",
                        error="numeric"))

## Utility design matrix and error
setClass("RndUtilMatrix",
         contains="RndUtility",
         representation(U="MatrixOrNull",
                        error="numeric"))

## Utility (XB) and error
setClass("RndUtilNum",
         contains="RndUtility",
         representation(U="numeric",
                        error="numeric"))

## Actions
setClass("StratNode",
         representation(error="numeric"))
setClass("StratActionNode",
         representation("StratNode", player="character"))
setClass("StratTerminalNode",
         representation("StratNode", outcome="character"))

## Taken from the generalTree class defined in Chambers 'green book' p. 286
## TODO - look at the data.frame version in Chambers 2008 and in the phylo4 package.
## 'names' slot needed because S4 that extend list interpret the list as a simple vector
## see Chamber 2008.
## TODO [ does not work.  I assume that no structure after subsetting
setClass("StratTree",
         contains="list",
         representation(names="character",
                        parents="character"))

## Strat Model
setClass("StratModel",
         contains="array",
         representation(tree="StratTree"))

setClass("StratMatrix",
         contains="array",
         representation(tree="StratTree",
                         Y="matrix"))

setClass("StratUtility",
         contains="array",
         representation(tree="StratTree",
                         Y="matrix"))

## Strat MLE class extends the stats-4 mle class
setClass("StratMLE",
         representation(
                        "mle",
                        "StratModel",
                        nobs = "integer"))

