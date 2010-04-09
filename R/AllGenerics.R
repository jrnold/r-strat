#!/usr/bin/env Rscript
### AllGenerics.R
###
### Title: S4 Generic Functions
### Author: Jeffrey Arnold <jarnold7@mail.rochester.edu>
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description: All S4 Generic Functions for package Rino
###
### Copyright (C): 2008  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

## Stats - generics
setGeneric("formula")
setGeneric("terms")

## New Generics
setGeneric("prAction",
           function(object, b, ...) standardGeneric("prAction"))

## Generics used by StratActions Class ###
setGeneric("getTerminalNodes",
           function(object, ...) standardGeneric("getTerminalNodes"))
setGeneric("getActionNodes",
           function(object, ...) standardGeneric("getActionNodes"))
setGeneric("getChildren",
           function(object, ...) standardGeneric("getChildren"))
setGeneric("getRootNode",
           function(object, ...) standardGeneric("getRootNode"))
setGeneric("getPaths",
           function(object, ...) standardGeneric("getPaths"))
setGeneric("getOutcomes",
           function(object, ...) standardGeneric("getOutcomes"))
setGeneric("getPlayers",
           function(object, ...) standardGeneric("getPlayers"))
setGeneric("getTermNames",
           function(object, ...) standardGeneric("getTermNames"))
setGeneric("getCoefNames",
           function(object, ...) standardGeneric("getCoefNames"))

