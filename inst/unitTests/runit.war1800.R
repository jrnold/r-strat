####!/usr/bin/env Script
###### test.war1800.R
######
###### Title:
###### Author:jeffrey.arnold@gmail.com
###### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
######
###### Description: Unit Tests to test it against known results war 1800.R
######
###### Copyright (C): 2008  Curtis Signorino  <curt.signorino@rochester.edu>
###### License: GPL-3 <http://www.gnu.org/licenses>
###
##### Controlled Data
##### Obs 113, 179, 7 from war1800
###mySample <- c(113, 179, 7)
###FooX <- war1800[ mySample, c("peaceyrs", "balanc", "s.wt.re1")]
###FooX$cons.1 <- 1
###FooX$cons.2 <- 1
###FooY <- war1800[mySample, c("sq", "capit", "war")]
###FooU <- list(list(sq=c("peaceyrs","s.wt.re1"), capit="cons.1", war="balanc"),
###              list(capit=NULL, war=c("cons.2","balanc")))
###
##### Given coefs (From MLE)
###FooB <- c(0.0123602952198007, -0.632558849085763, 4.59727071473131,
###          -7.92385456597843, 0.825208091153018, -1.32959745371046)
##### Known answers
###p2 <- c(0.513756292582175, 0.650799915130119, 0.64570680593501)
###p4 <- c(0.409618356696974, 0.43359151941548, 0.395815654409182)
###P <- cbind((1-p2), p2, (1-p4), p4)
###POut <- cbind( (1-p2), p2 * (1 - p4), p2 * p4)
###llik <- c(-0.72104532517625, -0.997992797977122, -1.03763047682107)
###
###Bar <- new("StratModel-1-2",
###           Y = FooY,
###           X = FooX,
###           utilities = FooU,
###           terminalNodes=c("sq", "capit", "war"),
###           errorVar=1)
###
###Qux <-  new("StratModel-1-2",
###            Y = subset(war1800, select=c("sq", "capit", "war")),
###            X = data.frame(subset(war1800, select=c("peaceyrs", "balanc", "s.wt.re1")),
###            cons.1=1, cons.2 = 1),
###            utilities = FooU,
###            terminalNodes=c("sq", "capit", "war"),
###            errorVar=1)
###
###test.utilities <- function() {
###    bar.utilities <- utilities(Bar, b=FooB)
###    checkEqualsNumeric(bar.utilities[[1]][[1]],
###                       as.matrix(FooX[, c("peaceyrs", "s.wt.re1")]) %*% as.matrix(FooB[1:2]))
###    checkEqualsNumeric(bar.utilities[[1]][[2]],
###                       as.matrix(FooX[, c("cons.1")]) %*% as.matrix(FooB[3]))
###    checkEqualsNumeric(bar.utilities[[1]][[3]],
###                       as.matrix(FooX[, c("balanc")]) %*% as.matrix(FooB[4]))
###    checkEqualsNumeric(bar.utilities[[2]][[2]], 0)
###    checkEqualsNumeric(bar.utilities[[2]][[3]],
###                       as.matrix(FooX[, c("cons.2", "balanc")]) %*% as.matrix(FooB[5:6]))
###
###}
###
###test.stratProb <- function() {
###    bar.prob <- stratProb(Bar, b=FooB)
###    checkEqualsNumeric(bar.prob[,2], p2)
###    checkEqualsNumeric(bar.prob[,4], p4)
###    checkEqualsNumeric(bar.prob, P)
###
###    qux.prob <- stratProb(Qux, b=FooB)
###    checkEqualsNumeric(qux.prob[,2], benchmark.stratMLE$p2)
###    checkEqualsNumeric(qux.prob[,4], benchmark.stratMLE$p4)
###    ## checkEqualsNumeric(bar.prob,)
###}
###
###test.stratModelPredict <- function() {
###    bar.Q <- predict(Bar, b=FooB, type="terminal")
###    checkEqualsNumeric(bar.Q, POut)
###
###    bar.Y <- predict(Bar, b=FooB, type="outcomes")
###    checkEqualsNumeric(bar.Q, POut)
###
###    bmkP2 <- benchmark.stratMLE$p2
###    bmkP4 <- benchmark.stratMLE$p4
###    bmkP <- cbind( (1 - bmkP2), bmkP2, (1 - bmkP4), bmkP4)
###    bmkPOut <- cbind(bmkP[,1], bmkP[,2]*bmkP[,3], bmkP[,2] * bmkP[,4])
###
###    qux.Q <- predict(Qux, b=FooB, type="terminal")
###    checkEqualsNumeric(qux.Q, bmkPOut)
###
###    qux.Y <- predict(Qux, b=FooB, type="outcomes")
###    checkEqualsNumeric(qux.Q, bmkPOut)
###
###
###}
###
###test.logLikObs <- function() {
###    bar.ll <- logLikObs(Bar, b=FooB)
###    checkEqualsNumeric(bar.ll, llik)
###
###    checkEqualsNumeric(logLikObs(Qux, b=FooB),
###                       benchmark.stratMLE$llik)
###
###    checkEqualsNumeric(logLik(Qux, b=FooB),
###                       benchmark.stratMLE$value)
###
###    checkEqualsNumeric(.stratModelLogLikOptim(FooB, Qux),
###                       benchmark.stratMLE$value)
###
###}
###
###
###test.stratMLE <- function() {
###    set.seed(259)
###    quux <- StratMLE(Qux)
###
###    checkEqualsNumeric(unlist(quux@coefs), benchmark.war1800$par)
###    checkEqualsNumeric(unlist(quux@hessian), benchmark.war1800$hessian)
###    checkEqualsNumeric(unlist(quux@logLik), benchmark.war1800$value)
###}
###
