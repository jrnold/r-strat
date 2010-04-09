### runit.Actions.R
###
### Author:jeffrey.arnold@gmail.com
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description: Runit tests for Actions.
###
### Copyright (C): 2009  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

## 1-2 Game Tree
nodes <- list(a0=StratActionNode("1", 1),
              a1=StratTerminalNode("sq", 1),
              a2=StratActionNode("2", 1),
              a3=StratTerminalNode("capit", 1),
              a4=StratTerminalNode("war", 1))
parents <- c(NA, "a0", "a0", "a2", "a2")
foo <- StratTree(nodes, parents)

bar <- lapply(rep(1, 6), RndUtilNum, error=1)
names(bar) <- paste(rep(c("sq", "capit", "war"), each=2),
                    rep(as.character(1:2), 3), sep=".")

bar <- RndUtilNum(1, 1)
baz <- RndUtilNum(2, 3)

qux <- array(lapply(rep(1, 6), RndUtilNum, error=1),
             dim=c(3,2),
             dimnames=list(c("sq", "capit", "war"), as.character(1:2)))

## Single Choice Game Tree
## really simple to debug
nodes <- list(a0=StratActionNode("1", 0),
              a1=StratTerminalNode("alpha", 1),
              a2=StratTerminalNode("bravo", 1))
parents <- c(NA, "a0", "a0")
corge <- StratTree(nodes, parents)

grault <- array(list(RndUtilNum(1, 1), RndUtilNum(0, 0)),
                dim=c(2, 1),
                dimnames=list(c("alpha", "bravo"), "1"))

## Actions
checkIdentical(getActionNodes(foo),
               c("a0", "a2"))

checkIdentical(getTerminalNodes(foo),
               c("a1", "a3", "a4"))

checkIdentical(getRootNode(foo), "a0")

checkIdentical(getChildren(foo),
               list(a0=c("a1", "a2"), a1=character(0), a2=c("a3", "a4"),
                    a3=character(0), a4=character(0)))

checkIdentical(getPaths(foo),
               list(a1 = c("a1", "a0"),
                    a3 = c("a3", "a2", "a0"),
                    a4 = c("a4", "a2", "a0")))

checkIdentical(getOutcomes(foo), c(a1="sq", a3="capit", a4="war"))
checkIdentical(.getOut2Term(foo), list(sq = "a1",
                                       capit="a3",
                                       war="a4"))


## Utilities

checkIdentical(baz + bar, RndUtilNum(3, 4))
checkIdentical(bar + 1, RndUtilNum(2, 1))
checkIdentical(1 + bar, bar + 1)
checkIdentical(2 * bar, RndUtilNum(2, 4))
checkIdentical(2 * bar, bar * 2)

