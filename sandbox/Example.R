### Example.R
###
### Author:jeffrey.arnold@gmail.com
### Maintainer: Curtis Signorino <curt.signorino@rochester.edu>
###
### Description:
###
### Copyright (C): 2009  Curtis Signorino  <curt.signorino@rochester.edu>
### License: GPL-3 <http://www.gnu.org/licenses>

library(svUnit)

alfa <- rep(c(0, qnorm(0.8), qnorm(0.2)), each=3)
bravo <- rep(c(0, qnorm(0.8), qnorm(0.2)), 3)
charlie <- rep(c(0, qnorm(0.8), qnorm(0.2)), 3)
foo <- data.frame(alfa, bravo, charlie)

Y <- with(war1800, cbind(sq, capit, war))
y <- apply(Y, 1, function(x) which(x))
y <- factor(y, labels=colnames(Y))
war1800$Y <- y


## Model with only balanc
formulae1 <- list(u1 = Y ~ 1,
                u2 =  Y ~ balanc - 1,
                u3 = Y ~ balanc)
foo <- strat(formulae1, data=war1800)
print(summary(foo))

