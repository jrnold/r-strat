\name{strat}
\alias{strat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fit Statistical Strategic Models}
\description{
  \code{strat} estimates statistical strategic models using maximum likelihood
  estimation.  
}
\usage{
strat(formulae, tree, tNodes, alpha = list(), data = parent.frame(), subset = NULL, weights = NULL, na.action = "na.omit", model = FALSE, start = NULL, method = "BFGS", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formulae}{Nested List.  Formulas for utilities and private
    information variances for each outcome and player player. This list has a specific
    structure. Each element of the top-level list is an outcome; these do not
    need to be named as the name of the outcome variable can be
    determined from the formulas. Each outcome is a list with elements
    named for each players; these elements must be named.
    Each player is a list with two elements. The
    first is the formula describing the utility function of the player
    for that outcome; the second is the variance of the private
    information error term for that (player, outcome). The private
    information element is optional, and is assumed to be 0 (meaning, no
    private information error term) if not present. If an outcome,
    player element is missing, the utility of that player for that
    outcome is assumed to be 0.}

  \item{tree}{Character. Name of model to be estimated.}

  \item{tNodes}{ Character. Name of dependent variable associated with
    each terminal node. }

  \item{alpha}{ Named list. Variance of agent error for each
    action. Each element must have the same name as its action.}
  
  \item{data}{An optional data frame, list or environment (or object
          coercible by \code{\link{as.data.frame}} to a data frame) containing the
          variables in the model.  If not found in \code{data}, the
          variables are taken from \code{environment(formula)}, typically
          the environment from which \code{strat} is called  }
	
  \item{subset}{an optional vector specifying a subset of observations to be
    used in the fitting process. }
  
  \item{weights}{an optional vector of weights to be used in the fitting
    process.}
  
  \item{na.action}{Character or function. A function which indicates
    what should happen when the data contain \code{NA}s.
    \code{\link{na.omit}} is the default.}
  
  \item{model}{ Logical. If \code{TRUE} returns a list of model frames
    used in the estimation. }
  
  \item{start}{ Numeric.  Initial values for optimizer. }
  
  \item{method}{ Character. Optimization method to use. Default is
    "BFGS". See \code{\link{optim}} }
  
  \item{\dots}{ Further arguments to pass to \code{\link{optim}} }
}
\details{

  \code{strat} is a general function to estimate statistical strategic
  models with binary choices. Each extensive game form must be implemented by adding a class
  that extends \code{"\linkS4class{StratModel}"} and adding a
  \code{prAction} method to calculate the probabilities of each
  action for that particular game form.  Future plans are to
  allow for arbitrary extensive form games. For a simpler front-end
  to \code{strat}, written for the 1-2 extensive form game, see \code{\link{strat_1_2}}.
    
  The details of the estimation procedure is taken primarily from Signorino
  (2003).  See the other references for more details on statistical strategic models.

  The \code{\link{optim}} optimizer is used to find the minimum of the
  negative log-likelihood. Since \code{\link{optim}} is called by the 
  \code{\link{mle}} function, \code{strat} returns an S4 object that
  extends \code{"\linkS4class{mle}"}.
  
  Currently \code{strat} has the following restrictions
  \itemize{
    
    \item only binary choices are allowed.
    
    \item errors are distributed normally.
    
    \item regressor error is not supported.
  }
}
\value{
  An object of class \code{\linkS4class{StratMLE}}.
}

\references{ ~put references to the literature/web site here ~ }

\author{ Jeffrey Arnold, jarnold7 AT mail DOT rochester DOT edu}

\seealso{  See Also as \code{\link{strat_1_2}}, \code{\link{mle}}, \code{\link{optim}} }

\examples{
data(war1800)

## Estimate a Strategic 1-2 Model

## Define the utility functions for each player at each outcome
utils <- list(
            list("1" = list(sq ~ - 1 + peaceyrs + s.wt.re1, 1)),
            list("1" = list(capit ~ 1, 1),
                 "2" = list(capit ~ 0, 1)),
            list("1" = list(war ~ -1 + balanc, 1),
                 "2" = list(war ~ balanc, 1))
            )

## Define the structure of the Tree
nodes <- list(a0 = StratActionNode("1", 0),
              a1 = StratTerminalNode("sq", 0),
              a2 = StratActionNode("1", 0),
              a3 = StratTerminalNode("capit", 0),
              a4 = StratTerminalNode("war", 0))

gameTree <- StratTree(nodes,
                      c(NA, "a0", "a0", "a1", "a2", "a2"))

quux <- strat(utils, tree="1-2", data=war1800,
              tNodes = c("sq", "capit", "war"),
              model=TRUE)

## Summary of the model
summary(quux)

## Other methods inherited from 'mle'
print(quux)
vcov(quux)
ll <- logLik(quux)
AIC(ll)
BIC(ll)

## Note confint() and profile() are very slow for any non-trivial
## strategic model

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{nonlinear}% __ONLY ONE__ keyword per line
