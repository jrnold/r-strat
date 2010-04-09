\name{StratMLE-class}
\docType{class}
\alias{StratMLE-class}
\alias{coef-method}
\alias{call-method}
\alias{logLik-method}
\alias{terms-method}
\alias{formula-method}

\title{Fitted Strategic Discrete Choice Models}

\description{
  This class encapsulates results of a maximum likelihood estimation of
  the parameters of strategic discrete choice model.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("StratMLE", ...)},
  but most often as a result of a call to \code{\link{strat}}.
}

\section{Slots}{

  All the slots from \code{"\linkS4class{mle}"} are present.
  In addition, the object contains the following slots.
  
  \describe{

    \item{\code{call}:}{
      Object of class \code{"language"}. The call to
      \code{\link{strat}}.
    }

    \item{\code{nobs}:}{
      Object of class \code{integer}. Number of observations.
    }
    
    \item{\code{na.action}:}{
      Object of class \code{character}.
    }
    
    \item{\code{contrasts}:}{
      Object of class \code{lists}.
    }
    
    \item{\code{xlevels}:}{
      Object of class \code{list}. Each element contains the levels used
      for variables in the corresponding terms.
    }
    
    \item{\code{terms}:}{
      Object of class \code{list}. Each element is a \code{terms}.
      These are the formulas for the utilities of all (outcome, player) combinations.
    }

    \item{\code{model}:}{
      Object of class \code{list}. Each element is a \code{model.frame}.
    }

    \item{\code{pinfo}:}{
      Object of class \code{list}. Each element is \code{numeric}.
      List of the variances on the private utility terms for each
      (outcome, player) combination.
    }


    \item{\code{oPlayers}}{
      Object of class \code{character}. 
    }

    \item{\code{oResponses}}{
      Object of class \code{character}. 
    }


    \item{\code{alpha}}{
      Object of class \code{list}.
    }


    \item{\code{players}}{
      Object of class \code{character}.
      Names of all players in the game.
      The order of the elements in this vector determines the player's
      number.  For example, \code{object@players[1]} is the name of
      player 1, and so forth.
    }

    \item{\code{outcome}}{
      Object of class \code{character}.
      Names of all outcomes in the game.
      This is redundant, as it is the same information as \code{colnames(object@Y)}}.
    }

    \item{\code{tNodes}}{
      Object of class \code{character}.
      Each element is the outcome associated with the terminal node.
      For example, \code{object@tNodes[1]} is the name of the response
      variable of terminal node 1.
    }


    \item{\code{paths}}{
      Object of class \code{list}.
      Each element is a character vector of the names of the actions
      forming the path to that terminal node.
      For example, \code{object@paths[1]} is the path to terminal node 1.
    }
    
  }


\section{Extends}{
  Class \code{"\linkS4class{mle}"}, directly.
}

\section{Methods}{

  Since \code{"\linkS4class{StratMLE}"} extends
  \code{"\linkS4class{mle}"} and can use all methods thereof.
  In addition, the following methods are available.
  
  \describe{
    \item{call}{
      \code{signature(object = "StratMLE")}:
       the estimates of the model parameters.
    }
    
    \item{coef}{
      \code{signature(object = "StratMLE")}:
      Returns the estimates of the model parameters.
    }

    \item{coef}{
      \code{signature(object = "logLik")}:
      Returns an object of class \code{\link{logLik}}, with
      the log-likelihood of the model.
    }

    
    \item{formula}{
      \code{signature(object = "StratMLE")}:
      Returns a list of the model formula.
    }

    \item{terms}{
      \code{signature(object = "StratMLE")}:
      Returns a list of the model terms.
    }
  } 
}

\note{
  This class provides a common interface to the mle results all classes
  extending \code{\linkS4class{StratModel}} and describing specific games.
  
  For a detailed explanation of the estimating procedure, see the
  references.  
  
  For a another implementation of statistical discrete choice models
  not in R, see STRAT.
}

\references{
  
  Signorino, C. S. (1999) Strategic interaction and the statistical analysis
  of international conflict ,\emph{American Political Science Review} 
  \bold{93}, 279--297.
  
  Signorino, C. S. (2002) Strategy and selection in international relations,
  \emph{International Interactions} \bold{28}, 93--115.
  
  Signorino, C. S. (2003) Structure and Uncertainty in Discrete Choice
  Models, \emph{Political Analysis} \bold{11}, 316--344.
  
  Signorino, C. S. (2003) STRAT: A Program for Analyzing Statistical
  Strategic Models, Version 1.4, \url{http://www.rochester.edu/college/psc/signorino/}.
  
  Signorino, C. S. and Yilmaz, K. (2003) Strategic misspecification in
  regression models \emph{American Journal Of Political Science}
  \bold{47}, 551--566.
   
}

\seealso{
  See also \code{StratMLE()} should be used to generate objects
  of this  class, and \code{"\linkS4class{StratModel}"} which this class extends.
}


\keyword{classes}
