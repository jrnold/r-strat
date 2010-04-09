\name{StratModel-class}
\docType{class}
\alias{StratModel-class}
\alias{StratModel_1-2-class}

\title{Strategic Discrete Choice Models}
\description{ Objects of class \code{StratModel} describe statistical
  strategic discrete choice models.
  This class is extended by classes specific to different game forms.
  The following extended classes are defined
}

\usage{
## Methods with "surprising" arguments
\S4method{prAction}{StratModel}(object, b)
}

\section{Objects from the Class}{
  Objects of this class are used internally by the \code{strat}
  function. The functions to estimate the maximum likelihood
  of a strategic discrete choice model all operate on objects
  of this class. 

  Classes derived from \code{StratModel} are used to differentiate
  the specific game forms that \code{strat} can estimate.
  Different game trees only differ in their log-likelihood functions
  in the estimation of the probabilities of each action.
  Then adding a new extensive game form is a matter of defining
  a new class that contains \code{StatModel} and an associated \code{prAction} method 
}

\section{Slots}{

  The class \code{"StratModel"} represents a statistical discrete
  choice model and contains the slots:
  
  \describe
  \item{\code{Y}:}{Object of class \code{"matrix"}.
    Matrix containing the response variables.}
  
  \item{\code{X}:}{Object of class \code{"list"}
    List of design matrices corresponding to each player and outcome.
    Utilities that are set to 0 are not included in this list}

  \item{\code{pi}:}{Object of class \code{"list"} with
    \code{"numerical"} elements.  The list corresponds to variance of
    the normal distribution for the private information error term of
    the utility function corresponding to the design matrices in
    \code{matrix}. Entries of 0 indicate that there is no private
    information regarding that utility.
  }
  
  \item{\code{oOutcomes}:}{Object of class \code{"character"}.
    Names of outcome variables associated with the design matrices in slot \code{X}.}
  
  \item{\code{oPlayers}:}{Object of class \code{"character"}.
    Names of players associated with the design matrices in slot \code{X}.}

  \item{\code{alpha}:}{Object of class \code{"list"} with
    \code{"numerical"} elements.  The list corresponds to the variance of
    the normal distribution for the agent error term for each action.
    An element of 0 means that there is no private information error
    term for that action.
  }

    \item{\code{players:}}
  {
    Object of class \code{"character"}. The names of the unique players.
    The first element in this vector is the label for player 1,
    the second element is the label for player 2, ....
  }
  
  \item{\code{outcomes}:}
  {
    Object of class \code{"character"}. The names of the unique outcome
    nodes. This is the same vector as \code{colnames(object@Y)}.
  }

  \item{\code{tNodes}:}{Object of class \code{"character"}.
    Each element is the outcome variable (column of \code{Y}),
    associated with the terminal node. 
    The \code{terminalNodes=c("foo",...)} indicates that column "foo"
    in slot \code{Y} indicates the outcome in terminal node 1.
  }
  
  \item{\code{paths}}{Object of class \code{"list"} specifying the
    structure of the extensive form game with the path of actions to each
    terminal node.  
  }

  The \code{StratModel} class is extended by several classes for
  specific game forms.  
  
  The class \code{StratModel_1-2} has the following restrictions

  \describe{

    \item{\code{paths}:}{Takes the value \code{list(1, c(2,3), c(2,4))}.}

    \item{\code{terminalNodes}:}{There are three terminal nodes.}

    \item{\code{utilities}:}{There are two players.}
  }    
  
}

\section{Methods}{
  \describe{

    \item{prAction}{\code{signature(object = "StratModel", b="list")}
      Given coefficients in \code{b}, estimates the probability for each
      action in the game.}

  }
  
}

\keyword{classes}
