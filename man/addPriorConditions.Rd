\name{addPriorConditions}
\alias{addPriorConditions}
\title{Additional Prior Conditions}
\concept{Prior conditions}
\description{%
  Add prior conditions on the model parameters.
}
\usage{%
  addPriorConditions(psi)
}
\arguments{%
  \item{psi}{model parameters regrouped within a \eqn{4 \times 1}{4x1} vector \eqn{\psi :=
      (\alpha_0 \ \alpha_1 \ \beta \ \nu)'}{psi:=(alpha0 alpha1 beta nu)'}.}
}
\details{%
  The function \code{addPriorConditions} allows to add prior conditions on the model
  parameters \eqn{\psi := (\alpha_0 \ \alpha_1 \ \beta \
    \nu)'}{psi:=(alpha0 alpha1 beta nu)'}. The
  function must return \code{TRUE} if the constraint holds and
  \code{FALSE} otherwise.

  By default, the function is:
  \preformatted{%
    addPriorConditions <- function(psi)
    {
      TRUE
    }
  }
  and therefore does not add any other constraint than the positivity of
  the parameters which are obtained through the prior distribution
  for \eqn{\psi}. See \link{bayesGARCH} for further details.

  You simply need to modify \code{AddPriorConditions} in order to add
  constraints on the model parameters \eqn{\psi}. For instance, to impose the
  covariance-stationary conditions to hold,
  i.e. \eqn{\alpha_1 + \beta < 1}, just redefine
  the function \code{addPriorConditions} as follows:
  \preformatted{%
    addPriorConditions <- function(psi)
    {
      psi[2] + psi[3] < 1
    }
  }
  
  Finally, note that adding prior constraints on the model parameters
  can diminish the acceptance rate and therefore
  lead to a very inefficient sampler. This would however indicate that the
  condition is not supported by the data.
}
\note{%
  Please cite the package in publications. Use \code{citation("bayesGARCH")}.
}
\author{%
  David Ardia <\email{david.ardia@unifr.ch}>.
}%
\seealso{%
  \code{\link{bayesGARCH}} for the Bayesian estimation of the GARCH(1,1)
  model with Student-t innovations.
}
\examples{%
  ## DON'T FORGET TO UPDATE THE NAMESPACE FUNCTION TO RENDER
  ## THE MODIFICATION EFFECTIVE
  ## ==> assignInNamespace("addPriorConditions", addPriorConditions, "bayesGARCH")

  ## COVARIANCE STATIONARITY REQUIRED
  addPriorConditions <- function(psi)
  {
      psi[2] + psi[3] < 1
  }

  ## ALPHA1 > 0.3, BETA < 1, ALPHA1 + BETA < 0.8
  addPriorConditions <- function(psi)
  {
      (psi[2] > 0.3) & (psi[3] < 1) & (psi[2] + psi[3] < 0.8)
  } 

  ## BETA < 0.1, 4 < NU < 15
  addPriorConditions <- function(psi)
  {
      (psi[3] < 0.1) & (psi[4] < 15)
  }
  ## AND THE CONSTRAINT 4 < NU IS INTEGRATED THROUGH THE PRIOR
  ## IN bayesGARCH(y, delta = 4)
}
\keyword{misc}
