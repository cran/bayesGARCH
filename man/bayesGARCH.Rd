\name{bayesGARCH}
\alias{bayesGARCH}
\title{Bayesian Estimation of the GARCH(1,1) Model with Student-t Innovations}
\concept{Bayesian GARCH}
\description{%
  Performs the Bayesian estimation of the GARCH(1,1) model with
  Student-t innovations.
}
\usage{%
  bayesGARCH(y, mu.alpha = c(0,0), Sigma.alpha = 1000 * diag(1,2), 
             mu.beta = 0, Sigma.beta = 1000,
             lambda = 0.01, delta = 2, control = list())
}
\arguments{%
  \item{y}{vector of observations of size \eqn{T}. \code{NA} values are not allowed.}
  \item{mu.alpha}{hyperparameter \eqn{\mu_\alpha}{mu_alpha} (prior mean)
    for the truncated Normal prior on parameter
    \eqn{\alpha := (\alpha_0 \ \alpha_1)'}{alpha:=(alpha0
      alpha1)'}. Default: a \eqn{2 \times 1}{2x1} vector of zeros.}
  \item{Sigma.alpha}{hyperparameter \eqn{\Sigma_\alpha}{Sigma_alpha}
    (prior covariance matrix) for
    the truncated Normal prior on parameter
    \eqn{\alpha}{alpha}. Default: a \eqn{2 \times 2}{2x2} diagonal matrix whose
    variances are set to 1'000, i.e., a diffuse prior. Note that the matrix
    must be symmetric positive definite.}
  \item{mu.beta}{hyperparameter \eqn{\mu_\beta}{mu_beta} (prior mean) for the truncated Normal prior on
    parameter \eqn{\beta}{beta}. Default: zero.}
  \item{Sigma.beta}{hyperparameter \eqn{\Sigma_\beta > 0}{Sigma_beta>0} (prior variance) for the truncated
    Normal prior on paramter \eqn{\beta}{beta}. Default: 1'000, i.e.,
    a diffuse prior.} 
  \item{lambda}{hyperparameter \eqn{\lambda > 0}{lambda>0} for the translated
    Exponential distribution on parameter \eqn{\nu}{nu}. Default: 0.01.}
  \item{delta}{hyperparameter \eqn{\delta \ge 2}{delta>=2} for the translated
    Exponential distribution on parameter \eqn{\nu}{nu}. Default: 2 (to
    ensure the existence of the conditional variance).}
  \item{control}{list of control parameters (See *Details*).}
}
\details{%
  The function \code{bayesGARCH} performs the Bayesian estimation of the
  GARCH(1,1) model with Student-t innovations. The underlying algorithm is based on Nakatsuma
  (1998, 2000) for generating the parameters of the GARCH(1,1) scedastic
  function \eqn{\alpha := (\alpha_0 \ \alpha_1)'}{alpha:=(alpha0 alpha1)'} and \eqn{\beta}{beta} and on
  Geweke (1993) and Deschamps (2006) for the generating the degrees of freedom
  parameter \eqn{\nu}{nu}. Further details and examples can be found in Ardia (2008, 2009b). See also
  Ardia (2009a) for an extension of the algorithm for Markov-switching GARCH models.

  The \code{control} argument is a list that can supply any of
  the following components:

  \describe{%
    \item{\code{n.chain}}{number of MCMC chain(s) to be
      generated. Default: \code{n.chain=1}.}
    \item{\code{l.chain}}{length of each MCMC chain. Default: \code{l.chain=10000}.}
    \item{\code{start.val}}{vector of starting values of
      chain(s). Default: \code{start.val=c(0.01,0.1,0.7,20)}. A matrix of
      size \eqn{n \times 4}{nx4}
      containing starting values in rows can also be provided. This will generate \eqn{n} chains starting at the
      different row values.}
    \item{\code{refresh}}{frequency of reports. Default: \code{refresh=10} iterations.}
    \item{\code{digits}}{number of printed digits in the
      reports. Default: \code{digits=4}.}
  }
}
\value{%
  A list of class \code{mcmc.list} (\R package \pkg{coda}).
}
\note{%
  The GARCH(1,1) model with Student-t innovations may be written as follows:
  \deqn{
    y_t = \epsilon_t (\varrho h_t)^{1/2}
  }{
    y(t) = e(t)*(varrho * h(t))^(1/2)  
  }
  for \eqn{t=1,\ldots,T}{t=1,...,T}, where the conditional variance equation is defined as:
  \deqn{
    h_t := \alpha_0 + \alpha_1 y_{t-1}^2 + \beta h_{t-1}
  }{
    h(t) := alpha0 + alpha1 * y(t-1)^2 + beta * h(t-1)
  }
  where \eqn{\alpha_0 > 0, \alpha_1 \ge 0, \beta \ge 0}{alpha0>0,alpha1,beta>=0} to ensure a
  positive conditional variance. We set the initial variance to
  \eqn{h_0 := 0}{h(0):=0} for convenience. The parameter \eqn{\varrho :=
    (\nu-2)/\nu}{varrho:=(nu-2)/nu} is a scaling factor which ensures
  the conditional variance of \eqn{y_t}{y(t)} to be
  \eqn{h_t}{h(t)}. Finally, \eqn{\epsilon_t}{e(t)}
  follows a Student-t distribution with \eqn{\nu}{nu} degrees of freedom.

  The prior distributions on \eqn{\alpha}{alpha} is a bivariate truncated
  Normal distribution:
  \deqn{
    p(\alpha) \propto N_2(\alpha \mid \mu_\alpha, \Sigma_\alpha)
    I_{[\alpha > 0]}
  }{
    p(alpha) prop N2(alpha | mu_alpha, Sigma_alpha) I[alpha>0]
  }
  where \eqn{\mu_\alpha}{mu_alpha} is the prior mean vector, \eqn{\Sigma_\alpha}{Sigma_alpha} is
  the prior covariance matrix and \eqn{I_{[\bullet]}}{I[alpha>0]} is the indicator function.
  
  The prior distribution on \eqn{\beta}{beta} is a univariate truncated Normal
  distribution:
  \deqn{
    p(\beta) \propto N(\beta \mid \mu_\beta, \Sigma_\beta) I_{[\beta > 0]}
  }{
    p(theta) prop N(beta | mu_beta, Sigma_beta) I[beta>0]
  }
  where \eqn{\mu_\beta}{mu_beta} is the prior mean and \eqn{\Sigma_\beta}{Sigma_beta} is the
  prior variance.
  
  The prior distribution on \eqn{\nu}{nu} is a translated Exponential
  distribution:
  \deqn{
    p(\nu) = \lambda \exp [ - \lambda (\nu - \delta) ] I_{[\nu > \delta]}
  }{
    p(nu) = lambda * exp(-lambda(nu-delta)) I[nu>delta]
  }
  where \eqn{\lambda > 0}{lambda>0} and \eqn{\delta \ge 2}{delta>=2}. The prior mean for
  \eqn{\nu}{nu} is \eqn{\delta + 1/\lambda}{delta + 1/lambda}.
  
  The joint prior on parameter \eqn{\psi := (\alpha,
    \beta, \nu)}{psi:=(alpha,beta,nu)} is obtained by assuming prior independance:
  \deqn{
    p(\psi) = p(\alpha) p(\beta) p(\nu).
  }{
    p(psi) = p(alpha) * p(beta) * p(nu).
  }

  The default hyperparameters \eqn{\mu_\alpha}{mu_alpha}, \eqn{\Sigma_\alpha}{Sigma_alpha},
  \eqn{\mu_\beta}{mu_beta}, \eqn{\Sigma_\beta}{Sigma_beta} and \eqn{\lambda}{lambda} define a rather
  vague prior. The hyperparameter \eqn{\delta \geq 2}{delta>=2} ensures the
  existence of the conditional variance. The \eqn{k}th conditional
  moment for \eqn{\epsilon_t}{e(t)} is guaranteed by setting \eqn{\delta \geq k}{delta>=k}.
  
  The Bayesian estimation of the GARCH(1,1) model with Normal
  innovations is obtained as a special case by setting \code{lambda=100}
  and \code{delta=500}. In
  this case, the generated values for
  \eqn{\nu}{nu} are centered around 500 which ensure approximate Normality
  for the innovations. 

  !!! WARNING !!!
  For some time series, the sampler can get stuck to a given
  value so that the chain doesn't move anymore. In this case, run the sampler again with a
  different seed or a different starting value. Good starting values can
  be obtained using the function \code{garchFit} (\R package \pkg{fGarch}).
  
  Please cite the package in publications. Use \code{citation("bayesGARCH")}.
}
\references{%
  Ardia, D. (2009a).
  Bayesian Estimation of a Markov-Switching Threshold Asymmetric GARCH Model with Student-t Innovations.
  \emph{Econometrics Journal} \bold{12}(1), pp. 105--126. doi: 10.1111/j.1368-423X.2008.00253.x.

  Ardia, D. (2009b).
  Bayesian Estimation of the GARCH(1,1) Model with Student-t Innovations in R. 
  \emph{MPRA working paper}. pp.1--8. 
  URL \url{http://mpra.ub.uni-muenchen.de/17414/}

  Ardia, D. (2008). 
  Financial Risk Management with Bayesian Estimation of GARCH Models. 
  Lecture Notes in Economics and Mathematical Systems \bold{612}. Springer-Verlag, Berlin, Germany.
  ISBN 978-3-540-78656-6, e-ISBN 978-3-540-78657-3, doi: 10.1007/978-3-540-78657-3. 
  URL \url{http://www.springer.com/economics/econometrics/book/978-3-540-78656-6}

  Ardia, D. (2006).
  Bayesian Estimation of the GARCH(1,1) Model with Normal Innovations. 
  \emph{Student} \bold{5}(3--4), pp.283--298.
  URL \url{http://mpra.ub.uni-muenchen.de/12985/}

  Deschamps, P.J. (2006). 
  A Flexible Prior Distribution for Markov Switching Autoregressions with Student-t Errors. 
  \emph{Journal of Econometrics} \bold{133}, pp.153--190.

  Geweke, J.F. (1993). 
  Bayesian Treatment of the Independent Student-t Linear Model. 
  \emph{Journal of Applied Econometrics} \bold{8}, pp.19--40.

  Nakatsuma, T. (2000). 
  Bayesian Analysis of ARMA-GARCH Models: A Markov Chain Sampling Approach. 
  \emph{Journal of Econometrics} \bold{95}(1), pp.57--69.

  Nakatsuma, T. (1998). 
  A Markov-Chain Sampling Algorithm for GARCH Models'. 
  \emph{Studies in Nonlinear Dynamics and Econometrics} \bold{3}(2), pp.107--117.
}
\author{%
  David Ardia <\email{david.ardia@unifr.ch}>
}
\seealso{%
  \code{\link{addPriorConditions}} to integrate additional prior
  conditions on the model parameters in the sampler;
  \code{\link[fGarch:garchFit]{garchFit}} (\R package \pkg{fGarch}) for the classical
  Maximum Likelihood estimation of GARCH models.
}
\examples{%
  ## LOAD DATA
  data(dem2gbp)
  y <- dem2gbp[1:750]

  ## RUN THE SAMPLER (2 chains)
  MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 2000))

  ## MCMC ANALYSIS (using coda)
  plot(MCMC)
  autocorr.diag(MCMC)
  gelman.diag(MCMC)
  1-rejectionRate(MCMC)

  ## FORM THE POSTERIOR SAMPLE
  smpl <- formSmpl(MCMC, l.bi = 500)

  ## POSTERIOR STATISTICS
  summary(smpl)
  smpl <- as.matrix(smpl)
  pairs(smpl)

  ## GARCH(1,1) WITH NORMAL INNOVATIONS
  MCMC <- bayesGARCH(y, lambda = 100, delta = 500,
                     control = list(n.chain = 2, l.chain = 2000))
}
\keyword{models}
