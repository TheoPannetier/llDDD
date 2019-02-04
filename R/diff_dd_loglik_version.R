#' Evaluate and compare dd_loglik() from two versions of DDD
#'
#' The function calls \code{dd_loglik()} from DDD_3.2 and from the current version and computes their difference.
#'
#' @param brts a set of branching times, all positive.
#' @param pars1 numerical parameters to be passed to \code{dd_loglik()}
#' @param pars2 numerical parameters to be passed to \code{dd_loglik()}
#' @param missnumspec parameter passed to \code{dd_loglik()}, default to 0.
#'
#' @details \code{dd_loglik()} parameters \code{pars2} are set to \code{c(4,1,1,0,2,1000)} here. \code{methode} is set to 'analytical' as I don't know whetehr other inputs work.
#'
#' @return a numeric vector of length 6 containing the input parameter values, the loglikelihood returned by \code{dd_loglik_3.2()} and \code{dd_loglik()} and their difference.
#'
#' @author Théo Pannetier
#'
#' @export

diff_dd_loglik_version <- function(brts, pars1, pars2, missnumspec = 0)
{

  methode  ='analytical' # I don't know yet whether other inputs work

  # loglik with ode()
  loglik_3.2 <- llDDD::dd_loglik_3.2(
    pars1 = pars1,
    pars2 = pars2,
    brts = brts,
    missnumspec = missnumspec,
    methode = methode
  )
  # loglik with dd_ode_FORTRAN()
  loglik_3.8 <- dd_loglik(
    pars1 = pars1,
    pars2 = pars2,
    brts = brts,
    missnumspec = missnumspec,
    methode = methode
  )

  loglik_diff = abs(loglik_3.8 - loglik_3.2)

  output = c(
    "lambda0" = pars1[1],
    "mu0" = pars1[2],
    "K" = pars1[2],
    "loglik_3.2" = loglik_3.2,
    "loglik_3.8" = loglik_3.8,
    "loglik_diff" = loglik_diff
  )

  return(output)
}
