#' Evaluate and compare bd_loglik() from two versions of DDD
#'
#' The function calls \code{bd_loglik()} from DDD_3.2 and from the current version and computes their difference.
#'
#' @param brts a set of branching times, all positive.
#' @param pars1 numerical parameters to be passed to \code{bd_loglik()}
#' @param pars2 numerical parameters to be passed to \code{bd_loglik()}
#' @param missnumspec parameter passed to \code{bd_loglik()}, default to 0.
#'
#' @details \code{bd_loglik()} parameters \code{pars2} are set to \code{c(4,1,1,0,2,1000)} here. bd_loglik argument methode is set to 'lsoda'.
#'
#' @return a numeric vector of length 6 containing the input parameter values, the loglikelihood returned by \code{bd_loglik_3.2()} and \code{bd_loglik()} and their difference.
#'
#' @author Théo Pannetier
#'
#' @export

diff_bd_loglik_version <- function(brts, pars1, pars2, missnumspec = 0)
{

  methode = "lsoda" # other arguments were not tested yet

  # loglik with ode()
  loglik_3.2 <- bd_loglik_3.2(
    pars1 = pars1,
    pars2 = pars2,
    brts = brts,
    missnumspec = missnumspec,
    methode = methode
  )
  # loglik with dd_ode_FORTRAN()
  loglik_3.8 <- bd_loglik(
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
