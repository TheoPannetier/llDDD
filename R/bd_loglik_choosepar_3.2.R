#' Internal DDD function
#'
#' Calls bd_loglik for bd_ML. Only difference with the function from DDD is that this one calls to bd_loglik_3.2.
#'
#' @author Rampal S. Etienne & Bart Haegeman
#'

bd_loglik_choosepar_3.2 = function(trparsopt,trparsfix,idparsopt,idparsfix,pars2,brts,missnumspec,methode)
{
  trpars1 = rep(0,4)
  trpars1[idparsopt] = trparsopt
  if(length(idparsfix) != 0)
  {
    trpars1[idparsfix] = trparsfix
  }
  if(max(trpars1) > 1 | min(trpars1) < 0)
  {
    loglik = -Inf
  } else {
    pars1 = trpars1/(1 - trpars1)
    loglik = bd_loglik_3.2(pars1 = pars1,pars2 = pars2[1:6],brts = brts,missnumspec = missnumspec, methode = methode)
    if(is.nan(loglik) || is.na(loglik))
    {
      cat("There are parameter values used which cause numerical problems.\n")
      loglik = -Inf
    }
  }
  return(loglik)
}
