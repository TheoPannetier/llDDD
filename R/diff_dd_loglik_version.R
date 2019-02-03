diff_dd_loglik_version <- function(brts, pars1, methode  ='lsoda')
{
  # Set parameters
  pars2 = c(
    1000, # max lx
    1, # ddmodel
    1, # cond
    1, # btorph
    0, # print
    2  # soc
  )
  missnumspec = 0
  
  source("./scripts/compare_bd_loglik/dd_loglik_3.2.R")
  # loglik with ode()
  loglik_3.2 <- dd_loglik_3.2(
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
  
  loglik_diff = abs(loglik_3.8 - loglik_3.8)
  
  return(c(loglik_3.8, loglik_3.8, loglik_diff))
}
