#' Maximization of the loglikelihood under the diversity-independent, possibly time-dependent diversification model (version 3.2)
#'
#' Identical to DDD::bd_ML(), except that this one calls bd_loglik_3.2(). See DDD documentation for details.
#'
#' @author Rampal S. Etienne & Bart Haegeman
#'
#' @export

bd_ML_3.2 = function(brts, initparsopt = c(0.1,0.05 * (tdmodel <= 1) + 10 * (length(brts) + missnumspec) * (tdmodel > 1)), idparsopt = c(1,2 + (tdmodel > 1)), idparsfix = (1:4)[-idparsopt], parsfix = rep(0,4)[idparsfix], missnumspec = 0, tdmodel = 0, cond = 1, btorph = 1, soc = 2, tol = c(1E-3, 1E-4, 1E-6), maxiter = 1000 * round((1.25)^length(idparsopt)), changeloglikifnoconv = FALSE, optimmethod = 'subplex',methode = 'lsoda', verbose = 0)
{

  options(warn = -1)
  if(!verbose %in% c(0,1)){stop("Verbose must be 1 or 0.")}
  brts = sort(abs(as.numeric(brts)),decreasing = TRUE)
  out2 = invisible(data.frame(lambda0 = -1,mu0 = -1,lambda1 = -1, mu1 = -1, loglik = -1, df = -1, conv = -1))
  if(is.numeric(brts) == FALSE)
  {
    cat("The branching times should be numeric.\n")
    return(out2)
  }
  idpars = sort(c(idparsopt,idparsfix))
  if((prod(idpars == (1:4)) != 1) || (length(initparsopt) != length(idparsopt)) || (length(parsfix) != length(idparsfix)))
  {
    cat("The parameters to be optimized and/or fixed are incoherent.\n")
    return(out2)
  }
  if(tdmodel == 0 & length(initparsopt) > 2)
  {
    cat("tdmodel = 0 only accepts a constant speciation rate and extinction rate.\n")
    return(out2)
  }
  namepars1 = c("lambda0","mu0","lambda1","mu1")
  namepars2 = c("lambda0","mu0","K","")
  if(tdmodel == 2 | tdmodel == 3) { namepars = namepars2 } else  { namepars = namepars1 }
  if(length(namepars[idparsopt]) == 0) { optstr = "nothing" } else { optstr = namepars[idparsopt] }
  cat("You are optimizing",optstr,"\n")
  if(length(namepars[idparsfix]) == 0) { fixstr = "nothing" } else { fixstr = namepars[idparsfix] }
  cat("You are fixing",fixstr,"\n")
  cat("Optimizing the likelihood - this may take a while.","\n")
  flush.console()
  trparsopt = initparsopt/(1 + initparsopt)
  trparsopt[which(initparsopt == Inf)] = 1
  trparsfix = parsfix/(1 + parsfix)
  trparsfix[which(parsfix == Inf)] = 1
  pars2 = c(tdmodel,cond,btorph,verbose,soc,1000,tol,maxiter)
  optimpars = c(tol,maxiter)
  initloglik = bd_loglik_choosepar_3.2(trparsopt = trparsopt,trparsfix = trparsfix,idparsopt = idparsopt,idparsfix = idparsfix,pars2 = pars2,brts = brts,missnumspec = missnumspec, methode = methode)
  cat("The loglikelihood for the inital parameter values is",initloglik,"\n")
  if(initloglik == -Inf)
  {
    cat("The initial parameter values have a likelihood that is equal to 0 or below machine precision. Try again with different initial values.\n")
    return(out2)
  }
  out = optimizer(optimmethod = optimmethod,optimpars = optimpars,fun = bd_loglik_choosepar_3.2,trparsopt = trparsopt,idparsopt = idparsopt,trparsfix = trparsfix,idparsfix = idparsfix,pars2 = pars2,brts = brts,missnumspec = missnumspec,methode = methode)
  if(out$conv != 0)
  {
    cat("Optimization has not converged. Try again with different initial values.\n")
    return(out2)
  }
  MLtrpars = as.numeric(unlist(out$par))
  MLpars = MLtrpars/(1-MLtrpars)
  MLpars1 = rep(0,4)
  MLpars1[idparsopt] = MLpars
  if(length(idparsfix) != 0) { MLpars1[idparsfix] = parsfix }
  ML = as.numeric(unlist(out$fvalues))
  out2 = data.frame(lambda0 = MLpars1[1], mu0 = MLpars1[2], lambda1 = MLpars1[3], mu1 = MLpars1[4], loglik = ML, df = length(initparsopt), conv = unlist(out$conv))
  if(out2$conv != 0 & changeloglikifnoconv == T) { out2$loglik = -Inf }
  s1 = sprintf('Maximum likelihood parameter estimates: lambda0: %f, mu0: %f, lambda1: %f, mu1: %f: ',MLpars1[1],MLpars1[2],MLpars1[3],MLpars1[4])
  s2 = sprintf('Maximum loglikelihood: %f',ML)
  cat("\n",s1,"\n",s2,"\n")
  out2 = invisible(out2)
  return(out2)
}
