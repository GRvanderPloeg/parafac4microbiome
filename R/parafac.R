#' Parallel Factor Analysis
#'
#' @param Tensor 3-way matrix of numeric data
#' @param nfac Number of factors (components) to fit.
#' @param nstart Number of models to randomly initialize (default 1).
#' @param maxit Maximum number of iterations allowed without convergence in the ALS case (default 500).
#' @param max_fn Maximum number of function evaluations allowed without convergence in the OPT case (default 10000).
#' @param ctol Relative change in loss tolerated to call the algorithm converged in the ALS case (default 1e-4).
#' @param rel_tol Relative change in loss tolerated to call the algorithm converged in the OPT case (default 1e-8).
#' @param abs_tol Absolute loss tolerated to call the algorithm converged in the OPT case (default 1e-8).
#' @param grad_tol Tolerance on the two-norm of the gradient divided over the number of elements in the gradient in the OPT case (default 1e-8).
#' @param initialization "Random" for randomly initialized input vectors or "nvec" for svd-based best guess.
#' @param method Use ALS algorithm ("als", default) or use all-at-once optimization ("opt"). The all-at-once optimization is based on a nonlinear conjugate gradient method with Hestenes-Stiefel updates and the More-Thuente line search algorithm.
#' @param verbose `r lifecycle::badge('deprecated')` verbose output
#' @param output String ("best"/"all") Return only the best model of the nstart models ("best") or return all of them in a list object ("all").
#' @param sortComponents Boolean to sort the components based on their variance explained (default FALSE)
#'
#' @return List object of the PARAFAC model or models.
#' @export
#'
#' @examples
#' X = array(rnorm(108*100*10), c(108,100,10))
#' model = parafac(X, 2)
parafac = function(Tensor, nfac, nstart=1, maxit=500, max_fn=10000, ctol=1e-4, rel_tol=1e-8, abs_tol=1e-8, grad_tol=1e-8, initialization="random", method="als", verbose=FALSE, output="best", sortComponents=FALSE){

  if(methods::is(Tensor,"Tensor")){
    Tensor = Tensor@data
  }

  # Put missing values to zero
  Tensor[is.na(Tensor)] = 0

  # Coerce to rTensor Tensor S4 object
  Tensor = rTensor::as.tensor(Tensor)

  # Prepare input
  inits = list()
  for(i in 1:nstart){
    inits[[i]] = initializePARAFAC(Tensor, nfac, initialization=initialization)
  }

  # Run core algorithm
  models = list()
  if(method == "opt"){
    for(i in 1:nstart){
      models[[i]] = mize::mize(par=fac_to_vect(inits[[i]]), fg=list("fn"=function(x){return(parafac4microbiome::parafac_fun(x,Tensor))}, "gr"=function(x){return(parafac4microbiome::parafac_gradient(x,Tensor))}), max_iter=maxit, max_fn=max_fn, abs_tol=abs_tol, rel_tol=rel_tol, grad_tol=grad_tol, method="CG", cg_update="HS", line_search="MT")
      models[[i]]$Fac = vect_to_fac(models[[i]]$par, Tensor)
      models[[i]]$iter = models[[i]]$nf
    }
  } else{
    for(i in 1:nstart){
      models[[i]] = parafac_core_als(Tensor, nfac, inits[[i]], maxit=maxit, ctol=ctol)
      models[[i]]$iter = length(models[[i]]$fs)
      models[[i]]$f = models[[i]]$fs[length(models[[i]]$fs)]
    }
  }

  # Attach extra model info
  for(i in 1:nstart){
    Fac = models[[i]]$Fac # for 1-component corner case
    Xhat = reinflateTensor(Fac[[1]], Fac[[2]], Fac[[3]], returnAsTensor=TRUE)
    models[[i]]$Xhat = Xhat@data
    models[[i]]$SSE = multiway::sumsq((Tensor - Xhat)@data)
    models[[i]]$varExp = (multiway::sumsq(Xhat@data) / multiway::sumsq(Tensor@data)) * 100
    models[[i]]$init = inits[[i]]

    if(sortComponents == TRUE){
      models[[i]]$Fac = sortComponents(models[[i]]$Fac, Tensor)
    }
  }

  # Return all models if specified, otherwise return only the best model
  if(output != "best"){
    return(models)
  }
  else{
    bestModel = 0
    bestObjective = Inf
    for(i in 1:nstart){
      model = models[[i]]
      if(model$f <= bestObjective){
        bestModel = model
        bestObjective = model$f
      }
    }
    return(bestModel)
  }
}
