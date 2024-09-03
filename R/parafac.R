#' Parallel Factor Analysis
#'
#' @param Tensor 3-way matrix of numeric data
#' @param nfac Number of factors (components) to fit.
#' @param nstart Number of models to randomly initialize (default 1).
#' @param maxit Maximum number of iterations allowed without convergence (default 500).
#' @param ctol Relative change in loss tolerated to call the algorithm converged (default 1e-4).
#' @param initialization "Random" for randomly initialized input vectors or "nvec" for svd-based best guess.
#' @param verbose DEPRECATED: verbose output
#' @param output String ("best"/"all") Return only the best model of the nstart models ("best") or return all of them in a list object ("all").
#'
#' @return List object of the PARAFAC model or models.
#' @export
#'
#' @examples
#' X = array(rnorm(108*100*10), c(108,100,10))
#' model = parafac(X, 2)
parafac = function(Tensor, nfac, nstart=1, maxit=500, ctol=1e-4, initialization="random", verbose=FALSE, output="best"){

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
  for(i in 1:nstart){
    models[[i]] = parafac_core_als(Tensor, nfac, inits[[i]])
  }

  # Attach extra model info
  for(i in 1:nstart){
    Fac = models[[i]]$Fac
    Xhat = reinflateTensor(Fac[[1]], Fac[[2]], Fac[[3]], returnAsTensor=TRUE)
    models[[i]]$Xhat = Xhat@data
    models[[i]]$iter = length(models[[i]]$fs)
    models[[i]]$SSE = sumsqr((Tensor - Xhat)@data)
    models[[i]]$varExp = (sumsqr(Xhat@data) / sumsqr(Tensor@data)) * 100
    models[[i]]$init = inits[[i]]
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
      fs = model$fs
      f = fs[length(fs)]
      if(f <= bestObjective){
        bestModel = model
        bestObjective = f
      }
    }
    return(bestModel)
  }
}
