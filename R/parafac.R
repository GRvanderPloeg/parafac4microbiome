#' Parallel Factor Analysis
#'
#' @param Tensor 3-way matrix of numeric data
#' @param nfac Number of factors (components) to fit.
#' @param nstart Number of models to randomly initialize (default 1).
#' @param maxit Maximum number of iterations allowed without convergence (default 500).
#' @param ctol Relative change in loss tolerated to call the algorithm converged (default 1e-4).
#' @param verbose DEPRECATED: verbose output
#' @param output String ("best"/"all") Return only the best model of the nstart models ("best") or return all of them in a list object ("all").
#'
#' @return List object of the PARAFAC model or models.
#' @export
#'
#' @examples
#' X = array(rnorm(108*100*10), c(108,100,10))
#' model = parafac(X, 2)
parafac = function(Tensor, nfac, nstart=1, maxit=500, ctol=1e-4, verbose=FALSE, output="best"){

  if(methods::is(Tensor,"Tensor")){
    Tensor = Tensor@data
  }

  # Put missing values to zero
  Tensor[is.na(Tensor)] = 0

  # Coerce to rTensor Tensor S4 object
  Tensor = rTensor::as.tensor(Tensor)

  # Prepare model input
  numModes = Tensor@num_modes
  modes = Tensor@modes
  tnsr_norm = rTensor::fnorm(Tensor)

  models = list()
  for(i in 1:nstart){
    Xhat = Tensor
    iteration = 1
    converged = FALSE
    fOld = 0
    fNew = Inf

    # Initialize input vectors
    Fac = list()
    unfolded_X = list()
    for(m in 1:numModes){
      unfolded_X[[m]] = rTensor::k_unfold(Tensor, m)@data
      Fac[[m]] = array(stats::rnorm(modes[m] * nfac), c(modes[m], nfac))
    }

    # Main CP loop
    while ((iteration < maxit) && (!converged)) {

      for (m in 1:numModes) {
        V = rTensor::hadamard_list(lapply(Fac[-m], function(x) {t(x) %*% x}))
        V_inv = solve(V)
        tmp = unfolded_X[[m]] %*% rTensor::khatri_rao_list(Fac[-m], reverse = TRUE) %*% V_inv
        lambdas = apply(tmp, 2, function(x){norm(as.matrix(x))})
        Fac[[m]] = sweep(tmp, 2, lambdas, "/")
        Xhat = reinflateTensor(Fac[[1]], Fac[[2]], Fac[[3]])
      }

      fNew = rTensor::fnorm(Xhat - Tensor)
      fChange = abs(fNew-fOld)/tnsr_norm

      if(iteration == 1){
        iteration = iteration + 1
        fOld = fNew
      } else if(fChange < ctol){
        converged = TRUE
      } else{
        iteration = iteration + 1
        fOld = fNew
      }

    norm_percent <- (1 - (utils::tail(fNew, 1)/tnsr_norm)) * 100
    models[[i]] = list(lambdas = lambdas,
                  U = Fac,
                  conv = converged,
                  est = rTensor::as.tensor(Xhat),
                  norm_percent = norm_percent,
                  f = fNew,
                  iter = iteration)
    }
  }


  # Run CP algorithm
  # models = list()
  # for(i in 1:nstart){
  #   utils::capture.output(models[[i]] <- rTensor::cp(Tensor, num_components=nfac, max_iter=maxit, tol=ctol))
  # }

  # Attach extra model info
  for(i in 1:nstart){
    models[[i]]$Fac = models[[i]]$U

    # Put variance (stored in lambda) into mode 1.
    for(n in 1:nfac){
      models[[i]]$Fac[[1]][,n] = models[[i]]$Fac[[1]][,n] * models[[i]]$lambdas[n]
    }

    models[[i]]$U = NULL
    models[[i]]$lambdas = NULL

    models[[i]]$SSE = sumsqr((Tensor - models[[i]]$est)@data)
    models[[i]]$varExp = sumsqr(models[[i]]$est@data) / sumsqr(Tensor@data) * 100
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
