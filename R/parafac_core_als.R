#' Internal PARAFAC alternating least-squares (ALS) core algorithm
#'
#' @param Tensor Tensor data object
#' @param nfac Number of components to compute
#' @param init Initialization from [initializePARAFAC].
#' @param maxit Maximum number of iterations to run (default 500).
#' @param ctol Loss function tolerance for convergence (default 1e-4)
#'
#' @return List containing a Fac object and the loss per iteration
#' @export
#'
#' @examples
#' A = array(rnorm(108*2), c(108,2))
#' B = array(rnorm(100*2), c(100,2))
#' C = array(rnorm(10*2), c(10,2))
#' Tensor = reinflateTensor(A, B, C)
#' init = initializePARAFAC(Tensor, 2)
#' model = parafac_core_als(Tensor, 2, init)
parafac_core_als = function(Tensor, nfac, init, maxit=500, ctol=1e-4){

  if(!methods::is(Tensor,"Tensor")){
    Tensor = rTensor::as.tensor(Tensor)
  }

  modes = Tensor@modes
  numModes = length(Tensor@modes)
  converged = FALSE
  fs = rep(0, maxit+1) # to allow storage of initial loss value
  rel_f = Inf
  tnsr_norm = rTensor::fnorm(Tensor)

  fs[1] = parafac_fun(Tensor, init) # store initial loss value

  # Main ALS loop
  Fac = init
  iteration = 2
  while ((iteration < maxit) && (rel_f > ctol)) {

    for (m in 1:numModes) {
      V = rTensor::hadamard_list(lapply(Fac[-m], function(x) {t(x) %*% x}))
      V_inv = pracma::pinv(V) #solve(V) throws errors when very near convergence
      tmp = rTensor::k_unfold(Tensor, m)@data %*% rTensor::khatri_rao_list(Fac[-m], reverse = TRUE) %*% V_inv
      lambdas = apply(tmp, 2, function(x){norm(as.matrix(x))})
      Fac[[m]] = sweep(tmp, 2, lambdas, "/")
    }

    fs[iteration] = parafac_fun(Tensor, Fac)
    rel_f = abs(fs[iteration]-fs[iteration-1])/tnsr_norm
    iteration = iteration + 1
  }


  # Put variation (the lambdas) in mode 1 (as described by Bro et al.)
  for(n in 1:nfac){
    Fac[[1]][,n] = Fac[[1]][,n] * lambdas[n]
  }

  # Prepare output: fac and loss
  model = list("Fac" = Fac, "fs" = fs[1:(iteration-1)])

  return(model)
}
