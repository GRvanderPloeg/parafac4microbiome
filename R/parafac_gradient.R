#' Calculate gradient of PARAFAC model.
#'
#' @inheritParams parafac_fun
#'
#' @return Gradient of the PARAFAC model.
#' @export
#'
#' @examples
#' A = array(rnorm(108*2), c(108,2))
#' B = array(rnorm(100*2), c(100,2))
#' C = array(rnorm(10*2), c(10,2))
#' X = reinflateTensor(A, B, C)
#' init = initializePARAFAC(X, 2)
#' g = parafac_gradient(init, X)
parafac_gradient = function(x, Tensor){

  if(!methods::is(Tensor, "Tensor")){
    Tensor = rTensor::as.tensor(Tensor)
  }

  if(!methods::is(x, "list")){
    Fac = vect_to_fac(x, Tensor)
  } else{
    Fac = x
  }

  numModes = length(dim(Tensor))
  W = rTensor::as.tensor(!is.na(Tensor@data))
  Xhat = reinflateFac(Fac, Tensor, returnAsTensor=TRUE)
  modes = 1:3
  gradient = list()

  # Multiply X and Xhat preemptively with W
  X = W * Tensor
  Xhat = W * Xhat

  # Gradients per mode stored in a list, will be vectorized at the end.
  for(i in 1:numModes){
    idx = which(modes==i)
    otherModes = modes[-idx]

    gradient[[i]] = array(0L, dim(Fac[[i]]))

    unfoldedX = rTensor::k_unfold(X, idx)
    unfoldedXhat = rTensor::k_unfold(Xhat, idx)

    gradientMode = (unfoldedXhat - unfoldedX)@data %*% multiway::krprod(Fac[[otherModes[2]]], Fac[[otherModes[1]]])
    gradient[[i]] = gradient[[i]] + gradientMode
  }

  g = fac_to_vect(gradient)
  return(g)
}
