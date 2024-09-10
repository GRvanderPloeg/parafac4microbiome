#' Initialize PARAFAC algorithm input vectors
#'
#' @param Tensor Input dataset matrix or tensor
#' @param nfac Number of components to initialize.
#' @param initialization Either "random" for random initialization or "svd" for svd based.
#' @param output Output the initialized components as a Fac object ("Fac", default) or as a vector ("vect").
#'
#' @return Fac or vector with initialized components.
#' @export
#'
#' @examples
#' A = array(rnorm(108,2), c(108,2))
#' B = array(rnorm(100,2), c(100,2))
#' C = array(rnorm(10,2), c(10,2))
#' Tensor = reinflateTensor(A, B, C, returnAsTensor=TRUE)
#' init = initializePARAFAC(Tensor, 2)
initializePARAFAC = function(Tensor, nfac, initialization="random", output="Fac"){

  if(!methods::is(Tensor,"Tensor")){
    Tensor = rTensor::as.tensor(Tensor)
  }

  init = list()
  modes = Tensor@modes
  numModes = length(Tensor@modes)

  if(initialization == "random"){
    for(i in 1:numModes){
      init[[i]] = array(stats::rnorm(modes[i] * nfac), c(modes[i], nfac))
    }
  } else if(initialization == "nvec"){
    for(i in 1:numModes){
      df = rTensor::k_unfold(Tensor, i)@data
      init[[i]] = svd(df, nfac)$u
    }
  }

  if(output=="vect"){
    return(fac_to_vect(init))
  }
  else{
    return(init)
  }
}
